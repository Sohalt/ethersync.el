;;; ethersync.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 sohalt
;;
;; Author: sohalt <mail@sohalt.net>
;; Maintainer: sohalt <mail@sohalt.net>
;; Created: August 09, 2024
;; Modified: August 09, 2024
;; Version: 0.0.1
;; Keywords: comm convenience files languages tools
;; Homepage: https://github.com/sohalt/ethersync
;; Package-Requires: ((emacs "29.1") (compat "27.1") (eldoc "1.14.0") (external-completion "0.1") (flymake "1.2.1") (jsonrpc "1.0.24") (project "0.9.8") (seq "2.23") (track-changes "1.2") (xref "1.6.2"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:


(require 'imenu)
(require 'cl-lib)

(require 'url-parse)
(require 'url-util)
(require 'pcase)
(require 'compile) ; for some faces
(require 'warnings)
(require 'filenotify)
(require 'ert)
(require 'text-property-search nil t)
(require 'diff-mode)
(require 'diff)
(require 'track-changes)
(require 'compat)

;; These dependencies are also GNU ELPA core packages.  Because of
;; bug#62576, since there is a risk that M-x package-install, despite
;; having installed them, didn't correctly re-load them over the
;; built-in versions.
(eval-and-compile
  ;; For those packages that are preloaded, reload them if needed,
  ;; since that's the best we can do anyway.
  ;; FIXME: Maybe the ELPA packages for those preloaded packages should
  ;; force-reload themselves eagerly when the package is activated!
  (let ((reload (if (fboundp 'require-with-check) ;Emacs≥30
                    #'require-with-check
                  (lambda (feature &rest _)
                    ;; Just blindly reload like we used to do before
                    ;; `require-with-check'.
                    (load (symbol-name feature) nil 'nomessage)))))

    (funcall reload 'eldoc nil 'reload)
    (funcall reload 'seq nil 'reload)
    ;; For those packages which are not preloaded OTOH, signal an error if
    ;; the loaded file is not the one that should have been loaded.
    (mapc reload '(project flymake xref jsonrpc external-completion))))

;; Keep the eval-when-compile requires at the end, in case it's already been
;; required unconditionally by some earlier `require'.
(eval-when-compile (require 'subr-x))

;; forward-declare, but don't require (Emacs 28 doesn't seem to care)
(defvar markdown-fontify-code-blocks-natively)
(defvar company-backends)
(defvar company-tooltip-align-annotations)
(defvar tramp-ssh-controlmaster-options)
(defvar tramp-use-ssh-controlmaster-options)


;;; User tweakable stuff
(defgroup ethersync nil
  "Interaction with Ethersync servers."
  :prefix "ethersync-"
  :group 'applications)

(defface ethersync-mode-line
  '((t (:inherit font-lock-constant-face :weight bold)))
  "Face for package-name in Ethersync's mode line.")

(defcustom ethersync-autoreconnect 3
  "Control ability to reconnect automatically to the Ethersync server.
If t, always reconnect automatically (not recommended).  If nil,
never reconnect automatically after unexpected server shutdowns,
crashes or network failures.  A positive integer number says to
only autoreconnect if the previous successful connection attempt
lasted more than that many seconds."
  :type '(choice (const :tag "Reconnect automatically" t)
          (const :tag "Never reconnect" nil)
          (integer :tag "Number of seconds")))

(defcustom ethersync-connect-timeout 30
  "Number of seconds before timing out Ethersync connection attempts.
If nil, never time out."
  :type '(choice (number :tag "Number of seconds")
          (const  :tag "Never time out" nil)))

(defcustom ethersync-sync-connect 3
  "Control blocking of Ethersync connection attempts.
If t, block for `ethersync-connect-timeout' seconds.  A positive
integer number means block for that many seconds, and then wait
for the connection in the background.  nil has the same meaning
as 0, i.e. don't block at all."
  :type '(choice (const :tag "Block for `ethersync-connect-timeout' seconds" t)
          (const :tag "Never block" nil)
          (integer :tag "Number of seconds to block")))

(defcustom ethersync-autoshutdown nil
  "If non-nil, shut down server after killing last managed buffer."
  :type 'boolean)

(defcustom ethersync-send-changes-idle-time 0.5
  "Don't tell server of changes before Emacs's been idle for this many seconds."
  :type 'number)

(defcustom ethersync-events-buffer-config
  (list :size (or (bound-and-true-p ethersync-events-buffer-size) 2000000)
        :format 'full)
  "Configure the Ethersync events buffer.

Value is a plist accepting the keys `:size', which controls the
size in characters of the buffer (0 disables, nil means
infinite), and `:format', which controls the shape of each log
entry (`full' includes the original JSON, `lisp' uses
pretty-printed Lisp).

For changes on this variable to take effect, you need to restart
the LSP connection.  That can be done by `ethersync-reconnect'."
  :type '(plist :key-type (symbol :tag "Keyword")
          :options (((const :tag "Size" :size)
                     (choice
                      (const :tag "No limit" nil)
                      (integer :tag "Number of characters")))
                    ((const :tag "Format" :format)
                     (choice
                      (const :tag "Full with original JSON" full)
                      (const :tag "Shortened" short)
                      (const :tag "Pretty-printed lisp" lisp))))))

(defcustom ethersync-menu-string "ethersync"
  "String displayed in mode line when Ethersync is active."
  :type 'string)

;;; Constants
;;;

(defvaralias 'ethersync-{} 'ethersync--{})

(defconst ethersync--{} (make-hash-table :size 0) "The empty JSON object.")

(defconst ethersync--uri-path-allowed-chars
  (let ((vec (copy-sequence url-path-allowed-chars)))
    (aset vec ?: nil) ;; see github#639
    vec)
  "Like `url-path-allowed-chars' but more restrictive.")

;;; Message verification helpers
;;;
(eval-and-compile
  (defvar ethersync--interface-alist
    `(
      (Position ((:line . integer) (:character . integer)))
      (Range (:start :end))
      (Delta (:range :replacement))
      (RevisionedDelta (:delta :revision))

      ;; Editor to Server
      (Open (:uri))
      (Close (:uri))
      (ESCursor (:uri :ranges))

      ;; Server to Editor
      (SECursor (:userid :uri :ranges) ((:name . string)))

      ;; Both directions
      (Edit (:uri :revision :delta)))
    "Alist (INTERFACE-NAME . INTERFACE) of known Ethersync messages.

INTERFACE-NAME is a symbol designated by the spec as
\"interface\".  INTERFACE is a list (REQUIRED OPTIONAL) where
REQUIRED and OPTIONAL are lists of KEYWORD designating field
names that must be, or may be, respectively, present in a message
adhering to that interface.  KEY can be a keyword or a cons (SYM
TYPE), where type is used by `cl-typep' to check types at
runtime.

Here's what an element of this alist might look like:

    (Command ((:title . string) (:command . string)) (:arguments))"))

(eval-and-compile
  (defvar ethersync-strict-mode
    '(;; Uncomment next lines for fun and debugging
      ;; disallow-non-standard-keys
      ;; enforce-required-keys
      ;; enforce-optional-keys
      no-unknown-interfaces)
    "How strictly to check LSP interfaces at compile- and run-time.

Value is a list of symbols (if the list is empty, no checks are
performed).

If the symbol `disallow-non-standard-keys' is present, an error
is raised if any extraneous fields are sent by the server.  At
compile-time, a warning is raised if a destructuring spec
includes such a field.

If the symbol `enforce-required-keys' is present, an error is
raised if any required fields are missing from the message sent
from the server.  At compile-time, a warning is raised if a
destructuring spec doesn't use such a field.

If the symbol `enforce-optional-keys' is present, nothing special
happens at run-time.  At compile-time, a warning is raised if a
destructuring spec doesn't use all optional fields.

If the symbol `disallow-unknown-methods' is present, Ethersync warns
on unknown notifications and errors on unknown requests.

If the symbol `no-unknown-interfaces' is present, Ethersync warns at
compile time if an undeclared Message type is used."))

(cl-defun ethersync--check-object (interface-name
                                   object
                                   &optional
                                   (enforce-required t)
                                   (disallow-non-standard t)
                                   (check-types t))
  "Check that OBJECT conforms to INTERFACE.  Error otherwise."
  (cl-destructuring-bind
      (&key types required-keys optional-keys &allow-other-keys)
      (ethersync--interface interface-name)
    (when-let ((missing (and enforce-required
                             (cl-set-difference required-keys
                                                (ethersync--plist-keys object)))))
      (ethersync--error "A `%s' must have %s" interface-name missing))
    (when-let ((excess (and disallow-non-standard
                            (cl-set-difference
                             (ethersync--plist-keys object)
                             (append required-keys optional-keys)))))
      (ethersync--error "A `%s' mustn't have %s" interface-name excess))
    (when check-types
      (cl-loop
       for (k v) on object by #'cddr
       for type = (or (cdr (assoc k types)) t) ;; FIXME: enforce nil type?
       unless (cl-typep v type)
       do (ethersync--error "A `%s' must have a %s as %s, but has %s"
                            interface-name)))
    t))

(eval-and-compile
  (defun ethersync--keywordize-vars (vars)
    (mapcar (lambda (var) (intern (format ":%s" var))) vars))

  (defun ethersync--ensure-type (k) (if (consp k) k (cons k t)))

  (defun ethersync--interface (interface-name)
    (let* ((interface (assoc interface-name ethersync--interface-alist))
           (required (mapcar #'ethersync--ensure-type (car (cdr interface))))
           (optional (mapcar #'ethersync--ensure-type (cadr (cdr interface)))))
      (list :types (append required optional)
            :required-keys (mapcar #'car required)
            :optional-keys (mapcar #'car optional))))

  (defun ethersync--check-dspec (interface-name dspec)
    "Check destructuring spec DSPEC against INTERFACE-NAME."
    (cl-destructuring-bind (&key required-keys optional-keys &allow-other-keys)
        (ethersync--interface interface-name)
      (cond ((or required-keys optional-keys)
             (let ((too-many
                    (and
                     (memq 'disallow-non-standard-keys ethersync-strict-mode)
                     (cl-set-difference
                      (ethersync--keywordize-vars dspec)
                      (append required-keys optional-keys))))
                   (ignored-required
                    (and
                     (memq 'enforce-required-keys ethersync-strict-mode)
                     (cl-set-difference
                      required-keys (ethersync--keywordize-vars dspec))))
                   (missing-out
                    (and
                     (memq 'enforce-optional-keys ethersync-strict-mode)
                     (cl-set-difference
                      optional-keys (ethersync--keywordize-vars dspec)))))
               (when too-many (byte-compile-warn
                               "Destructuring for %s has extraneous %s"
                               interface-name too-many))
               (when ignored-required (byte-compile-warn
                                       "Destructuring for %s ignores required %s"
                                       interface-name ignored-required))
               (when missing-out (byte-compile-warn
                                  "Destructuring for %s is missing out on %s"
                                  interface-name missing-out))))
            ((memq 'no-unknown-interfaces ethersync-strict-mode)
             (byte-compile-warn "Unknown ethersync interface %s" interface-name))))))

(cl-defmacro ethersync--dbind (vars object &body body)
  "Destructure OBJECT, binding VARS in BODY.
VARS is ([(INTERFACE)] SYMS...)
Honor `ethersync-strict-mode'."
  (declare (indent 2) (debug (sexp sexp &rest form)))
  (let ((interface-name (if (consp (car vars))
                            (car (pop vars))))
        (object-once (make-symbol "object-once"))
        (fn-once (make-symbol "fn-once")))
    (cond (interface-name
           (ethersync--check-dspec interface-name vars)
           `(let ((,object-once ,object))
              (cl-destructuring-bind (&key ,@vars &allow-other-keys) ,object-once
                (ethersync--check-object ',interface-name ,object-once
                                         (memq 'enforce-required-keys ethersync-strict-mode)
                                         (memq 'disallow-non-standard-keys ethersync-strict-mode)
                                         (memq 'check-types ethersync-strict-mode))
                ,@body)))
          (t
           `(let ((,object-once ,object)
                  (,fn-once (lambda (,@vars) ,@body)))
              (if (memq 'disallow-non-standard-keys ethersync-strict-mode)
                  (cl-destructuring-bind (&key ,@vars) ,object-once
                    (funcall ,fn-once ,@vars))
                (cl-destructuring-bind (&key ,@vars &allow-other-keys) ,object-once
                  (funcall ,fn-once ,@vars))))))))

(cl-defmacro ethersync--lambda (cl-lambda-list &body body)
  "Function of args CL-LAMBDA-LIST for processing INTERFACE objects.
Honor `ethersync-strict-mode'."
  (declare (indent 1) (debug (sexp &rest form)))
  (let ((e (cl-gensym "jsonrpc-lambda-elem")))
    `(lambda (,e) (cl-block nil (ethersync--dbind ,cl-lambda-list ,e ,@body)))))

(cl-defmacro ethersync--dcase (obj &rest clauses)
  "Like `pcase', but for the LSP object OBJ.
CLAUSES is a list (DESTRUCTURE FORMS...) where DESTRUCTURE is
treated as in `ethersync--dbind'."
  (declare (indent 1) (debug (sexp &rest (sexp &rest form))))
  (let ((obj-once (make-symbol "obj-once")))
    `(let ((,obj-once ,obj))
       (cond
        ,@(cl-loop
           for (vars . body) in clauses
           for vars-as-keywords = (ethersync--keywordize-vars vars)
           for interface-name = (if (consp (car vars))
                                    (car (pop vars)))
           for condition =
           (cond (interface-name
                  (ethersync--check-dspec interface-name vars)
                  ;; In this mode, in runtime, we assume
                  ;; `ethersync-strict-mode' is partially on, otherwise we
                  ;; can't disambiguate between certain types.
                  `(ignore-errors
                     (ethersync--check-object
                      ',interface-name ,obj-once
                      t
                      (memq 'disallow-non-standard-keys ethersync-strict-mode)
                      t)))
                 (t
                  ;; In this interface-less mode we don't check
                  ;; `ethersync-strict-mode' at all: just check that the object
                  ;; has all the keys the user wants to destructure.
                  `(null (cl-set-difference
                          ',vars-as-keywords
                          (ethersync--plist-keys ,obj-once)))))
           collect `(,condition
                     (cl-destructuring-bind (&key ,@vars &allow-other-keys)
                         ,obj-once
                       ,@body)))
        (t
         (ethersync--error "%S didn't match any of %S"
                           ,obj-once
                           ',(mapcar #'car clauses)))))))

(cl-defmacro ethersync--when-live-buffer (buf &rest body)
  "Check BUF live, then do BODY in it." (declare (indent 1) (debug t))
  (let ((b (cl-gensym)))
    `(let ((,b ,buf)) (if (buffer-live-p ,b) (with-current-buffer ,b ,@body)))))

(cl-defmacro ethersync--when-buffer-window (buf &body body)
  "Check BUF showing somewhere, then do BODY in it." (declare (indent 1) (debug t))
  (let ((b (cl-gensym)))
    `(let ((,b ,buf))
       ;;notice the exception when testing with `ert'
       (when (or (get-buffer-window ,b) (ert-running-test))
         (with-current-buffer ,b ,@body)))))

(cl-defmacro ethersync--widening (&rest body)
  "Save excursion and restriction.  Widen.  Then run BODY." (declare (debug t))
  `(save-excursion (save-restriction (widen) ,@body)))

;;; Public Elisp API
;;;
(cl-defgeneric ethersync-handle-request (server method &rest params)
  "Handle SERVER's METHOD request with PARAMS.")

(cl-defgeneric ethersync-handle-notification (server method &rest params)
  "Handle SERVER's METHOD notification with PARAMS.")

(defclass ethersync-server (jsonrpc-process-connection)
  ((project-nickname
    :documentation "Short nickname for the associated project."
    :accessor ethersync--project-nickname
    :reader ethersync-project-nickname)
   (shutdown-requested
    :initform nil
    :documentation "Flag set when server is shutting down."
    :accessor ethersync--shutdown-requested)
   (project
    :initform nil
    :documentation "Project associated with server."
    :accessor ethersync--project)
   (inhibit-autoreconnect
    :initform t
    :documentation "Generalized boolean inhibiting auto-reconnection if true."
    :accessor ethersync--inhibit-autoreconnect)
   (managed-buffers
    :initform nil
    :documentation "List of buffers managed by server."
    :accessor ethersync--managed-buffers)
   (path-to-buffer
    :documentation "Map (path -> buffer)."
    :initform (make-hash-table :test #'equal)
    :accessor ethersync--path-to-buffer)
   (saved-initargs
    :documentation "Saved initargs for reconnection purposes."
    :accessor ethersync--saved-initargs))
  :documentation
  "Represents a server. Wraps a process for LSP communication.")

(declare-function w32-long-file-name "w32proc.c" (fn))
(defun ethersync-uri-to-path (uri)
  "Convert URI to file path, helped by `ethersync-current-server'."
  (when (keywordp uri) (setq uri (substring (symbol-name uri) 1)))
  (let* ((server (ethersync-current-server))
         (remote-prefix (and server (ethersync--trampish-p server)))
         (url (url-generic-parse-url uri)))
    ;; Only parse file:// URIs, leave other URI untouched as
    ;; `file-name-handler-alist' should know how to handle them
    ;; (bug#58790).
    (if (string= "file" (url-type url))
        (let* ((retval (url-unhex-string (url-filename url)))
               ;; Remove the leading "/" for local MS Windows-style paths.
               (normalized (if (and (not remote-prefix)
                                    (eq system-type 'windows-nt)
                                    (cl-plusp (length retval)))
                               (w32-long-file-name (substring retval 1))
                             retval)))
          (concat remote-prefix normalized))
      uri)))

(cl-defun ethersync-path-to-uri (path &key truenamep)
  "Convert PATH, a file name, to LSP URI string and return it.
TRUENAMEP indicated PATH is already a truename."
  ;; LSP servers should not be expected to access the filesystem, and
  ;; therefore are generally oblivious that some filenames are
  ;; different, but point to the same file, like a symlink and its
  ;; target.  Make sure we hand the server the true name of a file by
  ;; calling file-truename.
  (let ((truepath (if truenamep path (file-truename path))))
    (if (and (url-type (url-generic-parse-url path))
             ;; PATH might be MS Windows file name which includes a
             ;; drive letter that looks like a URL scheme (bug#59338).
             (not (and (eq system-type 'windows-nt)
                       (file-name-absolute-p truepath))))
        ;; PATH is already a URI, so forward it to the LSP server
        ;; untouched.  The server should be able to handle it, since
        ;; it provided this URI to clients in the first place.
        path
      (concat "file://"
              ;; Add a leading "/" for local MS Windows-style paths.
              (if (and (eq system-type 'windows-nt)
                       (not (file-remote-p truepath)))
                  "/")
              (url-hexify-string
               ;; Again watch out for trampy paths.
               (directory-file-name (file-local-name truepath))
               ethersync--uri-path-allowed-chars)))))

(defun ethersync-range-region (range &optional markers)
  "Return a cons (BEG . END) of positions representing LSP RANGE.
If optional MARKERS, make markers instead."
  (let* ((st (plist-get range :start))
         (beg (ethersync--lsp-position-to-point st markers))
         (end (ethersync--lsp-position-to-point (plist-get range :end) markers)))
    (cons beg end)))

;;; Process/server management
(defvar ethersync--server-by-project (make-hash-table :test #'equal)
  "Keys are projects.  Values are processes.")

(defun ethersync-shutdown (server &optional _interactive timeout preserve-buffers)
  "Politely ask SERVER to quit.
Interactively, read SERVER from the minibuffer unless there is
only one and it's managing the current buffer.

Forcefully quit it if it doesn't respond within TIMEOUT seconds.
TIMEOUT defaults to 1.5 seconds.  Don't leave this function with
the server still running.

If PRESERVE-BUFFERS is non-nil (interactively, when called with a
prefix argument), do not kill events and output buffers of
SERVER."
  (interactive (list (ethersync--read-server "Shutdown which server"
                                             (ethersync-current-server))
                     t nil current-prefix-arg))
  (ethersync--message "Asking %s politely to terminate" (jsonrpc-name server))
  (unwind-protect
      (progn
        (setf (ethersync--shutdown-requested server) t)
        (ethersync--request server :shutdown nil :timeout (or timeout 1.5))
        (jsonrpc-notify server :exit nil))
    ;; Now ask jsonrpc.el to shut down the server.
    (jsonrpc-shutdown server (not preserve-buffers))
    (unless preserve-buffers (kill-buffer (jsonrpc-events-buffer server)))))

(defun ethersync-shutdown-all (&optional preserve-buffers)
  "Politely ask all language servers to quit, in order.
PRESERVE-BUFFERS as in `ethersync-shutdown', which see."
  (interactive (list current-prefix-arg))
  (cl-loop for s being the hash-values of ethersync--server-by-project
           do (with-demoted-errors "[ethersync] shutdown all: %s"
                (ethersync-shutdown s nil nil preserve-buffers))))

(defvar ethersync--servers-by-xrefed-file (make-hash-table :test 'equal))

(defun ethersync--on-shutdown (server)
  "Called by jsonrpc.el when SERVER is already dead."
  ;; Turn off `ethersync--managed-mode' where appropriate.
  (dolist (buffer (ethersync--managed-buffers server))
    (let (;; Avoid duplicate shutdowns (github#389)
          (ethersync-autoshutdown nil))
      (ethersync--when-live-buffer buffer (ethersync--managed-mode-off))))
  ;; Sever the project/server relationship for `server'
  (remhash (ethersync--project server) ethersync--server-by-project)
  (cond ((ethersync--shutdown-requested server)
         t)
        ((not (ethersync--inhibit-autoreconnect server))
         (ethersync--warn "Reconnecting after unexpected server exit.")
         (ethersync-reconnect server))
        ((timerp (ethersync--inhibit-autoreconnect server))
         (ethersync--warn "Not auto-reconnecting, last one didn't last long."))))

(defun ethersync--all-major-modes ()
  "Return all known major modes."
  (let ((retval))
    (mapatoms (lambda (sym)
                (when (plist-member (symbol-plist sym) 'derived-mode-parent)
                  (push sym retval))))
    retval))

(defvar ethersync-command-history nil
  "History of CONTACT arguments to `ethersync'.")

(defvar ethersync-lsp-context nil
  "Dynamically non-nil when searching for projects in LSP context.")

(defun ethersync--current-project ()
  "Return a project object for Ethersync's LSP purposes.
This relies on `project-current' and thus on
`project-find-functions'.  Functions in the latter
variable (which see) can query the value `ethersync-lsp-context' to
decide whether a given directory is a project containing a
suitable root directory for a given LSP server's purposes."
  (let ((ethersync-lsp-context t))
    (or (project-current)
        `(transient . ,(expand-file-name default-directory)))))

(cl-defmethod project-root ((project (head ethersync--project)))
  (cadr project))

;;;###autoload
(defun ethersync (project class contact)
  "Start Ethersync for PROJECT's buffers under MANAGED-MAJOR-MODES.

This starts a Language Server Protocol (LSP) server suitable for
the buffers of PROJECT whose `major-mode' is among
MANAGED-MAJOR-MODES.  CLASS is the class of the LSP server to
start and CONTACT specifies how to connect to the server.

PROJECT is guessed from `project-find-functions'.  The search for active
projects in this context binds `ethersync-lsp-context' (which see).

PROJECT is a project object as returned by `project-current'."
  (interactive
   (let ((current-server (ethersync-current-server)))
     (unless (or (null current-server)
                 (y-or-n-p "\
[ethersync] Shut down current connection before attempting new one?"))
       (user-error "[ethersync] Connection attempt aborted by user"))
     (prog1 (ethersync--guess-contact t)
       (when current-server (ignore-errors (ethersync-shutdown current-server))))))
  (ethersync--connect project class contact))

(defun ethersync-reconnect (server &optional interactive)
  "Reconnect to SERVER.
INTERACTIVE is t if called interactively."
  (interactive (list (ethersync--current-server-or-lose) t))
  (when (jsonrpc-running-p server)
    (ignore-errors (ethersync-shutdown server interactive nil 'preserve-buffers)))
  (ethersync--connect (ethersync--project server)
                      (eieio-object-class-name server)
                      (ethersync--saved-initargs server))
  (ethersync--message "Reconnected!"))

(defvar ethersync--managed-mode) ; forward decl

(defun ethersync--guess-contact (&optional interactive)
  `(,(project-current) ethersync-server ("ethersync" "client")))

;;;###autoload
(ignore
 (defun ethersync-ensure ()
   "Start Ethersync session for current buffer if there isn't one.

Only use this function (in major mode hooks, etc) if you are
confident that Ethersync can be started safely and efficiently for
*every* buffer visited where these hooks may execute.

Since it is difficult to establish this confidence fully, it's
often wise to use the interactive command `ethersync' instead.  This
command only needs to be invoked once per project, as all other
files of a given major mode visited within the same project will
automatically become managed with no further user intervention
needed."
   (let ((buffer (current-buffer)))
     (cl-labels
         ((maybe-connect
            ()
            (ethersync--when-live-buffer buffer
              (remove-hook 'post-command-hook #'maybe-connect t)
              (unless ethersync--managed-mode
                (condition-case-unless-debug oops
                    (apply #'ethersync--connect (ethersync--guess-contact))
                  (error (ethersync--warn (error-message-string oops))))))))
       (when buffer-file-name
         (add-hook 'post-command-hook #'maybe-connect 'append t))))))

(defun ethersync-events-buffer (server)
  "Display events buffer for SERVER.
Use current server's or first available Ethersync events buffer."
  (interactive (list (ethersync-current-server)))
  (let ((buffer (if server (jsonrpc-events-buffer server)
                  (cl-find "\\*ETHERSYNC.*events\\*"
                           (buffer-list)
                           :key #'buffer-name :test #'string-match))))
    (if buffer (display-buffer buffer)
      (ethersync--error "Can't find an Ethersync events buffer!"))))

(defun ethersync-stderr-buffer (server)
  "Display stderr buffer for SERVER."
  (interactive (list (ethersync--current-server-or-lose)))
  (display-buffer (jsonrpc-stderr-buffer server)))

(defun ethersync-forget-pending-continuations (server)
  "Forget pending requests for SERVER."
  (interactive (list (ethersync--current-server-or-lose)))
  (jsonrpc-forget-pending-continuations server))

(defvar ethersync-connect-hook
  '()
  "Hook run after connecting in `ethersync--connect'.")

(defvar ethersync-server-initialized-hook
  '()
  "Hook run after a `ethersync-server' instance is created.

That is before a connection was established.  Use
`ethersync-connect-hook' to hook into when a connection was
successfully established and the server on the other side has
received the initializing configuration.

Each function is passed the server as an argument")

(defun ethersync--cmd (contact)
  "Helper for `ethersync--connect'."
  (if (file-remote-p default-directory)
      ;; TODO: this seems like a bug, although it’s everywhere. For
      ;; some reason, for remote connections only, over a pipe, we
      ;; need to turn off line buffering on the tty.
      ;;
      ;; Not only does this seem like there should be a better way,
      ;; but it almost certainly doesn’t work on non-unix systems.
      (list shell-file-name "-c"
            (string-join (cons "stty raw > /dev/null;"
                               (mapcar #'shell-quote-argument contact))
                         " "))
    contact))

(defvar-local ethersync--cached-server nil
  "A cached reference to the current Ethersync server.")

(defun ethersync--connect (project class contact)
  "Connect to MANAGED-MODES, LANGUAGE-IDS, PROJECT, CLASS and CONTACT.
This docstring appeases checkdoc, that's all."
  (let* ((default-directory (project-root project))
         (nickname (project-name project))
         (readable-name (format "ETHERSYNC (%s)" nickname))
         server-info
         (contact (if (functionp contact) (funcall contact) contact))
         (initargs
          (cond ((stringp (car contact))
                 (let* ((probe (cl-position-if #'keywordp contact))
                        (more-initargs (and probe (cl-subseq contact probe)))
                        (contact (cl-subseq contact 0 probe)))
                   `(:process
                     ,(lambda ()
                        (let ((default-directory default-directory)
                              ;; bug#61350: Tramp turns on a feature
                              ;; by default that can't (yet) handle
                              ;; very much data so we turn it off
                              ;; unconditionally -- just for our
                              ;; process.
                              (tramp-use-ssh-controlmaster-options 'suppress)
                              (tramp-ssh-controlmaster-options
                               "-o ControlMaster=no -o ControlPath=none"))
                          (make-process
                           :name readable-name
                           :command (setq server-info (ethersync--cmd contact))
                           :connection-type 'pipe
                           :coding 'utf-8-emacs-unix
                           :noquery t
                           :stderr (get-buffer-create
                                    (format "*%s stderr*" readable-name))
                           :file-handler t)))
                     ,@more-initargs)))))
         (spread (lambda (fn) (lambda (server method params)
                                (let ((ethersync--cached-server server))
                                  (apply fn server method (append params nil))))))
         (server
          (apply
           #'make-instance class
           :name readable-name
           :events-buffer-config ethersync-events-buffer-config
           :notification-dispatcher (funcall spread #'ethersync-handle-notification)
           :request-dispatcher (funcall spread #'ethersync-handle-request)
           :on-shutdown #'ethersync--on-shutdown
           initargs)))
    (when server-info
      (jsonrpc--debug server "Running ethersync client: %s"
                      (string-join server-info " ")))
    (setf (ethersync--saved-initargs server) initargs)
    (setf (ethersync--project server) project)
    (setf (ethersync--project-nickname server) nickname)
    (run-hook-with-args 'ethersync-server-initialized-hook server)
    ;; Now start the handshake.  To honor `ethersync-sync-connect'
    ;; maybe-sync-maybe-async semantics we use `jsonrpc-async-request'
    ;; and mimic most of `jsonrpc-request'.
    (map-put! ethersync--server-by-project project server)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        ;; No need to pass SERVER as an argument: it has
        ;; been registered in `ethersync--server-by-project',
        ;; so that it can be found (and cached) from
        ;; `ethersync--maybe-activate-editing-mode' in any
        ;; managed buffer.
        (ethersync--maybe-activate-editing-mode)))
    (setf (ethersync--inhibit-autoreconnect server)
          (cond
           ((booleanp ethersync-autoreconnect)
            (not ethersync-autoreconnect))
           ((cl-plusp ethersync-autoreconnect)
            (run-with-timer
             ethersync-autoreconnect nil
             (lambda ()
               (setf (ethersync--inhibit-autoreconnect server)
                     (null ethersync-autoreconnect)))))))
    (run-hook-with-args 'ethersync-connect-hook server)
    (ethersync--message
     "Connected! Server `%s' now managing buffers in project `%s'."
     (jsonrpc-name server)
     (ethersync-project-nickname server))))

;;; Helpers (move these to API?)
;;;
(defun ethersync--error (format &rest args)
  "Error out with FORMAT with ARGS."
  (error "[ethersync] %s" (apply #'format format args)))

(defun ethersync--message (format &rest args)
  "Message out with FORMAT with ARGS."
  (message "[ethersync] %s" (apply #'format format args)))

(defun ethersync--warn (format &rest args)
  "Warning message with FORMAT and ARGS."
  (apply #'ethersync--message (concat "(warning) " format) args)
  (let ((warning-minimum-level :error))
    (display-warning 'ethersync (apply #'format format args) :warning)))

(defalias 'ethersync--bol
  (if (fboundp 'pos-bol) #'pos-bol
    (lambda (&optional n) (let ((inhibit-field-text-motion t))
                            (line-beginning-position n))))
  "Return position of first character in current line.")

(cl-defun ethersync--request (server method params &key
                                     immediate
                                     timeout cancel-on-input
                                     cancel-on-input-retval)
  "Like `jsonrpc-request', but for Ethersync requests.
Unless IMMEDIATE, send pending changes before making request."
  (unless immediate (ethersync--signal-edit))
  (jsonrpc-request server method params
                   :timeout timeout
                   :cancel-on-input cancel-on-input
                   :cancel-on-input-retval cancel-on-input-retval))

;;; Encoding fever
;;;
(defvar ethersync-current-linepos-function #'ethersync-utf-32-linepos
  "Function calculating position relative to line beginning.

It is a function of no arguments considering the text from line
beginning up to current point.  The return value is the number of
UTF code units needed to encode that text from the LSP server's
perspective.  This may be a number of octets, 16-bit words or
Unicode code points, depending on whether the LSP server's
`positionEncoding' capability is UTF-8, UTF-16 or UTF-32,
respectively.  Position of point should remain unaltered if that
return value is fed through the corresponding inverse function
`ethersync-move-to-linepos-function' (which see).")

(defun ethersync-utf-32-linepos ()
  "Calculate number of Unicode codepoints from line beginning."
  (- (point) (ethersync--bol)))

(defun ethersync--pos-to-lsp-position (&optional pos)
  "Convert point POS to LSP position."
  (ethersync--widening
   ;; LSP line is zero-origin; emacs is one-origin.
   (list :line (1- (line-number-at-pos pos t))
         :character (progn (when pos (goto-char pos))
                           (funcall ethersync-current-linepos-function)))))

(defun ethersync--virtual-pos-to-lsp-position (pos string)
  "Return the LSP position at the end of STRING if it were inserted at POS."
  (ethersync--widening
   (goto-char pos)
   (forward-line 0)
   ;; LSP line is zero-origin; Emacs is one-origin.
   (let ((posline (1- (line-number-at-pos nil t)))
         (linebeg (buffer-substring (point) pos))
         (colfun ethersync-current-linepos-function))
     ;; Use a temp buffer because:
     ;; - I don't know of a fast way to count newlines in a string.
     ;; - We currently don't have `ethersync-current-linepos-function' for strings.
     (with-temp-buffer
       (insert linebeg string)
       (goto-char (point-max))
       (list :line (+ posline (1- (line-number-at-pos nil t)))
             :character (funcall colfun))))))

(defvar ethersync-move-to-linepos-function #'ethersync-move-to-utf-32-linepos
  "Function to move to a position within a line reported by the LSP server.

Per the LSP spec, character offsets in LSP Position objects count
UTF-16 code units, not actual code points.  So when LSP says
position 3 of a line containing just \"aXbc\", where X is a funny
looking character in the UTF-16 \"supplementary plane\", it
actually means `b', not `c'.  The default value
`ethersync-move-to-utf-16-linepos' accounts for this.

This variable can also be set to `ethersync-move-to-utf-8-linepos' or
`ethersync-move-to-utf-32-linepos' for servers not closely following
the spec.  Also, since LSP 3.17 server and client may agree on an
encoding and Ethersync will set this variable automatically.")

(defun ethersync-move-to-utf-32-linepos (n)
  "Move to line's Nth codepoint as computed by LSP's UTF-32 criterion."
  ;; We cannot use `move-to-column' here, because it moves to *visual*
  ;; columns, which can be different from LSP characters in case of
  ;; `whitespace-mode', `prettify-symbols-mode', etc.  (github#296,
  ;; github#297)
  (goto-char (min (+ (ethersync--bol) n) (line-end-position))))

(defun ethersync--lsp-position-to-point (pos-plist &optional marker)
  "Convert LSP position POS-PLIST to Emacs point.
If optional MARKER, return a marker instead"
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (forward-line (min most-positive-fixnum
                         (plist-get pos-plist :line)))
      (unless (eobp) ;; if line was excessive leave point at eob
        (let ((col (plist-get pos-plist :character)))
          (unless (wholenump col)
            (ethersync--warn
             "Caution: LSP server sent invalid character position %s. Using 0 instead."
             col)
            (setq col 0))
          (funcall ethersync-move-to-linepos-function col)))
      (if marker (copy-marker (point-marker)) (point)))))

;;; More helpers
(defconst ethersync--uri-path-allowed-chars
  (let ((vec (copy-sequence url-path-allowed-chars)))
    (aset vec ?: nil) ;; see github#639
    vec)
  "Like `url-path-allowed-chars' but more restrictive.")

(defun ethersync--snippet-expansion-fn ()
  "Compute a function to expand snippets.
Doubles as an indicator of snippet support."
  (and (fboundp 'yas-minor-mode)
       (lambda (&rest args)
         (with-no-warnings
           (unless (bound-and-true-p yas-minor-mode) (yas-minor-mode 1))
           (apply #'yas-expand-snippet args)))))

(defun ethersync--format-markup (markup)
  "Format MARKUP according to LSP's spec."
  (pcase-let ((`(,string ,mode)
               (if (stringp markup) (list markup 'gfm-view-mode)
                 (list (plist-get markup :value)
                       (pcase (plist-get markup :kind)
                         ("markdown" 'gfm-view-mode)
                         ("plaintext" 'text-mode)
                         (_ major-mode))))))
    (with-temp-buffer
      (setq-local markdown-fontify-code-blocks-natively t)
      (insert string)
      (let ((inhibit-message t)
            (message-log-max nil)
            match)
        (ignore-errors (delay-mode-hooks (funcall mode)))
        (font-lock-ensure)
        (goto-char (point-min))
        (let ((inhibit-read-only t))
          (while (setq match (text-property-search-forward 'invisible))
            (delete-region (prop-match-beginning match)
                           (prop-match-end match))))
        (string-trim (buffer-string))))))

(defun ethersync--read-server (prompt &optional dont-if-just-the-one)
  "Read a running Ethersync server from minibuffer using PROMPT.
If DONT-IF-JUST-THE-ONE and there's only one server, don't prompt
and just return it.  PROMPT shouldn't end with a question mark."
  (let ((servers (cl-loop for server
                          being hash-values of ethersync--server-by-project
                          collect server))
        (name (lambda (srv)
                (ethersync-project-nickname srv))))
    (cond ((null servers)
           (ethersync--error "No servers!"))
          ((or (cdr servers) (not dont-if-just-the-one))
           (let* ((default (when-let ((current (ethersync-current-server)))
                             (funcall name current)))
                  (read (completing-read
                         (if default
                             (format "%s (default %s)? " prompt default)
                           (concat prompt "? "))
                         (mapcar name servers)
                         nil t
                         nil nil
                         default)))
             (cl-find read servers :key name :test #'equal)))
          (t (car servers)))))

(defun ethersync--trampish-p (server)
  "Tell if SERVER's project root is `file-remote-p'."
  (file-remote-p (project-root (ethersync--project server))))

(defun ethersync--plist-keys (plist) "Get keys of a plist."
       (cl-loop for (k _v) on plist by #'cddr collect k))

(defalias 'ethersync--ensure-list
  (if (fboundp 'ensure-list) #'ensure-list
    (lambda (x) (if (listp x) x (list x)))))


;;; Minor modes
;;;
(defvar ethersync-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap display-local-help] #'eldoc-doc-buffer)
    map))

(defvar-local ethersync--saved-bindings nil
  "Bindings saved by `ethersync--setq-saving'.")

(defvar ethersync-stay-out-of '()
  "List of Emacs things that Ethersync should try to stay of.
Each element is a string, a symbol, or a regexp which is matched
against a variable's name.  Examples include the string
\"company\" or the symbol `xref'.

Before Ethersync starts \"managing\" a particular buffer, it
opinionatedly sets some peripheral Emacs facilities, such as
Flymake, Xref and Company.  These overriding settings help ensure
consistent Ethersync behavior and only stay in place until
\"managing\" stops (usually via `ethersync-shutdown'), whereupon the
previous settings are restored.

However, if you wish for Ethersync to stay out of a particular Emacs
facility that you'd like to keep control of add an element to
this list and Ethersync will refrain from setting it.

For example, to keep your Company customization, add the symbol
`company' to this variable.")

(defun ethersync--stay-out-of-p (symbol)
  "Tell if Ethersync should stay out of SYMBOL."
  (cl-find (symbol-name symbol) ethersync-stay-out-of
           :test (lambda (s thing)
                   (let ((re (if (symbolp thing) (symbol-name thing) thing)))
                     (string-match re s)))))

(defmacro ethersync--setq-saving (symbol binding)
  `(unless (or (not (boundp ',symbol)) (ethersync--stay-out-of-p ',symbol))
     (push (cons ',symbol (symbol-value ',symbol)) ethersync--saved-bindings)
     (setq-local ,symbol ,binding)))

(defun ethersync-managed-p ()
  "Tell if current buffer is managed by Ethersync."
  ethersync--managed-mode)

(defvar ethersync-managed-mode-hook nil
  "A hook run by Ethersync after it started/stopped managing a buffer.
Use `ethersync-managed-p' to determine if current buffer is managed.")

(defvar-local ethersync--track-changes nil)

(make-variable-buffer-local 'ethersync-last-post-command-position)

(defun ethersync--track-cursor ()
  (unless (equal (point) ethersync-last-post-command-position)
    (let* ((server (ethersync--current-server-or-lose))
           (lsp-pos (ethersync--pos-to-lsp-position (point))))
      (message "move to %s" lsp-pos)
      (jsonrpc-notify
       ;; server
       (ethersync--current-server-or-lose)
       :cursor
       `(:uri ,(ethersync--TextDocumentIdentifier)
         :ranges [(:start ,lsp-pos :end ,lsp-pos)]))
      (setq ethersync--recent-changes nil)
      (jsonrpc--call-deferred server))
    (setq ethersync-last-post-command-position (point))))

(define-minor-mode ethersync--managed-mode
  "Mode for source buffers managed by some Ethersync project."
  :init-value nil :lighter nil :keymap ethersync-mode-map :interactive nil
  (cond
   (ethersync--managed-mode
    (ethersync--setq-saving ethersync-current-linepos-function #'ethersync-utf-32-linepos)
    (ethersync--setq-saving ethersync-move-to-linepos-function #'ethersync-move-to-utf-32-linepos)
    (unless ethersync--track-changes
      (setq ethersync--track-changes
            (track-changes-register
             #'ethersync--track-changes-signal :disjoint t)))
    (add-hook 'kill-buffer-hook #'ethersync--managed-mode-off nil t)
    ;; Prepend "didClose" to the hook after the "nonoff", so it will run first
    (add-hook 'kill-buffer-hook #'ethersync--signal-close nil t)
    (add-hook 'before-revert-hook #'ethersync--signal-close nil t)
    (add-hook 'after-revert-hook #'ethersync--after-revert-hook nil t)
    (add-hook 'change-major-mode-hook #'ethersync--managed-mode-off nil t)
    (add-hook 'post-command-hook #'ethersync--track-cursor nil t)
    (cl-pushnew (current-buffer) (ethersync--managed-buffers (ethersync-current-server))))
   (t
    (remove-hook 'kill-buffer-hook #'ethersync--managed-mode-off t)
    (remove-hook 'kill-buffer-hook #'ethersync--signal-close t)
    (remove-hook 'before-revert-hook #'ethersync--signal-close t)
    (remove-hook 'after-revert-hook #'ethersync--after-revert-hook t)
    (remove-hook 'change-major-mode-hook #'ethersync--managed-mode-off t)
    (remove-hook 'post-command-hook #'ethersync--track-cursor t)
    (cl-loop for (var . saved-binding) in ethersync--saved-bindings
             do (set (make-local-variable var) saved-binding))
    (run-hooks 'ethersync-managed-mode-hook)
    (let ((server ethersync--cached-server))
      (setq ethersync--cached-server nil)
      (when server
        (setf (ethersync--managed-buffers server)
              (delq (current-buffer) (ethersync--managed-buffers server)))
        (when (and ethersync-autoshutdown
                   (null (ethersync--managed-buffers server)))
          (ethersync-shutdown server))))
    (when ethersync--track-changes
      (track-changes-unregister ethersync--track-changes)
      (setq ethersync--track-changes nil)))))

(defun ethersync--managed-mode-off ()
  "Turn off `ethersync--managed-mode' unconditionally."
  (remove-overlays nil nil 'ethersync--overlay t)
  (ethersync--managed-mode -1))

(defun ethersync-current-server ()
  "Return logical Ethersync server for current buffer, nil if none."
  (setq ethersync--cached-server
        (or ethersync--cached-server
            (map-elt ethersync--server-by-project (ethersync--current-project)))))

(defun ethersync--current-server-or-lose ()
  "Return current logical Ethersync server connection or error."
  (or (ethersync-current-server)
      (jsonrpc-error "No current JSON-RPC connection")))

(defvar-local ethersync--diagnostics nil
  "Flymake diagnostics for this buffer.")

(defvar revert-buffer-preserve-modes)
(defun ethersync--after-revert-hook ()
  "Ethersync's `after-revert-hook'."
  (when revert-buffer-preserve-modes (ethersync--signal-open)))

(defun ethersync--maybe-activate-editing-mode ()
  "Maybe activate `ethersync--managed-mode'.

If it is activated, also signal 'open'."
  (unless ethersync--managed-mode
    ;; Called when `revert-buffer-in-progress-p' is t but
    ;; `revert-buffer-preserve-modes' is nil.
    (let ((server (ethersync-current-server)))
      (when (and buffer-file-name server)
        (setq ethersync--diagnostics nil)
        (ethersync--managed-mode)
        (ethersync--signal-open)
        (map-put! (ethersync--path-to-buffer server) buffer-file-name (current-buffer))
        ;; Run user hook after 'open' so server knows
        ;; about the buffer.
        (run-hooks 'ethersync-managed-mode-hook)))))

(add-hook 'after-change-major-mode-hook #'ethersync--maybe-activate-editing-mode)

(defun ethersync-clear-status (server)
  "Clear the last JSONRPC error for SERVER."
  (interactive (list (ethersync--current-server-or-lose)))
  (setf (jsonrpc-last-error server) nil))


;;; Mode-line, menu and other sugar
;;;
(defvar ethersync--mode-line-format `(:eval (ethersync--mode-line-format)))

(put 'ethersync--mode-line-format 'risky-local-variable t)

(defun ethersync--mouse-call (what &optional update-mode-line)
  "Make an interactive lambda for calling WHAT with the mouse."
  (lambda (event)
    (interactive "e")
    (let ((start (event-start event))) (with-selected-window (posn-window start)
                                         (save-excursion
                                           (goto-char (or (posn-point start)
                                                          (point)))
                                           (call-interactively what)
                                           (when update-mode-line
                                             (force-mode-line-update t)))))))

(defun ethersync-manual () "Read Ethersync's manual."
       (declare (obsolete info "1.10"))
       (interactive) (info "(ethersync)"))

;;;###autoload
(defun ethersync-upgrade-ethersync (&rest _) "Update Ethersync to latest version."
       (interactive)
       (with-no-warnings
         (require 'package)
         (unless package-archive-contents (package-refresh-contents))
         (when-let ((existing (cadr (assoc 'ethersync package-alist))))
           (package-delete existing t))
         (package-install (cadr (assoc 'ethersync package-archive-contents)))))

(easy-menu-define ethersync-server-menu nil "Monitor server communication."
  '("Debugging the server communication"
    ["Reconnect to server" ethersync-reconnect]
    ["Quit server" ethersync-shutdown]
    "--"
    ["Ethersync events buffer" ethersync-events-buffer]
    ["Ethersync stderr buffer" ethersync-stderr-buffer]
    ["Customize event buffer size"
     (lambda ()
       (interactive)
       (customize-variable 'ethersync-events-buffer-size))]))

(defun ethersync--mode-line-props (thing face defs &optional prepend)
  "Helper for function `ethersync--mode-line-format'.
Uses THING, FACE, DEFS and PREPEND."
  (cl-loop with map = (make-sparse-keymap)
           for (elem . rest) on defs
           for (key def help) = elem
           do (define-key map `[mode-line ,key] (ethersync--mouse-call def t))
           concat (format "%s: %s" key help) into blurb
           when rest concat "\n" into blurb
           finally (return `(:propertize ,thing
                             face ,face
                             keymap ,map help-echo ,(concat prepend blurb)
                             mouse-face mode-line-highlight))))

(defun ethersync--mode-line-format ()
  "Compose Ethersync's mode-line."
  (let* ((server (ethersync-current-server))
         (nick (and server (ethersync-project-nickname server)))
         (last-error (and server (jsonrpc-last-error server))))
    (append
     `(,(propertize
         ethersync-menu-string
         'face 'ethersync-mode-line
         'mouse-face 'mode-line-highlight
         'help-echo "Ethersync"))
     (when nick
       `(":"
         ,(propertize
           nick
           'face 'ethersync-mode-line
           'mouse-face 'mode-line-highlight
           'help-echo (format "Project '%s'\nmouse-1: Ethersync control menu" nick)
           'keymap (let ((map (make-sparse-keymap)))
                     (define-key map [mode-line down-mouse-1] ethersync-server-menu)
                     map))
         ,@(when last-error
             `("/" ,(ethersync--mode-line-props
                     "error" 'compilation-mode-line-fail
                     '((mouse-3 ethersync-clear-status  "Clear this status"))
                     (format "An error occurred: %s\n" (plist-get last-error
                                                                  :message))))))))))

(add-to-list 'mode-line-misc-info
             `(ethersync--managed-mode (" [" ethersync--mode-line-format "] ")))

;;; Protocol implementation (Requests, notifications, etc)
;;;
(defun ethersync-replace-range (range replacement)
  "Replace the region specified by line and column START-LINE, START-COL, END-LINE, and END-COL with REPLACEMENT."
  (let* ((start (plist-get range :start))
         (end (plist-get range :end))
         (start-line (plist-get start :line))
         (start-char (plist-get start :character))
         (end-line (plist-get end :line))
         (end-char (plist-get end :character))
         ;; Convert line/character to buffer positions
         (start-pos (save-excursion
                      (goto-char (point-min))
                      (forward-line (1- start-line))
                      (forward-char start-char)
                      (point))))
    (save-excursion
      (save-excursion
        (goto-char (point-min))
        (forward-line (1- end-line))
        (forward-char end-char)
        (delete-region start-pos (point))
        (insert replacement)))))

(cl-defun ethersync--apply-text-edits (edits &optional version silent)
  "Apply EDITS for current buffer if at VERSION, or if it's nil.
If SILENT, don't echo progress in mode-line."
  (unless edits (cl-return-from ethersync--apply-text-edits))
  (unless (or (not version) (equal version ethersync--versioned-identifier))
    (jsonrpc-error "Edits on `%s' require version %d, you have %d"
                   (current-buffer) version ethersync--versioned-identifier))
  (atomic-change-group
    (let* ((change-group (prepare-change-group))
           (howmany (length edits))
           (reporter (unless silent
                       (make-progress-reporter
                        (format "[ethersync] applying %s edits to `%s'..."
                                howmany (current-buffer))
                        0 howmany)))
           (done 0))
      (mapc (pcase-lambda (`(,newText ,beg . ,end))
              (let ((source (current-buffer)))
                (with-temp-buffer
                  (insert newText)
                  (let ((temp (current-buffer)))
                    (with-current-buffer source
                      (save-excursion
                        (save-restriction
                          (narrow-to-region beg end)
                          (replace-buffer-contents temp)))
                      (when reporter
                        (ethersync--reporter-update reporter (cl-incf done))))))))
            (mapcar (ethersync--lambda ((Delta) range replacement)
                      (cons replacement (ethersync-range-region range 'markers)))
                    (reverse edits)))
      (undo-amalgamate-change-group change-group)
      (when reporter
        (progress-reporter-done reporter)))))

(cl-defmethod ethersync-handle-notification
  (_server (_method (eql edit)) &key uri delta revision)
  "Handle notification edit."
  (let* ((server (ethersync--current-server-or-lose))
         (buffer (map-elt (ethersync--path-to-buffer server) (ethersync-uri-to-path uri))))
    (with-current-buffer buffer
      (ethersync--apply-text-edits delta revision))))

(cl-defmethod ethersync-handle-notification
  (_server method &key &allow-other-keys)
  "Handle unknown notification."
  (unless (or (string-prefix-p "$" (format "%s" method))
              (not (memq 'disallow-unknown-methods ethersync-strict-mode)))
    (ethersync--warn "Server sent unknown notification method `%s'" method)))

(cl-defmethod ethersync-handle-request
  (_server method &key &allow-other-keys)
  "Handle unknown request."
  (when (memq 'disallow-unknown-methods ethersync-strict-mode)
    (jsonrpc-error "Unknown request method `%s'" method)))

(defface ethersync-cursor-face
  '((t (:inverse-video t)))
  "The face used for fake cursors"
  :group 'ethersync)

(defface ethersync-cursor-bar-face
  `((t (:height 1 :background ,(face-attribute 'cursor :background))))
  "The face used for fake cursors if the cursor-type is bar"
  :group 'ethersync)

(defface ethersync-region-face
  '((t :inherit region))
  "The face used for fake regions"
  :group 'ethersync)

(defface ethersync-cursor-name-face
  '((t (:height 0.8 :inherit shadow)))
  "Face used for cursor name overlays."
  :group 'ethersync)

(defcustom ethersync-match-cursor-style t
  "If non-nil, attempt to match the cursor style that the user
has selected.  Namely, use vertical bars the user has configured
Emacs to use that cursor.

If nil, just use standard rectangle cursors for all fake cursors.

In some modes/themes, the bar fake cursors are either not
rendered or shift text."
  :type '(boolean)
  :group 'ethersync)

(defun ethersync-cursor-is-bar ()
  "Return non-nil if the cursor is a bar."
  (let ((cursor-type
         (if (eq cursor-type t)
             (frame-parameter nil 'cursor-type)
           cursor-type)))
    (or (eq cursor-type 'bar)
        (and (listp cursor-type)
             (eq (car cursor-type) 'bar)))))

(defun ethersync-make-cursor-overlay-at-eol (pos)
  "Create overlay to look like cursor at end of line."
  (let ((overlay (make-overlay pos pos nil nil nil)))
    (if (and ethersync-match-cursor-style (ethersync-cursor-is-bar))
        (overlay-put overlay 'before-string (propertize "|" 'face 'ethersync-cursor-bar-face))
      (overlay-put overlay 'after-string (propertize " " 'face 'ethersync-cursor-face)))
    overlay))

(defun ethersync-make-cursor-overlay-inline (pos)
  "Create overlay to look like cursor inside text."
  (let ((overlay (make-overlay pos (1+ pos) nil nil nil)))
    (if (and ethersync-match-cursor-style (ethersync-cursor-is-bar))
        (overlay-put overlay 'before-string (propertize "|" 'face 'ethersync-cursor-bar-face))
      (overlay-put overlay 'face 'ethersync-cursor-face))
    overlay))

(defun ethersync-make-cursor-overlay-at-point ()
  "Create overlay to look like cursor.
Special case for end of line, because overlay over a newline
highlights the entire width of the window."
  (if (eolp)
      (ethersync-make-cursor-overlay-at-eol (point))
    (ethersync-make-cursor-overlay-inline (point))))

(defun ethersync-make-region-overlay-between-point-and-mark ()
  "Create overlay to look like active region."
  (let ((overlay (make-overlay (mark) (point) nil nil t)))
    (overlay-put overlay 'face 'ethersync-region-face)
    (overlay-put overlay 'type 'additional-region)
    overlay))

(defun highlight-ranges (ranges)
  "Highlight multiple RANGES in the current buffer.
Each range should be of the form:
  '(:start (:character START-CHAR :line START-LINE)
    :end (:character END-CHAR :line END-LINE))."
  (interactive)
  (dotimes (i (length ranges))
    (let* ((range (aref ranges i))
           (start (plist-get range :start))
           (end (plist-get range :end))
           (start-line (plist-get start :line))
           (start-char (plist-get start :character))
           (end-line (plist-get end :line))
           (end-char (plist-get end :character))
           ;; Convert line/character to buffer positions
           (start-pos (save-excursion
                        (goto-char (point-min))
                        (forward-line (1- start-line))
                        (forward-char start-char)
                        (point)))
           (end-pos (save-excursion
                      (goto-char (point-min))
                      (forward-line (1- end-line))
                      (forward-char end-char)
                      (point))))
      ;; Create the overlay and set its face
      (let* ((overlay (make-overlay start-pos end-pos)))
        (overlay-put overlay 'after-string (propertize " " 'face 'ethersync-cursor-face))
        (ethersync--message "start-pos=%s,end-pos=%s,buffer=%s" start-pos end-pos (current-buffer))))))

(cl-defmethod ethersync-handle-notification
  (_server (_method (eql cursor)) &key userid uri ranges name)
  "Handle notification cursor."
  (let* ((server (ethersync--current-server-or-lose))
         (buffer (map-elt (ethersync--path-to-buffer server) (ethersync-uri-to-path uri))))
    (with-current-buffer buffer (highlight-ranges ranges)))
  (ethersync--message "Received cursor (userid=%s, uri=%s, name=%s, ranges=%s)"
                      userid uri name ranges))

(defvar-local ethersync--TextDocumentIdentifier-cache nil
  "LSP TextDocumentIdentifier-related cached info for current buffer.
Value is (TRUENAME . (:uri STR)), where STR is what is sent to the
server on open and similar calls.  TRUENAME is the
expensive cached value of `file-truename'.")

(defvar-local ethersync--versioned-identifier 0)

(defvar-local ethersync--recent-changes nil
  "Recent buffer changes as collected by `ethersync--track-changes-fetch'.")

(cl-defmethod jsonrpc-connection-ready-p ((_server ethersync-server) _what)
  "Tell if SERVER is ready for WHAT in current buffer."
  (and (cl-call-next-method) (not ethersync--recent-changes)))

(defvar-local ethersync--change-idle-timer nil "Idle timer for edit signals.")

(defvar ethersync--document-changed-hook '(ethersync--signal-edit)
  "Internal hook for doing things when the document changes.")

(defun ethersync--track-changes-fetch (id)
  (if (eq ethersync--recent-changes :pending) (setq ethersync--recent-changes nil))
  (track-changes-fetch
   id (lambda (beg end before)
        (cl-incf ethersync--versioned-identifier)
        (cond
         ((eq ethersync--recent-changes :emacs-messup) nil)
         ((eq before 'error) (setf ethersync--recent-changes :emacs-messup))
         (t (push `(,(ethersync--pos-to-lsp-position beg)
                    ,(ethersync--virtual-pos-to-lsp-position beg before)
                    ,(length before)
                    ,(buffer-substring-no-properties beg end))
                  ethersync--recent-changes))))))

(defun ethersync--add-one-shot-hook (hook function &optional append local)
  "Like `add-hook' but calls FUNCTION only once."
  (let* ((fname (make-symbol (format "ethersync--%s-once" function)))
         (fun (lambda (&rest args)
                (remove-hook hook fname local)
                (apply function args))))
    (fset fname fun)
    (add-hook hook fname append local)))

(defun ethersync--track-changes-signal (id &optional distance)
  (cond
   (distance
    ;; When distance is <100, we may as well coalesce the changes.
    (when (> distance 100) (ethersync--track-changes-fetch id)))
   (ethersync--recent-changes nil)
   ;; Note that there are pending changes, for the benefit of those
   ;; who check it as a boolean.
   (t (setq ethersync--recent-changes :pending)))
  (when ethersync--change-idle-timer (cancel-timer ethersync--change-idle-timer))
  (setq ethersync--change-idle-timer
        (run-with-idle-timer
         ethersync-send-changes-idle-time nil
         (lambda (buf)
           (ethersync--when-live-buffer buf
             (when ethersync--managed-mode
               (if (track-changes-inconsistent-state-p)
                   ;; Not a good time (e.g. in the middle of Quail thingy,
                   ;; bug#70541): reschedule for the next idle period.
                   (ethersync--add-one-shot-hook
                    'post-command-hook
                    (lambda ()
                      (ethersync--when-live-buffer buf
                        (ethersync--track-changes-signal id))))
                 (run-hooks 'ethersync--document-changed-hook)
                 (setq ethersync--change-idle-timer nil)))))
         (current-buffer))))

(defun ethersync--TextDocumentIdentifier ()
  "Compute TextDocumentIdentifier object for current buffer.
Sets `ethersync--TextDocumentIdentifier-uri' (which see) as a side effect."
  (unless ethersync--TextDocumentIdentifier-cache
    (let ((truename (file-truename (or buffer-file-name
                                       (ignore-errors
                                         (buffer-file-name
                                          (buffer-base-buffer)))))))
      (setq ethersync--TextDocumentIdentifier-cache
            `(,truename . ,(ethersync-path-to-uri truename :truenamep t)))))
  (cdr ethersync--TextDocumentIdentifier-cache))

(defun ethersync--signal-edit ()
  "Send edit to server."
  (ethersync--track-changes-fetch ethersync--track-changes)
  (when ethersync--recent-changes
    (ethersync--TextDocumentIdentifier)
    (let* ((server (ethersync--current-server-or-lose)))
      (jsonrpc-notify
       server
       :edit
       (list
        :uri (ethersync--TextDocumentIdentifier)
        :revision ethersync--versioned-identifier
        :delta
        ;;FIXME handle multiple changes
        (cl-destructuring-bind (beg end len text) (car ethersync--recent-changes)
          (list :range `(:start ,beg :end ,end)
                :replacement text))))
      (setq ethersync--recent-changes nil)
      (jsonrpc--call-deferred server))))

(defun ethersync--signal-open ()
  "Send open to server."
  (setq ethersync--recent-changes nil
        ethersync--versioned-identifier 0
        ethersync--TextDocumentIdentifier-cache nil)
  (ethersync--TextDocumentIdentifier)
  (jsonrpc-notify
   (ethersync--current-server-or-lose)
   :open `(:uri ,(ethersync--TextDocumentIdentifier))))

(defun ethersync--signal-close ()
  "Send close to server."
  (with-demoted-errors
      "[ethersync] error sending close: %s"
    (ethersync--TextDocumentIdentifier)
    (jsonrpc-notify
     (ethersync--current-server-or-lose)
     :close `(:uri ,(ethersync--TextDocumentIdentifier)))))

(defvar ethersync-cache-session-completions t
  "If non-nil Ethersync caches data during completion sessions.")

;;; List connections mode

(define-derived-mode ethersync-list-connections-mode  tabulated-list-mode
  "" "Ethersync mode for listing server connections
\\{ethersync-list-connections-mode-map}"
  :interactive nil
  (setq-local tabulated-list-format
              `[("Server" 16) ("Project name" 16)])
  (tabulated-list-init-header))

(defun ethersync-list-connections ()
  "List currently active Ethersync connections."
  (interactive)
  (with-current-buffer
      (get-buffer-create "*ETHERSYNC connections*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (ethersync-list-connections-mode)
      (setq-local tabulated-list-entries
                  (mapcar
                   (lambda (server)
                     (list server
                           `[,(jsonrpc-name server)
                             ,(ethersync-project-nickname server)]))
                   (hash-table-values ethersync--server-by-project)))
      (revert-buffer)
      (pop-to-buffer (current-buffer)))))

;;; Hacks
;;;
;; Emacs bug#56407, the optimal solution is in desktop.el, but that's
;; harder. For now, use `with-eval-after-load'. See also github#1183.
(with-eval-after-load 'desktop
  (add-to-list 'desktop-minor-mode-handlers '(ethersync--managed-mode . ignore)))


;;; Misc
;;;
;;;###autoload
(progn
  (put 'ethersync--debbugs-or-github-bug-uri 'bug-reference-url-format t)
  (defun ethersync--debbugs-or-github-bug-uri ()
    (format (if (string= (match-string 2) "github")
                "https://github.com/joaotavora/eglot/issues/%s"
              "https://debbugs.gnu.org/%s")
            (match-string 3))))

;; Add command-mode property manually for compatibility with Emacs < 28.
(dolist (sym '(ethersync-clear-status
               ethersync-forget-pending-continuations
               ethersync-reconnect
               ethersync-stderr-buffer))
  (function-put sym 'command-modes '(ethersync--managed-mode)))

;; Local Variables:
;; bug-reference-bug-regexp: "\\(\\(github\\|bug\\)#\\([0-9]+\\)\\)"
;; bug-reference-url-format: ethersync--debbugs-or-github-bug-uri
;; checkdoc-force-docstrings-flag: nil
;; End:

;;; ethersync.el ends here

(provide 'ethersync)
;;; ethersync.el ends here
