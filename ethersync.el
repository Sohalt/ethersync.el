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
;; Package-Requires: ((emacs "26.3") (compat "27.1") (eldoc "1.14.0") (external-completion "0.1") (flymake "1.2.1") (jsonrpc "1.0.24") (project "0.9.8") (seq "2.23") (track-changes "1.2") (xref "1.6.2"))
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

(defface ethersync-highlight-symbol-face
  '((t (:inherit bold)))
  "Face used to highlight the symbol at point.")

(defface ethersync-mode-line
  '((t (:inherit font-lock-constant-face :weight bold)))
  "Face for package-name in Ethersync's mode line.")

(defface ethersync-diagnostic-tag-unnecessary-face
  '((t (:inherit shadow)))
  "Face used to render unused or unnecessary code.")

(defface ethersync-diagnostic-tag-deprecated-face
  '((t . (:inherit shadow :strike-through t)))
  "Face used to render deprecated or obsolete code.")

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
  (defvar ethersync--lsp-interface-alist
    `(
      ;; Editor to Server
      (Open (:uri))
      (Close (:uri))
      (ESCursor (:uri :ranges))

      ;; Server to Editor
      (SECursor ((:userid . integer) :uri :ranges) ((:name . string)))

      ;; Both directions
      (Edit (:uri :delta)))
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
    (let* ((interface (assoc interface-name ethersync--lsp-interface-alist))
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
             (byte-compile-warn "Unknown LSP interface %s" interface-name))))))

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

(cl-defgeneric ethersync-execute-command (_ _ _)
  (declare (obsolete ethersync-execute "30.1"))
  (:method
   (server command arguments)
   (ethersync--request server :workspace/executeCommand
                       `(:command ,(format "%s" command) :arguments ,arguments))))

(cl-defgeneric ethersync-execute (server action)
  "Ask SERVER to execute ACTION.
ACTION is an LSP object of either `CodeAction' or `Command' type."
  (:method
   (server action) "Default implementation."
   (ethersync--dcase action
                     (((Command)) (ethersync--request server :workspace/executeCommand action))
                     (((CodeAction) edit command data)
                      (if (and (null edit) (null command) data
                               (ethersync-server-capable :codeActionProvider :resolveProvider))
                          (ethersync-execute server (ethersync--request server :codeAction/resolve action))
                        (when edit (ethersync--apply-workspace-edit edit this-command))
                        (when command (ethersync--request server :workspace/executeCommand command)))))))

(cl-defgeneric ethersync-initialization-options (server)
  "JSON object to send under `initializationOptions'."
  (:method (s)
           (let ((probe (plist-get (ethersync--saved-initargs s) :initializationOptions)))
             (cond ((functionp probe) (funcall probe s))
                   (probe)
                   (t ethersync--{})))))

(cl-defgeneric ethersync-register-capability (server method id &rest params)
  "Ask SERVER to register capability METHOD marked with ID."
  (:method
   (_s method _id &rest _params)
   (ethersync--warn "Server tried to register unsupported capability `%s'"
                    method)))

(cl-defgeneric ethersync-unregister-capability (server method id &rest params)
  "Ask SERVER to register capability METHOD marked with ID."
  (:method
   (_s method _id &rest _params)
   (ethersync--warn "Server tried to unregister unsupported capability `%s'"
                    method)))

(cl-defgeneric ethersync-client-capabilities (server)
  "What the Ethersync LSP client supports for SERVER."
  (:method (s)
           (list
            :workspace (list
                        :applyEdit t
                        :executeCommand `(:dynamicRegistration :json-false)
                        :workspaceEdit `(:documentChanges t)
                        :didChangeWatchedFiles
                        `(:dynamicRegistration
                          ,(if (ethersync--trampish-p s) :json-false t))
                        :symbol `(:dynamicRegistration :json-false)
                        :configuration t
                        :workspaceFolders t)
            :textDocument
            (list
             :synchronization (list
                               :dynamicRegistration :json-false
                               :willSave t :willSaveWaitUntil t :didSave t)
             :completion      (list :dynamicRegistration :json-false
                                    :completionItem
                                    `(:snippetSupport
                                      ,(if (and
                                            (not (ethersync--stay-out-of-p 'yasnippet))
                                            (ethersync--snippet-expansion-fn))
                                           t
                                         :json-false)
                                      :deprecatedSupport t
                                      :resolveSupport (:properties
                                                       ["documentation"
                                                        "details"
                                                        "additionalTextEdits"])
                                      :tagSupport (:valueSet [1]))
                                    :contextSupport t)
             :hover              (list :dynamicRegistration :json-false
                                       :contentFormat (ethersync--accepted-formats))
             :signatureHelp      (list :dynamicRegistration :json-false
                                       :signatureInformation
                                       `(:parameterInformation
                                         (:labelOffsetSupport t)
                                         :documentationFormat ,(ethersync--accepted-formats)
                                         :activeParameterSupport t))
             :references         `(:dynamicRegistration :json-false)
             :definition         (list :dynamicRegistration :json-false
                                       :linkSupport t)
             :declaration        (list :dynamicRegistration :json-false
                                       :linkSupport t)
             :implementation     (list :dynamicRegistration :json-false
                                       :linkSupport t)
             :typeDefinition     (list :dynamicRegistration :json-false
                                       :linkSupport t)
             :documentSymbol     (list
                                  :dynamicRegistration :json-false
                                  :hierarchicalDocumentSymbolSupport t
                                  :symbolKind `(:valueSet
                                                [,@(mapcar
                                                    #'car ethersync--symbol-kind-names)]))
             :documentHighlight  `(:dynamicRegistration :json-false)
             :codeAction         (list
                                  :dynamicRegistration :json-false
                                  :resolveSupport `(:properties ["edit" "command"])
                                  :dataSupport t
                                  :codeActionLiteralSupport
                                  '(:codeActionKind
                                    (:valueSet
                                     ["quickfix"
                                      "refactor" "refactor.extract"
                                      "refactor.inline" "refactor.rewrite"
                                      "source" "source.organizeImports"]))
                                  :isPreferredSupport t)
             :formatting         `(:dynamicRegistration :json-false)
             :rangeFormatting    `(:dynamicRegistration :json-false)
             :rename             `(:dynamicRegistration :json-false)
             :inlayHint          `(:dynamicRegistration :json-false)
             :publishDiagnostics (list :relatedInformation :json-false
                                       ;; TODO: We can support :codeDescription after
                                       ;; adding an appropriate UI to
                                       ;; Flymake.
                                       :codeDescriptionSupport :json-false
                                       :tagSupport
                                       `(:valueSet
                                         [,@(mapcar
                                             #'car ethersync--tag-faces)])))
            :window `(:showDocument (:support t)
                      :workDoneProgress ,(if ethersync-report-progress t :json-false))
            :general (list :positionEncodings ["utf-32" "utf-8" "utf-16"])
            :experimental ethersync--{})))

(cl-defgeneric ethersync-workspace-folders (server)
  "Return workspaceFolders for SERVER."
  (let ((project (ethersync--project server)))
    (vconcat
     (mapcar (lambda (dir)
               (list :uri (ethersync-path-to-uri dir)
                     :name (abbreviate-file-name dir)))
             `(,(project-root project) ,@(project-external-roots project))))))

(defclass ethersync-lsp-server (jsonrpc-process-connection)
  ((project-nickname
    :documentation "Short nickname for the associated project."
    :accessor ethersync--project-nickname
    :reader ethersync-project-nickname)
   (languages
    :initform nil
    :documentation "Alist ((MODE . LANGUAGE-ID-STRING)...) of managed languages."
    :accessor ethersync--languages)
   (capabilities
    :initform nil
    :documentation "JSON object containing server capabilities."
    :accessor ethersync--capabilities)
   (server-info
    :initform nil
    :documentation "JSON object containing server info."
    :accessor ethersync--server-info)
   (shutdown-requested
    :initform nil
    :documentation "Flag set when server is shutting down."
    :accessor ethersync--shutdown-requested)
   (project
    :initform nil
    :documentation "Project associated with server."
    :accessor ethersync--project)
   (progress-reporters
    :initform (make-hash-table :test #'equal) :accessor ethersync--progress-reporters
    :documentation "Maps LSP progress tokens to progress reporters.")
   (inhibit-autoreconnect
    :initform t
    :documentation "Generalized boolean inhibiting auto-reconnection if true."
    :accessor ethersync--inhibit-autoreconnect)
   (file-watches
    :documentation "Map (DIR -> (WATCH ID1 ID2...)) for `didChangeWatchedFiles'."
    :initform (make-hash-table :test #'equal) :accessor ethersync--file-watches)
   (managed-buffers
    :initform nil
    :documentation "List of buffers managed by server."
    :accessor ethersync--managed-buffers)
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

(defun ethersync-server-capable (&rest feats)
  "Determine if current server is capable of FEATS."
  (unless (cl-some (lambda (feat)
                     (memq feat ethersync-ignored-server-capabilities))
                   feats)
    (cl-loop for caps = (ethersync--capabilities (ethersync--current-server-or-lose))
             then (cadr probe)
             for (feat . more) on feats
             for probe = (plist-member caps feat)
             if (not probe) do (cl-return nil)
             if (eq (cadr probe) :json-false) do (cl-return nil)
             if (not (listp (cadr probe))) do (cl-return (if more nil (cadr probe)))
             finally (cl-return (or (cadr probe) t)))))

(defun ethersync-server-capable-or-lose (&rest feats)
  "Like `ethersync-server-capable', but maybe error out."
  (let ((retval (apply #'ethersync-server-capable feats)))
    (unless retval
      (ethersync--error "Unsupported or ignored LSP capability `%s'"
                        (mapconcat #'symbol-name feats " ")))
    retval))


;;; Process/server management
(defun ethersync--major-modes (s) "Major modes server S is responsible for."
       (mapcar #'car (ethersync--languages s)))

(defun ethersync--language-ids (s) "LSP Language ID strings for server S's modes."
       (mapcar #'cdr (ethersync--languages s)))

(cl-defmethod initialize-instance :before ((_server ethersync-lsp-server) &optional args)
  (cl-remf args :initializationOptions))

(defvar ethersync--servers-by-project (make-hash-table :test #'equal)
  "Keys are projects.  Values are lists of processes.")

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
  (cl-loop for ss being the hash-values of ethersync--servers-by-project
           do (with-demoted-errors "[ethersync] shutdown all: %s"
                (cl-loop for s in ss do (ethersync-shutdown s nil nil preserve-buffers)))))

(defvar ethersync--servers-by-xrefed-file (make-hash-table :test 'equal))

(defun ethersync--on-shutdown (server)
  "Called by jsonrpc.el when SERVER is already dead."
  ;; Turn off `ethersync--managed-mode' where appropriate.
  (dolist (buffer (ethersync--managed-buffers server))
    (let (;; Avoid duplicate shutdowns (github#389)
          (ethersync-autoshutdown nil))
      (ethersync--when-live-buffer buffer (ethersync--managed-mode-off))))
  ;; Kill any expensive watches
  (maphash (lambda (_dir watch-and-ids)
             (file-notify-rm-watch (car watch-and-ids)))
           (ethersync--file-watches server))
  ;; Sever the project/server relationship for `server'
  (setf (gethash (ethersync--project server) ethersync--servers-by-project)
        (delq server
              (gethash (ethersync--project server) ethersync--servers-by-project)))
  (maphash (lambda (f s)
             (when (eq s server) (remhash f ethersync--servers-by-xrefed-file)))
           ethersync--servers-by-xrefed-file)
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

(defun ethersync--lookup-mode (mode)
  "Lookup `ethersync-server-programs' for MODE.
Return (LANGUAGES . CONTACT-PROXY).

MANAGED-MODES is a list with MODE as its first element.
Subsequent elements are other major modes also potentially
managed by the server that is to manage MODE.

LANGUAGE-IDS is a list of the same length as MANAGED-MODES.  Each
elem is derived from the corresponding mode name, if not
specified in `ethersync-server-programs' (which see).

CONTACT-PROXY is the value of the corresponding
`ethersync-server-programs' entry."
  (cl-flet ((languages (main-mode-sym specs)
              (let* ((res
                      (mapcar (jsonrpc-lambda (sym &key language-id &allow-other-keys)
                                (cons sym
                                      (or language-id
                                          (or (get sym 'ethersync-language-id)
                                              (replace-regexp-in-string
                                               "\\(?:-ts\\)?-mode$" ""
                                               (symbol-name sym))))))
                              specs))
                     (head (cl-find main-mode-sym res :key #'car)))
                (cons head (delq head res)))))
    (cl-loop
     for (modes . contact) in ethersync-server-programs
     for specs = (mapcar #'ethersync--ensure-list
                         (if (or (symbolp modes) (keywordp (cadr modes)))
                             (list modes) modes))
     thereis (cl-some (lambda (spec)
                        (cl-destructuring-bind (sym &key &allow-other-keys) spec
                          (and (provided-mode-derived-p mode sym)
                               (cons (languages sym specs) contact))))
                      specs))))

(defun ethersync--guess-contact (&optional interactive)
  "Helper for `ethersync'.
Return (MANAGED-MODES PROJECT CLASS CONTACT LANG-IDS).  If INTERACTIVE is
non-nil, maybe prompt user, else error as soon as something can't
be guessed."
  (let* ((project (ethersync--current-project))
         (guessed-mode (if buffer-file-name major-mode))
         (guessed-mode-name (and guessed-mode (symbol-name guessed-mode)))
         (main-mode
          (cond
           ((and interactive
                 (or (>= (prefix-numeric-value current-prefix-arg) 16)
                     (not guessed-mode)))
            (intern
             (completing-read
              "[ethersync] Start a server to manage buffers of what major mode? "
              (mapcar #'symbol-name (ethersync--all-major-modes)) nil t
              guessed-mode-name nil guessed-mode-name nil)))
           ((not guessed-mode)
            (ethersync--error "Can't guess mode to manage for `%s'" (current-buffer)))
           (t guessed-mode)))
         (languages-and-contact (ethersync--lookup-mode main-mode))
         (managed-modes (mapcar #'car (car languages-and-contact)))
         (language-ids (mapcar #'cdr (car languages-and-contact)))
         (guess (cdr languages-and-contact))
         (guess (if (functionp guess)
                    (pcase (cdr (func-arity guess))
                      (1 (funcall guess interactive))
                      (_ (funcall guess interactive project)))
                  guess))
         (class (or (and (consp guess) (symbolp (car guess))
                         (prog1 (unless current-prefix-arg (car guess))
                           (setq guess (cdr guess))))
                    'ethersync-lsp-server))
         (program (and (listp guess)
                       (stringp (car guess))
                       ;; A second element might be the port of a (host, port)
                       ;; pair, but in that case it is not a string.
                       (or (null (cdr guess)) (stringp (cadr guess)))
                       (car guess)))
         (base-prompt
          (and interactive
               "Enter program to execute (or <host>:<port>): "))
         (full-program-invocation
          (and program
               (cl-every #'stringp guess)
               (combine-and-quote-strings guess)))
         (prompt
          (and base-prompt
               (cond (current-prefix-arg base-prompt)
                     ((null guess)
                      (format "[ethersync] Couldn't guess LSP server for `%s'\n%s"
                              main-mode base-prompt))
                     ((and program
                           (not (file-name-absolute-p program))
                           (not (compat-call executable-find program t)))
                      (if full-program-invocation
                          (concat (format "[ethersync] I guess you want to run `%s'"
                                          full-program-invocation)
                                  (format ", but I can't find `%s' in PATH!"
                                          program)
                                  "\n" base-prompt)
                        (ethersync--error
                         (concat "`%s' not found in PATH, but can't form"
                                 " an interactive prompt for help you fix"
                                 " this.")
                         program guess))))))
         (input (and prompt (read-shell-command prompt
                                                full-program-invocation
                                                'ethersync-command-history)))
         (contact
          (if input
              (if (string-match
                   "^[\s\t]*\\(.*\\):\\([[:digit:]]+\\)[\s\t]*$" input)
                  ;; <host>:<port> special case (bug#67682)
                  (list (match-string 1 input)
                        (string-to-number (match-string 2 input)))
                (split-string-and-unquote input))
            guess)))
    (list managed-modes project class contact language-ids)))

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
(defun ethersync (managed-major-modes project class contact language-ids
                                      &optional _interactive)
  "Start LSP server for PROJECT's buffers under MANAGED-MAJOR-MODES.

This starts a Language Server Protocol (LSP) server suitable for
the buffers of PROJECT whose `major-mode' is among
MANAGED-MAJOR-MODES.  CLASS is the class of the LSP server to
start and CONTACT specifies how to connect to the server.

Interactively, the command attempts to guess MANAGED-MAJOR-MODES,
CLASS, CONTACT, and LANGUAGE-IDS from `ethersync-server-programs',
according to the current buffer's `major-mode'.  PROJECT is
guessed from `project-find-functions'.  The search for active
projects in this context binds `ethersync-lsp-context' (which see).

If it can't guess, it prompts the user for the mode and the
server.  With a single \\[universal-argument] prefix arg, it
always prompts for COMMAND.  With two \\[universal-argument], it
also always prompts for MANAGED-MAJOR-MODE.

The LSP server of CLASS is started (or contacted) via CONTACT.
If this operation is successful, current *and future* file
buffers of MANAGED-MAJOR-MODE inside PROJECT become \"managed\"
by the LSP server, meaning the information about their contents is
exchanged periodically with the server to provide enhanced
code-analysis via `xref-find-definitions', `flymake-mode',
`eldoc-mode', and `completion-at-point', among others.

PROJECT is a project object as returned by `project-current'.

CLASS is a subclass of `ethersync-lsp-server'.

CONTACT specifies how to contact the server.  It is a
keyword-value plist used to initialize CLASS or a plain list as
described in `ethersync-server-programs', which see.

LANGUAGE-IDS is a list of language ID string to send to the
server for each element in MANAGED-MAJOR-MODES.

INTERACTIVE is ignored and provided for backward compatibility."
  (interactive
   (let ((current-server (ethersync-current-server)))
     (unless (or (null current-server)
                 (y-or-n-p "\
[ethersync] Shut down current connection before attempting new one?"))
       (user-error "[ethersync] Connection attempt aborted by user."))
     (prog1 (append (ethersync--guess-contact t) '(t))
       (when current-server (ignore-errors (ethersync-shutdown current-server))))))
  (ethersync--connect (ethersync--ensure-list managed-major-modes)
                      project class contact
                      (ethersync--ensure-list language-ids)))

(defun ethersync-reconnect (server &optional interactive)
  "Reconnect to SERVER.
INTERACTIVE is t if called interactively."
  (interactive (list (ethersync--current-server-or-lose) t))
  (when (jsonrpc-running-p server)
    (ignore-errors (ethersync-shutdown server interactive nil 'preserve-buffers)))
  (ethersync--connect (ethersync--major-modes server)
                      (ethersync--project server)
                      (eieio-object-class-name server)
                      (ethersync--saved-initargs server)
                      (ethersync--language-ids server))
  (ethersync--message "Reconnected!"))

(defvar ethersync--managed-mode) ; forward decl

;;;###autoload
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
        (add-hook 'post-command-hook #'maybe-connect 'append t)))))

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
  '(ethersync-signal-didChangeConfiguration)
  "Hook run after connecting in `ethersync--connect'.")

(defvar ethersync-server-initialized-hook
  '()
  "Hook run after a `ethersync-lsp-server' instance is created.

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

(defun ethersync--connect (managed-modes project class contact language-ids)
  "Connect to MANAGED-MODES, LANGUAGE-IDS, PROJECT, CLASS and CONTACT.
This docstring appeases checkdoc, that's all."
  (let* ((default-directory (project-root project))
         (nickname (project-name project))
         (readable-name (format "ETHERSYNC (%s/%s)" nickname managed-modes))
         server-info
         (contact (if (functionp contact) (funcall contact) contact))
         (initargs
          (cond ((keywordp (car contact)) contact)
                ((integerp (cadr contact))
                 (setq server-info (list (format "%s:%s" (car contact)
                                                 (cadr contact))))
                 `(:process ,(lambda ()
                               (apply #'open-network-stream
                                      readable-name nil
                                      (car contact) (cadr contact)
                                      (cddr contact)))))
                ((and (stringp (car contact))
                      (cl-find-if (lambda (x)
                                    (or (eq x :autoport)
                                        (eq (car-safe x) :autoport)))
                                  contact))
                 (setq server-info (list "<inferior process>"))
                 `(:process ,(jsonrpc-autoport-bootstrap
                              readable-name
                              contact
                              :connect-args '(:noquery t))))
                ((stringp (car contact))
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
           initargs))
         (canceled nil)
         (tag (make-symbol "connected-catch-tag")))
    (when server-info
      (jsonrpc--debug server "Running language server: %s"
                      (string-join server-info " ")))
    (setf (ethersync--saved-initargs server) initargs)
    (setf (ethersync--project server) project)
    (setf (ethersync--project-nickname server) nickname)
    (setf (ethersync--languages server)
          (cl-loop for m in managed-modes for l in language-ids
                   collect (cons m l)))
    (run-hook-with-args 'ethersync-server-initialized-hook server)
    ;; Now start the handshake.  To honor `ethersync-sync-connect'
    ;; maybe-sync-maybe-async semantics we use `jsonrpc-async-request'
    ;; and mimic most of `jsonrpc-request'.
    (unwind-protect
        (condition-case _quit
            (let ((retval
                   (catch tag
                     (jsonrpc-async-request
                      server
                      :initialize
                      (list :processId
                            (unless (or ethersync-withhold-process-id
                                        (file-remote-p default-directory)
                                        (eq (jsonrpc-process-type server)
                                            'network))
                              (emacs-pid))
                            :clientInfo
                            (append
                             '(:name "Ethersync")
                             (let ((v (package-get-version)))
                               (and v (list :version v))))
                            ;; Maybe turn trampy `/ssh:foo@bar:/path/to/baz.py'
                            ;; into `/path/to/baz.py', so LSP groks it.
                            :rootPath (file-local-name
                                       (expand-file-name default-directory))
                            :rootUri (ethersync-path-to-uri default-directory)
                            :initializationOptions (ethersync-initialization-options
                                                    server)
                            :capabilities (ethersync-client-capabilities server)
                            :workspaceFolders (ethersync-workspace-folders server))
                      :success-fn
                      (ethersync--lambda ((InitializeResult) capabilities serverInfo)
                                         (unless canceled
                                           (push server
                                                 (gethash project ethersync--servers-by-project))
                                           (setf (ethersync--capabilities server) capabilities)
                                           (setf (ethersync--server-info server) serverInfo)
                                           (jsonrpc-notify server :initialized ethersync--{})
                                           (dolist (buffer (buffer-list))
                                             (with-current-buffer buffer
                                               ;; No need to pass SERVER as an argument: it has
                                               ;; been registered in `ethersync--servers-by-project',
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
                                            "Connected! Server `%s' now managing `%s' buffers \
in project `%s'."
                                            (or (plist-get serverInfo :name)
                                                (jsonrpc-name server))
                                            managed-modes
                                            (ethersync-project-nickname server))
                                           (when tag (throw tag t))))
                      :timeout ethersync-connect-timeout
                      :error-fn (ethersync--lambda ((ResponseError) code message)
                                                   (unless canceled
                                                     (jsonrpc-shutdown server)
                                                     (let ((msg (format "%s: %s" code message)))
                                                       (if tag (throw tag `(error . ,msg))
                                                         (ethersync--error msg)))))
                      :timeout-fn (lambda ()
                                    (unless canceled
                                      (jsonrpc-shutdown server)
                                      (let ((msg (format "Timed out after %s seconds"
                                                         ethersync-connect-timeout)))
                                        (if tag (throw tag `(error . ,msg))
                                          (ethersync--error msg))))))
                     (cond ((numberp ethersync-sync-connect)
                            (accept-process-output nil ethersync-sync-connect))
                           (ethersync-sync-connect
                            (while t (accept-process-output
                                      nil ethersync-connect-timeout)))))))
              (pcase retval
                (`(error . ,msg) (ethersync--error msg))
                (`nil (ethersync--message "Waiting in background for server `%s'"
                                          (jsonrpc-name server))
                      nil)
                (_ server)))
          (quit (jsonrpc-shutdown server) (setq canceled 'quit)))
      (setq tag nil))))


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
  "Like `jsonrpc-request', but for Ethersync LSP requests.
Unless IMMEDIATE, send pending changes before making request."
  (unless immediate (ethersync--signal-textDocument/didChange))
  (jsonrpc-request server method params
                   :timeout timeout
                   :cancel-on-input cancel-on-input
                   :cancel-on-input-retval cancel-on-input-retval))


;;; Encoding fever
;;;
(defvar ethersync-current-linepos-function #'ethersync-utf-16-linepos
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

(defun ethersync-utf-8-linepos ()
  "Calculate number of UTF-8 bytes from line beginning."
  (length (encode-coding-region (ethersync--bol) (point) 'utf-8-unix t)))

(defun ethersync-utf-16-linepos (&optional lbp)
  "Calculate number of UTF-16 code units from position given by LBP.
LBP defaults to `ethersync--bol'."
  (/ (- (length (encode-coding-region (or lbp (ethersync--bol))
                                      ;; FIXME: How could `point' ever be
                                      ;; larger than `point-max' (sounds like
                                      ;; a bug in Emacs).
                                      ;; Fix github#860
                                      (min (point) (point-max)) 'utf-16 t))
        2)
     2))

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

(defvar ethersync-move-to-linepos-function #'ethersync-move-to-utf-16-linepos
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

(defun ethersync-move-to-utf-8-linepos (n)
  "Move to line's Nth byte as computed by LSP's UTF-8 criterion."
  (let* ((bol (ethersync--bol))
         (goal-byte (+ (position-bytes bol) n))
         (eol (line-end-position)))
    (goto-char bol)
    (while (and (< (position-bytes (point)) goal-byte) (< (point) eol))
      ;; raw bytes take 2 bytes in the buffer
      (when (>= (char-after) #x3fff80) (setq goal-byte (1+ goal-byte)))
      (forward-char 1))))

(defun ethersync-move-to-utf-16-linepos (n)
  "Move to line's Nth code unit as computed by LSP's UTF-16 criterion."
  (let* ((bol (ethersync--bol))
         (goal-char (+ bol n))
         (eol (line-end-position)))
    (goto-char bol)
    (while (and (< (point) goal-char) (< (point) eol))
      ;; code points in the "supplementary place" use two code units
      (when (<= #x010000 (char-after) #x10ffff) (setq goal-char (1- goal-char)))
      (forward-char 1))))

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
  (let ((servers (cl-loop for servers
                          being hash-values of ethersync--servers-by-project
                          append servers))
        (name (lambda (srv)
                (format "%s %s" (ethersync-project-nickname srv)
                        (ethersync--major-modes srv)))))
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

(defvar-local ethersync--current-flymake-report-fn nil
  "Current flymake report function for this buffer.")

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

(define-minor-mode ethersync--managed-mode
  "Mode for source buffers managed by some Ethersync project."
  :init-value nil :lighter nil :keymap ethersync-mode-map :interactive nil
  (cond
   (ethersync--managed-mode
    (pcase (plist-get (ethersync--capabilities (ethersync-current-server))
                      :positionEncoding)
      ("utf-32"
       (ethersync--setq-saving ethersync-current-linepos-function #'ethersync-utf-32-linepos)
       (ethersync--setq-saving ethersync-move-to-linepos-function #'ethersync-move-to-utf-32-linepos))
      ("utf-8"
       (ethersync--setq-saving ethersync-current-linepos-function #'ethersync-utf-8-linepos)
       (ethersync--setq-saving ethersync-move-to-linepos-function #'ethersync-move-to-utf-8-linepos)))
    (unless ethersync--track-changes
      (setq ethersync--track-changes
            (track-changes-register
             #'ethersync--track-changes-signal :disjoint t)))
    (add-hook 'kill-buffer-hook #'ethersync--managed-mode-off nil t)
    ;; Prepend "didClose" to the hook after the "nonoff", so it will run first
    (add-hook 'kill-buffer-hook #'ethersync--signal-textDocument/didClose nil t)
    (add-hook 'before-revert-hook #'ethersync--signal-textDocument/didClose nil t)
    (add-hook 'after-revert-hook #'ethersync--after-revert-hook nil t)
    (add-hook 'before-save-hook #'ethersync--signal-textDocument/willSave nil t)
    (add-hook 'after-save-hook #'ethersync--signal-textDocument/didSave nil t)
    (unless (ethersync--stay-out-of-p 'xref)
      (add-hook 'xref-backend-functions #'ethersync-xref-backend nil t))
    (add-hook 'completion-at-point-functions #'ethersync-completion-at-point nil t)
    (add-hook 'completion-in-region-mode-hook #'ethersync--capf-session-flush nil t)
    (add-hook 'company-after-completion-hook #'ethersync--capf-session-flush nil t)
    (add-hook 'change-major-mode-hook #'ethersync--managed-mode-off nil t)
    (add-hook 'post-self-insert-hook #'ethersync--post-self-insert-hook nil t)
    (add-hook 'pre-command-hook #'ethersync--pre-command-hook nil t)
    (ethersync--setq-saving xref-prompt-for-identifier nil)
    (ethersync--setq-saving flymake-diagnostic-functions '(ethersync-flymake-backend))
    (ethersync--setq-saving company-backends '(company-capf))
    (ethersync--setq-saving company-tooltip-align-annotations t)
    (ethersync--setq-saving eldoc-documentation-strategy
                            #'eldoc-documentation-compose)
    (unless (ethersync--stay-out-of-p 'imenu)
      (add-function :before-until (local 'imenu-create-index-function)
                    #'ethersync-imenu))
    (unless (ethersync--stay-out-of-p 'flymake) (flymake-mode 1))
    (unless (ethersync--stay-out-of-p 'eldoc)
      (add-hook 'eldoc-documentation-functions #'ethersync-hover-eldoc-function
                nil t)
      (add-hook 'eldoc-documentation-functions #'ethersync-signature-eldoc-function
                nil t)
      (eldoc-mode 1))
    (cl-pushnew (current-buffer) (ethersync--managed-buffers (ethersync-current-server))))
   (t
    (remove-hook 'kill-buffer-hook #'ethersync--managed-mode-off t)
    (remove-hook 'kill-buffer-hook #'ethersync--signal-textDocument/didClose t)
    (remove-hook 'before-revert-hook #'ethersync--signal-textDocument/didClose t)
    (remove-hook 'after-revert-hook #'ethersync--after-revert-hook t)
    (remove-hook 'before-save-hook #'ethersync--signal-textDocument/willSave t)
    (remove-hook 'after-save-hook #'ethersync--signal-textDocument/didSave t)
    (remove-hook 'xref-backend-functions #'ethersync-xref-backend t)
    (remove-hook 'completion-at-point-functions #'ethersync-completion-at-point t)
    (remove-hook 'completion-in-region-mode-hook #'ethersync--capf-session-flush t)
    (remove-hook 'company-after-completion-hook #'ethersync--capf-session-flush t)
    (remove-hook 'change-major-mode-hook #'ethersync--managed-mode-off t)
    (remove-hook 'post-self-insert-hook #'ethersync--post-self-insert-hook t)
    (remove-hook 'pre-command-hook #'ethersync--pre-command-hook t)
    (remove-hook 'eldoc-documentation-functions #'ethersync-hover-eldoc-function t)
    (remove-hook 'eldoc-documentation-functions #'ethersync-signature-eldoc-function t)
    (cl-loop for (var . saved-binding) in ethersync--saved-bindings
             do (set (make-local-variable var) saved-binding))
    (remove-function (local 'imenu-create-index-function) #'ethersync-imenu)
    (when ethersync--current-flymake-report-fn
      (ethersync--report-to-flymake nil)
      (setq ethersync--current-flymake-report-fn nil))
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
  (ethersync-inlay-hints-mode -1)
  (ethersync--managed-mode -1))

(defun ethersync-current-server ()
  "Return logical Ethersync server for current buffer, nil if none."
  (setq ethersync--cached-server
        (or ethersync--cached-server
            (and (not (eq major-mode 'fundamental-mode)) ; gh#1330
                 (or
                  (cl-find-if #'ethersync--languageId
                              (gethash (ethersync--current-project)
                                       ethersync--servers-by-project))
                  (and ethersync-extend-to-xref
                       buffer-file-name
                       (gethash (expand-file-name buffer-file-name)
                                ethersync--servers-by-xrefed-file)))))))

(defun ethersync--current-server-or-lose ()
  "Return current logical Ethersync server connection or error."
  (or (ethersync-current-server)
      (jsonrpc-error "No current JSON-RPC connection")))

(defvar-local ethersync--diagnostics nil
  "Flymake diagnostics for this buffer.")

(defvar revert-buffer-preserve-modes)
(defun ethersync--after-revert-hook ()
  "Ethersync's `after-revert-hook'."
  (when revert-buffer-preserve-modes (ethersync--signal-textDocument/didOpen)))

(defun ethersync--maybe-activate-editing-mode ()
  "Maybe activate `ethersync--managed-mode'.

If it is activated, also signal textDocument/didOpen."
  (unless ethersync--managed-mode
    ;; Called when `revert-buffer-in-progress-p' is t but
    ;; `revert-buffer-preserve-modes' is nil.
    (when (and buffer-file-name (ethersync-current-server))
      (setq ethersync--diagnostics nil)
      (ethersync--managed-mode)
      (ethersync--signal-textDocument/didOpen)
      ;; Run user hook after 'textDocument/didOpen' so server knows
      ;; about the buffer.
      (ethersync-inlay-hints-mode 1)
      (run-hooks 'ethersync-managed-mode-hook))))

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

(easy-menu-define ethersync-menu nil "Ethersync"
  `("Ethersync"
    ;; Commands for getting information and customization.
    ["Customize Ethersync" (lambda () (interactive) (customize-group "ethersync"))]
    "--"
    ;; xref like commands.
    ["Find definitions" xref-find-definitions
     :help "Find definitions of identifier at point"
     :active (ethersync-server-capable :definitionProvider)]
    ["Find references" xref-find-references
     :help "Find references to identifier at point"
     :active (ethersync-server-capable :referencesProvider)]
    ["Find symbols in workspace (apropos)" xref-find-apropos
     :help "Find symbols matching a query"
     :active (ethersync-server-capable :workspaceSymbolProvider)]
    ["Find declaration" ethersync-find-declaration
     :help "Find declaration for identifier at point"
     :active (ethersync-server-capable :declarationProvider)]
    ["Find implementation" ethersync-find-implementation
     :help "Find implementation for identifier at point"
     :active (ethersync-server-capable :implementationProvider)]
    ["Find type definition" ethersync-find-typeDefinition
     :help "Find type definition for identifier at point"
     :active (ethersync-server-capable :typeDefinitionProvider)]
    "--"
    ;; LSP-related commands (mostly Ethersync's own commands).
    ["Rename symbol" ethersync-rename
     :active (ethersync-server-capable :renameProvider)]
    ["Format buffer" ethersync-format-buffer
     :active (ethersync-server-capable :documentFormattingProvider)]
    ["Format active region" ethersync-format
     :active (and (region-active-p)
                  (ethersync-server-capable :documentRangeFormattingProvider))]
    ["Show Flymake diagnostics for buffer" flymake-show-buffer-diagnostics]
    ["Show Flymake diagnostics for project" flymake-show-project-diagnostics]
    ["Show Eldoc documentation at point" eldoc-doc-buffer]
    "--"
    ["All possible code actions" ethersync-code-actions
     :active (ethersync-server-capable :codeActionProvider)]
    ["Organize imports" ethersync-code-action-organize-imports
     :visible (ethersync-server-capable :codeActionProvider)]
    ["Extract" ethersync-code-action-extract
     :visible (ethersync-server-capable :codeActionProvider)]
    ["Inline" ethersync-code-action-inline
     :visible (ethersync-server-capable :codeActionProvider)]
    ["Rewrite" ethersync-code-action-rewrite
     :visible (ethersync-server-capable :codeActionProvider)]
    ["Quickfix" ethersync-code-action-quickfix
     :visible (ethersync-server-capable :codeActionProvider)]))

(easy-menu-define ethersync-server-menu nil "Monitor server communication"
  '("Debugging the server communication"
    ["Reconnect to server" ethersync-reconnect]
    ["Quit server" ethersync-shutdown]
    "--"
    ["LSP events buffer" ethersync-events-buffer]
    ["Server stderr buffer" ethersync-stderr-buffer]
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
         (pending (and server (jsonrpc-continuation-count server)))
         (last-error (and server (jsonrpc-last-error server))))
    (append
     `(,(propertize
         ethersync-menu-string
         'face 'ethersync-mode-line
         'mouse-face 'mode-line-highlight
         'help-echo "Ethersync: Emacs LSP client\nmouse-1: Display minor mode menu"
         'keymap (let ((map (make-sparse-keymap)))
                   (define-key map [mode-line down-mouse-1] ethersync-menu)
                   map)))
     (when nick
       `(":"
         ,(propertize
           nick
           'face 'ethersync-mode-line
           'mouse-face 'mode-line-highlight
           'help-echo (format "Project '%s'\nmouse-1: LSP server control menu" nick)
           'keymap (let ((map (make-sparse-keymap)))
                     (define-key map [mode-line down-mouse-1] ethersync-server-menu)
                     map))
         ,@(when last-error
             `("/" ,(ethersync--mode-line-props
                     "error" 'compilation-mode-line-fail
                     '((mouse-3 ethersync-clear-status  "Clear this status"))
                     (format "An error occurred: %s\n" (plist-get last-error
                                                                  :message)))))
         ,@(when (cl-plusp pending)
             `("/" ,(ethersync--mode-line-props
                     (format "%d" pending) 'warning
                     '((mouse-3 ethersync-forget-pending-continuations
                        "Forget pending continuations"))
                     "Number of outgoing, \
still unanswered LSP requests to the server\n")))
         ,@(cl-loop for pr hash-values of (ethersync--progress-reporters server)
                    when (eq (car pr)  'ethersync--mode-line-reporter)
                    append `("/" ,(ethersync--mode-line-props
                                   (format "%s%%%%" (or (nth 4 pr) "?"))
                                   'ethersync-mode-line
                                   nil
                                   (format "(%s) %s %s" (nth 1 pr)
                                           (nth 2 pr) (nth 3 pr))))))))))

(add-to-list 'mode-line-misc-info
             `(ethersync--managed-mode (" [" ethersync--mode-line-format "] ")))


;;; Flymake customization
;;;
(put 'ethersync-note 'flymake-category 'flymake-note)
(put 'ethersync-warning 'flymake-category 'flymake-warning)
(put 'ethersync-error 'flymake-category 'flymake-error)

(defalias 'ethersync--make-diag #'flymake-make-diagnostic)
(defalias 'ethersync--diag-data #'flymake-diagnostic-data)

(defvar ethersync-diagnostics-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-2] #'ethersync-code-actions-at-mouse)
    map)
  "Keymap active in Ethersync-backed Flymake diagnostic overlays.")

(cl-loop for i from 1
         for type in '(ethersync-note ethersync-warning ethersync-error)
         do (put type 'flymake-overlay-control
                 `((mouse-face . highlight)
                   (priority . ,(+ 50 i))
                   (keymap . ,ethersync-diagnostics-map))))


;;; Protocol implementation (Requests, notifications, etc)
;;;
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

(cl-defmethod ethersync-handle-notification
  (_server (_method (eql window/showMessage)) &key type message)
  "Handle notification window/showMessage."
  (ethersync--message (propertize "Server reports (type=%s): %s"
                                  'face (if (<= type 1) 'error))
                      type message))

(cl-defmethod ethersync-handle-request
  (_server (_method (eql window/showMessageRequest))
           &key type message actions &allow-other-keys)
  "Handle server request window/showMessageRequest."
  (let* ((actions (append actions nil)) ;; gh#627
         (label (completing-read
                 (concat
                  (format (propertize "[ethersync] Server reports (type=%s): %s"
                                      'face (if (or (not type) (<= type 1)) 'error))
                          type message)
                  "\nChoose an option: ")
                 (or (mapcar (lambda (obj) (plist-get obj :title)) actions)
                     '("OK"))
                 nil t (plist-get (elt actions 0) :title))))
    (if label `(:title ,label) :null)))

(cl-defmethod ethersync-handle-notification
  (_server (_method (eql window/logMessage)) &key _type _message)
  "Handle notification window/logMessage.") ;; noop, use events buffer

(cl-defmethod ethersync-handle-notification
  (_server (_method (eql telemetry/event)) &rest _any)
  "Handle notification telemetry/event.") ;; noop, use events buffer

(defalias 'ethersync--reporter-update
  (if (> emacs-major-version 26) #'progress-reporter-update
    (lambda (a b &optional _c) (progress-reporter-update a b))))

(cl-defmethod ethersync-handle-notification
  (server (_method (eql $/progress)) &key token value)
  "Handle $/progress notification identified by TOKEN from SERVER."
  (when ethersync-report-progress
    (cl-flet ((fmt (&rest args) (mapconcat #'identity args " "))
              (mkpr (title)
                (if (eq ethersync-report-progress 'messages)
                    (make-progress-reporter
                     (format "[ethersync] %s %s: %s"
                             (ethersync-project-nickname server) token title))
                  (list 'ethersync--mode-line-reporter token title)))
              (upd (pcnt msg &optional
                         (pr (gethash token (ethersync--progress-reporters server))))
                (cond
                 ((eq (car pr) 'ethersync--mode-line-reporter)
                  (setcdr (cddr pr) (list msg pcnt))
                  (force-mode-line-update t))
                 (pr (ethersync--reporter-update pr pcnt msg)))))
      (ethersync--dbind ((WorkDoneProgress) kind title percentage message) value
                        (pcase kind
                          ("begin"
                           (upd percentage (fmt title message)
                                (puthash token (mkpr title)
                                         (ethersync--progress-reporters server))))
                          ("report" (upd percentage message))
                          ("end" (upd (or percentage 100) message)
                           (run-at-time 2 nil
                                        (lambda ()
                                          (remhash token (ethersync--progress-reporters server))))))))))

(defvar-local ethersync--TextDocumentIdentifier-cache nil
  "LSP TextDocumentIdentifier-related cached info for current buffer.
Value is (TRUENAME . (:uri STR)), where STR is what is sent to the
server on textDocument/didOpen and similar calls.  TRUENAME is the
expensive cached value of `file-truename'.")

(cl-defmethod ethersync-handle-notification
  (server (_method (eql textDocument/publishDiagnostics)) &key uri diagnostics
          &allow-other-keys) ; FIXME: doesn't respect `ethersync-strict-mode'
  "Handle notification publishDiagnostics."
  (cl-flet ((ethersync--diag-type (sev)
              (cond ((null sev) 'ethersync-error)
                    ((<= sev 1) 'ethersync-error)
                    ((= sev 2)  'ethersync-warning)
                    (t          'ethersync-note)))
            (mess (source code message)
              (concat source (and code (format " [%s]" code)) ": " message))
            (find-it (abspath)
              ;; `find-buffer-visiting' would be natural, but calls the
              ;; potentially slow `file-truename' (bug#70036).
              (cl-loop for b in (ethersync--managed-buffers server)
                       when (with-current-buffer b
                              (equal (car ethersync--TextDocumentIdentifier-cache)
                                     abspath))
                       return b)))
    (if-let* ((path (expand-file-name (ethersync-uri-to-path uri)))
              (buffer (find-it path)))
        (with-current-buffer buffer
          (cl-loop
           initially
           (setq flymake-list-only-diagnostics
                 (assoc-delete-all path flymake-list-only-diagnostics))
           for diag-spec across diagnostics
           collect (ethersync--dbind ((Diagnostic) range code message severity source tags)
                                     diag-spec
                                     (setq message (mess source code message))
                                     (pcase-let
                                         ((`(,beg . ,end) (ethersync-range-region range)))
                                       ;; Fallback to `flymake-diag-region' if server
                                       ;; botched the range
                                       (when (= beg end)
                                         (if-let* ((st (plist-get range :start))
                                                   (diag-region
                                                    (flymake-diag-region
                                                     (current-buffer) (1+ (plist-get st :line))
                                                     (plist-get st :character))))
                                             (setq beg (car diag-region) end (cdr diag-region))
                                           (ethersync--widening
                                            (goto-char (point-min))
                                            (setq beg
                                                  (ethersync--bol
                                                   (1+ (plist-get (plist-get range :start) :line))))
                                            (setq end
                                                  (line-end-position
                                                   (1+ (plist-get (plist-get range :end) :line)))))))
                                       (ethersync--make-diag
                                        (current-buffer) beg end
                                        (ethersync--diag-type severity)
                                        message `((ethersync-lsp-diag . ,diag-spec))
                                        (when-let ((faces
                                                    (cl-loop for tag across tags
                                                             when (alist-get tag ethersync--tag-faces)
                                                             collect it)))
                                          `((face . ,faces))))))
           into diags
           finally (cond ((and
                           ;; only add to current report if Flymake
                           ;; starts on idle-timer (github#958)
                           (not (null flymake-no-changes-timeout))
                           ethersync--current-flymake-report-fn)
                          (ethersync--report-to-flymake diags))
                         (t
                          (setq ethersync--diagnostics diags)))))
      (cl-loop
       for diag-spec across diagnostics
       collect (ethersync--dbind ((Diagnostic) code range message severity source) diag-spec
                                 (setq message (mess source code message))
                                 (let* ((start (plist-get range :start))
                                        (line (1+ (plist-get start :line)))
                                        (char (1+ (plist-get start :character))))
                                   (ethersync--make-diag
                                    path (cons line char) nil (ethersync--diag-type severity) message)))
       into diags
       finally
       (setq flymake-list-only-diagnostics
             (assoc-delete-all path flymake-list-only-diagnostics))
       (push (cons path diags) flymake-list-only-diagnostics)))))

(cl-defun ethersync--register-unregister (server things how)
  "Helper for `registerCapability'.
THINGS are either registrations or unregisterations (sic)."
  (cl-loop
   for thing in (cl-coerce things 'list)
   do (ethersync--dbind ((Registration) id method registerOptions) thing
                        (apply (cl-ecase how
                                 (register 'ethersync-register-capability)
                                 (unregister 'ethersync-unregister-capability))
                               server (intern method) id registerOptions))))

(cl-defmethod ethersync-handle-request
  (server (_method (eql client/registerCapability)) &key registrations)
  "Handle server request client/registerCapability."
  (ethersync--register-unregister server registrations 'register))

(cl-defmethod ethersync-handle-request
  (server (_method (eql client/unregisterCapability))
          &key unregisterations) ;; XXX: "unregisterations" (sic)
  "Handle server request client/unregisterCapability."
  (ethersync--register-unregister server unregisterations 'unregister))

(cl-defmethod ethersync-handle-request
  (_server (_method (eql workspace/applyEdit)) &key _label edit)
  "Handle server request workspace/applyEdit."
  (ethersync--apply-workspace-edit edit last-command)
  `(:applied t))

(cl-defmethod ethersync-handle-request
  (server (_method (eql workspace/workspaceFolders)))
  "Handle server request workspace/workspaceFolders."
  (ethersync-workspace-folders server))

(cl-defmethod ethersync-handle-request
  (_server (_method (eql window/showDocument)) &key
           uri external takeFocus selection)
  "Handle request window/showDocument."
  (let ((success t)
        (filename))
    (cond
     ((eq external t) (browse-url uri))
     ((file-readable-p (setq filename (ethersync-uri-to-path uri)))
      ;; Use run-with-timer to avoid nested client requests like the
      ;; "synchronous imenu" floated in bug#62116 presumably caused by
      ;; which-func-mode.
      (run-with-timer
       0 nil
       (lambda ()
         (with-current-buffer (find-file-noselect filename)
           (cond (takeFocus
                  (pop-to-buffer (current-buffer))
                  (select-frame-set-input-focus (selected-frame)))
                 ((display-buffer (current-buffer))))
           (when selection
             (pcase-let ((`(,beg . ,end) (ethersync-range-region selection)))
               ;; FIXME: it is very naughty to use someone else's `--'
               ;; function, but `xref--goto-char' happens to have
               ;; exactly the semantics we want vis-a-vis widening.
               (xref--goto-char beg)
               (pulse-momentary-highlight-region beg end 'highlight)))))))
     (t (setq success :json-false)))
    `(:success ,success)))

(defun ethersync--TextDocumentIdentifier ()
  "Compute TextDocumentIdentifier object for current buffer.
Sets `ethersync--TextDocumentIdentifier-uri' (which see) as a side effect."
  (unless ethersync--TextDocumentIdentifier-cache
    (let ((truename (file-truename (or buffer-file-name
                                       (ignore-errors
                                         (buffer-file-name
                                          (buffer-base-buffer)))))))
      (setq ethersync--TextDocumentIdentifier-cache
            `(,truename . (:uri ,(ethersync-path-to-uri truename :truenamep t))))))
  (cdr ethersync--TextDocumentIdentifier-cache))

(defvar-local ethersync--versioned-identifier 0)

(defun ethersync--VersionedTextDocumentIdentifier ()
  "Compute VersionedTextDocumentIdentifier object for current buffer."
  (append (ethersync--TextDocumentIdentifier)
          `(:version ,ethersync--versioned-identifier)))

(cl-defun ethersync--languageId (&optional (server (ethersync--current-server-or-lose)))
  "Compute LSP \\='languageId\\=' string for current buffer.
Doubles as an predicate telling if SERVER can manage current
buffer."
  (cl-loop for (mode . languageid) in
           (ethersync--languages server)
           when (provided-mode-derived-p major-mode mode)
           return languageid))

(defun ethersync--TextDocumentItem ()
  "Compute TextDocumentItem object for current buffer."
  (append
   (ethersync--VersionedTextDocumentIdentifier)
   (list :languageId (ethersync--languageId)
         :text
         (ethersync--widening
          (buffer-substring-no-properties (point-min) (point-max))))))

(defun ethersync--TextDocumentPositionParams ()
  "Compute TextDocumentPositionParams."
  (list :textDocument (ethersync--TextDocumentIdentifier)
        :position (ethersync--pos-to-lsp-position)))

(defvar-local ethersync--last-inserted-char nil
  "If non-nil, value of the last inserted character in buffer.")

(defun ethersync--post-self-insert-hook ()
  "Set `ethersync--last-inserted-char', maybe call on-type-formatting."
  (setq ethersync--last-inserted-char last-command-event)
  (let ((ot-provider (ethersync-server-capable :documentOnTypeFormattingProvider)))
    (when (and ot-provider
               (ignore-errors ; github#906, some LS's send empty strings
                 (or (eq ethersync--last-inserted-char
                         (seq-first (plist-get ot-provider :firstTriggerCharacter)))
                     (cl-find ethersync--last-inserted-char
                              (plist-get ot-provider :moreTriggerCharacter)
                              :key #'seq-first))))
      (ethersync-format (point) nil ethersync--last-inserted-char))))

(defvar ethersync--workspace-symbols-cache (make-hash-table :test #'equal)
  "Cache of `workspace/Symbol' results  used by `xref-find-definitions'.")

(defun ethersync--pre-command-hook ()
  "Reset some temporary variables."
  (clrhash ethersync--workspace-symbols-cache)
  (setq ethersync--last-inserted-char nil))

(defun ethersync--CompletionParams ()
  (append
   (ethersync--TextDocumentPositionParams)
   `(:context
     ,(if-let (trigger (and (characterp ethersync--last-inserted-char)
                            (cl-find ethersync--last-inserted-char
                                     (ethersync-server-capable :completionProvider
                                                               :triggerCharacters)
                                     :key (lambda (str) (aref str 0))
                                     :test #'char-equal)))
          `(:triggerKind 2 :triggerCharacter ,trigger) `(:triggerKind 1)))))

(defvar-local ethersync--recent-changes nil
  "Recent buffer changes as collected by `ethersync--track-changes-fetch'.")

(cl-defmethod jsonrpc-connection-ready-p ((_server ethersync-lsp-server) _what)
  "Tell if SERVER is ready for WHAT in current buffer."
  (and (cl-call-next-method) (not ethersync--recent-changes)))

(defvar-local ethersync--change-idle-timer nil "Idle timer for didChange signals.")

(defvar ethersync--document-changed-hook '(ethersync--signal-textDocument/didChange)
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

(defvar-local ethersync-workspace-configuration ()
  "Configure LSP servers specifically for a given project.

This variable's value should be a plist (SECTION VALUE ...).
SECTION is a keyword naming a parameter section relevant to a
particular server.  VALUE is a plist or a primitive type
converted to JSON also understood by that server.

Instead of a plist, an alist ((SECTION . VALUE) ...) can be used
instead, but this variant is less reliable and not recommended.

This variable should be set as a directory-local variable.  See
info node `(emacs)Directory Variables' for various ways to do that.

Here's an example value that establishes two sections relevant to
the Pylsp and Gopls LSP servers:

  (:pylsp (:plugins (:jedi_completion (:include_params t
                                       :fuzzy t)
                     :pylint (:enabled :json-false)))
   :gopls (:usePlaceholders t))

The value of this variable can also be a unary function of a
single argument, which will be a connected `ethersync-lsp-server'
instance.  The function runs with `default-directory' set to the
root of the current project.  It should return an object of the
format described above.")

;;;###autoload
(put 'ethersync-workspace-configuration 'safe-local-variable #'listp)

(defun ethersync-show-workspace-configuration (&optional server)
  "Dump `ethersync-workspace-configuration' as JSON for debugging."
  (interactive (list (ethersync--read-server "Show workspace configuration for" t)))
  (let ((conf (ethersync--workspace-configuration-plist server)))
    (with-current-buffer (get-buffer-create "*ETHERSYNC workspace configuration*")
      (erase-buffer)
      (insert (jsonrpc--json-encode conf))
      (with-no-warnings
        (require 'json)
        (when (require 'json-mode nil t) (json-mode))
        (json-pretty-print-buffer))
      (pop-to-buffer (current-buffer)))))

(defun ethersync--workspace-configuration-plist (server &optional path)
  "Returns SERVER's workspace configuration as a plist.
If PATH consider that file's `file-name-directory' to get the
local value of the `ethersync-workspace-configuration' variable, else
use the root of SERVER's `ethersync--project'."
  (let ((val (with-temp-buffer
               (setq default-directory
                     ;; See github#1281
                     (if path (if (file-directory-p path)
                                  (file-name-as-directory path)
                                (file-name-directory path))
                       (project-root (ethersync--project server))))
               ;; Set the major mode to be the first of the managed
               ;; modes.  This is the one the user started ethersync in.
               (setq major-mode (car (ethersync--major-modes server)))
               (hack-dir-local-variables-non-file-buffer)
               (if (functionp ethersync-workspace-configuration)
                   (funcall ethersync-workspace-configuration server)
                 ethersync-workspace-configuration))))
    (or (and (consp (car val))
             (cl-loop for (section . v) in val
                      collect (if (keywordp section) section
                                (intern (format ":%s" section)))
                      collect v))
        val)))

(defun ethersync-signal-didChangeConfiguration (server)
  "Send a `:workspace/didChangeConfiguration' signal to SERVER.
When called interactively, use the currently active server"
  (interactive (list (ethersync--current-server-or-lose)))
  (jsonrpc-notify
   server :workspace/didChangeConfiguration
   (list
    :settings
    (or (ethersync--workspace-configuration-plist server)
        ethersync--{}))))

(cl-defmethod ethersync-handle-request
  (server (_method (eql workspace/configuration)) &key items)
  "Handle server request workspace/configuration."
  (apply #'vector
         (mapcar
          (ethersync--lambda ((ConfigurationItem) scopeUri section)
                             (cl-loop
                              with scope-uri-path = (and scopeUri (ethersync-uri-to-path scopeUri))
                              for (wsection o)
                              on (ethersync--workspace-configuration-plist server scope-uri-path)
                              by #'cddr
                              when (string=
                                    (if (keywordp wsection)
                                        (substring (symbol-name wsection) 1)
                                      wsection)
                                    section)
                              return o))
          items)))

(defun ethersync--signal-textDocument/didChange ()
  "Send textDocument/didChange to server."
  (ethersync--track-changes-fetch ethersync--track-changes)
  (when ethersync--recent-changes
    (let* ((server (ethersync--current-server-or-lose))
           (sync-capability (ethersync-server-capable :textDocumentSync))
           (sync-kind (if (numberp sync-capability) sync-capability
                        (plist-get sync-capability :change)))
           (full-sync-p (or (eq sync-kind 1)
                            (eq :emacs-messup ethersync--recent-changes))))
      (jsonrpc-notify
       server :textDocument/didChange
       (list
        :textDocument (ethersync--VersionedTextDocumentIdentifier)
        :contentChanges
        (if full-sync-p
            (vector `(:text ,(ethersync--widening
                              (buffer-substring-no-properties (point-min)
                                                              (point-max)))))
          (cl-loop for (beg end len text) in (reverse ethersync--recent-changes)
                   vconcat `[,(list :range `(:start ,beg :end ,end)
                                    :rangeLength len :text text)]))))
      (setq ethersync--recent-changes nil)
      (jsonrpc--call-deferred server))))

(defun ethersync--signal-textDocument/didOpen ()
  "Send textDocument/didOpen to server."
  (setq ethersync--recent-changes nil
        ethersync--versioned-identifier 0
        ethersync--TextDocumentIdentifier-cache nil)
  (jsonrpc-notify
   (ethersync--current-server-or-lose)
   :textDocument/didOpen `(:textDocument ,(ethersync--TextDocumentItem))))

(defun ethersync--signal-textDocument/didClose ()
  "Send textDocument/didClose to server."
  (with-demoted-errors
      "[ethersync] error sending textDocument/didClose: %s"
    (jsonrpc-notify
     (ethersync--current-server-or-lose)
     :textDocument/didClose `(:textDocument ,(ethersync--TextDocumentIdentifier)))))

(defun ethersync--signal-textDocument/willSave ()
  "Maybe send textDocument/willSave to server."
  (let ((server (ethersync--current-server-or-lose))
        (params `(:reason 1 :textDocument ,(ethersync--TextDocumentIdentifier))))
    (when (ethersync-server-capable :textDocumentSync :willSave)
      (jsonrpc-notify server :textDocument/willSave params))
    (when (ethersync-server-capable :textDocumentSync :willSaveWaitUntil)
      (ignore-errors
        (ethersync--apply-text-edits
         (ethersync--request server :textDocument/willSaveWaitUntil params
                             :timeout 0.5))))))

(defun ethersync--signal-textDocument/didSave ()
  "Maybe send textDocument/didSave to server."
  (ethersync--signal-textDocument/didChange)
  (when (ethersync-server-capable :textDocumentSync :save)
    (jsonrpc-notify
     (ethersync--current-server-or-lose)
     :textDocument/didSave
     (list
      ;; TODO: Handle TextDocumentSaveRegistrationOptions to control this.
      :text (buffer-substring-no-properties (point-min) (point-max))
      :textDocument (ethersync--TextDocumentIdentifier)))))

(defun ethersync-flymake-backend (report-fn &rest _more)
  "A Flymake backend for Ethersync.
Calls REPORT-FN (or arranges for it to be called) when the server
publishes diagnostics.  Between calls to this function, REPORT-FN
may be called multiple times (respecting the protocol of
`flymake-diagnostic-functions')."
  (cond (ethersync--managed-mode
         (setq ethersync--current-flymake-report-fn report-fn)
         (ethersync--report-to-flymake ethersync--diagnostics))
        (t
         (funcall report-fn nil))))

(defun ethersync--report-to-flymake (diags)
  "Internal helper for `ethersync-flymake-backend'."
  (save-restriction
    (widen)
    (funcall ethersync--current-flymake-report-fn diags
             ;; If the buffer hasn't changed since last
             ;; call to the report function, flymake won't
             ;; delete old diagnostics.  Using :region
             ;; keyword forces flymake to delete
             ;; them (github#159).
             :region (cons (point-min) (point-max))))
  (setq ethersync--diagnostics diags))

(defun ethersync-xref-backend () "Ethersync xref backend." 'ethersync)

(defvar ethersync--temp-location-buffers (make-hash-table :test #'equal)
  "Helper variable for `ethersync--collecting-xrefs'.")

(defvar ethersync-xref-lessp-function #'ignore
  "Compare two `xref-item' objects for sorting.")

(cl-defmacro ethersync--collecting-xrefs ((collector) &rest body)
  "Sort and handle xrefs collected with COLLECTOR in BODY."
  (declare (indent 1) (debug (sexp &rest form)))
  (let ((collected (cl-gensym "collected")))
    `(unwind-protect
         (let (,collected)
           (cl-flet ((,collector (xref) (push xref ,collected)))
             ,@body)
           (setq ,collected (nreverse ,collected))
           (sort ,collected ethersync-xref-lessp-function))
       (maphash (lambda (_uri buf) (kill-buffer buf)) ethersync--temp-location-buffers)
       (clrhash ethersync--temp-location-buffers))))

(defun ethersync--xref-make-match (name uri range)
  "Like `xref-make-match' but with LSP's NAME, URI and RANGE.
Try to visit the target file for a richer summary line."
  (pcase-let*
      ((file (ethersync-uri-to-path uri))
       (visiting (or (find-buffer-visiting file)
                     (gethash uri ethersync--temp-location-buffers)))
       (collect (lambda ()
                  (ethersync--widening
                   (pcase-let* ((`(,beg . ,end) (ethersync-range-region range))
                                (bol (progn (goto-char beg) (ethersync--bol)))
                                (substring (buffer-substring bol (line-end-position)))
                                (hi-beg (- beg bol))
                                (hi-end (- (min (line-end-position) end) bol)))
                     (add-face-text-property hi-beg hi-end 'xref-match
                                             t substring)
                     (list substring (line-number-at-pos (point) t)
                           (ethersync-utf-32-linepos) (- end beg))))))
       (`(,summary ,line ,column ,length)
        (cond
         (visiting (with-current-buffer visiting (funcall collect)))
         ((file-readable-p file) (with-current-buffer
                                     (puthash uri (generate-new-buffer " *temp*")
                                              ethersync--temp-location-buffers)
                                   (insert-file-contents file)
                                   (funcall collect)))
         (t ;; fall back to the "dumb strategy"
          (let* ((start (cl-getf range :start))
                 (line (1+ (cl-getf start :line)))
                 (start-pos (cl-getf start :character))
                 (end-pos (cl-getf (cl-getf range :end) :character)))
            (list name line start-pos (- end-pos start-pos)))))))
    (setf (gethash (expand-file-name file) ethersync--servers-by-xrefed-file)
          (ethersync--current-server-or-lose))
    (xref-make-match summary (xref-make-file-location file line column) length)))

(defun ethersync--workspace-symbols (pat &optional buffer)
  "Ask for :workspace/symbol on PAT, return list of formatted strings.
If BUFFER, switch to it before."
  (with-current-buffer (or buffer (current-buffer))
    (ethersync-server-capable-or-lose :workspaceSymbolProvider)
    (mapcar
     (lambda (wss)
       (ethersync--dbind ((WorkspaceSymbol) name containerName kind) wss
                         (propertize
                          (format "%s%s %s"
                                  (if (zerop (length containerName)) ""
                                    (concat (propertize containerName 'face 'shadow) " "))
                                  name
                                  (propertize (alist-get kind ethersync--symbol-kind-names "Unknown")
                                              'face 'shadow))
                          'ethersync--lsp-workspaceSymbol wss)))
     (ethersync--request (ethersync--current-server-or-lose) :workspace/symbol
                         `(:query ,pat)))))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql ethersync)))
  "Yet another tricky connection between LSP and Elisp completion semantics."
  (let ((buf (current-buffer)) (cache ethersync--workspace-symbols-cache))
    (cl-labels ((refresh (pat) (ethersync--workspace-symbols pat buf))
                (lookup-1 (pat) ;; check cache, else refresh
                  (let ((probe (gethash pat cache :missing)))
                    (if (eq probe :missing) (puthash pat (refresh pat) cache)
                      probe)))
                (lookup (pat _point)
                  (let ((res (lookup-1 pat))
                        (def (and (string= pat "") (gethash :default cache))))
                    (append def res nil)))
                (score (c)
                  (cl-getf (get-text-property
                            0 'ethersync--lsp-workspaceSymbol c)
                           :score 0)))
      (external-completion-table
       'ethersync-indirection-joy
       #'lookup
       `((cycle-sort-function
          . ,(lambda (completions)
               (cl-sort completions #'> :key #'score))))))))

(defun ethersync--recover-workspace-symbol-meta (string)
  "Search `ethersync--workspace-symbols-cache' for rich entry of STRING."
  (catch 'found
    (maphash (lambda (_k v)
               (while (consp v)
                 ;; Like mess? Ask minibuffer.el about improper lists.
                 (when (equal (car v) string) (throw 'found (car v)))
                 (setq v (cdr v))))
             ethersync--workspace-symbols-cache)))

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql ethersync)))
  (let ((attempt
         (and (xref--prompt-p this-command)
              (puthash :default
                       (ignore-errors
                         (ethersync--workspace-symbols (symbol-name (symbol-at-point))))
                       ethersync--workspace-symbols-cache))))
    (if attempt (car attempt) "LSP identifier at point")))

(defvar ethersync--lsp-xref-refs nil
  "`xref' objects for overriding `xref-backend-references''s.")

(cl-defun ethersync--lsp-xrefs-for-method (method &key extra-params capability)
  "Make `xref''s for METHOD, EXTRA-PARAMS, check CAPABILITY."
  (ethersync-server-capable-or-lose
   (or capability
       (intern
        (format ":%sProvider"
                (cadr (split-string (symbol-name method)
                                    "/"))))))
  (let ((response
         (ethersync--request
          (ethersync--current-server-or-lose)
          method (append (ethersync--TextDocumentPositionParams) extra-params))))
    (ethersync--collecting-xrefs (collect)
                                 (mapc
                                  (lambda (loc-or-loc-link)
                                    (let ((sym-name (symbol-name (symbol-at-point))))
                                      (ethersync--dcase loc-or-loc-link
                                                        (((LocationLink) targetUri targetSelectionRange)
                                                         (collect (ethersync--xref-make-match sym-name
                                                                                              targetUri targetSelectionRange)))
                                                        (((Location) uri range)
                                                         (collect (ethersync--xref-make-match sym-name
                                                                                              uri range))))))
                                  (if (vectorp response) response (and response (list response)))))))

(cl-defun ethersync--lsp-xref-helper (method &key extra-params capability)
  "Helper for `ethersync-find-declaration' & friends."
  (let ((ethersync--lsp-xref-refs (ethersync--lsp-xrefs-for-method
                                   method
                                   :extra-params extra-params
                                   :capability capability)))
    (if ethersync--lsp-xref-refs
        (xref-find-references "LSP identifier at point.")
      (ethersync--message "%s returned no references" method))))

(defun ethersync-find-declaration ()
  "Find declaration for SYM, the identifier at point."
  (interactive)
  (ethersync--lsp-xref-helper :textDocument/declaration))

(defun ethersync-find-implementation ()
  "Find implementation for SYM, the identifier at point."
  (interactive)
  (ethersync--lsp-xref-helper :textDocument/implementation))

(defun ethersync-find-typeDefinition ()
  "Find type definition for SYM, the identifier at point."
  (interactive)
  (ethersync--lsp-xref-helper :textDocument/typeDefinition))

(cl-defmethod xref-backend-definitions ((_backend (eql ethersync)) id)
  (let ((probe (ethersync--recover-workspace-symbol-meta id)))
    (if probe
        (ethersync--dbind ((WorkspaceSymbol) name location)
                          (get-text-property 0 'ethersync--lsp-workspaceSymbol probe)
                          (ethersync--dbind ((Location) uri range) location
                                            (list (ethersync--xref-make-match name uri range))))
      (ethersync--lsp-xrefs-for-method :textDocument/definition))))

(cl-defmethod xref-backend-references ((_backend (eql ethersync)) _identifier)
  (or
   ethersync--lsp-xref-refs
   (ethersync--lsp-xrefs-for-method
    :textDocument/references :extra-params `(:context (:includeDeclaration t)))))

(cl-defmethod xref-backend-apropos ((_backend (eql ethersync)) pattern)
  (when (ethersync-server-capable :workspaceSymbolProvider)
    (ethersync--collecting-xrefs (collect)
                                 (mapc
                                  (ethersync--lambda ((SymbolInformation) name location)
                                                     (ethersync--dbind ((Location) uri range) location
                                                                       (collect (ethersync--xref-make-match name uri range))))
                                  (ethersync--request (ethersync--current-server-or-lose)
                                                      :workspace/symbol
                                                      `(:query ,pattern))))))

(defun ethersync-format-buffer ()
  "Format contents of current buffer."
  (interactive)
  (ethersync-format nil nil))

(defun ethersync-format (&optional beg end on-type-format)
  "Format region BEG END.
If either BEG or END is nil, format entire buffer.
Interactively, format active region, or entire buffer if region
is not active.

If non-nil, ON-TYPE-FORMAT is a character just inserted at BEG
for which LSP on-type-formatting should be requested."
  (interactive (and (region-active-p) (list (region-beginning) (region-end))))
  (pcase-let ((`(,method ,cap ,args)
               (cond
                ((and beg on-type-format)
                 `(:textDocument/onTypeFormatting
                   :documentOnTypeFormattingProvider
                   ,`(:position ,(ethersync--pos-to-lsp-position beg)
                      :ch ,(string on-type-format))))
                ((and beg end)
                 `(:textDocument/rangeFormatting
                   :documentRangeFormattingProvider
                   (:range ,(list :start (ethersync--pos-to-lsp-position beg)
                                  :end (ethersync--pos-to-lsp-position end)))))
                (t
                 '(:textDocument/formatting :documentFormattingProvider nil)))))
    (ethersync-server-capable-or-lose cap)
    (ethersync--apply-text-edits
     (ethersync--request
      (ethersync--current-server-or-lose)
      method
      (cl-list*
       :textDocument (ethersync--TextDocumentIdentifier)
       :options (list :tabSize tab-width
                      :insertSpaces (if indent-tabs-mode :json-false t)
                      :insertFinalNewline (if require-final-newline t :json-false)
                      :trimFinalNewlines (if delete-trailing-lines t :json-false))
       args))
     nil
     on-type-format)))

(defvar ethersync-cache-session-completions t
  "If non-nil Ethersync caches data during completion sessions.")

(defvar ethersync--capf-session :none "A cache used by `ethersync-completion-at-point'.")

(defun ethersync--capf-session-flush (&optional _) (setq ethersync--capf-session :none))

(defun ethersync--dumb-flex (pat comp ignorecase)
  "Return destructively fontified COMP iff PAT matches it."
  (cl-loop with lcomp = (length comp)
           with case-fold-search = ignorecase
           initially (remove-list-of-text-properties 0 lcomp '(face) comp)
           for x across pat
           for i = (cl-loop for j from (if i (1+ i) 0) below lcomp
                            when (char-equal x (aref comp j)) return j)
           unless i do (cl-return nil)
           ;; FIXME: could do much better here and coalesce intervals
           do (add-face-text-property i (1+ i) 'completions-common-part
                                      nil comp)
           finally (cl-return comp)))

(defun ethersync--dumb-allc (pat table pred _point) (funcall table pat pred t))
(defun ethersync--dumb-tryc (pat table pred point)
  (let ((probe (funcall table pat pred nil)))
    (cond ((eq probe t) t)
          (probe (cons probe (length probe)))
          (t (cons pat point)))))

(add-to-list 'completion-category-defaults '(ethersync-capf (styles ethersync--dumb-flex)))
(add-to-list 'completion-styles-alist '(ethersync--dumb-flex ethersync--dumb-tryc ethersync--dumb-allc))

(defun ethersync-completion-at-point ()
  "Ethersync's `completion-at-point' function."
  ;; Commit logs for this function help understand what's going on.
  (when-let (completion-capability (ethersync-server-capable :completionProvider))
    (let* ((server (ethersync--current-server-or-lose))
           (bounds (or (bounds-of-thing-at-point 'symbol)
                       (cons (point) (point))))
           (bounds-string (buffer-substring (car bounds) (cdr bounds)))
           (sort-completions
            (lambda (completions)
              (cl-sort completions
                       #'string-lessp
                       :key (lambda (c)
                              (plist-get
                               (get-text-property 0 'ethersync--lsp-item c)
                               :sortText)))))
           (metadata `(metadata (category . ethersync-capf)
                       (display-sort-function . ,sort-completions)))
           (local-cache :none)
           (orig-pos (point))
           (resolved (make-hash-table))
           (proxies
            (lambda ()
              (if (listp local-cache) local-cache
                (let* ((resp (ethersync--request server
                                                 :textDocument/completion
                                                 (ethersync--CompletionParams)
                                                 :cancel-on-input t))
                       (items (append
                               (if (vectorp resp) resp (plist-get resp :items))
                               nil))
                       (cachep (and (listp resp) items
                                    ethersync-cache-session-completions
                                    (eq (plist-get resp :isIncomplete) :json-false)))
                       (retval
                        (mapcar
                         (jsonrpc-lambda
                             (&rest item &key label insertText insertTextFormat
                                    textEdit &allow-other-keys)
                           (let ((proxy
                                  ;; Snippet or textEdit, it's safe to
                                  ;; display/insert the label since
                                  ;; it'll be adjusted.  If no usable
                                  ;; insertText at all, label is best,
                                  ;; too.
                                  (cond ((or (eql insertTextFormat 2)
                                             textEdit
                                             (null insertText)
                                             (string-empty-p insertText))
                                         (string-trim-left label))
                                        (t insertText))))
                             (unless (zerop (length proxy))
                               (put-text-property 0 1 'ethersync--lsp-item item proxy))
                             proxy))
                         items)))
                  ;; (trace-values "Requested" (length proxies) cachep bounds)
                  (setq ethersync--capf-session
                        (if cachep (list bounds retval resolved orig-pos
                                         bounds-string) :none))
                  (setq local-cache retval)))))
           (resolve-maybe
            ;; Maybe completion/resolve JSON object `lsp-comp' into
            ;; another JSON object, if at all possible.  Otherwise,
            ;; just return lsp-comp.
            (lambda (lsp-comp)
              (or (gethash lsp-comp resolved)
                  (setf (gethash lsp-comp resolved)
                        (if (and (ethersync-server-capable :completionProvider
                                                           :resolveProvider)
                                 (plist-get lsp-comp :data))
                            (ethersync--request server :completionItem/resolve
                                                lsp-comp :cancel-on-input t)
                          lsp-comp))))))
      (when (and (consp ethersync--capf-session)
                 (= (car bounds) (car (nth 0 ethersync--capf-session)))
                 (>= (cdr bounds) (cdr (nth 0 ethersync--capf-session))))
        (setq local-cache (nth 1 ethersync--capf-session)
              resolved (nth 2 ethersync--capf-session)
              orig-pos (nth 3 ethersync--capf-session)
              bounds-string (nth 4 ethersync--capf-session))
        ;; (trace-values "Recalling cache" (length local-cache) bounds orig-pos)
        )
      (list
       (car bounds)
       (cdr bounds)
       (lambda (pattern pred action)
         (cond
          ((eq action 'metadata) metadata)               ; metadata
          ((eq action 'lambda)                           ; test-completion
           (test-completion pattern (funcall proxies)))
          ((eq (car-safe action) 'boundaries) nil)       ; boundaries
          ((null action)                                 ; try-completion
           (try-completion pattern (funcall proxies)))
          ((eq action t)                                 ; all-completions
           (let ((comps (funcall proxies)))
             (dolist (c comps) (ethersync--dumb-flex pattern c t))
             (all-completions
              ""
              comps
              (lambda (proxy)
                (let* ((item (get-text-property 0 'ethersync--lsp-item proxy))
                       (filterText (plist-get item :filterText)))
                  (and (or (null pred) (funcall pred proxy))
                       (ethersync--dumb-flex
                        pattern (or filterText proxy) completion-ignore-case)))))))))
       :annotation-function
       (lambda (proxy)
         (ethersync--dbind ((CompletionItem) detail kind)
                           (get-text-property 0 'ethersync--lsp-item proxy)
                           (let* ((detail (and (stringp detail)
                                               (not (string= detail ""))
                                               detail))
                                  (annotation
                                   (or detail
                                       (cdr (assoc kind ethersync--kind-names)))))
                             (when annotation
                               (concat " "
                                       (propertize annotation
                                                   'face 'font-lock-function-name-face))))))
       :company-kind
       ;; Associate each lsp-item with a lsp-kind symbol.
       (lambda (proxy)
         (when-let* ((lsp-item (get-text-property 0 'ethersync--lsp-item proxy))
                     (kind (alist-get (plist-get lsp-item :kind)
                                      ethersync--kind-names)))
           (pcase kind
             ("EnumMember" 'enum-member)
             ("TypeParameter" 'type-parameter)
             (_ (intern (downcase kind))))))
       :company-deprecated
       (lambda (proxy)
         (when-let ((lsp-item (get-text-property 0 'ethersync--lsp-item proxy)))
           (or (seq-contains-p (plist-get lsp-item :tags)
                               1)
               (eq t (plist-get lsp-item :deprecated)))))
       :company-docsig
       ;; FIXME: autoImportText is specific to the pyright language server
       (lambda (proxy)
         (when-let* ((lsp-comp (get-text-property 0 'ethersync--lsp-item proxy))
                     (data (plist-get (funcall resolve-maybe lsp-comp) :data))
                     (import-text (plist-get data :autoImportText)))
           import-text))
       :company-doc-buffer
       (lambda (proxy)
         (let* ((documentation
                 (let ((lsp-comp (get-text-property 0 'ethersync--lsp-item proxy)))
                   (plist-get (funcall resolve-maybe lsp-comp) :documentation)))
                (formatted (and documentation
                                (ethersync--format-markup documentation))))
           (when formatted
             (with-current-buffer (get-buffer-create " *ethersync doc*")
               (erase-buffer)
               (insert formatted)
               (current-buffer)))))
       :company-require-match 'never
       :company-prefix-length
       (save-excursion
         (goto-char (car bounds))
         (when (listp completion-capability)
           (looking-back
            (regexp-opt
             (cl-coerce (cl-getf completion-capability :triggerCharacters) 'list))
            (ethersync--bol))))
       :exit-function
       (lambda (proxy status)
         (ethersync--capf-session-flush)
         (when (memq status '(finished exact))
           ;; To assist in using this whole `completion-at-point'
           ;; function inside `completion-in-region', ensure the exit
           ;; function runs in the buffer where the completion was
           ;; triggered from.  This should probably be in Emacs itself.
           ;; (github#505)
           (with-current-buffer (if (minibufferp)
                                    (window-buffer (minibuffer-selected-window))
                                  (current-buffer))
             (ethersync--dbind ((CompletionItem) insertTextFormat
                                insertText textEdit additionalTextEdits label)
                               (funcall
                                resolve-maybe
                                (or (get-text-property 0 'ethersync--lsp-item proxy)
                                    ;; When selecting from the *Completions*
                                    ;; buffer, `proxy' won't have any properties.
                                    ;; A lookup should fix that (github#148)
                                    (get-text-property
                                     0 'ethersync--lsp-item
                                     (cl-find proxy (funcall proxies) :test #'string=))))
                               (let ((snippet-fn (and (eql insertTextFormat 2)
                                                      (ethersync--snippet-expansion-fn))))
                                 (cond (textEdit
                                        ;; Revert buffer back to state when the edit
                                        ;; was obtained from server. If a `proxy'
                                        ;; "bar" was obtained from a buffer with
                                        ;; "foo.b", the LSP edit applies to that
                                        ;; state, _not_ the current "foo.bar".
                                        (delete-region orig-pos (point))
                                        (insert (substring bounds-string (- orig-pos (car bounds))))
                                        (ethersync--dbind ((TextEdit) range newText) textEdit
                                                          (pcase-let ((`(,beg . ,end)
                                                                       (ethersync-range-region range)))
                                                            (delete-region beg end)
                                                            (goto-char beg)
                                                            (funcall (or snippet-fn #'insert) newText))))
                                       (snippet-fn
                                        ;; A snippet should be inserted, but using plain
                                        ;; `insertText'.  This requires us to delete the
                                        ;; whole completion, since `insertText' is the full
                                        ;; completion's text.
                                        (delete-region (- (point) (length proxy)) (point))
                                        (funcall snippet-fn (or insertText label))))
                                 (when (cl-plusp (length additionalTextEdits))
                                   (ethersync--apply-text-edits additionalTextEdits)))
                               (ethersync--signal-textDocument/didChange)))))))))

(defun ethersync--hover-info (contents &optional _range)
  (mapconcat #'ethersync--format-markup
             (if (vectorp contents) contents (list contents)) "\n"))

(defun ethersync--sig-info (sig &optional sig-active briefp)
  (ethersync--dbind ((SignatureInformation)
                     ((:label siglabel))
                     ((:documentation sigdoc)) parameters activeParameter)
                    sig
                    (with-temp-buffer
                      (insert siglabel)
                      ;; Add documentation, indented so we can distinguish multiple signatures
                      (when-let (doc (and (not briefp) sigdoc (ethersync--format-markup sigdoc)))
                        (goto-char (point-max))
                        (insert "\n" (replace-regexp-in-string "^" "  " doc)))
                      ;; Try to highlight function name only
                      (let (first-parlabel)
                        (cond ((and (cl-plusp (length parameters))
                                    (vectorp (setq first-parlabel
                                                   (plist-get (aref parameters 0) :label))))
                               (save-excursion
                                 (goto-char (elt first-parlabel 0))
                                 (skip-syntax-backward "^w")
                                 (add-face-text-property (point-min) (point)
                                                         'font-lock-function-name-face)))
                              ((save-excursion
                                 (goto-char (point-min))
                                 (looking-at "\\([^(]*\\)([^)]*)"))
                               (add-face-text-property (match-beginning 1) (match-end 1)
                                                       'font-lock-function-name-face))))
                      ;; Now to the parameters
                      (cl-loop
                       with active-param = (or sig-active activeParameter)
                       for i from 0 for parameter across parameters do
                       (ethersync--dbind ((ParameterInformation)
                                          ((:label parlabel))
                                          ((:documentation pardoc)))
                                         parameter
                                         ;; ...perhaps highlight it in the formals list
                                         (when (eq i active-param)
                                           (save-excursion
                                             (goto-char (point-min))
                                             (pcase-let
                                                 ((`(,beg ,end)
                                                   (if (stringp parlabel)
                                                       (let ((case-fold-search nil))
                                                         (and (search-forward parlabel (line-end-position) t)
                                                              (list (match-beginning 0) (match-end 0))))
                                                     (mapcar #'1+ (append parlabel nil)))))
                                               (if (and beg end)
                                                   (add-face-text-property
                                                    beg end
                                                    'eldoc-highlight-function-argument)))))
                                         ;; ...and/or maybe add its doc on a line by its own.
                                         (let (fpardoc)
                                           (when (and pardoc (not briefp)
                                                      (not (string-empty-p
                                                            (setq fpardoc (ethersync--format-markup pardoc)))))
                                             (insert "\n  "
                                                     (propertize
                                                      (if (stringp parlabel) parlabel
                                                        (apply #'substring siglabel (mapcar #'1+ parlabel)))
                                                      'face (and (eq i active-param) 'eldoc-highlight-function-argument))
                                                     ": " fpardoc)))))
                      (buffer-string))))

(defun ethersync-signature-eldoc-function (cb)
  "A member of `eldoc-documentation-functions', for signatures."
  (when (ethersync-server-capable :signatureHelpProvider)
    (let ((buf (current-buffer)))
      (jsonrpc-async-request
       (ethersync--current-server-or-lose)
       :textDocument/signatureHelp (ethersync--TextDocumentPositionParams)
       :success-fn
       (ethersync--lambda ((SignatureHelp)
                           signatures activeSignature (activeParameter 0))
                          (ethersync--when-buffer-window buf
                                                         (let ((active-sig (and (cl-plusp (length signatures))
                                                                                (aref signatures (or activeSignature 0)))))
                                                           (if (not active-sig) (funcall cb nil)
                                                             (funcall
                                                              cb (mapconcat (lambda (s)
                                                                              (ethersync--sig-info s (and (eq s active-sig)
                                                                                                          activeParameter)
                                                                                                   nil))
                                                                            signatures "\n")
                                                              :echo (ethersync--sig-info active-sig activeParameter t))))))
       :deferred :textDocument/signatureHelp))
    t))

(defun ethersync-hover-eldoc-function (cb)
  "A member of `eldoc-documentation-functions', for hover."
  (when (ethersync-server-capable :hoverProvider)
    (let ((buf (current-buffer)))
      (jsonrpc-async-request
       (ethersync--current-server-or-lose)
       :textDocument/hover (ethersync--TextDocumentPositionParams)
       :success-fn (ethersync--lambda ((Hover) contents range)
                                      (ethersync--when-buffer-window buf
                                                                     (let ((info (unless (seq-empty-p contents)
                                                                                   (ethersync--hover-info contents range))))
                                                                       (funcall cb info
                                                                                :echo (and info (string-match "\n" info))))))
       :deferred :textDocument/hover))
    (ethersync--highlight-piggyback cb)
    t))

(defvar ethersync--highlights nil "Overlays for textDocument/documentHighlight.")

(defun ethersync--highlight-piggyback (_cb)
  "Request and handle `:textDocument/documentHighlight'."
  ;; FIXME: Obviously, this is just piggy backing on eldoc's calls for
  ;; convenience, as shown by the fact that we just ignore cb.
  (let ((buf (current-buffer)))
    (when (ethersync-server-capable :documentHighlightProvider)
      (jsonrpc-async-request
       (ethersync--current-server-or-lose)
       :textDocument/documentHighlight (ethersync--TextDocumentPositionParams)
       :success-fn
       (lambda (highlights)
         (mapc #'delete-overlay ethersync--highlights)
         (setq ethersync--highlights
               (ethersync--when-buffer-window buf
                                              (mapcar
                                               (ethersync--lambda ((DocumentHighlight) range)
                                                                  (pcase-let ((`(,beg . ,end)
                                                                               (ethersync-range-region range)))
                                                                    (let ((ov (make-overlay beg end)))
                                                                      (overlay-put ov 'face 'ethersync-highlight-symbol-face)
                                                                      (overlay-put ov 'modification-hooks
                                                                                   `(,(lambda (o &rest _) (delete-overlay o))))
                                                                      ov)))
                                               highlights))))
       :deferred :textDocument/documentHighlight)
      nil)))

(defun ethersync--imenu-SymbolInformation (res)
  "Compute `imenu--index-alist' for RES vector of SymbolInformation."
  (mapcar
   (pcase-lambda (`(,kind . ,objs))
     (cons
      (alist-get kind ethersync--symbol-kind-names "Unknown")
      (mapcan
       (pcase-lambda (`(,container . ,objs))
         (let ((elems (mapcar
                       (ethersync--lambda ((SymbolInformation) kind name location)
                                          (let ((reg (ethersync-range-region
                                                      (plist-get location :range)))
                                                (kind (alist-get kind ethersync--symbol-kind-names)))
                                            (cons (propertize name
                                                              'breadcrumb-region reg
                                                              'breadcrumb-kind kind)
                                                  (car reg))))
                       objs)))
           (if container (list (cons container elems)) elems)))
       (seq-group-by
        (ethersync--lambda ((SymbolInformation) containerName) containerName) objs))))
   (seq-group-by (ethersync--lambda ((SymbolInformation) kind) kind) res)))

(defun ethersync--imenu-DocumentSymbol (res)
  "Compute `imenu--index-alist' for RES vector of DocumentSymbol."
  (cl-labels ((dfs (&key name children range kind &allow-other-keys)
                (let* ((reg (ethersync-range-region range))
                       (kind (alist-get kind ethersync--symbol-kind-names))
                       (name (propertize name
                                         'breadcrumb-region reg
                                         'breadcrumb-kind kind)))
                  (if (seq-empty-p children)
                      (cons name (car reg))
                    (cons name
                          (mapcar (lambda (c) (apply #'dfs c)) children))))))
    (mapcar (lambda (s) (apply #'dfs s)) res)))

(cl-defun ethersync-imenu ()
  "Ethersync's `imenu-create-index-function'.
Returns a list as described in docstring of `imenu--index-alist'."
  (unless (ethersync-server-capable :documentSymbolProvider)
    (cl-return-from ethersync-imenu))
  (let* ((res (ethersync--request (ethersync--current-server-or-lose)
                                  :textDocument/documentSymbol
                                  `(:textDocument
                                    ,(ethersync--TextDocumentIdentifier))
                                  :cancel-on-input non-essential))
         (head (and (cl-plusp (length res)) (elt res 0))))
    (when head
      (ethersync--dcase head
                        (((SymbolInformation)) (ethersync--imenu-SymbolInformation res))
                        (((DocumentSymbol)) (ethersync--imenu-DocumentSymbol res))))))

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
            (mapcar (ethersync--lambda ((TextEdit) range newText)
                                       (cons newText (ethersync-range-region range 'markers)))
                    (reverse edits)))
      (undo-amalgamate-change-group change-group)
      (when reporter
        (progress-reporter-done reporter)))))

(defun ethersync--confirm-server-edits (origin _prepared)
  "Helper for `ethersync--apply-workspace-edit.
ORIGIN is a symbol designating a command.  Reads the
`ethersync-confirm-server-edits' user option and returns a symbol
like `diff', `summary' or nil."
  (let (v)
    (cond ((symbolp ethersync-confirm-server-edits) ethersync-confirm-server-edits)
          ((setq v (assoc origin ethersync-confirm-server-edits)) (cdr v))
          ((setq v (assoc t ethersync-confirm-server-edits)) (cdr v)))))

(defun ethersync--propose-changes-as-diff (prepared)
  "Helper for `ethersync--apply-workspace-edit'.
Goal is to popup a `diff-mode' buffer containing all the changes
of PREPARED, ready to apply with C-c C-a.  PREPARED is a
list ((FILENAME EDITS VERSION)...)."
  (with-current-buffer (get-buffer-create "*ETHERSYNC proposed server changes*")
    (buffer-disable-undo (current-buffer))
    (let ((inhibit-read-only t)
          (target (current-buffer)))
      (diff-mode)
      (erase-buffer)
      (pcase-dolist (`(,path ,edits ,_) prepared)
        (with-temp-buffer
          (let* ((diff (current-buffer))
                 (existing-buf (find-buffer-visiting path))
                 (existing-buf-label (prin1-to-string existing-buf)))
            (with-temp-buffer
              (if existing-buf
                  (insert-buffer-substring existing-buf)
                (insert-file-contents path))
              (ethersync--apply-text-edits edits nil t)
              (diff-no-select (or existing-buf path) (current-buffer) nil t diff)
              (when existing-buf
                ;; Here we have to pretend the label of the unsaved
                ;; buffer is the actual file, just so that we can
                ;; diff-apply without troubles.  If there's a better
                ;; way, it probably involves changes to `diff.el'.
                (with-current-buffer diff
                  (goto-char (point-min))
                  (while (search-forward existing-buf-label nil t)
                    (replace-match (buffer-file-name existing-buf))))))
            (with-current-buffer target
              (insert-buffer-substring diff))))))
    (setq-local buffer-read-only t)
    (buffer-enable-undo (current-buffer))
    (goto-char (point-min))
    (pop-to-buffer (current-buffer))
    (font-lock-ensure)))

(defun ethersync--apply-workspace-edit (wedit origin)
  "Apply (or offer to apply) the workspace edit WEDIT.
ORIGIN is a symbol designating the command that originated this
edit proposed by the server."
  (ethersync--dbind ((WorkspaceEdit) changes documentChanges) wedit
                    (let ((prepared
                           (mapcar (ethersync--lambda ((TextDocumentEdit) textDocument edits)
                                                      (ethersync--dbind ((VersionedTextDocumentIdentifier) uri version)
                                                                        textDocument
                                                                        (list (ethersync-uri-to-path uri) edits version)))
                                   documentChanges)))
                      (unless (and changes documentChanges)
                        ;; We don't want double edits, and some servers send both
                        ;; changes and documentChanges.  This unless ensures that we
                        ;; prefer documentChanges over changes.
                        (cl-loop for (uri edits) on changes by #'cddr
                                 do (push (list (ethersync-uri-to-path uri) edits) prepared)))
                      (cl-flet ((notevery-visited-p ()
                                  (cl-notevery #'find-buffer-visiting
                                               (mapcar #'car prepared)))
                                (accept-p ()
                                  (y-or-n-p
                                   (format "[ethersync] Server wants to edit:\n%sProceed? "
                                           (cl-loop
                                            for (f eds _) in prepared
                                            concat (format
                                                    "  %s (%d change%s)\n"
                                                    f (length eds)
                                                    (if (> (length eds) 1) "s" ""))))))
                                (apply ()
                                  (cl-loop for edit in prepared
                                           for (path edits version) = edit
                                           do (with-current-buffer (find-file-noselect path)
                                                (ethersync--apply-text-edits edits version))
                                           finally (eldoc) (ethersync--message "Edit successful!"))))
                        (let ((decision (ethersync--confirm-server-edits origin prepared)))
                          (cond
                           ((or (eq decision 'diff)
                                (and (eq decision 'maybe-diff) (notevery-visited-p)))
                            (ethersync--propose-changes-as-diff prepared))
                           ((or (memq decision '(t summary))
                                (and (eq decision 'maybe-summary) (notevery-visited-p)))
                            (when (accept-p) (apply)))
                           (t
                            (apply))))))))

(defun ethersync-rename (newname)
  "Rename the current symbol to NEWNAME."
  (interactive
   (list (read-from-minibuffer
          (format "Rename `%s' to: " (or (thing-at-point 'symbol t)
                                         "unknown symbol"))
          nil nil nil nil
          (symbol-name (symbol-at-point)))))
  (ethersync-server-capable-or-lose :renameProvider)
  (ethersync--apply-workspace-edit
   (ethersync--request (ethersync--current-server-or-lose)
                       :textDocument/rename `(,@(ethersync--TextDocumentPositionParams)
                                              :newName ,newname))
   this-command))

(defun ethersync--code-action-bounds ()
  "Calculate appropriate bounds depending on region and point."
  (let (diags boftap)
    (cond ((use-region-p) `(,(region-beginning) ,(region-end)))
          ((setq diags (flymake-diagnostics (point)))
           (cl-loop for d in diags
                    minimizing (flymake-diagnostic-beg d) into beg
                    maximizing (flymake-diagnostic-end d) into end
                    finally (cl-return (list beg end))))
          ((setq boftap (bounds-of-thing-at-point 'sexp))
           (list (car boftap) (cdr boftap)))
          (t
           (list (point) (point))))))

(defun ethersync-code-actions (beg &optional end action-kind interactive)
  "Find LSP code actions of type ACTION-KIND between BEG and END.
Interactively, offer to execute them.
If ACTION-KIND is nil, consider all kinds of actions.
Interactively, default BEG and END to region's bounds else BEG is
point and END is nil, which results in a request for code actions
at point.  With prefix argument, prompt for ACTION-KIND."
  (interactive
   `(,@(ethersync--code-action-bounds)
     ,(and current-prefix-arg
           (completing-read "[ethersync] Action kind: "
                            '("quickfix" "refactor.extract" "refactor.inline"
                              "refactor.rewrite" "source.organizeImports")))
     t))
  (ethersync-server-capable-or-lose :codeActionProvider)
  (let* ((server (ethersync--current-server-or-lose))
         (actions
          (ethersync--request
           server
           :textDocument/codeAction
           (list :textDocument (ethersync--TextDocumentIdentifier)
                 :range (list :start (ethersync--pos-to-lsp-position beg)
                              :end (ethersync--pos-to-lsp-position end))
                 :context
                 `(:diagnostics
                   [,@(cl-loop for diag in (flymake-diagnostics beg end)
                               when (cdr (assoc 'ethersync-lsp-diag
                                                (ethersync--diag-data diag)))
                               collect it)]
                   ,@(when action-kind `(:only [,action-kind]))))))
         ;; Redo filtering, in case the `:only' didn't go through.
         (actions (cl-loop for a across actions
                           when (or (not action-kind)
                                    ;; github#847
                                    (string-prefix-p action-kind (plist-get a :kind)))
                           collect a)))
    (if interactive
        (ethersync--read-execute-code-action actions server action-kind)
      actions)))

(defalias 'ethersync-code-actions-at-mouse (ethersync--mouse-call 'ethersync-code-actions)
  "Like `ethersync-code-actions', but intended for mouse events.")

(defun ethersync--read-execute-code-action (actions server &optional action-kind)
  "Helper for interactive calls to `ethersync-code-actions'."
  (let* ((menu-items
          (or (cl-loop for a in actions
                       collect (cons (plist-get a :title) a))
              (apply #'ethersync--error
                     (if action-kind `("No \"%s\" code actions here" ,action-kind)
                       `("No code actions here")))))
         (preferred-action (cl-find-if
                            (lambda (menu-item)
                              (plist-get (cdr menu-item) :isPreferred))
                            menu-items))
         (default-action (car (or preferred-action (car menu-items))))
         (chosen (if (and action-kind (null (cadr menu-items)))
                     (cdr (car menu-items))
                   (if (listp last-nonmenu-event)
                       (x-popup-menu last-nonmenu-event `("Ethersync code actions:"
                                                          ("dummy" ,@menu-items)))
                     (cdr (assoc (completing-read
                                  (format "[ethersync] Pick an action (default %s): "
                                          default-action)
                                  menu-items nil t nil nil default-action)
                                 menu-items))))))
    (ethersync-execute server chosen)))

(defmacro ethersync--code-action (name kind)
  "Define NAME to execute KIND code action."
  `(defun ,name (beg &optional end)
     ,(format "Execute `%s' code actions between BEG and END." kind)
     (interactive (ethersync--code-action-bounds))
     (ethersync-code-actions beg end ,kind t)))

(ethersync--code-action ethersync-code-action-organize-imports "source.organizeImports")
(ethersync--code-action ethersync-code-action-extract "refactor.extract")
(ethersync--code-action ethersync-code-action-inline "refactor.inline")
(ethersync--code-action ethersync-code-action-rewrite "refactor.rewrite")
(ethersync--code-action ethersync-code-action-quickfix "quickfix")


;;; Dynamic registration
;;;
(cl-defmethod ethersync-register-capability
  (server (method (eql workspace/didChangeWatchedFiles)) id &key watchers)
  "Handle dynamic registration of workspace/didChangeWatchedFiles."
  (ethersync-unregister-capability server method id)
  (let* (success
         (globs (mapcar
                 (ethersync--lambda ((FileSystemWatcher) globPattern kind)
                                    (cons (ethersync--glob-compile globPattern t t)
                                          ;; the default "7" means bitwise OR of
                                          ;; WatchKind.Create (1), WatchKind.Change
                                          ;; (2), WatchKind.Delete (4)
                                          (or kind 7)))
                 watchers))
         (dirs-to-watch
          (delete-dups (mapcar #'file-name-directory
                               (project-files
                                (ethersync--project server))))))
    (cl-labels
        ((handle-event (event)
           (pcase-let* ((`(,desc ,action ,file ,file1) event)
                        (action-type (cl-case action
                                       (created 1) (changed 2) (deleted 3)))
                        (action-bit (when action-type
                                      (ash 1 (1- action-type)))))
             (cond
              ((and (memq action '(created changed deleted))
                    (cl-loop for (glob . kind-bitmask) in globs
                             thereis (and (> (logand kind-bitmask action-bit) 0)
                                          (funcall glob file))))
               (jsonrpc-notify
                server :workspace/didChangeWatchedFiles
                `(:changes ,(vector `(:uri ,(ethersync-path-to-uri file)
                                      :type ,action-type))))
               (when (and (eq action 'created)
                          (file-directory-p file))
                 (watch-dir file)))
              ((eq action 'renamed)
               (handle-event `(,desc 'deleted ,file))
               (handle-event `(,desc 'created ,file1))))))
         (watch-dir (dir)
           (when-let ((probe
                       (and (file-readable-p dir)
                            (or (gethash dir (ethersync--file-watches server))
                                (puthash dir (list (file-notify-add-watch
                                                    dir '(change) #'handle-event))
                                         (ethersync--file-watches server))))))
             (push id (cdr probe)))))
      (unwind-protect
          (progn
            (mapc #'watch-dir dirs-to-watch)
            (setq
             success
             `(:message ,(format "OK, watching %s directories in %s watchers"
                                 (length dirs-to-watch) (length watchers)))))
        (unless success
          (ethersync-unregister-capability server method id))))))

(cl-defmethod ethersync-unregister-capability
  (server (_method (eql workspace/didChangeWatchedFiles)) id)
  "Handle dynamic unregistration of workspace/didChangeWatchedFiles."
  (maphash (lambda (dir watch-and-ids)
             (setcdr watch-and-ids (delete id (cdr watch-and-ids)))
             (when (null (cdr watch-and-ids))
               (file-notify-rm-watch (car watch-and-ids))
               (remhash dir (ethersync--file-watches server))))
           (ethersync--file-watches server))
  (list t "OK"))


;;; Glob heroics
;;;
(defun ethersync--glob-parse (glob)
  "Compute list of (STATE-SYM EMITTER-FN PATTERN)."
  (with-temp-buffer
    (save-excursion (insert glob))
    (cl-loop
     with grammar = '((:**      "\\*\\*/?"              ethersync--glob-emit-**)
                      (:*       "\\*"                   ethersync--glob-emit-*)
                      (:?       "\\?"                   ethersync--glob-emit-?)
                      (:{}      "{[^][*{}]+}"           ethersync--glob-emit-{})
                      (:range   "\\[\\^?[^][/,*{}]+\\]" ethersync--glob-emit-range)
                      (:literal "[^][,*?{}]+"           ethersync--glob-emit-self))
     until (eobp)
     collect (cl-loop
              for (_token regexp emitter) in grammar
              thereis (and (re-search-forward (concat "\\=" regexp) nil t)
                           (list (cl-gensym "state-") emitter (match-string 0)))
              finally (error "Glob '%s' invalid at %s" (buffer-string) (point))))))

(defun ethersync--glob-compile (glob &optional byte-compile noerror)
  "Convert GLOB into Elisp function.  Maybe BYTE-COMPILE it.
If NOERROR, return predicate, else erroring function."
  (let* ((states (ethersync--glob-parse glob))
         (body `(with-current-buffer (get-buffer-create " *ethersync-glob-matcher*")
                  (erase-buffer)
                  (save-excursion (insert string))
                  (cl-labels ,(cl-loop for (this that) on states
                                       for (self emit text) = this
                                       for next = (or (car that) 'eobp)
                                       collect (funcall emit text self next))
                    (or (,(caar states))
                        (error "Glob done but more unmatched text: '%s'"
                               (buffer-substring (point) (point-max)))))))
         (form `(lambda (string) ,(if noerror `(ignore-errors ,body) body))))
    (if byte-compile (byte-compile form) form)))

(defun ethersync--glob-emit-self (text self next)
  `(,self () (re-search-forward ,(concat "\\=" (regexp-quote text))) (,next)))

(defun ethersync--glob-emit-** (_ self next)
  `(,self () (or (ignore-errors (save-excursion (,next)))
                 (and (re-search-forward "\\=/?[^/]+/?") (,self)))))

(defun ethersync--glob-emit-* (_ self next)
  `(,self () (re-search-forward "\\=[^/]")
    (or (ignore-errors (save-excursion (,next))) (,self))))

(defun ethersync--glob-emit-? (_ self next)
  `(,self () (re-search-forward "\\=[^/]") (,next)))

(defun ethersync--glob-emit-{} (arg self next)
  (let ((alternatives (split-string (substring arg 1 (1- (length arg))) ",")))
    `(,self ()
      (or (re-search-forward ,(concat "\\=" (regexp-opt alternatives)) nil t)
          (error "Failed matching any of %s" ',alternatives))
      (,next))))

(defun ethersync--glob-emit-range (arg self next)
  (when (eq ?! (aref arg 1)) (aset arg 1 ?^))
  `(,self () (re-search-forward ,(concat "\\=" arg)) (,next)))


;;; List connections mode

(define-derived-mode ethersync-list-connections-mode  tabulated-list-mode
  "" "Ethersync mode for listing server connections
\\{ethersync-list-connections-mode-map}"
  :interactive nil
  (setq-local tabulated-list-format
              `[("Language server" 16) ("Project name" 16) ("Modes handled" 16)])
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
                           `[,(or (plist-get (ethersync--server-info server) :name)
                                  (jsonrpc-name server))
                             ,(ethersync-project-nickname server)
                             ,(mapconcat #'symbol-name
                                         (ethersync--major-modes server)
                                         ", ")]))
                   (cl-reduce #'append
                              (hash-table-values ethersync--servers-by-project))))
      (revert-buffer)
      (pop-to-buffer (current-buffer)))))


;;; Inlay hints
(defface ethersync-inlay-hint-face '((t (:height 0.8 :inherit shadow)))
  "Face used for inlay hint overlays.")

(defface ethersync-type-hint-face '((t (:inherit ethersync-inlay-hint-face)))
  "Face used for type inlay hint overlays.")

(defface ethersync-parameter-hint-face '((t (:inherit ethersync-inlay-hint-face)))
  "Face used for parameter inlay hint overlays.")

(defvar-local ethersync--outstanding-inlay-hints-region (cons nil nil)
  "Jit-lock-calculated (FROM . TO) region with potentially outdated hints")

(defvar-local ethersync--outstanding-inlay-hints-last-region nil)

(defvar-local ethersync--outstanding-inlay-regions-timer nil
  "Helper timer for `ethersync--update-hints'")

(defun ethersync--update-hints (from to)
  "Jit-lock function for Ethersync inlay hints."
  (cl-symbol-macrolet ((region ethersync--outstanding-inlay-hints-region)
                       (last-region ethersync--outstanding-inlay-hints-last-region)
                       (timer ethersync--outstanding-inlay-regions-timer))
    (setcar region (min (or (car region) (point-max)) from))
    (setcdr region (max (or (cdr region) (point-min)) to))
    ;; HACK: We're relying on knowledge of jit-lock internals here.  The
    ;; condition comparing `jit-lock-context-unfontify-pos' to
    ;; `point-max' is a heuristic for telling whether this call to
    ;; `jit-lock-functions' happens after `jit-lock-context-timer' has
    ;; just run.  Only after this delay should we start the smoothing
    ;; timer that will eventually call `ethersync--update-hints-1' with the
    ;; coalesced region.  I wish we didn't need the timer, but sometimes
    ;; a lot of "non-contextual" calls come in all at once and do verify
    ;; the condition.  Notice it is a 0 second timer though, so we're
    ;; not introducing any more delay over jit-lock's timers.
    (when (= jit-lock-context-unfontify-pos (point-max))
      (if timer (cancel-timer timer))
      (let ((buf (current-buffer)))
        (setq timer (run-at-time
                     0 nil
                     (lambda ()
                       (ethersync--when-live-buffer buf
                                                    ;; HACK: In some pathological situations
                                                    ;; (Emacs's own coding.c, for example),
                                                    ;; jit-lock is calling `ethersync--update-hints'
                                                    ;; repeatedly with same sequence of
                                                    ;; arguments, which leads to
                                                    ;; `ethersync--update-hints-1' being called with
                                                    ;; the same region repeatedly.  This happens
                                                    ;; even if the hint-painting code does
                                                    ;; nothing else other than widen, narrow,
                                                    ;; move point then restore these things.
                                                    ;; Possible Emacs bug, but this fixes it.
                                                    (unless (equal last-region region)
                                                      (ethersync--update-hints-1 (max (car region) (point-min))
                                                                                 (min (cdr region) (point-max)))
                                                      (setq last-region region))
                                                    (setq region (cons nil nil)
                                                          timer nil)))))))))

(defun ethersync--update-hints-1 (from to)
  "Do most work for `ethersync--update-hints', including LSP request."
  (let* ((buf (current-buffer))
         (paint-hint
          (ethersync--lambda ((InlayHint) position kind label paddingLeft paddingRight)
                             (goto-char (ethersync--lsp-position-to-point position))
                             (when (or (> (point) to) (< (point) from)) (cl-return))
                             (let* ((left-pad (and paddingLeft
                                                   (not (eq paddingLeft :json-false))
                                                   (not (memq (char-before) '(32 9))) " "))
                                    (right-pad (and paddingRight
                                                    (not (eq paddingRight :json-false))
                                                    (not (memq (char-after) '(32 9))) " "))
                                    (peg-after-p (eql kind 1)))
                               (cl-labels
                                   ((make-ov ()
                                      (if peg-after-p
                                          (make-overlay (point) (1+ (point)) nil t)
                                        (make-overlay (1- (point)) (point) nil nil nil)))
                                    (do-it (label lpad rpad i n)
                                      (let* ((firstp (zerop i))
                                             (tweak-cursor-p (and firstp peg-after-p))
                                             (ov (make-ov))
                                             (text (concat lpad label rpad)))
                                        (when tweak-cursor-p (put-text-property 0 1 'cursor 1 text))
                                        (overlay-put ov (if peg-after-p 'before-string 'after-string)
                                                     (propertize
                                                      text
                                                      'face (pcase kind
                                                              (1 'ethersync-type-hint-face)
                                                              (2 'ethersync-parameter-hint-face)
                                                              (_ 'ethersync-inlay-hint-face))))
                                        (overlay-put ov 'priority (if peg-after-p i (- n i)))
                                        (overlay-put ov 'ethersync--inlay-hint t)
                                        (overlay-put ov 'evaporate t)
                                        (overlay-put ov 'ethersync--overlay t))))
                                 (if (stringp label) (do-it label left-pad right-pad 0 1)
                                   (cl-loop
                                    for i from 0 for ldetail across label
                                    do (ethersync--dbind ((InlayHintLabelPart) value) ldetail
                                                         (do-it value
                                                                (and (zerop i) left-pad)
                                                                (and (= i (1- (length label))) right-pad)
                                                                i (length label))))))))))
    (jsonrpc-async-request
     (ethersync--current-server-or-lose)
     :textDocument/inlayHint
     (list :textDocument (ethersync--TextDocumentIdentifier)
           :range (list :start (ethersync--pos-to-lsp-position from)
                        :end (ethersync--pos-to-lsp-position to)))
     :success-fn (lambda (hints)
                   (ethersync--when-live-buffer buf
                                                (ethersync--widening
                                                 ;; Overlays ending right at FROM with an
                                                 ;; `after-string' property logically belong to
                                                 ;; the (FROM TO) region.  Likewise, such
                                                 ;; overlays ending at TO don't logically belong
                                                 ;; to it.
                                                 (dolist (o (overlays-in (1- from) to))
                                                   (when (and (overlay-get o 'ethersync--inlay-hint)
                                                              (cond ((eq (overlay-end o) from)
                                                                     (overlay-get o 'after-string))
                                                                    ((eq (overlay-end o) to)
                                                                     (overlay-get o 'before-string))
                                                                    (t)))
                                                     (delete-overlay o)))
                                                 (mapc paint-hint hints))))
     :deferred 'ethersync--update-hints-1)))

(define-minor-mode ethersync-inlay-hints-mode
  "Minor mode for annotating buffers with LSP server's inlay hints."
  :global nil
  (cond (ethersync-inlay-hints-mode
         (if (ethersync-server-capable :inlayHintProvider)
             (jit-lock-register #'ethersync--update-hints 'contextual)
           (ethersync-inlay-hints-mode -1)))
        (t
         (jit-lock-unregister #'ethersync--update-hints)
         (remove-overlays nil nil 'ethersync--inlay-hint t))))


;;; Hacks
;;;
;; Emacs bug#56407, the optimal solution is in desktop.el, but that's
;; harder. For now, use `with-eval-after-load'. See also github#1183.
(with-eval-after-load 'desktop
  (add-to-list 'desktop-minor-mode-handlers '(ethersync--managed-mode . ignore))
  (add-to-list 'desktop-minor-mode-handlers '(ethersync-inlay-hints-mode . ignore)))


;;; Misc
;;;
;;;###autoload
(progn
  (put 'ethersync--debbugs-or-github-bug-uri 'bug-reference-url-format t)
  (defun ethersync--debbugs-or-github-bug-uri ()
    (format (if (string= (match-string 2) "github")
                "https://github.com/joaotavora/ethersync/issues/%s"
              "https://debbugs.gnu.org/%s")
            (match-string 3))))

;; Add command-mode property manually for compatibility with Emacs < 28.
(dolist (sym '(ethersync-clear-status
               ethersync-code-action-inline
               ethersync-code-action-organize-imports
               ethersync-code-action-quickfix
               ethersync-code-action-rewrite
               ethersync-code-action-rewrite
               ethersync-code-actions
               ethersync-find-declaration
               ethersync-find-implementation
               ethersync-find-typeDefinition
               ethersync-forget-pending-continuations
               ethersync-format
               ethersync-format-buffer
               ethersync-inlay-hints-mode
               ethersync-reconnect
               ethersync-rename
               ethersync-signal-didChangeConfiguration
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
