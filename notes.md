receive cursors:

```
(let ((ov (make-overlay (point) (1+ (point)) (current-buffer))))
  (overlay-put ov 'face 'highlight))
```

receive edit

```elisp
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
```

send edit

```
ethersync--post-self-insert-hook
```

### Incompat with LSP

why not send multiple edits?

why not use lsp types?

```
      (Position (:line :character))
      (Range (:start :end))
      (TextDocumentEdit (:textDocument :edits) ())
      (TextEdit (:range :newText))
      (VersionedTextDocumentIdentifier (:uri :version) ())
```
