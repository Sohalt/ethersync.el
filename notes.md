cursors:

`make-overlay`

edit

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
