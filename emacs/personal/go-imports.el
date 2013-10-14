;; Mostly copied from go-mode.el

;;;###autoload
(defun goimports-before-save ()
  "Add this to .emacs to run gofmt on the current buffer when saving:
 (add-hook 'before-save-hook 'gofmt-before-save).

Note that this will cause go-mode to get loaded the first time
you save any file, kind of defeating the point of autoloading."

  (interactive)
  (when (eq major-mode 'go-mode) (goimports)))

(defun goimports ()
  "Formats the current buffer according to the goimports tool."

  (interactive)
  (let ((tmpfile (make-temp-file "gofmt" nil ".go"))
        (patchbuf (get-buffer-create "*Gofmt patch*"))
        (errbuf (get-buffer-create "*Gofmt Errors*"))
        (coding-system-for-read 'utf-8)
        (coding-system-for-write 'utf-8))

    (with-current-buffer errbuf
      (setq buffer-read-only nil)
      (erase-buffer))
    (with-current-buffer patchbuf
      (erase-buffer))

    (write-region nil nil tmpfile)

    ;; We're using errbuf for the mixed stdout and stderr output. This
    ;; is not an issue because gofmt -w does not produce any stdout
    ;; output in case of success.
    (if (zerop (call-process "goimports" nil errbuf nil "-w" tmpfile))
        (if (zerop (call-process-region (point-min) (point-max) "diff" nil patchbuf nil "-n" "-" tmpfile))
            (progn
              (kill-buffer errbuf)
              (message "Buffer is already gofmted"))
          (go--apply-rcs-patch patchbuf)
          (kill-buffer errbuf)
          (message "Applied gofmt"))
      (message "Could not apply gofmt. Check errors for details")
      (gofmt--process-errors (buffer-file-name) tmpfile errbuf))

    (kill-buffer patchbuf)
    (delete-file tmpfile)))

(provide 'go-imports)
