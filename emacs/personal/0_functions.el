(defun select-current-line ()
  "Select the current line"
  (interactive)
  (end-of-line) ; move to end of line
  (set-mark (line-beginning-position)))

(defun copy-current-line (arg)
  "Copy lines (as many as prefix argument) in the kill ring"
  (interactive "p")
  (kill-ring-save (line-beginning-position)
                  (line-beginning-position (+ 1 arg)))
  (message "%d line%s copied" arg (if (= 1 arg) "" "s")))

(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)
    (next-line)))

;; Ditaa
(setq ditaa-cmd "java -jar /usr/share/java/ditaa/ditaa-0_9.jar")
(defun djcb-ditaa-generate ()
  (interactive)
  (shell-command
   (concat ditaa-cmd " " buffer-file-name)))

(defun get-string-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(defun goto-emacs-setting-file ()
  (interactive)
  (if (not (get-buffer "0_init.el"))
      (find-file (expand-file-name ".emacs.d/personal/0_init.el" (getenv "HOME"))))
  (switch-to-buffer "0_init.el"))

(defun goto-emacs-tips-file ()
  (interactive)
  (if (not (get-buffer "Emacs.org"))
      (find-file (expand-file-name "GIT/configure/Emacs.org" (getenv "HOME"))))
  (switch-to-buffer "Emacs.org"))

(defun vpnonline-hook ()
  (interactive)
  (if (not (get-buffer "*-jabber-roster-*"))
      (jabber-connect-all))
  (if (not (get-buffer "*weibo-timeline*"))
      (weibo-timeline))
  (if (not (get-buffer ":home"))
      (twit))
  )

(defun join-region (beg end)
  "Apply join-line over region."
  (interactive "r")
  (if mark-active
      (let ((beg (region-beginning))
            (end (copy-marker (region-end))))
        (goto-char beg)
        (while (< (point) end)
          (join-line 1)))))

;; search at points
(defvar isearch-initial-string nil)
(defun isearch-set-initial-string ()
  (remove-hook 'isearch-mode-hook 'isearch-set-initial-string)
  (setq isearch-string isearch-initial-string
        isearch-message isearch-initial-string)
  (isearch-search-and-update))

(defun isearch-forward-at-point (&optional regexp-p no-recursive-edit)
  "Interactive search forward for the symbol at point."
  (interactive "P\np")
  (if regexp-p (isearch-forward-regexp regexp-p no-recursive-edit)
    (let* ((end (progn (skip-syntax-forward "w_") (point)))
           (begin (progn (skip-syntax-backward "w_") (point))))
      (if (eq begin end)
          (isearch-forward-regexp regexp-p no-recursive-edit)
        (setq isearch-initial-string (buffer-substring begin end))
        (add-hook 'isearch-mode-hook 'isearch-set-initial-string)
        (isearch-forward-regexp regexp-p no-recursive-edit)))))


(defun delete-file-if-no-contents ()
  (when (and (buffer-file-name (current-buffer))
             (= 0 (buffer-size)))
    (when (y-or-n-p "Delete file and kill buffer ? ")
      (delete-file (buffer-file-name (current-buffer)))
      (kill-buffer (current-buffer)))))

(add-hook 'after-save-hook 'delete-file-if-no-contents)

(defun url-encode-string (str &optional encoding)
  (let ((encoding (or encoding 'utf-8)))
    (url-hexify-string (encode-coding-string str encoding))))

(defun url-decode-string (str &optional encoding)
  (let ((encoding (or encoding 'utf-8)))
    (decode-coding-string (url-unhex-string str) encoding)))

(defun url-decode-region (beg end)
  (interactive "r")
  (let ((pos beg)
        (str (buffer-substring beg end)))
    (goto-char beg)
    (delete-region beg end)
    (insert (url-decode-string str 'utf-8))))

(defun url-encode-region (beg end)
  (interactive "r")
  (let ((pos beg)
        (str (buffer-substring beg end)))
    (goto-char beg)
    (delete-region beg end)
    (insert (url-encode-string str 'utf-8))))

(defun jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.
This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))

(defun push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region
Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))
