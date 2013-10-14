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
