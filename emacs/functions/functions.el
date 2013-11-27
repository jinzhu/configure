(defun byte-compile-init-dir ()
  "Byte-compile all your dotfiles."
  (interactive)
  (byte-recompile-directory user-emacs-directory 0))

(defun remove-elc-on-save ()
  "If you're saving an elisp file, likely the .elc is no longer valid."
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))
            nil
            t))
(add-hook 'emacs-lisp-mode-hook 'remove-elc-on-save)

(defun open-with (arg)
  "Open visited file in default external program.
With a prefix ARG always prompt for command to use."
  (interactive "P")
  (when buffer-file-name
    (start-process "prelude-open-with-process"
                   "*prelude-open-with-output*"
                   (cond
                    ((and (not arg) (eq system-type 'darwin)) "open")
                    ((and (not arg) (member system-type '(gnu gnu/linux gnu/kfreebsd))) "xdg-open")
                    (t (read-shell-command "Open current file with: ")))
                   (shell-quote-argument buffer-file-name))))

(defun recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: "
                                   (-map 'abbreviate-file-name recentf-list)
                                   nil t)))
    (when file
      (find-file file))))

(defun comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command.
If no region is selected and current line is not blank and we are not at the
end of the line, then comment current line. Replaces default behaviour of
comment-dwim, when it inserts comment at the end of the line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position)
                                   (line-end-position))
    (comment-dwim arg)))

(defun join-region-or-line ()
  "Apply join-line over region or line."
  (interactive)
  (if mark-active

      (let ((beg (region-beginning))
            (end (copy-marker (region-end))))
        (goto-char beg)
        (while (< (point) end)
          (join-line 1)))

    (join-line 1)
    ))

(defun toggle-fullscreen ()
  "Make Emacs window fullscreen.

This follows freedesktop standards, should work in X servers."
  (interactive)
  (if (eq window-system 'x)
      (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                             '(2 "_NET_WM_STATE_FULLSCREEN" 0))
    (error "Only X server is supported")))

(bind-key "<f11>" 'toggle-fullscreen)

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
  (if (not (get-buffer "init.el"))
      (find-file (expand-file-name ".emacs.d/init.el" (getenv "HOME"))))
  (switch-to-buffer "init.el"))

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

(defun delete-file-if-no-contents ()
  (when (and (buffer-file-name (current-buffer))
             (= 0 (buffer-size)))
    (when (y-or-n-p "Delete file and kill buffer ? ")
      (delete-file (buffer-file-name (current-buffer)))
      (kill-buffer (current-buffer)))))

(add-hook 'after-save-hook 'delete-file-if-no-contents)

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

(defun add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))

(defun replace-region-by (fn)
  (let* ((beg (region-beginning))
         (end (region-end))
         (contents (buffer-substring beg end)))
    (delete-region beg end)
    (insert (funcall fn contents))))

(defun create-scratch-buffer nil
  "create a new scratch buffer to work in. (could be *scratch* - *scratchX*)"
  (interactive)
  (let ((n 0)
        bufname)
    (while (progn
             (setq bufname (concat "*scratch"
                                   (if (= n 0) "" (int-to-string n))
                                   "*"))
             (setq n (1+ n))
             (get-buffer bufname)))
    (switch-to-buffer (get-buffer-create bufname))
    (emacs-lisp-mode)
    ))
