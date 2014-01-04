;; (require-package 'git-gutter)
(require-package 'git-gutter-fringe)
(setq git-gutter-fr:side 'right-fringe)
(global-git-gutter-mode t)

(bind-key "<f4><f4>t" 'git-gutter:toggle)
(bind-key "<f4>j" 'git-gutter:next-diff)
(bind-key "<f4>k" 'git-gutter:previous-diff)
(bind-key "<f4><f4>d" 'git-gutter:popup-diff)
(bind-key "<f4><f4>r" 'git-gutter:revert-hunk)

(require-package 'magit)
(defun magit-diff-project ()
  (interactive)
  (magit-diff-working-tree "HEAD")
  (switch-to-buffer magit-diff-buffer-name)
  (delete-other-windows))

(bind-key "<escape><escape>m" `magit-diff-project)
(bind-key "<f1><f1>m" `magit-diff-project)

(require-package 'git-commit-mode)
(require-package 'gitconfig-mode)
(require-package 'gitignore-mode)
(require-package 'git-messenger)
(bind-key "C-x v p" 'git-messenger:popup-message)
(setq
 magit-stage-all-confirm nil
 magit-unstage-all-confirm nil)

(bind-key "<escape>m" 'magit-status)
(bind-key "<f1>m" 'magit-status)


(setq vc-follow-symlinks t)
(defadvice vc-git-mode-line-string (after plus-minus (file) compile activate)
  (setq ad-return-value
        (concat ad-return-value
                (let ((plus-minus (vc-git--run-command-string
                                   file "diff" "--numstat" "--")))
                  (and plus-minus
                       (string-match "^\\([0-9]+\\)\t\\([0-9]+\\)\t" plus-minus)
                       (format " +%s-%s" (match-string 1 plus-minus) (match-string 2 plus-minus))))
                )
        ))
