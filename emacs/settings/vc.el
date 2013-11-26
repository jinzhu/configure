(require-package 'git-gutter)
(global-git-gutter-mode t)

(bind-key "<escape><escape>t" 'git-gutter:toggle)
(bind-key "<escape><escape>j" 'git-gutter:next-diff)
(bind-key "<escape><escape>k" 'git-gutter:previous-diff)
(bind-key "<escape><escape>d" 'git-gutter:popup-diff)
(bind-key "<escape><escape>r" 'git-gutter:revert-hunk)


(require-package 'magit)
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
