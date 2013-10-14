;; git gutter
(global-git-gutter-mode t)
(global-set-key (kbd "<escape>gt") 'git-gutter:toggle)
(global-set-key (kbd "<escape>j") 'git-gutter:next-diff)
(global-set-key (kbd "<escape>k") 'git-gutter:previous-diff)
(global-set-key (kbd "<escape>gd") 'git-gutter:popup-diff)
(global-set-key (kbd "<escape>gr") 'git-gutter:revert-hunk)

;; VC
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
