(use-package git-gutter
  :init (global-git-gutter-mode t)
  :bind (
         ("<escape>gt" . git-gutter:toggle)
         ("<escape>j" . git-gutter:next-diff)
         ("<escape>k" . git-gutter:previous-diff)
         ("<escape>gd" . git-gutter:popup-diff)
         ("<escape>gr" . git-gutter:revert-hunk)
         ))

(use-package magit
  :init (setq
         magit-stage-all-confirm nil
         magit-unstage-all-confirm nil)
  :bind (
         ("<escape>m" . magit-status)
         ("<f1>m" . magit-status)
         ))

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
