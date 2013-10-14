;; Deft
(use-package deft
  :bind ("<f2>d" . deft)
  :init
  (progn
    (setq deft-extension "org"
          deft-directory "/jinzhu/Dropbox/Notebooks"
          deft-text-mode 'org-mode
          deft-use-filename-as-title t))
  )

;; Org
(require 'org-install)

(use-package org
  :init
  (progn
    (require 'org-install)

    (setq org-todo-keywords '(
                              (sequence "TODO(t)" "STARTED(s)" "NEXT(n)" "DONE(d)")
                              (sequence "QUESTION(q)" "WAITING(w)" "HOLD(h)" "CANCELED(c)")
                              )

          org-todo-keyword-faces
          '(("STARTED" . "yellow")
            ("DONE"     . (:foreground "#94ff94"))
            ("QUESTION" . (:foreground "blue"))
            ("DEFERRED" . (:foreground "#ffc358"))
            ("HOLD" :foreground "magenta" :weight bold)
            ("CANCELED" . (:foreground "#ff65ff" :weight bold)
             ))

          org-todo-state-tags-triggers
          (quote (("CANCELLED" ("CANCELLED" . t))
                  ("WAITING" ("WAITING" . t))
                  ("HOLD" ("WAITING" . t) ("HOLD" . t))
                  ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
                  ("DONE" ("WAITING") ("CANCELLED") ("HOLD"))))

          org-tag-alist '(("work" . ?w) ("idea" . ?i) ("home" . ?h) ("overwork" . ?o))

          org-startup-folded t
          org-completion-use-ido t

          org-clock-history-length 23
          org-clock-in-resume t
          org-clock-into-drawer t
          org-clock-out-remove-zero-time-clocks t
          org-clock-out-when-done t
          org-clock-persist 'history
          org-clock-persist-query-resume nil

          org-log-done 'time
          org-log-done 'note

          org-agenda-files (list "~/.todos")
          org-agenda-include-diary t
          org-agenda-ndays 7
          org-agenda-start-on-weekday 0
          org-agenda-repeating-timestamp-show-all t
          org-agenda-sorting-strategy (quote ((agenda time-up priority-down tag-up) (todo tag-up)))

          org-agenda-custom-commands '(
                                       ("w" todo "WAITING")
                                       ("u" tags-todo "+urgent")
                                       ("p" . "Priorities")
                                       ("pa" "A items" tags-todo "+PRIORITY=\"A\"")
                                       ("pb" "B items" tags-todo "+PRIORITY=\"B\"")
                                       ("pc" "C items" tags-todo "+PRIORITY=\"C\"")
                                       )

          org-confirm-babel-evaluate nil)

    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (python . t)
       (ruby . t)
       (sh . t)
       (sql . nil)
       ))

    (org-clock-persistence-insinuate)
    (org-agenda-to-appt t)

    (add-hook 'org-mode-hook (lambda () (electric-indent-mode -1)))

    (defun org-table-run-and-select-next-line ()
      (interactive)
      (progn
        (org-ctrl-c-ctrl-c)
        (next-line)
        )
      )
    )

  :bind (("<f2>b" . org-iswitchb)
         ("<f2>t" . org-todo-list)
         ("<f2>r" . remember)
         ("<f2>s" . org-search-view)
         ("<f2>e" . org-mu4e-compose-org-mode)
         ("<f2><f5>" . org-dblock-update)
         ("<M-f2>" . org-agenda-list)
         ("<f2>c" . org-table-run-and-select-next-line)

         ;; Archive
         ("<f2>A"  . org-archive-to-archive-sibling)
         ("<f2>a"  . org-toggle-archive-tag)
         ("<f2>M-a". org-advertized-archive-subtree)
         ("<f2>k"  . org-kill-note-or-show-branches)

         ("<f2>i" . org-clock-in)
         ("<f2>o" . org-clock-out)
         ("<f2>j" . org-clock-goto)

         ("<f2><f2>0" . org-timer-start)
         ("<f2><f2>." . org-timer)
         ("<f2><f2>-" . org-timer-item)

         ("<f2>." . org-time-stamp)
         ("<f2>l" . org-store-link)
         ("<f2><f2>l" . org-insert-link)
         ("<f2><f2>t" . org-todo)

         ("<f2><f2>e" . org-babel-execute-mayb)
         ("<f2>q" . org-set-tags-command)
         )
)
