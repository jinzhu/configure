;; Deft
(require-package 'deft)

(setq deft-extension "org"
      deft-directory "/jinzhu/Dropbox/Notebooks"
      deft-text-mode 'org-mode
      deft-use-filename-as-title t)

(bind-key* "<f2>d" 'deft)


;; Org
(require-package 'org)
(require-package 'org-screenshot)
(require-package 'org-magit)

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
      org-clock-persist-file (expand-file-name "org-clock-save.el" cache-dir)

      org-log-done 'time
      org-log-done 'note

      org-agenda-files (list "~/.todos")
      org-agenda-include-diary t
      org-agenda-ndays 7
      org-agenda-start-on-weekday 0
      org-agenda-repeating-timestamp-show-all t
      org-agenda-sorting-strategy (quote ((agenda time-up priority-down tag-up) (todo tag-up)))
      org-agenda-window-setup 'current-window

      org-mobile-directory "~/.mobileorg"
      org-directory "~/.notes"

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
 '((emacs-lisp . t) (python . t) (ruby . t) (sh . t) (sql . nil)))

(org-clock-persistence-insinuate)
(org-agenda-to-appt t)

(defun org-table-run-and-select-next-line ()
  (interactive)
  (progn
    (org-ctrl-c-ctrl-c)
    (next-line)
    )
  )

(add-hook 'org-mode-hook (lambda () (electric-indent-mode -1)))

(bind-key "<f2>b" 'org-iswitchb)
(bind-key "<f2>t" 'org-todo-list)
(bind-key "<f2>r" 'remember)
(bind-key "<f2>s" 'org-search-view)
(bind-key "<f2>e" 'org-mu4e-compose-org-mode)
(bind-key "<f2><f5>" 'org-dblock-update)
(bind-key "<M-f2>" 'org-agenda-list)
(bind-key "<f2>c" 'org-table-run-and-select-next-line)

(bind-key "<f2>A"  'org-archive-to-archive-sibling)
(bind-key "<f2>a"  'org-toggle-archive-tag)
(bind-key "<f2>M-a"'org-advertized-archive-subtree)
(bind-key "<f2>k"  'org-kill-note-or-show-branches)

(bind-key "<f2>i" 'org-clock-in)
(bind-key "<f2>o" 'org-clock-out)
(bind-key "<f2>j" 'org-clock-goto)

(bind-key "<f2><f2>0" 'org-timer-start)
(bind-key "<f2><f2>." 'org-timer)
(bind-key "<f2><f2>-" 'org-timer-item)

(bind-key "<f2>." 'org-time-stamp)
(bind-key "<f2>l" 'org-store-link)
(bind-key "<f2><f2>l" 'org-insert-link)
(bind-key "<f2><f2>t" 'org-todo)

(bind-key "<f2><f2>e" 'org-babel-execute-mayb)
(bind-key "<f2>q" 'org-set-tags-command)
