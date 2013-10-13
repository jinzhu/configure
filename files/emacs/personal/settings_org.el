;; deft
(require 'deft)
(setq deft-directory "/jinzhu/Dropbox/Notebooks")
(setq deft-extension "org")
(setq deft-text-mode 'org-mode)
(global-set-key (kbd "<C-f7>") 'deft)
(setq deft-use-filename-as-title t)

;; Org Mode
(require 'org-install)
(setq
 org-todo-keywords '(
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

 ;; Show lot of clocking history so it's easy to pick items off the C-F11 list
 org-clock-history-length 23
 ;; Resume clocking task on clock-in if the clock is open
 org-clock-in-resume t
 ;; Save clock data and state changes and notes in the LOGBOOK drawer
 org-clock-into-drawer t
 ;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
 org-clock-out-remove-zero-time-clocks t
 ;; Clock out when moving task to a done state
 org-clock-out-when-done t
 ;; save the clock history across Emacs sessions
 org-clock-persist 'history
 ;; Do not prompt to resume an active clock
 org-clock-persist-query-resume nil

 org-startup-folded nil
 ;; org-log-done t
 org-log-done 'time
 org-log-done 'note
 org-completion-use-ido t

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

 org-confirm-babel-evaluate nil
 )

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

(defun no-electric-indent-mode-hook ()
  (electric-indent-mode -1))

(add-hook 'org-mode-hook 'no-electric-indent-mode-hook)
(global-set-key (kbd "<f7>b") 'org-iswitchb)
(global-set-key (kbd "<f7>t") 'org-todo-list)
(global-set-key (kbd "<f7>r") 'remember)
(global-set-key (kbd "<f7>s") 'org-search-view)
(global-set-key (kbd "<f7>e") 'org-mu4e-compose-org-mode)
(global-set-key (kbd "<f7><f5>") 'org-dblock-update)
(global-set-key (kbd "<M-f7>") 'org-agenda-list)

(defun org-table-run-and-select-next-line ()
  (interactive)
  (progn
    (org-ctrl-c-ctrl-c)
    (next-line)
    )
  )

(global-set-key (kbd "<f7>c") 'org-table-run-and-select-next-line)

;; Archive
(global-set-key (kbd "<f7>A") 'org-archive-to-archive-sibling)
(global-set-key (kbd "<f7>a") 'org-toggle-archive-tag)
(global-set-key (kbd "<f7>M-a") 'org-advertized-archive-subtree)
(global-set-key (kbd "<f7>k") 'org-kill-note-or-show-branches)
(global-set-key (kbd "<f7>j") 'org-clock-goto)
(global-set-key (kbd "<f7>i") 'org-clock-in)
(global-set-key (kbd "<f7>o") 'org-clock-out)

(global-set-key (kbd "<f7><f7>0") 'org-timer-start)
(global-set-key (kbd "<f7><f7>.") 'org-timer)
(global-set-key (kbd "<f7><f7>-") 'org-timer-item)

(global-set-key (kbd "<f7>.") 'org-time-stamp)
(global-set-key (kbd "<f7>l") 'org-store-link)
(global-set-key (kbd "<f7><f7>l") 'org-insert-link)
(global-set-key (kbd "<f7><f7>t") 'org-todo)

(global-set-key (kbd "<f7><f7>e") 'org-babel-execute-mayb)
(global-set-key (kbd "<f7>q") 'org-set-tags-command)
