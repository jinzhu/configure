(require-package 'tabbar)
(tabbar-mode)

(set-face-attribute 'tabbar-default nil    :background "gray60")
(set-face-attribute 'tabbar-unselected nil :background "gray85" :foreground "gray30" :box nil)
(set-face-attribute 'tabbar-selected nil   :background "#f2f2f6" :foreground "black" :box nil)
(set-face-attribute 'tabbar-button nil     :box '(:line-width 1 :color "gray72" :style released-button))
(set-face-attribute 'tabbar-separator nil  :height 0.7)

(custom-set-variables '(tabbar-separator (quote (0.5))))

(defun my-tabbar-buffer-groups ()
  (list
   (cond
    ((memq major-mode '(mu4e-view-mode mu4e-main-mode mu4e-headers-mode mu4e-view-raw-mode
                                       twittering-mode weibo-timeline-mode
                                       jabber-roster-mode jabber-chat-mode erc-mode douban-music-mode
                                       ))
     "Activity"
     )
    ;; ((memq major-mode '(eshell-mode term-mode shell-mode))
    ;;  "Shell"
    ;;  )
    ;; ((string-equal "*" (substring (buffer-name) 0 1))
    ;;  "Emacs"
    ;;  )
    ;; ((memq major-mode '(fundamental-mode))
    ;;  "Emacs"
    ;; )
   ((memq major-mode '(org-mode org-agenda-mode diary-mode))
     "OrgMode"
     )
    (t
     (if (projectile-project-p) (projectile-project-name) "Common")
     )
    )))

(setq tabbar-buffer-groups-function 'my-tabbar-buffer-groups)

(bind-key "<f1><f1><left>" 'tabbar-backward-group)
(bind-key "<f1><f1><right>" 'tabbar-forward-group)
(bind-key "<f1><left>" 'tabbar-backward-tab)
(bind-key "<f1><right>" 'tabbar-forward-tab)
