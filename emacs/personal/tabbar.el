(require 'tabbar)
(tabbar-mode)

(set-face-attribute 'tabbar-default nil    :background "gray60")
(set-face-attribute 'tabbar-unselected nil :background "gray85" :foreground "gray30" :box nil)
(set-face-attribute 'tabbar-selected nil   :background "#f2f2f6" :foreground "black" :box nil)
(set-face-attribute 'tabbar-button nil     :box '(:line-width 1 :color "gray72" :style released-button))
(set-face-attribute 'tabbar-separator nil  :height 0.7)

(custom-set-variables
 '(tabbar-separator (quote (0.5))))

(defun my-tabbar-buffer-groups ()
  (list
   (cond
    ((memq major-mode '(erc-mode twittering-mode weibo-timeline-mode jabber-roster-mode))
     "Social"
     )
    ((memq major-mode '(eshell-mode term-mode shell-mode))
     "Shell"
     )
    ((memq major-mode '(mu4e-view-mode mu4e-main-mode mu4e-headers-mode mu4e-view-raw-mode))
     "Mail"
     )
    ((string-equal "*" (substring (buffer-name) 0 1))
     "Emacs"
     )
    (t
     "Common"
     )
    )))
(setq tabbar-buffer-groups-function 'my-tabbar-buffer-groups)

(bind-key "<escape><left>" 'tabbar-backward-tab)
(bind-key "<escape><right>" 'tabbar-forward-tab)
(bind-key "<escape><up>" 'tabbar-backward-group)
(bind-key "<escape><down>" 'tabbar-forward-group)
