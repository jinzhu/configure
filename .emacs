(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(package-initialize)

;; Fetch the list of packages available
(when (not package-archive-contents)
  (package-refresh-contents))


(setq package-list '( coffee-mode js2-mode haml-mode markdown-mode sass-mode scss-mode css-mode yaml-mode
                                  rinari ruby-test-mode
                                  color-theme color-theme-sanityinc-tomorrow
                                  yasnippet auto-complete
                                  projectile helm undo-tree ack-and-a-half magit
                                  quickrun
                                  evil
                                  ))

;; install the missing packages
(dolist (package package-list)
  (when (not (package-installed-p package))
    (package-install package)))

;; Vim mode
(evil-mode 1)

;; Find File in projects
(projectile-global-mode)

;; Theme
(color-theme-sanityinc-tomorrow-night)

;; Helm
(helm-mode 1)

;; yasnippet
(yas-global-mode 1)

;; Hide menu by default
(menu-bar-mode 0)
(global-set-key (kbd "<f12>") 'menu-bar-mode)
