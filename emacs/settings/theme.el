(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'monokai t)
(set-default-font "Monaco-14")
(setq default-frame-alist '((font . "Monaco-14"))) ;; emacs --daemon

;; Smart Mode Line
(require-package 'smart-mode-line)
(require 'smart-mode-line)
(if after-init-time (sml/setup)
  (add-hook 'after-init-hook 'sml/setup))
(setq sml/mode-width 5
      sml/name-width 30)


(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(menu-bar-mode -1)
(blink-cursor-mode -1)
(setq inhibit-startup-screen t)

(display-time-mode t)
(setq major-mode 'conf-mode)
(display-battery-mode t)
(electric-indent-mode t)
(global-hl-line-mode t)

(setq frame-title-format
      '("" invocation-name " : " (:eval (if (buffer-file-name)
                                            (abbreviate-file-name (buffer-file-name))
                                          "%b"))))

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)
