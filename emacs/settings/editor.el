(setq auto-save-interval 1
      auto-save-timeout 1
      auto-save-list-file-prefix (expand-file-name "auto-save-list" cache-dir)
      auto-save-file-name-transforms `((".*" ,cache-dir t))
      savehist-additional-variables '(kill-ring compile-command search-ring regexp-search-ring)

      backup-directory-alist `((".*" . ,cache-dir))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5    ; and how many of the old

      create-lockfiles nil   ; no lock file
      )

(setq-default  tab-width 2
               standard-indent 2
               indent-tabs-mode nil)	; makes sure tabs are not used.

(global-auto-revert-mode 1)

;; Bookmarks
(require 'bookmark)
(setq bookmark-default-file (expand-file-name "bookmarks" cache-dir)
      bookmark-save-flag 1)

;; Smex
(require-package 'smex)
(setq smex-save-file (expand-file-name ".smex-items" cache-dir))
(smex-initialize)
(bind-key "M-x" 'smex)
(bind-key "<menu>" 'smex)
(setq smex-history-length 100)

;; Pojectile
(require-packages '(projectile ag))
(setq projectile-cache-file (expand-file-name  "projectile.cache" cache-dir)
      projectile-known-projects-file (expand-file-name  "projectile-bookmarks.eld" cache-dir)
      ag-highlight-search t)
(projectile-global-mode)

(bind-key "<escape>a" 'ag-project)
(bind-key "<escape>A" 'ag)
(bind-key "<escape>d" 'projectile-find-dir)
(bind-key "<escape>D" 'projectile-dired)
(bind-key "<escape>f" 'projectile-find-file)
(bind-key "<escape>F" 'ido-find-file)
(bind-key "<escape>C-f" 'projectile-find-file-in-directory)
(bind-key "<escape>t" 'projectile-find-test-file)
(bind-key "<escape>g" 'projectile-grep)
(bind-key "<escape>T" 'projectile-regenerate-tags)
(bind-key "<escape>R" 'projectile-replace)
(bind-key "<escape>p" 'projectile-switch-project)

(bind-key "<escape>b" 'ido-switch-buffer)
(bind-key "<escape>B" 'projectile-recentf)
(bind-key "<escape>k" 'projectile-kill-buffers)
(bind-key "<escape>r" 'recentf-ido-find-file)

;; Smartparens
(require-package 'smartparens)
(require 'smartparens-config)
(smartparens-global-mode t)
(show-smartparens-global-mode t)
(setq sp-autoescape-string-quote nil)
;; https://github.com/Fuco1/smartparens/wiki/Example-configuration

;;; html-mode
(sp-with-modes '(html-mode sgml-mode)
  (sp-local-pair "<" ">"))

;; Rainbow Mode
(require-package 'rainbow-mode)
(eval-after-load 'css-mode
  '(progn
     (defun css-mode-defaults ()
       (rainbow-mode +1))

     (add-hook 'css-mode-hook (run-hooks 'css-mode-hook))))

;; Recentf
(require-package 'recentf)
(setq recentf-max-saved-items 500
      recentf-max-menu-items 15
      recentf-exclude '(
                        "\.hist$" "/COMMIT_EDITMSG$" "/tmp/" "/ssh:" "/sudo:"
                        ))
(recentf-mode +1)

;; Flyspell
(require 'flyspell)
(setq ispell-program-name "aspell" ; use aspell instead of ispell
      ispell-extra-args '("--sug-mode=ultra"))
(flyspell-mode +1)

;; Dired
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)
(setq dired-dwim-target t)
(bind-key "C-c j" 'dired-jump)

;; Undo Tree
(require-package 'undo-tree)
(setq undo-tree-visualizer-timestamps t)
(setq undo-tree-history-directory-alist (quote (("." . "~/.cache/emacs/undo-tree"))))
(setq undo-tree-auto-save-history t)
(global-undo-tree-mode)

;; Align
(bind-key "C-x \\" 'align-regexp)

;; Font size
(bind-key "C-+" 'text-scale-increase)
(bind-key "C--" 'text-scale-decrease)

;; Proced
(bind-key "C-x p" 'proced)

;; expand-region
(require-package 'expand-region)
(bind-key "C-=" 'er/expand-region)

;; Wrap Region
(require-package 'wrap-region)
(wrap-region-global-mode)

;; Ace Jump
(require-package 'ace-jump-mode)

;; key-chord
(require-package 'key-chord)
(setq key-chord-two-keys-delay 0.2)

(key-chord-define-global "uu" 'undo-tree-visualize)
(key-chord-define-global "jj" 'ace-jump-char-mode)
(key-chord-define-global "yy" 'copy-current-line)
(key-chord-define-global ";w" 'save-buffer)
(key-chord-define-global "vv" 'select-current-line)
(key-chord-define-global "JJ" 'mode-line-other-buffer) ; Switch to previous buffer
(key-chord-define-global ",," 'comment-or-uncomment-region-or-line)
(bind-key "M-j" 'indent-new-comment-line)

(require-package 'ace-jump-buffer)
(key-chord-define-global "bb" 'ace-jump-buffer)

(key-chord-mode +1)

;; Ack
(require-package 'ack-and-a-half)
(defalias 'ack 'ack-and-a-half)

;; Multiple Cursors
(require-package 'multiple-cursors)
(require-package 'region-bindings-mode)
(region-bindings-mode-enable)
(bind-key "C-m" 'mc/mark-more-like-this-extended region-bindings-mode-map)

;; White Space
(setq whitespace-line-column 100)

;; Cua Mode
;; (cua-mode 'emacs)
;; (setq cua-enable-cua-keys nil)

;; SQL Mode
(add-hook 'sql-interactive-mode-hook (lambda ()
                                       (yas-minor-mode -1)))

;; Gnutls
(setq gnutls-min-prime-bits 2048)

;; Doc View
(setq doc-view-continuous t)
(add-hook 'doc-view-mode-hook (lambda ()
                                (bind-key "j" 'doc-view-next-line-or-next-page doc-view-mode-hook)
                                (bind-key "k" 'doc-view-previous-line-or-previous-page doc-view-mode-hook)
                                (bind-key "h" 'image-backward-hscroll doc-view-mode-map)
                                (bind-key "l" 'image-forward-hscroll doc-view-mode-map)
                                ))

(setq mouse-drag-copy-region nil)  ; stops selection with a mouse being immediately injected to the kill ring
(setq x-select-enable-primary t)  ; stops killing/yanking interacting with primary X11 selection
(setq x-select-enable-clipboard t)  ; makes killing/yanking interact with clipboard X11 selection
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)
(setq mouse-yank-at-point t)
(bind-key "<mouse-2>" 'mouse-yank-at-click)
(bind-key "<S-mouse-2>" 'mouse-yank-at-click)

;; easy-kill
;; (require-package 'easy-kill)
;; (global-set-key [remap kill-ring-save] 'easy-kill)
;; (global-set-key [remap mark-sexp] 'easy-mark-sexp)
;; vim-region
(require-package 'vim-region)
(bind-key "C-SPC" 'vim-region-mode)

;; browse-kill-ring
(require-package 'browse-kill-ring)
;; (require-package 'popup-kill-ring)
(setq browse-kill-ring-highlight-current-entry t
      browse-kill-ring-display-duplicates nil)

(browse-kill-ring-default-keybindings)
(bind-key "C-n"      'browse-kill-ring-forward browse-kill-ring-mode-map)
(bind-key "C-p"      'browse-kill-ring-previous browse-kill-ring-mode-map)
(bind-key "C-g"      'browse-kill-ring-quit browse-kill-ring-mode-map)
(bind-key "<escape>" 'browse-kill-ring-quit browse-kill-ring-mode-map)
(bind-key "C-x y"    'browse-kill-ring)

;; Pomodoro
(require-package 'pomodoro)
(setq pomodoro-sound-player "/usr/bin/mplayer"
      pomodoro-break-start-sound "/usr/share/sounds/purple/alert.wav"
      pomodoro-work-start-sound "/usr/share/sounds/freedesktop/stereo/message-new-instant.oga"
      )
(add-hook 'after-init-hook 'pomodoro-add-to-mode-line)
(pomodoro-start nil)

;; Helm
(require-packages '(helm-descbinds helm-go-package))
(defun my-helm()
  (interactive)
  (require 'helm-files)
  (helm-other-buffer '(
                       helm-source-ido-virtual-buffers
                       helm-source-files-in-current-dir
                       helm-source-bookmarks
                       helm-source-etags-select
                       helm-source-recentf
                       helm-source-calculation-result
                       helm-source-locate)
                     "*my helm*"))
(bind-key "<escape>h" 'my-helm)


;; Google Translation
(require-package 'google-translate)

(setq google-translate-default-source-language "auto"
      google-translate-default-target-language "zh-CN"
      google-translate-show-phonetic t)
(bind-key "<f1>t" 'google-translate-query-or-region)

;; Smart Forward
(require-package 'smart-forward)
(global-set-key (kbd "M-<up>") 'smart-up)
(global-set-key (kbd "M-<down>") 'smart-down)
(global-set-key (kbd "M-<left>") 'smart-backward)
(global-set-key (kbd "M-<right>") 'smart-forward)

;; wgrep
(require-package 'wgrep)
