(prelude-ensure-module-deps '(use-package))
(require 'use-package)
(require 'netrc)

(global-unset-key (kbd "<f1>")) ;; Shell Mode
(global-unset-key (kbd "<f2>")) ;; Org Mode
(global-unset-key (kbd "<f3>")) ;; Social mode
(global-unset-key (kbd "<f4>")) ;; Useful Collections

(setq smart-window-remap-keys 0)

(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))

(prelude-require-packages '(vline quickrun auto-complete git-gutter go-eldoc
                                  multiple-cursors mark-multiple region-bindings-mode
                                  wrap-region yasnippet go-snippets switch-window
                                  emamux ctags-update multi-term smart-mode-line
                                  writegood-mode pos-tip ; pos tip is for youdao
                                  w3m jabber bash-completion shell-command
                                  rinari keyfreq point-undo smart-window
                                  mu4e-maildirs-extension sauron calfw-gcal
                                  org-pomodoro org-screenshot easy-kill org-magit
                                  esh-buf-stack multi-eshell rvm pcmpl-git
                                  google-this flycheck-color-mode-line
                                  twittering-mode tabbar
                                  ))

;; el-get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(setq
 el-get-sources
 '(
   (:name readline-complete
          :type github
          :pkgname "monsanto/readline-complete.el"
          :after (require 'readline-complete)
          )
   (:name em-zle
          :type github
          :pkgname "emacsmirror/em-zle"
          )
   (:name emacs-calfw
          :type github
          :pkgname "kiwanami/emacs-calfw"
          :after (require 'calfw)
          )
   (:name auto-complete-yasnippet
          :after (require 'auto-complete-yasnippet)
          )
   (:name weibo
          :type github
          :pkgname "austin-----/weibo.emacs"
          )
   (:name DoubanMusic
          :type github
          :pkgname "zhengyuli/DoubanMusic"
          )
   (:name youdao
          :type github
          :pkgname "zhenze12345/youdao.el"
          :after (progn
                   (require 'pos-tip)
                   (require 'youdao)
                   ;; http://fanyi.youdao.com/openapi?path=data-mode
                   (setf youdao-key-from "jinzhu")
                   (setf youdao-key "1159909992")
                   (global-set-key (kbd "<f6>") 'youdao-translate-word)
                   ))
   (:name pomodoro
          :type github
          :pkgname "vderyagin/pomodoro.el"
          :post-init (setq pomodoro-icon
                           (expand-file-name "pomodoro/pomodoro_technique.png" el-get-dir)))
   ))

(setq my-packages
      (append
       '(el-get sudo-save)
       (mapcar 'el-get-source-name el-get-sources)))

(el-get 'sync my-packages)
