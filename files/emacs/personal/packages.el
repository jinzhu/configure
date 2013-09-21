(require 'package)

;; My packages
(prelude-require-packages '(
                            vline quickrun pos-tip auto-complete git-gutter go-eldoc
                                  multiple-cursors mark-multiple region-bindings-mode
                                  wrap-region yasnippet go-snippets switch-window
                                  emamux ctags-update multi-term powerline
                                  writegood-mode evil
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
   (:name emacs-calfw
          :type github
          :pkgname "kiwanami/emacs-calfw"
          :after (require 'calfw)
          )
   (:name twittering-mode
          :type github
          :pkgname "hayamiz/twittering-mode"
          )
   (:name weibo
          :type github
          :pkgname "austin-----/weibo.emacs"
          :after (require 'weibo)
          )
   (:name DoubanMusic
          :type github
          :pkgname "zhengyuli/DoubanMusic"
          )
   ))

(setq my-packages
      (append
       '(el-get sudo-save evil-surround)
       (mapcar 'el-get-source-name el-get-sources)))

(el-get 'sync my-packages)
