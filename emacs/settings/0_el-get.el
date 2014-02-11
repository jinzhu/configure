(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

;; calfw
(setq
 el-get-sources
 '(
   (:name em-zle
          :type github
          :pkgname "emacsmirror/em-zle"
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
   (:name emacs-gradle-mode
          :type github
          :pkgname "jacobono/emacs-gradle-mode"
          )
   (:name youdao
          :type github
          :pkgname "zhenze12345/youdao.el"
          :after (progn
                   (require-package 'pos-tip)
                   (require 'youdao)
                   ;; http://fanyi.youdao.com/openapi?path=data-mode
                   (setf youdao-key-from "jinzhu")
                   (setf youdao-key "1159909992")
                   (bind-key "<f6>" 'youdao-translate-word)
                   ))
   ))

(setq my-packages
      (append
       '(el-get sudo-save)
       (mapcar 'el-get-source-name el-get-sources)))

(el-get 'sync my-packages)
