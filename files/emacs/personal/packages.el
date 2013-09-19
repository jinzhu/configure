(require 'package)

;; My packages
(prelude-require-packages '(
                            vline quickrun pos-tip auto-complete git-gutter go-eldoc
                                  xclip multiple-cursors mark-multiple region-bindings-mode
                                  wrap-region
                                  ))

;; el-get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(setq my-packages
      '(el-get sudo-save))

(el-get 'sync my-packages)
