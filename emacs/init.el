(require 'package)
(require 'netrc)

(setq package-archives '(("org" . "http://orgmode.org/elpa/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ))

(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
  If NO-REFRESH is non-nil, the available package lists will not be
  re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
	(package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t))))

 (require package)
)

(defun require-packages (packages)
  "Ensure PACKAGES are installed.
  Missing packages are installed automatically."
  (mapc #'require-package packages))

(package-initialize)

(global-unset-key (kbd "<f1>")) ;; Shell Mode
(global-unset-key (kbd "<f2>")) ;; Org Mode
(global-unset-key (kbd "<f3>")) ;; Social mode
(global-unset-key (kbd "<f4>")) ;; Useful Collections
(setq cache-dir (expand-file-name "~/.cache/emacs"))

(require-package 'use-package)
(require-package 'dash)
(require 'bind-key)
(savehist-mode)

(setq functions-dir (expand-file-name "~/.emacs.d/functions"))
(when (file-exists-p functions-dir)
  (mapc 'load (directory-files functions-dir 't "^[^#].*el$")))

(setq settings-dir (expand-file-name "~/.emacs.d/settings"))
(when (file-exists-p settings-dir)
  (mapc 'load (directory-files settings-dir 't "^[^#].*el$")))
