(require-package 'webjump)
(setq webjump-sites '(
                      ("Github" . "github.com")
                      ("Qortex" . "qortex.com")
                      ("Qortex cn" . "qortex.cn")
                      ("Weibo" . "weibo.com")
                      ("CnBeta" . "cnbeta.com")
                      ("Gmail" . "gmail.com")
                      ("Google Drive" . "drive.google.com")
                      ("Google Calendar" . "calendar.google.com")
                      ("ThePlant Drive" . "drive.google.com/a/theplant.jp")
                      ("Sina Finance" . "finance.sina.com.cn")
                      ("Melpa" . "melpa.milkbox.net")
                      ))

(bind-key "C-x j" 'webjump)


(require-package 'w3m)
(defun set_chromium_as_default_browser ()
  (interactive)
  (progn
    (setq browse-url-browser-function 'browse-url-generic
          browse-url-generic-program "chromium")
    (message "set chromium as default browser")))

(defun set_w3m_as_default_browser ()
  (interactive)
  (progn
    (setq browse-url-browser-function 'w3m-browse-url)
    (message "set w3m as default browser")))

(defun w3m-copy-current-url ()
  "Display the current url in the echo area and put it into `kill-ring'."
  (interactive)
  (when w3m-current-url
    (let ((deactivate-mark nil))
      (kill-new w3m-current-url)
      (w3m-print-current-url))))

(set_chromium_as_default_browser)

(setq
  w3m-default-display-inline-images t
  w3m-command-arguments '("-cookie" "-F")
  w3m-use-cookies t
  w3m-use-mule-ucs t
  w3m-new-session-in-background t
  w3m-home-page "http://www.google.com"
  )


(require 'w3m-search)
(add-to-list 'w3m-search-engine-alist
             '("emacs-wiki" "http://www.emacswiki.org/cgi-bin/wiki.pl?search=%s"))

(add-hook 'w3m-mode-hook (lambda ()
                           (define-key w3m-mode-map "n"     'w3m-next-anchor)
                           (define-key w3m-mode-map "p"     'w3m-previous-anchor)
                           (define-key w3m-mode-map "f"     'w3m-lnum-follow)
                           (define-key w3m-mode-map "d"     'w3m-download-this-url)
                           (define-key w3m-mode-map "Y"     'w3m-copy-current-url)
                           (define-key w3m-mode-map [(shift button2)] 'w3m-mouse-view-this-url-new-session)
                           ))

(bind-key "<escape>w3" 'w3m)
(bind-key "<escape>wo" 'browse-url-at-point)
(bind-key "<escape>wc" 'set_chromium_as_default_browser)
(bind-key "<escape>ww" 'set_w3m_as_default_browser)
