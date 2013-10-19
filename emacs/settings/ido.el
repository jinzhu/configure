(require-package 'flx-ido)
(require-package 'ido-ubiquitous)

(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-max-prospects 10
      ido-default-file-method 'selected-window
      ido-auto-merge-work-directories-length -1

      ido-everywhere t
      ido-enable-last-directory-history t
      ido-use-virtual-buffers t
      ido-file-extensions-order '(".org" ".txt" ".py" ".rb" ".go" ".emacs" ".xml" ".el"
				  ".ini" ".cfg" ".conf" ".rake" ".coffee" ".scss")
      ido-ignore-buffers '("\\` " "^\*Mess" "^\*Back" "^\*Buffer"
			   ".*Completion" "^\*Ido" "^\*trace" "^\*ediff" "^\*vc")

      ido-case-fold t
      )

(add-to-list 'ido-ignore-directories "\\.jabber-avatar")

(icomplete-mode t)
(ido-mode +1)
(flx-ido-mode +1)
(ido-ubiquitous-mode +1)

;; disable ido faces to see flx highlights.
(setq ido-use-faces nil)
