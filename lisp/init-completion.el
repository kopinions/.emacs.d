(use-package company
  :diminish company-mode
  :bind
  (:map company-active-map
	("C-n" . company-select-next)
	("C-p" . company-select-previous)
	("<tab>" . company-complete-common-or-cycle)
	:map company-search-map
	("C-p" . company-select-previous)
	("C-n" . company-select-next))
  :custom
  (company-idle-delay 0)
  (company-echo-delay 0)
  (company-dabbrev-downcase nil)
  (company-minimum-prefix-length 1)
  :config
  (global-company-mode)
  (use-package company-posframe
    :diminish company-posframe-mode
    :hook
    (company-mode . company-posframe-mode))
  (use-package company-flx
    :diminish company-flx-mode
    :requires company
    :config
    (company-flx-mode +1))
  (use-package company-box
    :diminish company-box-mode
    :hook (company-mode . company-box-mode)
    :config
    (setq company-box-backends-colors nil)
    (setq company-box-show-single-candidate nil)
    (setq company-box-max-candidates 50))
  (use-package company-quickhelp
    :diminish company-quickhelp-mode
    :defines company-quickhelp-delay
    :bind
    (:map company-active-map
	  ("M-h" . company-quickhelp-manual-begin))
    :hook
    (global-company-mode . company-quickhelp-mode)
    :custom
    (company-quickhelp-delay 0.8)))

(use-package yasnippet
  :config
  (use-package yasnippet-snippets
    :requires yasnippet
    :after company
    :preface
    (defvar company-mode/enable-yas t
      "Enable yasnippet for all backends.")
    (defun company-mode/backend-with-yas (backend)
      (if	(or (not company-mode/enable-yas)
		    (and (listp backend) (member 'company-yasnippet backend)))
	  backend
	(append (if (consp backend) backend (list backend))
		'(:with company-yasnippet))))

    :custom (yas-snippet-dirs `(,(expand-file-name "snippets" m/conf.d) ,yasnippet-snippets-dir))
    :config
    (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))
    (yas-reload-all))
  (yas-global-mode t))

(provide 'init-completion)
