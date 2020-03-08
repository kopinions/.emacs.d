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
    :hook
    (company-mode . company-posframe-mode))
  (use-package company-flx
    :requires company
    :config
    (company-flx-mode +1))
  (use-package company-box
    :diminish
    :hook (company-mode . company-box-mode)
    :config
    (setq company-box-backends-colors nil)
    (setq company-box-show-single-candidate nil)
    (setq company-box-max-candidates 50))
  (use-package company-quickhelp
    :defines company-quickhelp-delay
    :bind
    (:map company-active-map
      ("M-h" . company-quickhelp-manual-begin))
    :hook
    (global-company-mode . company-quickhelp-mode)
    :custom
    (company-quickhelp-delay 0.8)))

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config
  (use-package yasnippet-snippets
    :ensure t
    :requires yasnippet)
  (yas-reload-all)
  (yas-global-mode t)
  :custom (yas-snippet-dirs '("~/.emacs.d/snippets")))

(provide 'init-completion)