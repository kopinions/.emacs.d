(use-package yaml-mode
  :mode "\\.yml\\.erb\\.yaml\\'"
  :hook
  ((yaml-mode . goto-address-prog-mode)))

(use-package json-mode
  :custom
  (json-reformat:indent-width 2))

(use-package groovy-mode)

(use-package cmake-mode)

(use-package ruby-mode
  :mode "\\.rb\\'"
  :interpreter "ruby")

;; The package is "python" but the mode is "python-mode":
(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode))

(use-package rainbow-mode
  :diminish
  :hook
  (emacs-lisp-mode . rainbow-mode))

(use-package verilog-mode
  :load-path m/load-path
  :config
  (autoload 'verilog-mode "verilog-mode" "Verilog mode" t )
  (add-to-list 'auto-mode-alist '("\\.[ds]?vh?\\'" . verilog-mode)))

(use-package lua-mode)

(use-package meson-mode
  :hook
  (meson-mode . company-mode))

(provide 'init-languages)
