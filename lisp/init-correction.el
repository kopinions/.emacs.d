(use-package flycheck
  :custom
  (flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)
  :hook
  (after-init . global-flycheck-mode))
(use-package flycheck-color-mode-line
  :hook
  (flycheck-mode . flycheck-color-mode-line-mode))

(provide 'init-correction)
