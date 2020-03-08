(use-package projectile
  :diminish
  :after counsel
  :custom
  (projectile-sort-order 'recently-active)
  :bind
  (
   ("C-c p b" . counsel-projectile-switch-to-buffer)
   ("C-c p p" . counsel-projectile-switch-project)
   ("C-c p f" . counsel-projectile-find-file)
   ("C-c p s" . projectile-save-project-buffers)
   ("C-c p d" . counsel-projectile-find-dir)
   ("C-c p c" . projectile-compile-project)
   ("C-c p r" . counsel-projectile-rg)
   ("C-c p %" . projectile-replace-regexp)
   ("C-c p g" . projectile-grep))
  :config
  (setq projectile-enable-caching t)
  (setq projectile-mode-line-prefix "ρ")
  (use-package ibuffer-projectile
    :after ibuffer)
  (use-package counsel-projectile
    :after counsel)
  (projectile-mode))

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer))

(provide 'init-project)
