(when (eq m/os 'macos)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super))

(when (eq m/os 'linux)
   (setq x-super-keysym 'meta))

(use-package xclip
  :if (eq m/os 'linux)
  :config
  (xclip-mode 1))

(use-package exec-path-from-shell
  :config
  (when (memq m/os '(macos linux))
    (exec-path-from-shell-initialize)))

(use-package request)

(provide 'init-os)
