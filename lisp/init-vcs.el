(use-package gitignore-mode)
(use-package gitconfig-mode)
(use-package gitattributes-mode)

(use-package git-gutter
    :custom
    (git-gutter:modified-sign "~")
    (git-gutter:added-sign    "+")
    (git-gutter:deleted-sign  "-")
    :custom-face
    (git-gutter:modified ((t (:foreground "#f1fa8c" :background "#f1fa8c"))))
    (git-gutter:added    ((t (:foreground "#50fa7b" :background "#50fa7b"))))
    (git-gutter:deleted  ((t (:foreground "#ff79c6" :background "#ff79c6"))))
    :config
    (global-git-gutter-mode +1)
    :hook
    (magit-post-refresh . git-gutter:update-all-windows)
    (magit-post-commit . git-gutter:update-all-windows)
    (magit-refresh-buffer . git-gutter:update-all-windows))

(use-package magit
  :after (fullframe diff-hl)
  :custom
  (magit-diff-refine-hunk t)
  :bind
  (("C-x g" . magit-status))
  :config
  (use-package fullframe
    :config
    (fullframe magit-status magit-mode-quit-window)))	
(use-package magit-todos)

(use-package git-messenger
  :bind ("C-x v p" . git-messenger:popup-message)
  :config
  (setq git-messenger:show-detail t))

(use-package git-timemachine
  :config
  (defalias 'tm 'git-timemachine))

(provide 'init-vcs)

(use-package yagist)
(use-package bug-reference-github
  :hook
  (prog-mode . bug-reference-prog-mode))
(use-package github-clone)
(use-package forge)
(use-package github-review)
