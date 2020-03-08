(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))
(if (eq m/os 'macos)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (set-frame-parameter frame 'menu-bar-lines
                                     (if (display-graphic-p frame)
                                         1 0))))
  (when (fboundp 'menu-bar-mode)
    (menu-bar-mode -1)))

(let ((no-border '(internal-border-width . 0))
      (fullscreen '(fullscreen . maximized)))
  (add-to-list 'default-frame-alist no-border)
  (add-to-list 'initial-frame-alist no-border)
  (add-to-list 'initial-frame-alist fullscreen))

(when (and (eq m/os 'macos) (fboundp 'toggle-frame-fullscreen))
  ;; Command-Option-f to toggle fullscreen mode
  ;; Hint: Customize `ns-use-native-fullscreen'
  (global-set-key (kbd "M-ƒ") 'toggle-frame-fullscreen))

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))
(add-hook 'term-mode-hook
          (lambda ()
            (setq line-spacing 0)))

(defalias 'yes-or-no-p 'y-or-n-p)

(use-package ns-auto-titlebar
  :if (eq m/os 'macos)
  :config
  (ns-auto-titlebar-mode))

(use-package color-theme-sanityinc-tomorrow
  :config
  (setq custom-safe-themes t)
  (color-theme-sanityinc-tomorrow-night))

(use-package all-the-icons
  :config
  (unless (or (member "all-the-icons" (font-family-list)) m/offline)
    (ignore-errors (all-the-icons-install-fonts t)
		     (if (eq m/os 'linux)
		       (set-default-font "-WQYF-WenQuanYi Micro Hei Mono-normal-normal-normal-*-15-*-*-*-*-0-iso10646-1"))))
  )

(use-package ace-window
  :ensure t
  :config
  (global-set-key (kbd "s-w") 'ace-window)
  (global-set-key [remap other-window] 'ace-window))

(use-package page-break-lines
  :diminish page-break-lines-mode
  :config
  (global-page-break-lines-mode))

(use-package which-key
  :config
  (which-key-mode t))

(use-package counsel
  :diminish ivy-mode counsel-mode  
  :custom
  (counsel-yank-pop-height 15)
  (enable-recursive-minibuffers t)
  (ivy-use-selectable-prompt t)
  (ivy-use-virtual-buffers t)
  (ivy-on-del-error-function nil)
  (swiper-action-recenter t)
  :defines
  (projectile-completion-system magit-completing-read-function)
  :bind
  (("C-c C-r" . ivy-resume)
   ("C-x b" . counsel-switch-buffer)
   ("C-x f" . counsel-recentf)
   ("C-x C-b" . ivy-switch-buffer)
   ("C-x C-f" . counsel-find-file)
   ("C-h f" . counsel-describe-function)
   ("C-h v" . counsel-describe-variable)
   ("C-c y" . counsel-yank-pop))
  :preface
    (defun ivy-format-function-pretty (cands)
      "Transform CANDS into a string for minibuffer."
      (ivy--format-function-generic
       (lambda (str)
         (concat
             (all-the-icons-faicon "hand-o-right" :height .85 :v-adjust .05 :face 'font-lock-constant-face)
             (ivy--add-face str 'ivy-current-match)))
       (lambda (str)
         (concat "  " str))
       cands
       "\n"))
  :config
  ;; Default setting is not suitable for GuixSD.
  (setq counsel-linux-app-format-function
        #'counsel-linux-app-format-function-name-only)
  (setq ivy-re-builders-alist
    '((swiper . ivy--regex-plus)
     (ivy-switch-buffer . ivy--regex-plus)
     (counsel-projectile-rg . ivy--regex-plus)
     (counsel-rg . ivy--regex-plus)
     (t . ivy--regex-fuzzy)))
  (setq ivy-initial-inputs-alist nil)
  (with-eval-after-load 'projectile
      (setq projectile-completion-system 'ivy))
  (with-eval-after-load 'magit
      (setq magit-completing-read-function 'ivy-completing-read))
  (use-package flx)
  (use-package amx)
  (use-package counsel-projectile
    :config (counsel-projectile-mode 1))
  (use-package ivy-rich
    :defines
    (all-the-icons-dir-icon-alist bookmark-alist)
    :functions
    (all-the-icons-icon-family
      all-the-icons-match-to-alist
      all-the-icons-auto-mode-match?
      all-the-icons-octicon
      all-the-icons-dir-is-submodule)
    :hook 
    (ivy-rich-mode . (lambda ()
                              (setq ivy-virtual-abbreviate
                              (or (and ivy-rich-mode 'abbreviate)))))
    :config
    (ivy-rich-mode))
  :hook
  (after-init . ivy-mode)
  (ivy-mode . counsel-mode))

(use-package ivy-xref
  :after (ivy)
  :init (if (< emacs-major-version 27)
            (setq xref-show-xrefs-function #'ivy-xref-show-xrefs)
          (setq xref-show-definitions-function #'ivy-xref-show-defs)))

(use-package dashboard
   :diminish dashboard-mode
   :config
   (setq dashboard-startup-banner 'logo
         dashboard-banner-logo-title "Welcome to Emacs. Happy Hacking!"
         dashboard-items '((recents  . 5)
                        (bookmarks . 5)
                        (projects . 5)
                        (agenda . 5)
                        (registers . 5))
         dashboard-set-footer nil)
  :hook
  (after-init . dashboard-setup-startup-hook))

(provide 'init-ui)
