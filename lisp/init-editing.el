(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(cua-selection-mode t)

(auto-save-visited-mode)
(make-directory (expand-file-name "backups" m/conf.d) t)
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "backups/" m/conf.d) t)))
(setq auto-save-list-file-prefix
      (expand-file-name "backups/" m/conf.d))

(setq make-backup-files nil)
; stop to create the lock files
; lock files is used to prevent concurrent edit of a file
(setq create-lockfiles nil)

(setq delete-by-moving-to-trash t)

(setq track-eol t)
(setq line-move-visual nil)

(setq-default kill-whole-line t)

(setq set-mark-command-repeat-pop t)

(use-package beacon
  :diminish beacon-mode
  :config
  (setq beacon-blink-delay 0.1
        beacon-blink-duration 1
        beacon-color "#b1d631")

  (defun backward-paragraph-blink ()
    (interactive)
    (backward-paragraph)
    (beacon-blink))

  (defun forward-paragraph-blink ()
    (interactive)
    (forward-paragraph)
    (beacon-blink))

  (global-set-key (kbd "M-p") 'backward-paragraph-blink)
  (global-set-key (kbd "M-n") 'forward-paragraph-blink)

  (beacon-mode 1))

(use-package hl-line
  :ensure nil
  :hook
  (after-init . global-hl-line-mode))

(use-package mwim
  :bind
  ("C-a" . mwim-beginning-of-code-or-line)
  ("C-e" . mwim-end-of-code-or-line))

(use-package origami
:bind (("C-c f" . origami-recursively-toggle-node)
       ("C-c F" . origami-toggle-all-nodes))
:config
(global-origami-mode t))

(use-package undo-tree
  :ensure t
  :config
  ;; autosave the undo-tree history
  (setq undo-tree-history-directory-alist
        `((".*" . ,temporary-file-directory)))
  (setq undo-tree-auto-save-history t))

(use-package unfill
  :config
  (global-set-key (kbd "M-q") 'unfill-toggle))

(use-package goto-last-change
  :bind ("C-x /" . goto-last-change))

(use-package browse-kill-ring
  :custom
  (browse-kill-ring-separator "\f")
  :bind
  (("M-Y" . browse-kill-ring)
   :map browse-kill-ring-mode-map
   ("C-g" . browse-kill-ring-quit)
   ("M-n" . browse-kill-ring-forward)
   ("M-p" . browse-kill-ring-previous)
  ))

(setq tramp-terminal-type "tramp")

(use-package paren	
  :custom
  (show-paren-style 'mixed)
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-inside-periphery t)
  :config
  (show-paren-mode 1))

(use-package elec-pair
  :config
  (electric-pair-mode 1))

(use-package rainbow-delimiters
  :hook
  ((prog-mode . rainbow-delimiters-mode)))

(use-package smartparens
  :bind
  (([remap backward-up-list] . sp-backward-up-sexp)
   ("C-M-a" . sp-beginning-of-sexp)
   ("C-M-e" . sp-end-of-sexp)
   ("C-M-f" . sp-forward-sexp)
   ("C-M-b" . sp-backward-sexp)
   ("C-M-n" . sp-next-sexp)
   ("C-M-p" . sp-previous-sexp)
   ("M-F" . sp-forward-symbol)
   ("M-B" . sp-backward-symbol)
   ("C-M-w" . sp-copy-sexp)
   ("C-M-d" . sp-down-sexp)
   ("C-M-u" . sp-up-sexp)
   ("M-<backspace>" . backward-kill-word)
   ("C-<backspace>" . sp-backward-kill-word)
   ([remap sp-backward-kill-word] . backward-kill-word)
   ("C-c ("  . sp-wrap-round)
   ("C-c ["  . sp-wrap-square)
   ("C-c {"  . sp-wrap-curly)
   ("C-c '"  . (lambda() (interactive) (sp-wrap-with-pair "\'")))
   ("C-c \"" . (lambda() (interactive) (sp-wrap-with-pair "\"")))
   ("C-c _"  . (lambda() (interactive) (sp-wrap-with-pair "_")))
   ("C-c `"  . (lambda() (interactive) (sp-wrap-with-pair "`")))
   ("M-[" . sp-backward-unwrap-sexp)
   ("M-]" . sp-unwrap-sexp))
  :hook
  ((after-init . smartparens-global-mode))
  :config
  (use-package smartparens-config
  :ensure smartparens
  :config (progn (show-smartparens-global-mode t)))
  (sp-pair "=" "=" :actions '(wrap))
  (sp-pair "+" "+" :actions '(wrap))
  (sp-pair "<" ">" :actions '(wrap))
  (sp-pair "$" "$" :actions '(wrap)))

(use-package swiper
 :demand t
 :bind 
    (("M-s" . swiper-thing-at-point)
    ("C-s" . swiper)
    :map ivy-minibuffer-map
    ;; I use "C-x C-f" to open file, so bind "C-f" to
    ;; `ivy-immediate-done' is very useful.
    ("S-<return>" . ivy-immediate-done)
    ([mouse-1] . ignore)
    ([mouse-3] . ignore)
    ([mouse-4] . ivy-next-line)
    ([mouse-5] . ivy-previous-line))
 :config
 (ivy-mode 1)
 (setq ivy-count-format ""
       ivy-use-virtual-buffers t
       ivy-format-functions-alist
       '((t . ivy-format-function-arrow))
       ivy-display-style 'fancy
       ivy-use-selectable-prompt t)
 (setq ivy-initial-inputs-alist
       '((org-refile . "")
         (org-agenda-refile . "")
         (org-capture-refile . "")
         (counsel-M-x . "")
         (counsel-describe-function . "")
         (counsel-describe-variable . "")
         (counsel-org-capture . "")
         (Man-completion-table . "")
         (woman . ""))))

(use-package anzu
  :config
  (setq anzu-mode-lighter "")
  (global-anzu-mode t)
  :bind
  (([remap query-replace-regexp] . anzu-query-replace-regexp)
   ([remap query-replace] . anzu-query-replace)))

(use-package rg
  :preface
  (defun rg-occur-hide-lines-not-matching (search-text)
    "Hide lines that don't match the specified regexp."
    (interactive "MHide lines not matched by regexp: ")
    (set (make-local-variable 'line-move-ignore-invisible) t)
    (save-excursion
      (goto-char (point-min))
      (forward-line 5)
      (let ((inhibit-read-only t)
	    line)
	(while (not (looking-at-p "^\nrg finished "))
	  (setq line (buffer-substring-no-properties (point) (point-at-eol)))
	  (if (string-match-p search-text line)
	      (forward-line)
	    (when (not (looking-at-p "^\nrg finished "))
	      (delete-region (point) (1+ (point-at-eol)))))))))
  (defun rg-occur-hide-lines-matching  (search-text)
    "Hide lines matching the specified regexp."
    (interactive "MHide lines matching regexp: ")
    (set (make-local-variable 'line-move-ignore-invisible) t)
    (save-excursion
      (goto-char (point-min))
      (forward-line 5)
      (let ((inhibit-read-only t)
	    line)
	(while (not (looking-at-p "^\nrg finished "))
	  (setq line (buffer-substring-no-properties (point) (point-at-eol)))
	  (if (not (string-match-p search-text line))
	      (forward-line)
	    (when (not (looking-at-p "^\nrg finished "))
	      (delete-region (point) (1+ (point-at-eol)))))))))
  :custom
  (rg-show-header nil)
  :config
  (rg-enable-default-bindings)
  :bind
  (:map rg-mode-map ("/" . rg-occur-hide-lines-not-matching)
	("!" . rg-occur-hide-lines-matching)
	("M-N" . rg-next-file)
	("M-P" . rg-prev-file)))

(use-package wgrep
  :custom
  (wgrep-enable-key "e")
  (wgrep-auto-save-buffer nil)
  (wgrep-change-readonly-file t))

(use-package avy-zap
  :bind
  ("M-z" . avy-zap-to-char-dwim)
  ("M-z" . avy-zap-up-to-char-dwim))

(use-package expand-region
  :after org
  :bind
  (("C-=" . er/expand-region)))

(use-package multiple-cursors
  :bind (
  ("C-<" . mc/mark-previous-like-this)
  ("C->" . mc/mark-next-like-this)
  ("C-+" . mc/mark-next-like-this)
  ("C-c C-<" . mc/mark-all-like-this)
  ("C-c m r" . set-rectangular-region-anchor)
  ("C-c m c" . mc/edit-lines)
  ("C-c m e" . mc/edit-ends-of-lines)
  ("C-c m a" . mc/edit-beginnings-of-lines))
)

(global-unset-key [M-left])
(global-unset-key [M-right])

(use-package move-dup
  :bind (("M-<up>" . md-move-lines-up)
  ("M-<down>" . md-move-lines-down)
  ("M-S-<up>" . md-duplicate-up)
  ("M-S-<down>" . md-duplicate-down)
  ("C-c d" . md-duplicate-down)
  ("C-c u" . md-duplicate-up)))

(use-package whole-line-or-region
  :diminish whole-line-or-region-local-mode
  :config
  (whole-line-or-region-global-mode t))

(use-package highlight-escape-sequences
  :config
  (hes-mode t))

(global-set-key (kbd "M-/") 'hippie-expand)

(setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially
        try-complete-file-name
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill))

(use-package symbol-overlay
  :diminish symbol-overlay-mode
  :hook
  ((prog-mode . symbol-overlay-mode)
   (html-mode . symbol-overlay-mode)
   (yaml-mode . symbol-overlay-mode)
   (conf-mode . symbol-overlay-mode))
  :bind
  (:map symbol-overlay-mode-map 
    ("M-i" . symbol-overlay-put)
    ("M-I" . symbol-overlay-remove-all)
    ("M-n" . symbol-overlay-jump-next)
    ("M-p" . symbol-overlay-jump-prev)))

(use-package mode-line-bell
  :init
  (add-hook 'after-init-hook 'mode-line-bell-mode))

(global-set-key (kbd "RET") 'newline-and-indent)
(defun m/newline-at-end-of-line ()
  "Move to end of line, enter a newline, and reindent."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))

(global-set-key (kbd "S-<return>") 'm/newline-at-end-of-line)

(use-package subword
:config
(global-subword-mode))

(use-package backward-forward
  :bind
  ("C-," . backward-forward-previous-location)
  ("C-." . backward-forward-next-location)
  :custom
  (mark-ring-max 60)
  (set-mark-command-repeat-pop t)
  :config
  (backward-forward-mode t))

(use-package display-line-numbers
  :custom
  (display-line-numbers-width 3)
  :hook
  ((prog-mode yaml-mode systemd-mode) . display-line-numbers-mode))

(use-package goto-line-preview
  :after display-line-numbers
  :bind
  (([remap goto-line] . goto-line-preview))
  :config
  (defun m/with-display-line-numbers (f &rest args)
    (let ((display-line-numbers t))
      (apply f args)))
  (advice-add 'goto-line-preview :around #'m/with-display-line-numbers))

(when (fboundp 'global-prettify-symbols-mode)
  (add-hook 'after-init-hook 'global-prettify-symbols-mode))

(provide 'init-editing)
