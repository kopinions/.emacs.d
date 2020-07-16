(use-package lsp-mode
  :ensure t
  :custom
  ;; debug
  (lsp-log-io t)
  (lsp-trace nil)
  (lsp-enable-snippet nil)
  (lsp-print-performance nil)
  ;; general
  (lsp-auto-guess-root t)
  (lsp-document-sync-method 'incremental) ;; none, full, incremental, or nil
  (lsp-response-timeout 15)
  (lsp-prefer-flymake nil) ;; t(flymake), nil(lsp-ui), or :none
  ;; go-client
  (lsp-clients-go-server-args '("--cache-style=always" "--diagnostics-style=onsave" "--format-style=goimports"))
  (lsp-clients-clangd-args '("-j=4"
			     "--all-scopes-completion"
			     "--pch-storage=memory"
			     "-background-index"
			     "-log=info"
			     "--compile-commands-dir=build"
			     "--clang-tidy"
			     "--fallback-style=llvm"
			     "--header-insertion=iwyu"
			     "--limit-results=50"
			     "--suggest-missing-includes"
			     "--offset-encoding=utf-8"
			     "--completion-style=detailed"
			     "--pretty"))
  :hook
  (c-mode-common . lsp)
  :bind
  (:map lsp-mode-map
	("C-c r"   . lsp-rename))
  :config
  (use-package lsp-treemacs
    :ensure t
    :commands lsp-treemacs-errors-list
    )
  ;; LSP UI tools
  (use-package lsp-ui
    :ensure t
    :custom
    ;; lsp-ui-doc
    (lsp-ui-doc-enable nil)
    (lsp-ui-doc-header t)
    (lsp-ui-doc-include-signature t)
    (lsp-ui-doc-position 'at-point) ;; top, bottom, or at-point
    (lsp-ui-doc-max-width 120)
    (lsp-ui-doc-max-height 30)
    (lsp-ui-doc-use-childframe t)
    (lsp-ui-doc-use-webkit t)
    ;; lsp-ui-flycheck
    (lsp-ui-flycheck-enable t)
    ;; lsp-ui-sideline
    (lsp-ui-sideline-enable nil)
    (lsp-ui-sideline-ignore-duplicate t)
    (lsp-ui-sideline-show-symbol t)
    (lsp-ui-sideline-show-hover t)
    (lsp-ui-sideline-show-diagnostics nil)
    (lsp-ui-sideline-show-code-actions t)
    ;; lsp-ui-imenu
    (lsp-ui-imenu-enable t)
    (lsp-ui-imenu-kind-position 'left)
    ;; lsp-ui-peek
    (lsp-ui-peek-enable t)
    (lsp-ui-peek-peek-height 80)
    (lsp-ui-peek-list-width 150)
    (lsp-ui-peek-fontify 'on-demand) ;; never, on-demand, or always
    :preface
    (defun m/toggle-lsp-ui-doc ()
      (interactive)
      (if lsp-ui-doc-mode
	  (progn
	    (lsp-ui-doc-mode -1)
	    (lsp-ui-doc--hide-frame))
	(lsp-ui-doc-mode 1)))
    :bind
    (:map lsp-mode-map
	  ("M-?" . lsp-ui-peek-find-references)
	  ("M-." . lsp-ui-peek-find-definitions)
	  ("C-c C-i" . lsp-ui-peek-find-implementation)
	  ("C-c m"   . lsp-ui-imenu)
	  ("C-c s"   . lsp-ui-sideline-mode)
	  ("C-c d"   . m/toggle-lsp-ui-doc))
    :hook
    (lsp-mode . lsp-ui-mode)))

(use-package dap-mode
  :custom
  (dap-lldb-debugged-program-function (lambda () (read-file-name "Select file to debug.")))
  :after lsp-mode posframe
  :config
  (require 'dap-hydra)
  (require 'dap-lldb)  
  (require 'dap-go)
  (require 'dap-ui)
  (dap-mode 1)
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  ;; use tooltips for mouse hover
  ;; if it is not enabled `dap-mode' will use the minibuffer.
  (tooltip-mode 1)
  ;; displays floating panel with debug buttons
  ;; requies emacs 26+
  (dap-ui-controls-mode 1))

(use-package lsp-ivy
  :load-path m/load-path
  :requires lsp-mode
  :config
  (defun m/lsp-ivy-workspace-symbol-at-point ()
    (interactive)
    (let ((current-prefix-arg t))
      (call-interactively #'lsp-ivy-workspace-symbol)))

  (defun m/lsp-ivy-global-workspace-symbol-at-point ()
    (interactive)
    (let ((current-prefix-arg t))
      (call-interactively #'lsp-ivy-global-workspace-symbol)))
  :bind
  (:map lsp-mode-map
        ("C-c C-s" . m/lsp-ivy-workspace-symbol-at-point)
        ("C-c C-p" . m/lsp-ivy-global-workspace-symbol-at-point)))

(use-package company-lsp
  :requires (company yasnippet)
  :after (company yasnippet)
  :custom
  (push 'company-lsp company-backends)
  (company-lsp-cache-candidates nil)
  (company-lsp-async t))

(use-package cc-mode
  :bind 
    (:map c-mode-base-map
       ("C-c c" . compile)))

(provide 'init-lsp)
