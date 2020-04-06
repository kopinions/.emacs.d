;;(setq url-proxy-services '(("https" . "127.0.0.1:1089")
;;                        ("http" . "127.0.0.1:1089")))
(setq debug-on-error t)

(let ((minver "24.4"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))
(when (version< emacs-version "25.1")
  (message "Your Emacs is old, and some functionality in this config will be disabled. Please upgrade if possible."))

(defconst m/os
  (let ((os (symbol-name system-type)))
    (cond ((string= os "darwin") 'macos)
          ((string-prefix-p "gnu" os) 'linux)
          ((or (string-prefix-p "ms" os) (string-prefix-p "windows" os)) 'windows))))
(defvar m/conf.d (expand-file-name user-emacs-directory))

(defvar m/offline 
  (file-directory-p (expand-file-name "offline" user-emacs-directory)))

(setq gc-cons-threshold 100000000)

(require 'package)
  ;;; Install into separate package dirs for each Emacs version, to prevent bytecode incompatibility
(let ((versioned-package-dir
       (expand-file-name (format "elpa-%s.%s" emacs-major-version emacs-minor-version)
			 user-emacs-directory)))
  (setq package-user-dir versioned-package-dir))

(if m/offline
    (setq package-archives `(("gnu" . ,(expand-file-name "offline/gnu" user-emacs-directory))
			     ("melpa" . ,(expand-file-name "offline/melpa" user-emacs-directory))
			     ("org" . ,(expand-file-name "offline/org" user-emacs-directory))))
  (let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
		      (not (gnutls-available-p))))
	 (proto (if no-ssl "http" "https")))
    (if no-ssl
	(setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
				 ("melpa" . "http://elpa.emacs-china.org/melpa/")
				 ("org" . "http://elpa.emacs-china.org/org/")))
      (unless no-ssl
	(setq package-archives '(("gnu"   . "https://elpa.emacs-china.org/gnu/")
				 ("melpa" . "https://elpa.emacs-china.org/melpa/")
				 ("org" . "http://elpa.emacs-china.org/org/")))))))

;; Work-around for https://debbugs.gnu.org/cgi/bugreport.cgi?bug=34341
(when (version= "26.2" emacs-version)
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

(package-initialize)
(package-refresh-contents)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile (require 'use-package))

(setq use-package-always-ensure t)

(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

(use-package use-package-ensure-system-package
  :ensure t)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(use-package auto-compile
:ensure t
:custom (load-prefer-newer t)
:config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

(defconst m/load-path (lambda () (list 
			     (expand-file-name "lisp" user-emacs-directory)
			     (expand-file-name "verilog-mode" (expand-file-name "3rdparty" user-emacs-directory)))))
(use-package init-os
  :load-path m/load-path)
(use-package init-ui
  :load-path m/load-path)
(use-package init-correction
  :load-path m/load-path)
(use-package init-project
  :load-path m/load-path)
(use-package init-literate
  :load-path m/load-path)
(use-package init-vcs
  :load-path m/load-path)
(use-package init-lsp
  :load-path m/load-path)
(use-package init-editing
  :load-path m/load-path)
(use-package init-chinese
  :load-path m/load-path)
(use-package init-languages
  :load-path m/load-path)
(use-package init-terminal
  :load-path m/load-path)
(use-package init-completion
  :load-path m/load-path)
(use-package init-email
  :load-path m/load-path)

(provide 'init)
