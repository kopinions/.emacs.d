;;(setq url-proxy-services '(("https" . "127.0.0.1:1089")
;;                        ("http" . "127.0.0.1:1089")))
(setq debug-on-error t)

(let ((minver "24.4"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))
(when (version< emacs-version "25.1")
  (message "Your Emacs is old, and some functionality in this config will be disabled. Please upgrade if possible."))

(let ((home\.d (quote "/Users/neo"))
      (conf\.d (quote "/Users/neo/.emacs.d/")))
(defconst m/os
  (let ((os (symbol-name system-type)))
    (cond ((string= os "darwin") 'macos)
	  ((string-prefix-p "gnu" os) 'linux)
	  ((or (string-prefix-p "ms" os) (string-prefix-p "windows" os)) 'windows))))

(defvar m/conf.d conf.d)
(defvar m/home.d home.d)

(defun m/var (name)
  "Return value of variable or environment identified by NAME."
  (or (getenv name) (eval (read name))))

(defun m/resolve (path)
  "Interpolation variable like ${var} $var in PATH with environment or elisp variables."
  (if (string-prefix-p "~/" path)
      (m/resolve (concat (getenv "HOME") (substring path 1)))
    (let ((s (or (string-match "${\\([^ }]*\\)}" path)
		 (string-match "$\\([A-z_]*\\)" path)))
	  (e (match-end 0)))
      (if (not s)
	  path
	(m/resolve
	 (concat (substring path 0 s) (m/var (match-string 1 path)) (substring path e)))))))


(defun tangle-if-absent (path)
  (let* ((filename (m/resolve path)))
    (if (file-exists-p filename)
	nil
      filename)))
)

(global-unset-key (kbd "M-`"))

(defvar m/offline 
  (file-directory-p (expand-file-name "offline" m/conf.d)))

(setq gc-cons-threshold 100000000)

(require 'package)
  ;;; Install into separate package dirs for each Emacs version, to prevent bytecode incompatibility
(let ((versioned-package-dir
       (expand-file-name (format "elpa-%s.%s" emacs-major-version emacs-minor-version)
			 m/conf.d)))
  (setq package-user-dir versioned-package-dir))

(if m/offline
    (setq package-archives `(("gnu" . ,(expand-file-name "offline/gnu" m/conf.d))
			     ("melpa" . ,(expand-file-name "offline/melpa" m/conf.d))
			     ("org" . ,(expand-file-name "offline/org" m/conf.d))))
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

(unless (and 
	 (package-installed-p 'use-package)
	 (package-installed-p 'diminish))
  (package-refresh-contents)
  (package-install 'use-package)
  (package-install 'diminish))

(eval-when-compile (require 'use-package))
(require 'diminish)
(setq use-package-always-ensure t)

(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

(use-package use-package-ensure-system-package
  :ensure t)

(add-to-list 'load-path (expand-file-name "lisp" m/conf.d))
(setq custom-file (expand-file-name "custom.el" m/conf.d))

(use-package auto-compile
:ensure t
:custom (load-prefer-newer t)
:config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

(defconst m/load-path (lambda () (list 
			     (expand-file-name "lisp" m/conf.d)
			     (expand-file-name "verilog-mode" (expand-file-name "3rdparty" m/conf.d))
			     (expand-file-name "lsp-ivy" (expand-file-name "3rdparty" m/conf.d))
			     (expand-file-name "librime" (expand-file-name "3rdparty" m/conf.d))
			     (expand-file-name "liberime" (expand-file-name "3rdparty" m/conf.d)))))
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
