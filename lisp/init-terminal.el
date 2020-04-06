(setenv "PAGER" "cat")

(use-package shrink-path)
(use-package eshell
  :after shrink-path
  :init
  (defvar eshell-mode-map (make-sparse-keymap))
  :custom
  (eshell-scroll-to-bottom-on-input 'all)
  (eshell-error-if-no-glob t)
  (eshell-hist-ignoredups t)
  (eshell-save-history-on-exit t)
  (eshell-prefer-lisp-functions nil)
  (eshell-destroy-buffer-when-process-dies t)
  (eshell-cmpl-cycle-completions nil)
  (eshell-cmpl-ignore-case t)
  (eshell-ask-to-save-history (quote always))
  (eshell-prompt-regexp "❯❯❯ ")
  (eshell-prompt-function
   (lambda ()
     (format "%s %s\n%s%s%s "
	     (all-the-icons-octicon "repo")
	     (propertize (cdr (shrink-path-prompt default-directory)) 'face `(:foreground "white"))
	     (propertize "❯" 'face `(:foreground "#ff79c6"))
	     (propertize "❯" 'face `(:foreground "#f1fa8c"))
	     (propertize "❯" 'face `(:foreground "#50fa7b")))))
  :bind
  (:map eshell-mode-map
	("C-c c" . eshell-interrupt-process))
  :hook
  ((eshell-mode . (lambda() (add-to-list 'eshell-visual-commands "ssh")
		    (add-to-list 'eshell-visual-commands "tail")
		    (add-to-list 'eshell-visual-commands "top")
		    (setq-local eshell-mode-map (make-sparse-keymap))))))

(use-package eshell
  :hook
  (eshell-mode . (lambda() 
                   (eshell/alias "e" "find-file $1")
		   (eshell/alias "ff" "find-file $1")
		   (eshell/alias "emacs" "find-file $1")
		   (eshell/alias "ee" "find-file-other-window $1")

		   (eshell/alias "gd" "magit-diff-unstaged")
		   (eshell/alias "gds" "magit-diff-staged")
		   (eshell/alias "d" "dired $1")
		   (let ((ls (if (file-exists-p "/usr/local/bin/gls")
                   "/usr/local/bin/gls"
                   "/bin/ls")))
		   (eshell/alias "ll" (concat ls " -AlohG --color=always"))))))

(defun eshell/gst (&rest args)
    (magit-status (pop args) nil)
    (eshell/echo))

(defun eshell/find (&rest args)
  "Wrapper around the ‘find’ executable."
  (let ((cmd (concat "find " (string-join args " "))))
    (shell-command-to-string cmd)))
(defun eshell/f (filename &optional dir try-count)
  "Searches for files matching FILENAME in either DIR or the
current directory. Just a typical wrapper around the standard
`find' executable.

Since any wildcards in FILENAME need to be escaped, this wraps the shell command.

If not results were found, it calls the `find' executable up to
two more times, wrapping the FILENAME pattern in wildcat
matches. This seems to be more helpful to me."
  (let* ((cmd (concat
               (executable-find "find")
               " " (or dir ".")
               "      -not -path '*/.git*'"
               " -and -not -path '*node_modules*'"
               " -and -not -path '*classes*'"
               " -and "
               " -type f -and "
               "-iname '" filename "'"))
         (results (shell-command-to-string cmd)))

    (if (not (s-blank-str? results))
        results
      (cond
       ((or (null try-count) (= 0 try-count))
        (eshell/f (concat filename "*") dir 1))
       ((or (null try-count) (= 1 try-count))
        (eshell/f (concat "*" filename) dir 2))
       (t "")))))

(defun eshell/ef (filename &optional dir)
  "Searches for the first matching filename and loads it into a
file to edit."
  (let* ((files (eshell/f filename dir))
         (file (car (s-split "\n" files))))
    (find-file file)))

(use-package eshell-autojump)

(use-package eshell-toggle
  :bind
  (("M-<f12>" . eshell-toggle)))

(use-package eshell
  :config
  (defun eshell/clear ()
  "Clear the eshell buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input))))

(provide 'init-terminal)
