(use-package grab-mac-link
  :if (eq m/os 'macos))

(use-package ob-async
  :hook
  (org-babel-after-execute . org-redisplay-inline-images))

(defun m/org-screenshot ()
  "Take a screenshot into a time stamped unique-named file in the
same directory as the org-buffer and insert a link to this file."
  (interactive)
  (org-display-inline-images)
  (setq filename
	(concat
	 (make-temp-name
	  (concat (file-name-nondirectory (buffer-file-name))
		  "_imgs/"
		  (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))
  (unless (file-exists-p (file-name-directory filename))
    (make-directory (file-name-directory filename)))
					; take screenshot
  (if (eq system-type 'darwin)
      (call-process "screencapture" nil nil nil "-i" filename))
  (if (eq system-type 'gnu/linux)
      (call-process "import" nil nil nil filename))
					; insert into file if correctly taken
  (if (file-exists-p filename)
      (insert (concat "[[file:" filename "]]"))))

(use-package org
  :diminish org-mode
  :preface
  (defun m/refile-targets ()
    "Refile targets with inbox.org excluded"
    (let ((excluded '("mobile.org" "inbox.org" "notes.org")))
      (seq-filter (lambda (f) (if (member (file-name-nondirectory f) excluded) nil t)) org-agenda-files)))
  :custom
  (org-src-fontify-natively t)
  (org-descriptive-links nil)
  (org-special-ctrl-a/e t)
  (org-use-sub-superscripts '{})
  (org-export-with-sub-superscripts '{})
  (private-directory "~/agendas")
  (org-directory private-directory)
  (org-confirm-babel-evaluate nil)
  (org-clock-out-remove-zero-time-clocks t)
  (org-startup-folded 'content)
  (org-columns-default-format "%50ITEM(Task) %5TODO(Todo) %10Effort(Effort){:} %10CLOCKSUM(Clock) %2PRIORITY %TAGS")
  (org-agenda-columns-add-appointments-to-effort-sum t)
  (org-agenda-span 'day)
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-log-mode-items (quote (closed clock)))
  (org-agenda-clockreport-parameter-plist
   '(:maxlevel 5 :block t :tstart t :tend t :emphasize t :link t :narrow 80 :indent t :formula nil :timestamp t :level 5 :tcolumns nil :formatter nil))
  (org-global-properties (quote ((
				  "Effort_ALL" . "00:05 00:10 00:15 00:30 01:00 01:30 02:00 02:30 03:00"))))
  (org-agenda-files `(
		      ,(expand-file-name "inbox.org" private-directory)
		      ,(expand-file-name "todos.org" private-directory)
		      ,(expand-file-name "mobile.org" private-directory)
		      ,(expand-file-name "routine.org" private-directory)
		      ))
  (org-archive-location (concat (expand-file-name "archive.org" private-directory) "::datetree/* Finished Tasks"))
  (org-support-shift-select t)

  ;; Targets start with the file name - allows creating level 1 tasks
  ;;(setq org-refile-use-outline-path (quote file))
  (org-refile-use-outline-path t)
  (org-outline-path-complete-in-steps nil)

  ;; Allow refile to create parent tasks with confirmation
  (org-refile-use-cache t)

  (org-refile-allow-creating-parent-nodes 'confirm)
  (org-refile-targets '((m/refile-targets :maxlevel . 5)))
  (org-archive-mark-done nil)
	;;; Agenda views
  (org-agenda-clockreport-parameter-plist '(:link t :maxlevel 3))
  ;; (org-agenda-current-time-string "------------NOW------------")
  ;; (org-agenda-time-grid
  ;;     '((daily today require-timed)
  ;;       (0800 1000 1100 1200 1300 1400 1500 1600 1700 1800 1900 2000 2100 2200 2300)
  ;;       " ........ "
  ;;       "---------------------------"))
  (org-clock-persist t)
  (org-log-done t)
  (org-edit-timestamp-down-means-later t)
  (org-hide-emphasis-markers t)
  (org-catch-invisible-edits 'show)
  (org-export-coding-system 'utf-8)
  (org-fast-tag-selection-single-key 'expert)
  (org-clock-in-resume t)
  ;; Save clock data and notes in the LOGBOOK drawer
  (org-clock-into-drawer t)
  ;; Save state changes in the LOGBOOK drawer
  (org-log-into-drawer t)
  ;; Removes clocked tasks with 0:00 duration
  (org-clock-out-remove-zero-time-clocks t)
  ;; Show clock sums as hours and minutes, not "n days" etc.
  (org-time-clocksum-format '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))
  :custom-face
  (org-link ((t (:foreground "#ebe087" :underline t))))
  (org-list-dt ((t (:foreground "#bd93f9"))))
  (org-special-keyword ((t (:foreground "#6272a4"))))
  (org-todo ((t (:background "#272934" :foreground "#51fa7b" :weight bold))))
  (org-document-title ((t (:foreground "#f1fa8c" :weight bold))))
  (org-done ((t (:background "#373844" :foreground "#216933" :strike-through nil :weight bold))))
  (org-footnote ((t (:foreground "#76e0f3"))))

  ;; do not scale outline header
  ;; (org-level-1 ((t (:inherit outline-1 :height 1.0))))
  ;; (org-level-2 ((t (:inherit outline-2 :height 1.0))))
  ;; (org-level-3 ((t (:inherit outline-3 :height 1.0))))
  ;; (org-level-4 ((t (:inherit outline-4 :height 1.0))))
  ;; (org-level-5 ((t (:inherit outline-5 :height 1.0))))
  :bind (("C-c c" . org-capture)
	 ("C-c a" . org-agenda)
	 ("C-c b" . org-switchb)
	 ("C-x C-l" . org-store-link)
	 :map org-mode-map
	 ("C-c i" . org-clock-in)
	 ("C-c o" . org-clock-out)
	 ("C-c n" . org-narrow-to-subtree)
	 ("C-c b" . org-narrow-to-block)
	 ("C-c {" . sp-wrap-curly)
	 ("C-c (" . sp-wrap-round)
	 ("C-c [" . sp-wrap-square)
	 ("C-c w" . widen)
	 ("C-c e" . org-set-effort))
  :hook
  (org-agenda-after-show . org-show-entry)
  (org-agenda-mode . hl-line-mode)
  (org-mode . (lambda ()
		(dolist (key '("C-'" "C-," "C-."))
		  (unbind-key key org-mode-map))))
  :config
  (advice-add 'org-refile :after (lambda (&rest _) (org-save-all-org-buffers)))
  (advice-add 'org-deadline       :after (lambda (&rest _rest)  (org-save-all-org-buffers)))
  (advice-add 'org-schedule       :after (lambda (&rest _rest)  (org-save-all-org-buffers)))
  (advice-add 'org-agenda-schedule       :after (lambda (&rest _rest)  (org-save-all-org-buffers)))
  (advice-add 'org-agenda-capture       :after (lambda (&rest _rest)  (org-save-all-org-buffers)))
  (advice-add 'org-store-log-note :after (lambda (&rest _rest)  (org-save-all-org-buffers)))
  (advice-add 'org-todo           :after (lambda (&rest _rest)  (org-save-all-org-buffers)))
  (setq org-todo-keywords
	(quote (
		(sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
		(sequence "PROJECT(p)" "|" "DONE(d!/!)" "CANCELLED(c@/!)")
		(sequence "WAITING(w@/!)" "DELEGATED(e!)" "HOLD(h)" "|" "CANCELLED(c@/!)")
		))
	org-todo-repeat-to-state "NEXT")
  (setq org-todo-keyword-faces
	'(("WAIT" . (:foreground "#6272a4":weight bold))
	  ("NEXT"   . (:foreground "#f1fa8c" :weight bold))
	  ("CARRY/O" . (:foreground "#6272a4" :background "#373844" :weight bold))))
  ;; load babel languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((R . t)
     (dot . t)
     (ditaa . t)
     (emacs-lisp . t)
     (gnuplot . t)
     (haskell . nil)
     (latex . t)
     (ledger . t)
     (ocaml . nil)
     (octave . t)
     (plantuml . t)
     (python . t)
     (ruby . t)
     (screen . nil)
     (shell . t)
     (sql . t)
     (sqlite . t))))

(use-package org
  :custom
  (org-capture-templates
   `(
     ;; tasks
     ("t" "general task." entry
      (file ,(expand-file-name "inbox.org" private-directory))
      "* NEXT %?\n%U\n" :clock-resume 1)
     ("n" "notes." entry
      (file ,(expand-file-name "notes.org" private-directory))
      "* %? :NOTE:\n%U\n%a\n" :clock-resume t)
     ("r" "retrospective note" plain (file+olp+datetree ,(expand-file-name "retrospect.org" private-directory))
      ,(concat "%[" (expand-file-name "retrospect.tmpl" private-directory) "]")
      :tree-type week)
     ("i" "interrupt task" entry
      (file ,(expand-file-name "inbox.org" private-directory))
      "* NEXT %?\n%U\n" :clock-in 1 :clock-keep 1)
     ("e" "Collect hacking Emacs ideas!" item
      (file+headline ,(expand-file-name "inbox.org" private-directory) "Hacking Emacs")
      "- [ ] %?"
      :prepend t)
     ("p" "Add an event to the private calendar." entry
      (file+olp schedule-file "Calendar" "2019" "Private")
      "** %?\n   SCHEDULED: <%(org-read-date)>\n"
      :prepend t)
     ("w" "Add an event to the work calendar." entry
      (file+olp schedule-file "Calendar" "2019" "Work")
      "** %?\n   SCHEDULED: <%(org-read-date)>\n")
     ("l" "Store the link of the current position in the clocking task." item
      (clock)
      "- %A\n"
      :immediate t :prepend t))))

(use-package org
  :custom
  (org-agenda-custom-commands
   `(("N" "Notes" tags "NOTE"
      ((org-agenda-overriding-header "Notes")
       (org-tags-match-list-sublevels t)))
     ("a" "Agenda"
      ((agenda "" ((org-agenda-span 1)                      ; daily agenda
		   (org-deadline-warning-days 0)            ; 7 day advanced warning for deadlines
		   (org-agenda-scheduled-leaders '("" "Due %2dx: "))
		   (org-agenda-overriding-header "Today's Schedule:")
		   (org-agenda-start-day nil)
		   (org-agenda-start-on-weekday nil)
		   (org-agenda-compact-blocks t)
		   (org-agenda-use-time-grid nil)))
       (agenda "" ((org-agenda-span 7)                      ;; overview of appointments
		   (org-agenda-start-on-weekday nil)         ;; calendar begins today
		   (org-agenda-scheduled-leaders '("" "Due %2dx: "))
		   (org-agenda-deadline-leaders '("" "Due in %2dx: "))
		   (org-agenda-start-day "+1d")
		   (org-agenda-show-future-repeats nil)
		   (org-agenda-entry-types '(:timestamp :sexp :scheduled :deadline))))))
     ("g" "GTD"
      ((agenda "" ((org-agenda-span 1)                      ; daily agenda
		   (org-deadline-warning-days 0)            ; 7 day advanced warning for deadlines
		   (org-agenda-scheduled-leaders '("" "Due %2dx: "))
		   (org-agenda-overriding-header "Today's Schedule:")
		   (org-agenda-start-day nil)
		   (org-agenda-start-on-weekday nil)
		   (org-agenda-compact-blocks t)
		   (org-agenda-use-time-grid nil)))
       (agenda "" ((org-agenda-scheduled-leaders '("" "Due %2dx: "))
		   (org-agenda-skip-function '(lambda () (let ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
							  (current-headline (or (and (org-at-heading-p)
										     (point))
										(save-excursion (org-back-to-heading)))))
						      (if (and (member "routine" (org-get-tags-at current-headline)) (not (org-get-deadline-time current-headline)))
							  next-headline
							nil))))
		   (org-agenda-span 7)
		   (org-agenda-start-on-weekday nil)
		   (org-agenda-start-day "+1d")
		   (org-agenda-show-future-repeats nil)
		   (org-agenda-entry-types '(:timestamp :sexp :scheduled :deadline))))

       (tags "INBOX"
	     ((org-agenda-overriding-header "Inbox")
	      (org-agenda-skip-function
	       '(lambda ()
		  (or (org-agenda-skip-subtree-if 'todo '("DONE"))
		      (org-agenda-skip-entry-if 'nottodo '("TODO" "NEXT")))))
	      (org-tags-match-list-sublevels t)))
       (stuck ""
	      ((org-agenda-overriding-header "Stuck Projects")
	       (org-agenda-tags-todo-honor-ignore-options t)
	       (org-tags-match-list-sublevels t)
	       (org-agenda-skip-function
		'(lambda ()
		   (org-agenda-skip-entry-if 'nottodo '("PROJECT"))))
	       (org-agenda-todo-ignore-deadlines 'all)
	       (org-agenda-todo-ignore-scheduled 'all)))
       (tags-todo "-INBOX"
		  ((org-agenda-overriding-header "Next Actions")
		   (org-agenda-tags-todo-honor-ignore-options t)
		   (org-agenda-todo-ignore-scheduled 'all)
		   (org-agenda-todo-ignore-deadlines 'all)
		   (org-agenda-skip-function
		    '(lambda ()
		       (or (org-agenda-skip-subtree-if 'todo '("HOLD" "WAITING"))
			   (org-agenda-skip-entry-if 'nottodo '("NEXT")))))
		   (org-tags-match-list-sublevels t)
		   (org-agenda-sorting-strategy
		    '(todo-state-down effort-up category-keep))))
       (tags-todo "-INBOX/PROJECT"
		  ((org-agenda-overriding-header "Projects")
		   (org-tags-match-list-sublevels t)
		   (org-agenda-sorting-strategy
		    '(category-keep))))
       (tags-todo "-INBOX/-NEXT"
		  ((org-agenda-overriding-header "Orphaned Tasks")
		   (org-agenda-tags-todo-honor-ignore-options t)
		   (org-agenda-todo-ignore-scheduled 'all)
		   (org-agenda-todo-ignore-deadlines 'all)
		   (org-agenda-skip-function
		    '(lambda ()
		       (or (org-agenda-skip-subtree-if 'todo '("PROJECT" "HOLD" "WAITING" "DELEGATED"))
			   (org-agenda-skip-subtree-if 'nottododo '("TODO")))))
		   (org-tags-match-list-sublevels t)
		   (org-agenda-sorting-strategy
		    '(category-keep))))
       (tags-todo "/WAITING"
		  ((org-agenda-overriding-header "Waiting")
		   (org-agenda-tags-todo-honor-ignore-options t)
		   (org-agenda-todo-ignore-scheduled 'all)
		   (org-agenda-todo-ignore-deadlines 'all)
		   (org-agenda-sorting-strategy
		    '(category-keep))))
       (tags-todo "/DELEGATED"
		  ((org-agenda-overriding-header "Delegated")
		   (org-agenda-tags-todo-honor-ignore-options t)
		   (org-agenda-todo-ignore-scheduled 'all)
		   (org-agenda-todo-ignore-deadlines 'all)
		   (ORG-agenda-sorting-strategy
		    '(category-keep))))
       (tags-todo "-INBOX"
		  ((org-agenda-overriding-header "On Hold")
		   (org-agenda-skip-function
		    '(lambda ()
		       (or (org-agenda-skip-subtree-if 'todo '("WAITING"))
			   (org-agenda-skip-entry-if 'nottodo '("HOLD")))))
		   (org-tags-match-list-sublevels nil)
		   (org-agenda-sorting-strategy
		    '(category-keep))))

       ;; (tags-todo "-NEXT"
       ;;            ((org-agenda-overriding-header "All other TODOs")
       ;;             (org-match-list-sublevels t)))
       )))))

(use-package org
  :custom
  (m/pomodoro/focus 45)
  (m/pomodoro/break 5)
  (m/pomodoro/task/current nil)
  (m/pomodoro/task/next nil)
  (m/pomodoro/status/updater nil)
  :preface
  (defun m/org-clock-out-and-save-when-exit ()
    "Save buffers and stop clocking when kill emacs."
    (ignore-errors (org-clock-out) t)
    (save-some-buffers t))
  (defun m/pomodoro/reset ()
    "timer update"
    (if (and m/pomodoro/task/current (timerp m/pomodoro/task/current))
	(cancel-timer m/pomodoro/task/current)
      (setq m/pomodoro/task/current nil))

    (if (and m/pomodoro/task/next (timerp m/pomodoro/task/next))
	(cancel-timer m/pomodoro/task/next)
      (setq m/pomodoro/task/next nil))

    (if (and m/pomodoro/status/updater (timerp m/pomodoro/status/updater))
	(cancel-timer m/pomodoro/status/updater)
      (setq m/pomodoro/status/updater nil)))
  (defun m/clockin ()
    "clockin hook"
    (m/pomodoro/reset)
    (setq m/pomodoro/status/updater (run-at-time 0 60 '(lambda ()
							 (setq org-mode-line-string (m/task-clocked-time))
							 (force-mode-line-update))))
    (setq m/pomodoro/task/current (run-at-time (* m/pomodoro/focus 60) nil (lambda() (org-clock-out)))))
  (defun m/clockout ()
    "clock out hook"
    (m/pomodoro/reset)
    (setq m/pomodoro/task/next (run-at-time (* m/pomodoro/break 60) nil (lambda()
									  (ignore-errors
									    (request "http://127.0.0.1:13140"
									      :type "POST"
									      :data (json-encode `(("type" . "FOCUS")
												   ("title" . "Ready to work")
												   ("duration" . 5)))
									      :headers '(("Content-Type" . "application/json"))))))))
  (defun m/task-clocked-time ()
    "Return a string with the clocked time and effort, if any"
    (interactive)
    (let* ((clocked-time (org-clock-get-clocked-time))
	   (h (truncate clocked-time 60))
	   (m (mod clocked-time 60))
	   (work-done-str (format "%d:%02d" h m)))
      (if org-clock-effort
	  (let* ((effort-in-minutes
		  (org-duration-to-minutes org-clock-effort))
		 (effort-h (truncate effort-in-minutes 60))
		 (effort-m (truncate (mod effort-in-minutes 60)))
		 (effort-str (format "%d:%02d" effort-h effort-m)))
	    (format "%s/%s" work-done-str effort-str))
	(format "%s" work-done-str))))
  :hook
  (kill-emacs . m/org-clock-out-and-save-when-exit)
  (org-clock-in .
		(lambda ()
		  (m/clockin)
		  (ignore-errors (request "http://127.0.0.1:13140"
				   :type "POST"
				   :data (json-encode `(("type" . "FOCUSED")
							("title" . ,(or org-clock-current-task "interrupt task"))
							("duration" . 45)))
				   :headers '(("Content-Type" . "application/json"))))))
  (org-clock-out . (lambda ()
		     (m/clockout)
		     (ignore-errors
		       (request "http://127.0.0.1:13140"
			 :type "POST"
			 :data (json-encode `(("type" . "UNFOCUSED")
					      ("title" . "Have a rest")
					      ("duration" . 5)))
			 :headers '(("Content-Type" . "application/json"))))))
  (org-clock-in-last . (lambda ()
			 (m/clockin)
			 (ignore-errors
			   (request "http://127.0.0.1:13140"
			     :type "POST"
			     :data (json-encode `(("type" . "FOCUSED")
						  ("title" . ,(or org-clock-current-task "interrupt task"))
						  ("duration" . 45)))
			     :headers '(("Content-Type" . "application/json")))))))

(use-package org
  :custom
  (org-export-allow-bind-keywords t))

(use-package org
  :custom
  (org-ditaa-jar-path (expand-file-name "ditaa.jar" m/conf.d))
  (org-plantuml-jar-path (expand-file-name "plantuml.jar" m/conf.d))
  :config
  (unless (and (boundp 'org-ditaa-jar-path)
	       (file-exists-p org-ditaa-jar-path)
	       (not (file-directory-p org-ditaa-jar-path)))
    (let ((jar-name "ditaa.jar")
	  (url "https://github.com/stathissideris/ditaa/releases/download/v0.11.0/ditaa-0.11.0-standalone.jar"))
      (setq org-ditaa-jar-path (expand-file-name jar-name m/conf.d))
      (unless (file-exists-p org-ditaa-jar-path)
	(url-copy-file url org-ditaa-jar-path))))

  (unless (and (boundp 'org-plantuml-jar-path)
	       (file-exists-p org-plantuml-jar-path)
	       (not (file-directory-p org-plantuml-jar-path)))
    (let ((jar-name "plantuml.jar")
	  (url "https://downloads.sourceforge.net/project/plantuml/1.2020.2/plantuml.1.2020.2.jar"))
      (setq org-plantuml-jar-path (expand-file-name jar-name m/conf.d))
      (unless (file-exists-p org-plantuml-jar-path)
	(url-copy-file url org-plantuml-jar-path)))))

(use-package plantuml-mode
  :after org
  :config
  (add-to-list
   'org-src-lang-modes '("plantuml" . plantuml))
  (setq plantuml-jar-path org-plantuml-jar-path)
  (setq plantuml-default-exec-mode 'jar))

(use-package graphviz-dot-mode
  :after org
  :config
  (setq graphviz-dot-indent-width 4))
(use-package company-graphviz-dot
  :after company
  :ensure nil)

;; Download Drag&Drop images
(use-package org-download
  :after (org))

;; Pretty bullets
(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :config
  (setq org-bullets-face-name (quote org-bullet-face))
  (org-bullets-mode 1)
  (setq org-bullets-bullet-list '("✙" "♱" "♰" "☥" "✞" "✟" "✝" "†" "✠" "✚" "✜" "✛" "✢" "✣" "✤" "✥")))

(use-package ox-gfm)
(use-package ox-hugo
  :after (ox org)
  :custom
  (org-blackfriday--org-element-string '((src-block . "Code")
					 (table . "Table")
					 (figure . "Figure"))))

(provide 'init-literate)
