(defun sb/mbsync (&rest group)
  (interactive)
  (let ((group (or (and (car group) group) '("--all")))
	(command `("mbsync" "--verbose" "--quiet" ,@group)))
    (message "Starting %s.." (mapconcat 'identity command " "))
    (apply 'start-process "mbsync" "*mbsync*" command)))

(use-package notmuch
  :ensure-system-package (notmuch msmtp (mbsync . isync))
  :hook
  (notmuch-message-mode . turn-off-auto-fill)
  :bind (("C-x m" . sb/notmuch)
	 ("M-]" . notmuch-cycle-notmuch-buffers))
  :custom
  (notmuch-address-use-company nil)
  (notmuch-hello-thousands-separator ",")
  (notmuch-mua-cite-function (quote message-cite-original-without-signature))
  (notmuch-fcc-dirs "sent +sent -unread -inbox")
  (notmuch-saved-searches
   (quote
    ((:name "Inbox" :query "tag:inbox" :key "i")
     (:name "Flagged" :query "tag:flagged" :key "f")
     (:name "Drafts" :query "tag:draft" :key "d")
     (:name "New in Threads" :query "thread:\"{from:stig}\" and tag:new and not tag:sent" :key "t" :sort-order newest-first :search-type tree)
     (:name "All in Threads" :query "thread:\"{from:stig}\"" :key "T" :sort-order newest-first :search-type tree :count-query "tag:no-match")
     (:name "List Messages" :query "tag:lists and tag:new and not tag:sent" :key "l" :search-type tree)
     (:name "Recent-ish" :query "date:-4d..today and not tag:lists" :key "r" :count-query "tag:no-match" :sort-order newest-first))))
  (notmuch-tagging-keys
   (quote
    (("a" notmuch-archive-tags "Archive")
     ("u" notmuch-show-mark-read-tags "Mark read")
     ("f"
      ("+flagged")
      "Flag")
     ("s"
      ("+spam" "-inbox")
      "Mark as spam")
     ("d"
      ("+deleted" "-inbox")
      "Delete")
     ("m"
      ("+muted")
      "Mute Thread"))))

  :config
  (defun sb/notmuch (arg)
    "Launch notmuch. If ran with prefix arg, launch mbsync in the
  background, and automatically refresh the current buffer when
  done. With two prefix args, launch mbsync with `--all` rather
  than just for inboxes."
    (interactive "p")
    (notmuch)
    (if (> arg 1)
	(set-process-sentinel
	 (sb/mbsync (if (eq 4 arg) "inbox" "--all"))
	 (lambda (proc state)
	   (message nil) ;; clear minibuffer
	   (notmuch-poll-and-refresh-this-buffer))))))

(setq message-send-mail-function 'message-send-mail-with-sendmail
      sendmail-program "msmtp"
      message-sendmail-envelope-from 'header
      mail-envelope-from 'header
      mail-specify-envelope-from t)

(setq message-kill-buffer-on-exit t)

(provide 'init-email)
