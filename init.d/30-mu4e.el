;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mu4e
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (and (find-exe "mu")
	   (file-exists-p my-mu4e-lisp-path))
  (message "Configuring mu4e")

  (require 'mu4e)

  (add-to-list 'load-path my-mu4e-lisp-path)

  (when (package-installed-p 'eww)
    (require 'mu4e-contrib)
    (setq mu4e-html2text-command 'mu4e-shr2text))
  
  (add-hook 'message-send-hook 'mml-secure-message-sign-pgpmime)

  ;; From: http://hugoduncan.org/post/snarf-pgp-keys-in-emacs-mu4e/
  (defun mu4e-view-snarf-pgp-key (&optional msg)
    "Snarf the pgp key for the specified message."
    (interactive)
    (let* ((msg (or msg (mu4e-message-at-point)))
	   (path (mu4e-message-field msg :path))
	   (cmd (format "%s verify --verbose %s"
			mu4e-mu-binary
			(shell-quote-argument path)))
	   (output (shell-command-to-string cmd)))
      (let ((case-fold-search nil))
	(when (string-match "key:\\([A-F0-9]+\\)" output)
	  (let* ((cmd (format "%s --recv %s"
			      epg-gpg-program (match-string 1 output)))
		 (output (shell-command-to-string cmd)))
	    (message output))))))
  (add-to-list 'mu4e-view-actions '("Snarf PGP keys" . mu4e-view-snarf-pgp-key) t)

  (add-to-list 'mu4e-headers-actions
	       '("org-contact-add" . mu4e-action-add-org-contact) t)
  (add-to-list 'mu4e-view-actions
	       '("org-contact-add" . mu4e-action-add-org-contact) t))
