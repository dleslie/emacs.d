;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mu4e
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (and (find-exe "mu")
	   (file-exists-p my-mu4e-lisp-path))
  (message "Configuring mu4e")

  (add-to-list 'load-path my-mu4e-lisp-path)

  (require 'mu4e)

  (setq 
   mu4e-maildir 
   "~/Maildir"

   mu4e-drafts-folder
   mail-folder-drafts

   mu4e-sent-folder
   mail-folder-sent

   mu4e-trash-folder
   mail-folder-trash

   mu4e-sent-messages-behavior
   'sent

   mu4e-maildir-shortcuts
   `((,mail-folder-inbox . ?i)
     (,mail-folder-sent  . ?s)
     (,mail-folder-trash . ?t))

   mu4e-get-mail-command
   "offlineimap"

   message-kill-buffer-on-exit
   t

   mu4e-attachment-dir
   user-mail-attachment-directory

   mu4e-compose-reply-to-address
   user-mail-address

   mu4e-headers-include-related
   nil

   mu4e-headers-results-limit
   -1

   mu4e-sent-messages-behavior
   (quote sent)

   mu4e-update-interval
   -1

   mu4e-user-mail-address-list
   `(,user-mail-login ,user-mail-address)

   mu4e-view-show-addresses
   t

   mu4e-view-show-images
   t

   mu4e-org-contacts-file
   (concat org-directory "addresses.org"))

  (when (package-installed-p 'eww)
    (require 'mu4e-contrib)
    (setq mu4e-html2text-command 'mu4e-shr2text))

  (add-to-list 'mu4e-headers-actions
	       '("org-contact-add" . mu4e-action-add-org-contact) t)
  (add-to-list 'mu4e-view-actions
	       '("org-contact-add" . mu4e-action-add-org-contact) t))
