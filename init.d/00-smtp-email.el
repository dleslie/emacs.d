;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SMTP Email
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq
 message-send-mail-function 
 'smtpmail-send-it
 smtpmail-stream-type
 'starttls
 smtpmail-starttls-credentials
 '((mail-smtp-server mail-smtp-port nil nil))
 smtpmail-auth-credentials
 '((mail-smtp-server mail-smtp-port
		     user-mail-login nil))
 smtpmail-default-smtp-server
 mail-smtp-server
 smtpmail-smtp-server
 mail-smtp-server
 smtpmail-smtp-service
 mail-smtp-port
 gnus-ignored-newsgroups
 "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")
