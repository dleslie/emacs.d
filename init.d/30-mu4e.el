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

  (add-to-list 'mu4e-headers-actions
	       '("org-contact-add" . mu4e-action-add-org-contact) t)
  (add-to-list 'mu4e-view-actions
	       '("org-contact-add" . mu4e-action-add-org-contact) t))
