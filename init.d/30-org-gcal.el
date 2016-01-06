;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-gcal
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring org-gcal")

(with-eval-after-load "org"
  (when (file-exists-p (concat user-emacs-directory "gcal-settings.el"))
    (require-package 'org-gcal)

    (load (expand-file-name (concat user-emacs-directory "gcal-settings.el")))
    
    (defun update-gcal ()
      (interactive)
      (message "Updating Calendar")
      (org-gcal-fetch))))
