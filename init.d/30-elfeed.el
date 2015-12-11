;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Elfeed
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring Elfeed")

(require-package 'elfeed)

(defun update-elfeed ()
  (interactive)
  (message "Updating RSS")
  (elfeed-update))
 
(run-with-timer 3600 3600 'update-elfeed)
