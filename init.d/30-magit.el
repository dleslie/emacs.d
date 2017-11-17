;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Magit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when-find-exe/error "git" "Could not locate git."
  (message "Configuring Magit")

  (require-package 'magit)

  (setq magit-last-seen-setup-instructions "1.4.0"))
