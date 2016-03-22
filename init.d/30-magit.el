;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Magit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Magit on windows is ludicrously slow
(when (not (string= system-type "windows-nt"))
  (message "Configuring Magit")

  (require-package 'magit)

  (setq magit-last-seen-setup-instructions "1.4.0"))
