;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Nyan Cat
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (not (string= system-type "windows-nt"))
  (message "Configuring Nyan Cat")

  (require-package 'nyan-mode)

  (nyan-mode t))
