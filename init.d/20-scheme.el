;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scheme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring Scheme")

(if (file-exists-p my-debug-geiser-path)
    (progn
      (load my-debug-geiser-path)
      (require 'geiser))
  (require-package 'geiser)))
