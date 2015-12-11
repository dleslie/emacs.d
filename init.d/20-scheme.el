;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scheme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring Scheme")

(if (file-exists-p my-debug-geiser-path)
    (progn
      (load my-debug-geiser-path)
      (require 'geiser))
  (require-package 'geiser))

(eval-after-load "paredit-mode"
  '(add-hook 'scheme-mode-hook 'paredit-mode))
