;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scheme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring Scheme")

(if (file-exists-p my-debug-geiser-path)
    (progn
      (load my-debug-geiser-path)
      (require 'geiser))
  (require-package 'geiser))

(with-eval-after-load "paredit"
  (add-hook 'scheme-mode-hook 'paredit-mode))
