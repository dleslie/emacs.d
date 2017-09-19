;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scheme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring Scheme")

(require-package 'geiser)

(with-eval-after-load "auto-complete"
  (require-package 'ac-geiser)
  (defun ac-source-geiser-enable-hook ()
    (make-local-variable 'ac-sources)
    (add-to-list 'ac-sources 'ac-source-geiser))
  (add-hook 'scheme-mode-hook 'ac-source-geiser-enable-hook))
