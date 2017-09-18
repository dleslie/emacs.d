;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scheme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring Scheme")

(require-package 'geiser)

(with-eval-after-load "company"
  (require-package 'ac-geiser)
    (add-hook 'scheme-mode-hook
	      (lambda ()
		(add-to-list 'ac-sources 'ac-source-geiser))))
