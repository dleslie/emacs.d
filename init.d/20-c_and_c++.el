;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C/C++
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring C and C++")

(add-hook 'c++-mode-hook (lambda () (eldoc-mode 1)))
(add-hook 'c-mode-hook (lambda () (eldoc-mode 1)))

(with-eval-after-load "company"
  (require-package 'company-c-headers)
  (add-to-list 'company-backends 'company-c-headers))

(with-eval-after-load "auto-complete"
  (require-package 'ac-c-headers)
  (add-hook 'c-mode-hook
            (lambda ()
              (add-to-list 'ac-sources 'ac-source-c-headers)
              (add-to-list 'ac-sources 'ac-source-c-header-symbols t))))

(with-eval-after-load 'flycheck
  (add-hook 'c++-mode-hook
	    (lambda ()
	      (make-local-variable 'flycheck-gcc-language-standard)
	      (setq flycheck-gcc-language-standard "c++14")))
  (add-hook 'c-mode-hook
            (lambda ()
	      (make-local-variable 'flycheck-gcc-language-standard)
	      (setq flycheck-gcc-language-standard "c14"))))
