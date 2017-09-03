;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C/C++
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring C and C++")

(add-hook 'c++-mode-hook (lambda () (eldoc-mode 1)))
(add-hook 'c-mode-hook (lambda () (eldoc-mode 1)))

(with-eval-after-load 'flycheck
  (add-hook 'c++-mode-hook
	    (lambda ()
	      (make-local-variable 'flycheck-gcc-language-standard)
	      (setq flycheck-gcc-language-standard "c++14")))
  (add-hook 'c-mode-hook
            (lambda ()
	      (make-local-variable 'flycheck-gcc-language-standard)
	      (setq flycheck-gcc-language-standard "c14"))))
