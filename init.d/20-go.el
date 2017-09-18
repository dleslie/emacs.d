;;;;;;;;;;;;;;;;;;;;
;; Go
;;;;;;;;;;;;;;;;;;;;

(when (find-exe "go")
  (message "Configuring Go")

  (require-package 'go-mode)

  (when (find-exe "gocode")
    (require-package 'go-eldoc)
    (add-hook 'go-mode-hook 'go-eldoc-setup)
    
    (with-eval-after-load "company"
      (require-package 'company-go)
      (add-hook 'go-mode-hook #'(lambda () (make-local-variable 'company-backends) (setq company-backends (list 'company-go)))))))
