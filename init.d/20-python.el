;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when-find-exe/error "pip3" "Could not locate pip3."
  (message "Configuring Python Mode")

  (require-package 'elpy)
  (add-hook 'python-mode-hook 'elpy-enable)
  
  (with-eval-after-load "company"
    (require-package 'company-jedi)
    
    (defun python-company-fix ()
      (make-local-variable 'company-backends)
      (setq company-backends (list 'company-jedi)))
    (add-hook 'python-mode-hook 'python-company-fix)))
