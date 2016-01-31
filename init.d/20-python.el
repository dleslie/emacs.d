;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (and (find-exe "pip") (find-exe "python"))
  (message "Configuring Python Mode")

  (require-package 'elpy)
  (elpy-enable)
  
  (with-eval-after-load "company"
    (require-package 'company-jedi)
    (add-to-list 'company-backends 'company-files)
    (add-to-list 'company-backends 'company-jedi)))
