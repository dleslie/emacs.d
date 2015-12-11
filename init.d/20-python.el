;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring Python Mode")

(require-package 'jedi)
(require-package 'python-environment)

(jedi:install-server)

(eval-after-load "company"
  '(progn
     (require-package 'company-jedi)
     (add-to-list 'company-backends 'company-jedi)))

(defun python-custom-hook ()
  (jedi:setup))

(add-hook 'python-mode-hook 'python-custom-hook)
