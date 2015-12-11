;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Javascript
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring Javascript")

(require-package 'js2-mode)
(require-package 'tern)

(defun js2-mode-custom-hook ()
  (tern-mode t))

(add-hook 'js2-mode-hook 'js2-mode-custom-hook)

(eval-after-load "company"
  '(require-package 'company-tern))
 
(add-to-list 'auto-mode-alist '("\\.js?\\'" . js2-mode))

