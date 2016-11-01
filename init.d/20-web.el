;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Web
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring Web Mode")

(require-package 'web-mode)
(require-package 'restclient)

(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(with-eval-after-load "company"
  (require-package 'company-web)
  (require-package 'company-restclient)

  (defun web-company-fix ()
    (make-local-variable 'company-backends)
    (setq company-backends 
	  (list 'company-web-html
		'company-web-jade
		'company-web-slim
		'company-restclient)))
  (add-hook 'web-mode-hook 'web-company-fix))
