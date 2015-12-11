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

(eval-after-load "company"
  '(progn
     (require-package 'company-web)
     (require-package 'company-restclient)
     
     (add-to-list 'company-backends 'company-web-html)
     (add-to-list 'company-backends 'company-web-jade)
     (add-to-list 'company-backends 'company-web-slim)
     (add-to-list 'company-backends 'company-restclient)))
