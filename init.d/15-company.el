;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Company
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring Company")

(require-package 'company)
(global-company-mode)

(add-to-list 'company-backends 'company-ispell)
(add-to-list 'company-backends 'company-elisp)

(defun my-anti-ispell-prog-hook ()
  (make-local-variable 'company-backends)
  (setq company-backends (remove 'company-ispell company-backends)))

(add-hook 'prog-mode-hook 'my-anti-ispell-prog-hook)
(add-hook 'eshell-mode-hook 'my-anti-ispell-prog-hook)
