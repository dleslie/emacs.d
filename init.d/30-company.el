;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Company
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring Company")

(require-package 'company)

(setq company-backends
      '(company-ispell company-elisp))

(defun my-anti-ispell-prog-hook ()
  (make-local-variable 'company-backends)
  (setq company-backends (remove 'company-ispell company-backends)))

(add-hook 'prog-mode-hook 'my-anti-ispell-prog-hook)
(add-hook 'eshell-mode-hook 'my-anti-ispell-prog-hook)

(global-company-mode)
