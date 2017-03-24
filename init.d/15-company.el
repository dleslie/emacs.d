;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Company
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring Company")

(require-package 'company)
(global-company-mode)

(add-to-list 'company-backends 'company-elisp)

(defun my-company-ispell-hook ()
  (make-local-variable 'company-backends)
  (add-to-list 'company-backends 'company-ispell))

(add-hook 'text-mode-hook 'my-company-ispell-hook)
(add-hook 'org-mode-hook 'my-company-ispell-hook)
(add-hook 'writegood-mode-hook 'my-company-ispell-hook)
