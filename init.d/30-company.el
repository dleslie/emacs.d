;;; 30-company.el --- company

;;; Commentary:

;; Emacs configuration of Dan Leslie.
;; dan@ironoxide.ca

;;; Code:

(use-package company
  :init
  (add-hook 'prog-mode-hook 'company-mode)
  (setq company-backends
	'(company-capf company-gtags company-etags company-files))
  (setq company-tooltip-align-annotations t
	company-idle-delay 0.25
	company-minimum-prefix-length 1))

(use-package company-quickhelp
  :after company
  :init
  (add-hook 'company-mode-hook 'company-quickhelp-mode))

(provide '30-company)
;;; 30-company.el ends here
