;;; 30-company.el --- company

;;; Commentary:

;; Emacs configuration of Dan Leslie.
;; dan@ironoxide.ca

;;; Code:

(use-package company
  :init
  (add-hook 'prog-mode-hook 'company-mode)
  (setq company-backends
				'(company-capf company-files company-gtags company-etags))
  (setq company-tooltip-align-annotations t
				company-idle-delay 1
				company-minimum-prefix-length 1))

(use-package company-quickhelp
  :after company
  :init
  (add-hook 'company-mode-hook 'company-quickhelp-mode))

(use-package company-box
	:after company
	:hook (company-mode . company-box-mode))

(provide '30-company)
;;; 30-company.el ends here
