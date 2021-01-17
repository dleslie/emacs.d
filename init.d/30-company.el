;;; 30-company.el --- company

;;; Commentary:

;; Emacs configuration of Dan Leslie.
;; dan@ironoxide.ca

;;; Code:

(use-package company
  :hook
  (prog-mode . company-mode)
  :config
  (setq company-backends
	'(company-capf company-gtags company-etags company-files))
  (setq company-tooltip-align-annotations t
	company-idle-delay 0.25
	company-minimum-prefix-length 1))

(provide '30-company)
;;; 30-company.el ends here
