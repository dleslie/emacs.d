 ;;; 99-orderless.el --- orderless

;;; Commentary:

;; Emacs configuration of Dan Leslie.
;; dan@ironoxide.ca

;;; Code:

(use-package orderless
  :ensure t
	:init
	(icomplete-mode)
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))
																	 (eglot (styles orderless)))))

(provide '99-orderless)
;;; 99-orderless.el ends here
