 ;;; 99-orderless.el --- orderless

;;; Commentary:

;; Emacs configuration of Dan Leslie.
;; dan@ironoxide.ca

;;; Code:

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (setq
	 completion-category-overrides '((file (styles basic partial-completion))
																	 (eglot (styles orderless)))))

(provide '99-orderless)
;;; 99-orderless.el ends here
