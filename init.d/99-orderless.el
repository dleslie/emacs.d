 ;;; 99-orderless.el --- orderless

;;; Commentary:

;; Emacs configuration of Dan Leslie.
;; dan@ironoxide.ca

;;; Code:

(use-package orderless
  :ensure t
  :init
  (icomplete-mode)
  (setq completion-styles '(orderless orderless-flex))
  :config
  (defun just-one-face (fn &rest args)
    (let ((orderless-match-faces [completions-common-part]))
      (apply fn args)))
  
  (advice-add 'company-capf--candidates :around #'just-one-face))

(provide '99-orderless)
;;; 99-orderless.el ends here
