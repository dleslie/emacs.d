;;; 30-flycheck.el --- flycheck

;;; Commentary:

;; Emacs configuration of Dan Leslie.
;; dan@ironoxide.ca

;;; Code:

(use-package flycheck
  :init
  (add-hook 'prog-mode-hook 'flycheck-mode))

(provide '30-flycheck)
;;; 30-flycheck.el ends here
