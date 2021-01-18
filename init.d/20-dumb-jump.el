;;; 20-dumb-jump.el --- dumb-jump

;;; Commentary:

;; Emacs configuration of Dan Leslie.
;; dan@ironoxide.ca

;;; Code:

(use-package dumb-jump
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(provide '20-dumb-jump)
;;; 20-dumb-jump.el ends here
