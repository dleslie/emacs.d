;;; 20-ido.el --- ido

;;; Commentary:

;; Emacs configuration of Dan Leslie.
;; dan@ironoxide.ca

;;; Code:

(use-package ido
  :init
  (setq
   ido-create-new-buffer 'always
   ido-enable-flex-matching t
   ido-everywhere t))

(provide '20-ido)
;;; 20-ido.el ends here
