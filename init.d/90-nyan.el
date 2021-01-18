 ;;; 90-nyan.el --- nyan

;;; Commentary:

;; Emacs configuration of Dan Leslie.
;; dan@ironoxide.ca

;;; Code:

(use-package nyan-mode
  :functions (nyan-mode)
  :init
  (setq
   nyan-animate-nyancat t
   nyan-wavy-trail t)
  (nyan-mode t))

(provide '90-nyan)
;;; 90-nyan.el ends here
