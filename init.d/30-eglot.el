;;; 30-lsp.el --- lsp

;;; Commentary:

;; Emacs configuration of Dan Leslie.
;; dan@ironoxide.ca

;;; Code:

(use-package eglot
  :defer t
  :init
  (setq eglot-connect-timeout 240))

(provide '30-lsp)
;;; 30-lsp.el ends here
