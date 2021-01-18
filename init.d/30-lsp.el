;;; 30-lsp.el --- lsp

;;; Commentary:

;; Emacs configuration of Dan Leslie.
;; dan@ironoxide.ca

;;; Code:

(use-package lsp-mode
  :commands lsp
  :hook
  ((rust-mode . lsp)
   (ruby-mode . lsp)
   (python-mode . lsp)
   (js3-mode . lsp)
   (c-mode . lsp)
   (c++-mode . lsp)))

(use-package lsp-ui)

(provide '30-lsp)
;;; 30-lsp.el ends here
