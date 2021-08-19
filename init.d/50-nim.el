;;; 50-nim.el --- nim

;;; Commentary:

;; Emacs configuration of Dan Leslie.
;; dan@ironoxide.ca

;;; Code:

(use-package nim-mode
  :ensure t
  :hook
  (nim-mode . lsp))

(provide '50-nim)
;;; 50-nim.el ends here
