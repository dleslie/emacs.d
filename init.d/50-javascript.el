;;; 50-javascript.el --- javascript

;;; Commentary:

;; Emacs configuration of Dan Leslie.
;; dan@ironoxide.ca

;;; Code:

(use-package js3-mode
  :after (lsp-mode)
  :init
  (add-hook 'js3-mode-hook #'lsp)
  (add-hook 'typescript-mode-hook #'lsp)
  (add-hook 'css-mode-hook #'lsp))

(use-package typescript-mode
  :after (js2-mode))

(provide '50-javascript)
;;; 50-javascript.el ends here
