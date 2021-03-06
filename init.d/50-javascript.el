;;; 50-javascript.el --- javascript

;;; Commentary:

;; Emacs configuration of Dan Leslie.
;; dan@ironoxide.ca

;;; Code:

(use-package js2-mode
  :after (lsp-mode)
  :init
  (add-hook 'js2-mode-hook #'lsp))

(use-package css-mode
  :after js2-mode
  :init
  (add-hook 'css-mode-hook #'lsp))

(use-package typescript-mode
  :after (js2-mode)
  :init  
  (add-hook 'typescript-mode-hook #'lsp))

(use-package tern
  :after js2-mode
  :init
  (add-hook 'js-mode-hook #'tern-mode))

(use-package json-mode)

(provide '50-javascript)
;;; 50-javascript.el ends here
