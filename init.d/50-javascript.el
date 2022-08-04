;;; 50-javascript.el --- javascript

;;; Commentary:

;; Emacs configuration of Dan Leslie.
;; dan@ironoxide.ca

;;; Code:

(use-package js2-mode)

(use-package css-mode)

(use-package typescript-mode)

(use-package tern
  :init
  (add-hook 'js-mode-hook #'tern-mode))

(use-package json-mode)

(provide '50-javascript)
;;; 50-javascript.el ends here
