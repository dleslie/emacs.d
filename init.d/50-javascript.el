;;; 50-javascript.el --- javascript

;;; Commentary:

;; Emacs configuration of Dan Leslie.
;; dan@ironoxide.ca

;;; Code:

(use-package js2-mode
  :defer t)

(use-package css-mode
  :defer t)

(use-package typescript-mode
  :defer t)

(use-package tern
  :defer t
  :init
  (add-hook 'js-mode-hook #'tern-mode))

(use-package json-mode
  :defer t)

(provide '50-javascript)
;;; 50-javascript.el ends here
