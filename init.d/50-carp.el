;;; 50-carp.el --- carp

;;; Commentary:

;; Emacs configuration of Dan Leslie.
;; dan@ironoxide.ca

;;; Code:

(use-package carp-mode
  :after clojure-mode
  :straight
  '(carp :type git :host github :repo "carp-lang/carp-emacs")
  :init
  (require 'carp-mode)
  (require 'inf-carp-mode)
  (require 'carp-flycheck)
  (add-hook 'carp-mode-hook (lambda () (flycheck-mode 1)))
  (add-to-list 'auto-mode-alist '("\\.carp\\'" . carp-mode)))

(provide '50-carp)
;;; 50-carp.el ends here
