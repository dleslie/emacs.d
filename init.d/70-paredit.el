;;; 70-paredit.el --- ace jump

;;; Commentary:

;; Emacs configuration of Dan Leslie.
;; dan@ironoxide.ca

;;; Code:

(use-package paredit
  :bind
  (:map paredit-mode-map
        ("{" . paredit-open-curly)
        ("}" . paredit-close-curly))
  :init
  (mapc
   (lambda (mode-hook)
     (add-hook mode-hook #'paredit-mode))
   '(emacs-lisp-mode-hook
     eval-expression-minibuffer-setup-hook
     ielm-mode-hook
     lisp-mode-hook
     lisp-interaction-mode-hook
     scheme-mode-hook
     sly-mode-hook
     clojure-mode-hook)))

(provide '70-paredit)
;;; 70-paredit.el ends here
