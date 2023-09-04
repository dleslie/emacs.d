;;; 50-common-lisp.el --- common lisp

;;; Commentary:

;; Emacs configuration of Dan Leslie.
;; dan@ironoxide.ca

;;; Code:

(use-package sly
  :init
  (use-package sly-quicklisp)
  (use-package sly-macrostep)
  (use-package sly-repl-ansi-color)
  (require 'sly-autoloads)
  (add-hook 'sly-mode-hook 'enable-paredit-mode)
  (advice-add 
   'paredit-RET
   :after
   (lambda ()
     (when (string-prefix-p "*sly-mrepl for"
                            (buffer-name (current-buffer)))
       (sly-mrepl-return)))))

(provide '50-common-lisp)
;;; 50-common-lisp.el ends here
