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
  (require 'sly-autoloads))

(provide '50-common-lisp)
;;; 50-common-lisp.el ends here
