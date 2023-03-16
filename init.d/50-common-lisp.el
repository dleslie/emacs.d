;;; 50-common-lisp.el --- common lisp

;;; Commentary:

;; Emacs configuration of Dan Leslie.
;; dan@ironoxide.ca

;;; Code:

(use-package sly
  :init
  (use-package sly-asdf
    :init
    (add-to-list 'sly-contribs 'sly-asdf 'append))
  (use-package sly-quicklisp)
  (use-package sly-macrostep)
  (use-package sly-repl-ansi-color)
  (require 'sly-autoloads))

(let ((ros-file (expand-file-name "~/.roswell/helper.el")))
  (when (file-exists-p ros-file)
    (load ros-file)
    (setq inferior-lisp-program "ros -Q run")))

(provide '50-common-lisp)
;;; 50-common-lisp.el ends here
