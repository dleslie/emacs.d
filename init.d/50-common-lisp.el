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

  (add-hook 'sly-mode-hook 'company-mode)

  (setq sly-lisp-implementations
	(cl-remove-if-not (lambda (imp) (caadr imp))
		       `((sbcl (,(executable-find "sbcl")) :coding-system utf-8-unix)
			 (armcl (,(executable-find "armcl")))
			 (cmucl (,(executable-find "cmucl") "-quiet"))
			 (ecl (,(executable-find "ecl")))
			 ))))

(provide '50-common-lisp)
;;; 50-common-lisp.el ends here
