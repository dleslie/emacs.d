;;; 50-common-lisp.el --- common lisp

;;; Commentary:

;; Emacs configuration of Dan Leslie.
;; dan@ironoxide.ca

;;; Code:

(use-package sly
  :after (company)
  :hook ((sly-mode . company-mode))
  :init
  (setq sly-lisp-implementations
	(cl-remove-if-not (lambda (imp) (caadr imp))
		       `((sbcl (,(executable-find "sbcl")) :coding-system utf-8-unix)
			 (armcl (,(executable-find "armcl")))
			 (cmucl (,(executable-find "cmucl") "-quiet"))
			 (ecl (,(executable-find "ecl")))
			 ))))
(use-package sly-asdf
  :after (sly)
  :init
  (add-to-list 'sly-contribs 'sly-asdf 'append))
(use-package sly-quicklisp
  :after (sly))
(use-package sly-macrostep
  :after (sly))
(use-package sly-repl-ansi-color
  :after (sly))

(provide '50-common-lisp)
;;; 50-common-lisp.el ends here
