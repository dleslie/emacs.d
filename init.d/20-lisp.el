;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LISP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring LISP")

(require-package 'slime)

(setq slime-lisp-implementations
      '((sbcl ("/usr/bin/sbcl" "--core" "/usr/lib/sbcl/sbcl.core")
	      :coding-system utf-8-unix
	      :env ("SBCL_HOME=/usr/lib/sbcl"))))

(require 'slime-autoloads)
(slime-setup)

(defun sbcl-slime ()
  (interactive)
  (slime 'sbcl))

(defun my-lisp-mode-hook ()
  (slime-mode t))

(add-hook 'lisp-mode-hook 'my-lisp-mode-hook)

(eval-after-load "paredit"
  '(add-hook 'lisp-mode-hook 'paredit-mode))
