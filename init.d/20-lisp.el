;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LISP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (find-exe "sbcl")
  (message "Configuring LISP")

  (require-package 'slime)

  (require 'slime-autoloads)
  (slime-setup)

  (defun sbcl-slime ()
    (interactive)
    (slime 'sbcl))

  (defun my-lisp-mode-hook ()
    (slime-mode t))

  (add-hook 'lisp-mode-hook 'my-lisp-mode-hook)

  (eval-after-load "paredit"
    '(add-hook 'lisp-mode-hook 'paredit-mode)))
