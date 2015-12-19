;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LISP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (find-exe "sbcl")
  (message "Configuring LISP")

  (require-package 'slime)
  (require 'slime-autoloads)
  (setq slime-contribs '(slime-fancy))
  
  (defun run-sbcl ()
    (interactive)
    (slime 'sbcl))

  (eval-after-load "paredit"
    '(add-hook 'lisp-mode-hook 'paredit-mode)))
