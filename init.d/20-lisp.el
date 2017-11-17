;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LISP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when-find-exe/error "sbcl" "Could not locate sbcl."
  (message "Configuring LISP")

  (require-package 'slime)
  (require 'slime-autoloads)
  (setq slime-contribs '(slime-fancy))
  (slime-setup)
  
  (defun run-sbcl ()
    (interactive)
    (slime 'sbcl)))
