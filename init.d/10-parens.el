;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parens
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring Parenthesis")

(require-package 'paredit)

(add-hook 'emacs-lisp-mode-hook 'paredit-mode)

(put 'paredit-forward-delete 'delete-selection 'supersede)

(add-hook 'prog-mode-hook 'show-paren-mode)
