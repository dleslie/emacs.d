;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parens
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring Parenthesis")

(require-package 'paredit)
(require-package 'rainbow-delimiters)

(put 'paredit-forward-delete 'delete-selection 'supersede)

(defvar-local paren-modes
  (list 'emacs-lisp-mode-hook
       'eval-expression-minibuffer-setup-hook
       'ielm-mode-hook
       'lisp-mode-hook
       'lisp-interaction-mode-hook
       'scheme-mode-hook
       'slime-repl-mode-hook
       'clojure-mode-hook))

(mapc
 (lambda (mode-hook)
   (add-hook mode-hook #'enable-paredit-mode))
 paren-modes)

(mapc
 (lambda (mode-hook)
   (add-hook mode-hook #'rainbow-delimiters-mode))
 (append paren-modes
	 (list 'c-mode-hook
	       'c++-mode-hook)))

(eval-after-load "eldoc"
  '(eldoc-add-command
    'paredit-backward-delete
    'paredit-close-round))
