;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ruby
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (and (find-exe "irb") (find-exe "gem"))
  (message "Configuring Ruby Mode")

  (require-package 'enh-ruby-mode)
  (require-package 'inf-ruby)
  (require-package 'robe)

  (with-eval-after-load "company"
    (require-package 'company-inf-ruby)
    (defun inf-ruby-company-fix ()
      (make-local-variable 'company-backends)
      (setq company-backends (list 'company-jedi)))
    (add-hook 'inf-ruby-mode-hook 'inf-ruby-company-fix))

  (with-eval-after-load "auto-complete"
    (add-hook 'inf-ruby-mode-hook
	      (lambda ()
		(add-to-list 'ac-sources 'ac-source-robe))))
  
  (defun launch-ruby ()
    (interactive)

    (eval-after-load "projectile"
      '(projectile-rails-on))
    
    (unless (get-buffer "*ruby*")
      (let ((buf (current-buffer)))
	(inf-ruby)
	(robe-start)
	(set-buffer buf))))

  (defun kill-ruby ()
    (interactive)
    (when (get-buffer "*ruby*")
      (kill-buffer "*ruby*")))

  (eval-after-load "projectile"
    '(require-package 'projectile-rails))

  (advice-add 'projectile-rails-console :before #'kill-ruby)

  (add-hook 'ruby-mode-hook 'launch-ruby)
  (add-hook 'enh-ruby-mode-hook 'launch-ruby)

  (add-hook 'ruby-mode-hook 'robe-mode)
  (add-hook 'enh-ruby-mode-hook 'robe-mode))
