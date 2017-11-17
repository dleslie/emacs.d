;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ruby
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when-find-exe/error "gem" "Could not locate gem."
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
    (defun ac-asource-robe-enable-hook ()
      (make-local-variable 'ac-sources)
      (add-to-list 'ac-sources 'ac-source-robe))
    (add-hook 'inf-ruby-mode-hook 'ac-source-robe-enable-hook))
  
  (defun launch-ruby ()
    (interactive)

    (with-eval-after-load "projectile"
      (projectile-rails-on))
    
    (unless (get-buffer "*ruby*")
      (let ((buf (current-buffer)))
	(inf-ruby)
	(robe-start)
	(set-buffer buf))))

  (defun kill-ruby ()
    (interactive)
    (when (get-buffer "*ruby*")
      (kill-buffer "*ruby*")))

  (with-eval-after-load "projectile"
    (require-package 'projectile-rails))

  (advice-add 'projectile-rails-console :before #'kill-ruby)

  (add-hook 'ruby-mode-hook 'launch-ruby)
  (add-hook 'enh-ruby-mode-hook 'launch-ruby)

  (add-hook 'ruby-mode-hook 'robe-mode)
  (add-hook 'enh-ruby-mode-hook 'robe-mode))
