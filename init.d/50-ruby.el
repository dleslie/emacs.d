;;; 50-ruby.el --- ruby

;;; Commentary:

;; Emacs configuration of Dan Leslie.
;; dan@ironoxide.ca

;;; Code:

(use-package ruby-mode
  :init
  (autoload 'ruby-mode "ruby-mode" "Ruby Mode" t ".rb")
  (defun my-ruby-mode-hook ()
    (require 'inf-ruby)
    (inf-ruby-keys))
  (defun launch-ruby ()
    (interactive)
    (unless (get-buffer "*ruby*")
      (let ((buf (current-buffer)))
	(inf-ruby)
	(set-buffer buf))))
  (defun kill-ruby ()
    (interactive)
    (when (get-buffer "*ruby*")
      (kill-buffer "*ruby*")))
  (add-hook 'ruby-mode-hook 'my-ruby-mode-hook))

(use-package inf-ruby)

(use-package enh-ruby-mode)

(use-package projectile-rails
  :init
  (advice-add 'projectile-rails-console :before #'kill-ruby)
  (advice-add 'launch-ruby :after #'projectile-rails-on)
  (advice-add 'kill-ruby :after #'projectile-rails-off))

(use-package robe
  :init
  (advice-add 'launch-ruby :after #'robe-start))


(provide '50-ruby)
;;; 50-ruby.el ends here
