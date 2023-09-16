;;; 40-languages.el

;;; Commentary:

;; Emacs configuration of Dan Leslie.
;; dan@ironoxide.ca

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Eglot
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package eglot
  :init
  (setq eglot-connect-timeout 240))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package csharp-mode
  :ensure t
  :init
  (let* ((dotnet (executable-find "dotnet"))
	 (dotnet-script (executable-find "dotnet-script")))
    (when (and dotnet (not dotnet-script))
      (shell-command (concat "\"" dotnet "\" tool install -g dotnet-script"))))
  
  (defun my-csharp-repl ()
    "Switch to the CSharpRepl buffer, creating it if necessary."
    (interactive)
    (let ((repl (or (executable-find "dotnet-script") (executable-find "csharp"))))
      (when repl
	(if-let ((buf (get-buffer "*CSharpRepl*")))
            (pop-to-buffer buf)
	  (when-let ((b (make-comint "CSharpRepl" repl)))
            (switch-to-buffer-other-window b))))))
  (defun my/csharp-mode-hook ()
    (setq-local indent-tabs-mode nil)
    (setq-local comment-column 40)
    (setq-local c-basic-offset 4))
  (add-hook 'csharp-mode-hook #'my/csharp-mode-hook)
  :config
  (define-key csharp-mode-map (kbd "C-c C-z") 'my-csharp-repl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Janet
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package janet-mode)
(straight-use-package
 '(inf-janet
   :type git
   :host github
   :repo "velkyel/inf-janet"))
(add-hook 'janet-mode-hook #'inf-janet-minor-mode)
(add-hook 'janet-mode-hook #'paredit-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ruby
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rust
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package rust-mode)

(use-package flycheck-rust
  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scheme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package geiser
  :config
  (setq geiser-chicken-binary (or (executable-find "chicken-csi") (executable-find "csi"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package clojure-mode)
(use-package css-mode)
(use-package js2-mode)
(use-package json-mode)
(use-package nim-mode)
(use-package sly)
(use-package typescript-mode)
(use-package flatbuffers-mode)
(use-package meson-mode)

(add-to-list 'auto-mode-alist '("\\.ino?\\'" . c++-mode))

;;; 40-languages.el ends here
