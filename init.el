;;; init.el --- Startup Script

;;; Commentary:

;; Emacs configuration of Dan Leslie.
;; dan@ironoxide.ca

;;; Code:

;; Loads of legacy packages require 'cl
(require 'cl-lib)

;; Disable GC during load
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

;; Windows performance tweaks
(when (boundp 'w32-pipe-read-delay)
  (setq w32-pipe-read-delay 0))

;; Fix TLS on Windows
(when  
    (not (or (string-match-p "Microsoft" (shell-command-to-string "uname -a"))
	     (eq 'windows-nt system-type)))
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

;; Make custom file not this one
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Disable file handler search during load
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Where are we finding things?
(require 'package)
(setq package-archives
      '(("org" . "https://orgmode.org/elpa/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("elpy" . "https://jorgenschaefer.github.io/packages/"))
      package-archive-priorities
      '(("elpy" . 70)
	("melpa" . 80)
	("org" . 90)
	("gnu" . 100)))

;;(package-initialize 'no-activate)
(package-initialize)

;; Load all files in init.d
(let ((initd (concat user-emacs-directory (file-name-as-directory "init.d"))))
  (when (file-exists-p initd)
    (dolist (lsp (directory-files initd nil "\\.el$"))
      (load-file lsp))))

;; Load local configurations
(let ((localel (concat user-emacs-directory "local.el")))
  (load localel))

;; Load custom.el
(when (file-exists-p custom-file)
  (load custom-file))

;; Enable GC
(setq gc-cons-threshold 16777216
      gc-cons-percentage 0.1)

;; Enable file handler
(setq file-name-handler-alist default-file-name-handler-alist)
(garbage-collect)

(message "Emacs ready in %s with %d garbage collections."
         (format "%.2f seconds"
                 (float-time
                  (time-subtract after-init-time before-init-time)))
         gcs-done)

(provide 'init)
;;; init.el ends here


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package toml)
(use-package csharp-mode)
;(use-package restclient)
(use-package meson-mode)

(use-package gdb-mi
  :after hydra
  :straight (:host github :repo "weirdNox/emacs-gdb" :files ("*.el" "*.c" "*.h" "Makefile"))
  :init
  (fmakunbound 'gdb)
  (fmakunbound 'gdb-enable-debug))

(use-package dap-mode
  :hook
  ((web-mode . dap-mode)
   (python-mode . dap-mode)
   (ruby-mode . dap-mode)
   (c-mode . dap-mode)
   (js3-mode . dap-mode))
  :config
  (dap-ui-mode 1)
  ;; enables mouse hover support
  (dap-tooltip-mode 1)
  ;; use tooltips for mouse hover
  ;; if it is not enabled `dap-mode' will use the minibuffer.
  (tooltip-mode 1)
  (require 'dap-ruby)
  (require 'dap-gdb-lldb)
  (require 'dap-node)
  (require 'dap-python)
  (require 'dap-firefox))

(when (executable-find "ccls")
  (use-package ccls
    :after lsp-mode
    :config
    (add-hook 'c-mode-hook 'lsp)
    (add-hook 'c++-mode-hook 'lsp)
    (setq ccls-executable (executable-find "ccls"))))

(use-package rust-mode
  :after lsp-mode
  :config
  (setq rust-format-on-save t
	lsp-rust-all-features t
	lsp-rust-build-on-save nil)
  (when (executable-find "racer")
    (setq lsp-rust-racer-completion t))
  (when (executable-find "ra_lsp_server")
    (setq
     lsp-rust-racer-completion nil
     lsp-rust-server 'rust-analyzer))
  (defun my-ra-hack ()
    (let ((clients (lsp--filter-clients
		    (lambda (client)
		      (equalp 'rust-analyzer (lsp--client-server-id client))))))
      (when clients
	(mapcar (lambda (client)
		  (setf (lsp--client-priority client) 2))
		clients))))
  :hook
  ((rust-mode . my-ra-hack)))

(use-package flycheck-rust
  :after rust-mode
  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package js3-mode
  :after (lsp-mode)
  :init
  (add-hook 'js3-mode-hook #'lsp)
  (add-hook 'typescript-mode-hook #'lsp)
  (add-hook 'css-mode-hook #'lsp))

(use-package typescript-mode
  :after (js2-mode))

(use-package omnisharp
  :after (csharp-mode company flycheck)
  :bind
  (:map csharp-mode-map
        ("M-." . omnisharp-go-to-definition)
        ("C-c C-c" . recompile))
  :init
  (add-hook 'csharp-mode-hook 'omnisharp-mode)
  (add-hook 'csharp-mode-hook 'flycheck-mode)
  (push 'company-omnisharp company-backends))

(use-package markdown-mode
  :mode ("\\.markdown\\'" "\\.md\\'")
  :init
  (defun my-custom-markdown-mode ()
    (interactive)
    (visual-line-mode 1))
  (advice-add 'markdown-mode :after #'my-custom-markdown-mode))

(use-package ruby-mode
  :mode "\\.rb\\'"
  :interpreter "ruby"
  :functions (inf-ruby-keys launch-ruby kill-ruby)
  :init
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

(use-package inf-ruby
  :after ruby-mode)

(use-package enh-ruby-mode
  :after ruby-mode)

(use-package projectile-rails
  :functions (projectile-rails-on projectile-rails-off)
  :after (projectile ruby-mode)
  :init
  (advice-add 'projectile-rails-console :before #'kill-ruby)
  (advice-add 'launch-ruby :after #'projectile-rails-on)
  (advice-add 'kill-ruby :after #'projectile-rails-off))

(use-package robe
  :after ruby-mode
  :functions (robe-start)
  :init
  (advice-add 'launch-ruby :after #'robe-start))

(use-package company-inf-ruby
  :after (company ruby-mode)
  :init
  (push 'company-inf-ruby company-backends))

(use-package company-restclient
  :after (restclient company)
  :init
  (push 'company-restclient company-backends))

(use-package web-mode
  :mode ("\\.phtml\\'" "\\.tpl\\.php\\'" "\\.[agj]sp\\'" "\\.as[cp]x\\'" "\\.erb\\'" "\\.mustache\\'" "\\.djhtml\\'" "\\.html?\\'" "\\.tsx\\'" "\\.jsx\\'"))

(use-package company-web
  :after (company web-mode)
  :init
  (push 'company-web-html company-backends)
  (push 'company-web-jade company-backends)
  (push 'company-web-slim company-backends))

(use-package olivetti
  :init
  (define-key text-mode-map [menu-bar text olivetti-mode]
    '(menu-item "Olivetti" olivetti-mode
                :button (:toggle . (and (boundp 'olivetti-mode) olivetti-mode)))))

(use-package writeroom-mode
  :init
  (define-key text-mode-map [menu-bar text writeroom-mode]
    '(menu-item "Writeroom" writeroom-mode
                :button (:toggle . (and (boundp 'writeroom-mode) writeroom-mode)))))

(use-package writegood-mode
  :init
  (define-key text-mode-map [menu-bar text writeroom-mode]
    '(menu-item "Writegood" writegood-mode
                :button (:toggle . (and (boundp 'writegood-mode) writegood-mode))))
  :init
  (add-hook 'text-mode-hook 'writegood-mode))

(use-package ido
  :init
  (setq
   ido-create-new-buffer 'always
   ido-enable-flex-matching t
   ido-everywhere t))

(use-package nyan-mode
  :functions (nyan-mode)
  :init
  (nyan-mode t))

(when (executable-find "ag")
  (use-package ag
    :init
    (define-key-after global-map [menu-bar tools ag]
      '(menu-item "Search Files (ag)..." ag :help "Search files for strings or regexps (with ag)...")
      'grep)))

(use-package dumb-jump
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package dictionary
  :bind (("C-c d" . dictionary-search))
  :init
  (define-key-after global-map [menu-bar tools apps dictionary-search]
    '(menu-item "Dictionary" dictionary-search :help "Search dictionary") t))

(use-package powerthesaurus
  :bind (("C-c t" . powerthesaurus-lookup-word-dwim)))

(use-package quiz
  :bind (("C-c q" . quiz))
  :init
  (define-key global-map [menu-bar tools games quiz]
    '(menu-item "Quiz" quiz :help "Be quizzed")))

(use-package clang-format
  :init
  (defun my-clang-format-on-save ()
    (make-local-variable 'before-save-hook)
    (add-hook 'before-save-hook 'clang-format-buffer))
  (add-hook 'c-mode-hook 'my-clang-format-on-save))


(provide 'init)
;;; init.el ends here
