;;; init.el --- Startup Script

;;; Commentary:

;; Emacs configuration of Dan Leslie.
;; dan@ironoxide.ca

;;; Code:

;; Disable GC during load
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

;; Disable file handler search during load
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Emacs default scrolling behaviour is the worst
(setq
 scroll-step 2
 scroll-conservatively 10000
 auto-window-vscroll nil)

;; General Emacs Sanity
(setq indent-tabs-mode nil
      make-backup-files nil
      debug-on-error nil)
(delete-selection-mode 1)

;; Windows performance tweaks
(when (boundp 'w32-pipe-read-delay)
  (setq w32-pipe-read-delay 0))

;; Don't be a dick
(setq
 indent-tabs-mode nil
 truncate-lines t
 tab-width 2)

;; Some pretty visual indicators
(show-paren-mode t)
(global-eldoc-mode t)
(global-hl-line-mode t)

;; Useful bindings
(global-set-key "\C-w" 'clipboard-kill-region)
(global-set-key "\M-w" 'clipboard-kill-ring-save)
(global-set-key "\C-y" 'clipboard-yank)
(global-set-key "\C-c," 'scroll-bar-mode)
(global-set-key "\C-c." 'tool-bar-mode)
(global-set-key "\C-c?" 'menu-bar-mode)
(global-set-key "\C-c\\" 'comment-or-uncomment-region)
(global-set-key "\C-cs" 'eshell-here)
(global-set-key [f12] 'toggle-frame-fullscreen)

;; Where are we finding things?
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")
        ("elpy" . "https://jorgenschaefer.github.io/packages/")))

;; Sane defaults for company
(setq company-tooltip-align-annotations t
      company-idle-delay 0.25
      company-backends (remove 'company-semantic company-backends))

;; Disable magit instructions
(setq magit-last-seen-setup-instructions "1.4.0")

;; Sane defaults for projectile
(setq
 projectile-switch-project-action 'projectile-find-dir
 projectile-find-dir-includes-top-level t)

;; Package updator configuration
(setq auto-package-update-delete-old-versions t
      auto-package-update-hide-results t)

;; eldoc in C/C++
(add-hook 'c++-mode-hook (lambda () (eldoc-mode 1)))
(add-hook 'c-mode-hook (lambda () (eldoc-mode 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custom functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun reset-theme ()
  "Disable all active themes."
  (interactive)
  (while custom-enabled-themes
    (disable-theme (car custom-enabled-themes))))

(defun override-theme (theme)
  "Disable all enabled themes and then load the provided theme THEME."
  (interactive
   (list
    (intern (completing-read "Load custom theme: "
                             (mapcar 'symbol-name (custom-available-themes))))))
  (reset-theme)
  (load-theme theme t))

(defun eshell-here ()
  "Opens up a new shell in the directory associated with the current buffer's file.  The eshell is renamed to match that directory to make multiple eshell windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (height (/ (window-total-height) 3))
         (name   (car (last (split-string parent "/" t)))))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))
    (insert (concat "ls"))
    (eshell-send-input)))

(defun dir-locals-dir ()
  "Return the directory local variables directory.
Code taken from `hack-dir-local-variables'."
  (let ((variables-file (dir-locals-find-file (or (buffer-file-name) default-directory)))
        (dir-name nil))
    (cond
     ((stringp variables-file)
      (setq dir-name (file-name-directory variables-file)))
     ((consp variables-file)
      (setq dir-name (nth 0 variables-file))))
    dir-name))

(defmacro with-perf-metrics (name &rest body)
  "Show time of scope NAME taken to execute BODY."
  (declare (indent 1) (debug t))
  `(progn
     (let ((begin-time (float-time))
           (begin-cells cons-cells-consed))
       (message (format "[%s]" ,name))
       (with-demoted-errors "Error: %S"
         ,(cons 'progn body))
       (message (format "[%s : %sms, %s cells]" ,name (truncate (* 1000 (- (float-time) begin-time))) (- cons-cells-consed begin-cells))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fix compilation colours
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'ansi-color)
  
(defun my-compilation-custom-hook ()
  (visual-line-mode 1))
(defun my-colorize-compilation-buffer ()
  (read-only-mode nil)
  (ansi-color-apply-on-region (point-min) (point-max))
  (read-only-mode t))

(add-hook 'compilation-mode-hook 'my-compilation-custom-hook)
(add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; use-package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(dolist (dir (directory-files  (expand-file-name "lisp" user-emacs-directory) t "[^\\.]"))
  (add-to-list 'load-path dir))

(require 'use-package)
(require 'bind-key)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package auto-package-update
  :ensure t
  :config
  (auto-package-update-maybe))

(use-package use-package-ensure-system-package
  :ensure t)

(use-package f
  :ensure t)

(use-package dash
  :ensure t)

(use-package company
  :ensure t
  :bind
  (:map company-mode-map ("<C-tab>" . company-complete))
  :config
  (global-company-mode)

  (defun my-company-ispell-hook ()
    (make-local-variable 'company-backends)
    (push 'company-ispell company-backends))
      
  (add-hook 'text-mode-hook 'my-company-ispell-hook))

(use-package magit
  :ensure t
  :ensure-system-package git
  :bind (("C-c g" . magit-status)))

(use-package projectile
  :ensure t
  :config
  (projectile-global-mode t))

(use-package lsp-mode
  :ensure t)

(use-package lsp-ui
  :after (lsp-mode)
  :ensure t
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package company-lsp
  :after (lsp-mode company)
  :ensure t
  :config
  (push 'company-lsp company-backends))

(use-package cquery
  :after (lsp)
  :ensure t
  :ensure-system-package cquery)

(use-package meson-mode
  :ensure t
  :ensure-system-package meson)

(use-package csharp-mode 
  :ensure t)

(use-package omnisharp
  :after (csharp-mode company)
  :bind
  (:map csharp-mode-map
        ("M-." . omnisharp-go-to-definition)
        ("C-c C-c" . recompile))
  :config
  (add-hook 'csharp-mode-hook 'omnisharp-mode)
  (with-eval-after-load 'company
    (push 'company-omnisharp company-backends)))

(use-package go-mode
  :ensure t
  :ensure-system-package
  ((go)
   (gometalinter . "go get -u github.com/alecthomas/gometalinter; gometalinter --install --update")
   (gocode . "go get -u github.com/nsf/gocode")))

(use-package go-eldoc
  :after (go-mode)
  :ensure t
  :config
  (add-hook 'go-mode-hook 'go-eldoc-setup))

(use-package company-go
  :after (go-mode company)
  :ensure t
  :config
  (push 'company-go company-backends))
