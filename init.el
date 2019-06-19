;;; init.el --- Startup Script

;;; Commentary:

;; Emacs configuration of Dan Leslie.
;; dan@ironoxide.ca

;;; Code:

;; Disable GC during load
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

;; Make custom file not this one
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Disable file handler search during load
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; org-directory
(defvar org-directory "~/org")

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

;; eldoc in C/C++
(add-hook 'c++-mode-hook (lambda () (eldoc-mode 1)))
(add-hook 'c-mode-hook (lambda () (eldoc-mode 1)))

;; Arduino ino files
(add-to-list 'auto-mode-alist '("\\.ino?\\'" . c++-mode))

;; Where are we finding things?
(package-initialize)
(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")
        ("elpy" . "https://jorgenschaefer.github.io/packages/")))

;; Always show imenu
(defun my-try-to-add-imenu ()
  (condition-case nil (imenu-add-to-menubar "Imenu") (error nil)))
(add-hook 'font-lock-mode-hook 'my-try-to-add-imenu)

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
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t
	auto-package-update-hide-results t)
  (auto-package-update-maybe))

(use-package use-package-ensure-system-package
  :functions (use-package-ensure-system-package-exists?))

(use-package f
  :functions (f-join))

(use-package dash)
(use-package geiser)
(use-package toml)
(use-package csharp-mode)
(use-package restclient)
(use-package doom-themes)

(use-package company
  :functions (global-company-mode)
  :bind
  (:map company-mode-map ("<C-tab>" . company-complete))
  :config
  (setq company-tooltip-align-annotations t
	company-idle-delay 0.25
	company-backends (remove 'company-semantic company-backends))
  (global-company-mode)
  (defun my-company-ispell-hook ()
    (make-local-variable 'company-backends)
    (push 'company-ispell company-backends))
  (add-hook 'text-mode-hook 'my-company-ispell-hook))

(use-package magit
  :ensure-system-package git
  :bind (("C-c g" . magit-status))
  :config
  (setq magit-last-seen-setup-instructions "1.4.0"))

(use-package projectile
  :functions (projectile-mode)
  :config
  (setq projectile-switch-project-action 'projectile-find-dir
	projectile-find-dir-includes-top-level t)
  (projectile-mode t))

(use-package lsp-mode)

(use-package lsp-ui
  :after (lsp-mode)
  :config
  (require 'lsp-ui-imenu)
  (setq lsp-ui-sideline-ignore-duplicate t)
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))

(use-package company-lsp
  :after (lsp-mode company)
  :config
  (push 'company-lsp company-backends))

(use-package cquery
  :after (lsp)
  :ensure-system-package cquery)

(use-package meson-mode
  :ensure-system-package meson)

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
  :ensure-system-package
  ((go . golang)
   (gometalinter . "go get -u github.com/alecthomas/gometalinter; gometalinter --install --update")
   (gocode . "go get -u github.com/nsf/gocode")))

(use-package go-eldoc
  :after (go-mode)
  :config
  (add-hook 'go-mode-hook 'go-eldoc-setup))

(use-package company-go
  :after (go-mode company)
  :config
  (push 'company-go company-backends))

(use-package haskell-mode
  :ensure-system-package
  ((stack . haskell-stack)
   (ghc . "stack ghc")))

(use-package ghc
  :functions (ghc-init)
  :after (haskell-mode)
  :config
  (autoload 'ghc-init "ghc" nil t)
  (autoload 'ghc-debug "ghc" nil t)
  (add-hook 'haskell-mode-hook #'ghc-init))

(use-package company-ghc
  :after (haskell-mode company-mode)
  :config
  (push 'company-haskell company-backends))

(use-package slime
  :ensure-system-package (sbcl)
  :functions (slime-setup)
  :config
  (require 'slime-autoloads)
  (setq slime-contribs '(slime-fancy))
  (add-to-list 'slime-lisp-implementations '(sbcl ("sbcl") :coding-system utf-8-unix))
  (slime-setup))

(use-package paredit
  :functions (paredit-mode)
  :config
  (mapc
   (lambda (mode-hook)
     (add-hook mode-hook #'paredit-mode))
   '(emacs-lisp-mode-hook
     eval-expression-minibuffer-setup-hook
     ielm-mode-hook
     lisp-mode-hook
     lisp-interaction-mode-hook
     scheme-mode-hook
     slime-repl-mode-hook
     clojure-mode-hook)))

(use-package markdown-mode
  :ensure-system-package (markdown . discount)
  :mode ("\\.markdown\\'" "\\.md\\'")
  :config
  (defun my-custom-markdown-mode ()
    (interactive)
    (visual-line-mode 1))
  (advice-add 'markdown-mode :after #'my-custom-markdown-mode))

(use-package js2-mode
  :after (lsp-mode)
  :ensure-system-package
  ((npm)
   (html-languageserver . "npm install -g vscode-html-languageserver-bin")
   (css-languageserver . "npm install -g vrcode-css-languageserver-bin"))
  :config
  (add-hook 'js2-mode-hook #'lsp)
  (add-hook 'typescript-mode-hook #'lsp)
  (add-hook 'css-mode-hook #'lsp))

(use-package typescript-mode
  :after (js2-mode)
  :ensure-system-package
  ((npm)
   (tsc . "npm install -g typescript")))

(use-package rust-mode
  :after (lsp-mode)
  :ensure-system-package
  ((cargo) (rustup) (rls . "rustup component add rls"))
  :config
  (add-hook 'rust-mode-hook #'lsp))

(use-package python-mode
  :after (projectile lsp-mode)
  :functions (lsp-python-enable)
  :ensure-system-package
  ((pip) (python) (pyls . "pip install 'python-language-server[all]'"))
  :config
  (require 'lsp-python)
  (add-hook 'python-mode-hook #'lsp-python-enable))

(use-package ruby-mode
  :ensure-system-package
  ((gem)
   (rubocop     . "gem install rubocop")
   (ruby-lint   . "gem install ruby-lint")
   (ripper-tags . "gem install ripper-tags")
   (pry         . "gem install pry"))
  :mode "\\.rb\\'"
  :interpreter "ruby"
  :functions (inf-ruby-keys launch-ruby kill-ruby)
  :config
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
  :config
  (advice-add 'projectile-rails-console :before #'kill-ruby)
  (advice-add 'launch-ruby :after #'projectile-rails-on)
  (advice-add 'kill-ruby :after #'projectile-rails-off))

(use-package robe
  :after ruby-mode
  :functions (robe-start)
  :config
  (advice-add 'launch-ruby :after #'robe-start))

(use-package company-inf-ruby
  :after (company ruby-mode)
  :config
  (push 'company-inf-ruby company-backends))

(use-package company-restclient
  :after (restclient company)
  :config
  (push 'company-restclient company-backends))

(use-package web-mode
  :mode ("\\.phtml\\'" "\\.tpl\\.php\\'" "\\.[agj]sp\\'" "\\.as[cp]x\\'" "\\.erb\\'" "\\.mustache\\'" "\\.djhtml\\'" "\\.html?\\'" "\\.tsx\\'" "\\.jsx\\'"))

(use-package company-web
  :after (company web-mode)
  :config
  (push 'company-web-html company-backends)
  (push 'company-web-jade company-backends)
  (push 'company-web-slim company-backends))

(use-package ggtags
  :functions (ggtags-mode)
  :ensure-system-package (global)
  :bind
  (:map ggtags-mode-map
	("C-c g s" . ggtags-find-other-symbol)
	("C-c g h" . ggtags-view-tag-history)
	("C-c g r" . ggtags-find-reference)
	("C-c g f" . ggtags-find-file)
	("C-c g c" . ggtags-create-tags)
	("C-c g u" . ggtags-update-tags)
	("M-," . pop-tag-mark))
  :config
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
                (ggtags-mode 1))))  
  (push 'company-gtags company-backends ))

(use-package olivetti
  :config
  (define-key text-mode-map [menu-bar text olivetti-mode]
    '(menu-item "Olivetti" olivetti-mode
                :button (:toggle . (and (boundp 'olivetti-mode) olivetti-mode)))))

(use-package writeroom-mode
  :config
  (define-key text-mode-map [menu-bar text writeroom-mode]
    '(menu-item "Writeroom" writeroom-mode
                :button (:toggle . (and (boundp 'writeroom-mode) writeroom-mode)))))

(use-package writegood-mode
  :config
  (define-key text-mode-map [menu-bar text writeroom-mode]
    '(menu-item "Writegood" writegood-mode
                :button (:toggle . (and (boundp 'writegood-mode) writegood-mode))))
  :config
  (add-hook 'text-mode-hook 'writegood-mode))

(use-package smex
  :bind (("M-x" . smex)))

(use-package ido
  :config
  (setq
   ido-create-new-buffer 'always
   ido-enable-flex-matching t
   ido-everywhere t))

(use-package nyan-mode
  :functions (nyan-mode)
  :config
  (nyan-mode t))

(use-package ag
  :ensure-system-package (ag)
  :config
  (define-key-after global-map [menu-bar tools ag]
    '(menu-item "Search Files (ag)..." ag :help "Search files for strings or regexps (with ag)...")
    'grep))

(use-package dumb-jump
  :bind
  (("C-c j" . dumb-jump-go)
   ("C-c J" . dumb-jump-quick-look)
   ("C-x j" . dumb-jump-back))
  :config
  (define-key-after global-map [menu-bar edit dj-menu]
    (cons "Dumb Jump" (make-sparse-keymap "dumb jump")) 'goto)
  (define-key global-map [menu-bar edit dj-menu go]
    '(menu-item "Go" dumb-jump-go :help "Jump to definition"))
  (define-key global-map [menu-bar edit dj-menu quick-look]
    '(menu-item "Quick Look" dumb-jump-quick-look :help "Look at definition"))
  (define-key global-map [menu-bar edit dj-menu back]
    '(menu-item "Back" dumb-jump-back :help "Go back")))

(use-package dictionary
  :bind (("C-c d" . dictionary-search))
  :config
  (define-key-after global-map [menu-bar tools apps dictionary-search]
    '(menu-item "Dictionary" dictionary-search :help "Search dictionary") t))

(use-package quiz
  :bind (("C-c q" . quiz))
  :config
  (define-key global-map [menu-bar tools games quiz]
    '(menu-item "Quiz" quiz :help "Be quizzed")))

(use-package org
  :after (f)
  :bind
  (("C-c t" . org-todo-list)
   ("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c b" . org-iswitchb)
   ("C-c c" . org-capture))
  :config
  (require 'org)
  (setq
   org-default-notes-file (f-join org-directory "notes.org")
   org-agenda-files `(,(f-join org-directory "todo.org") ,(f-join org-directory "agenda.org"))
   org-agenda-diary-file (f-join org-directory "diary.org")
   org-todo-keywords
   '((sequence "TODO(t)" "PROG(p)" "BLCK(b)" "STAL(s)" "|" "DONE(d)" "WONT(w)"))
   org-todo-keyword-faces
   '(("TODO" . (:foreground "white" :weight bold))
     ("DOIN" . (:foreground "green" :weight bold))
     ("BLCK" . (:foreground "red" :weight bold))
     ("STAL" . (:foreground "yellow" :weight bold))
     ("WONT" . (:foreground "grey" :weight bold))
     ("DONE" . (:foreground "grey" :weight bold)))
   org-capture-templates
   '(("n" "Note" entry (file+headline "notes.org" "Notes")
      "* %^{topic} %T %^g\n   :CATEGORY: %^{category}\n%i%?\n")
     ("t" "To Do" entry (file+headline "todo.org" "To Do")
      "* TODO %^{todo} %^g\n   DEADLINE: %^{due}t\n   :CATEGORY: %^{category}\n")
     ("d" "Daily review" entry (file+headline "diary.org" "Daily Review")
      (format
       "* %%T %%^g\n   :CATEGORY: Review\n   %%?%%[%s/template_daily_review.org]\n"
       org-directory))
     ("i" "Idea" entry (file+headline "ideas.org" "Ideas")
      "* %^{topic} %T %^g\n   :CATEGORY: Idea\n   %i%?\n")
     ("j" "Journal" entry (file+headline "diary.org" "Journal")
      "* %^{topic} %T %^g\n   :CATEGORY: Journal\n   %i%?\n")
     ("l" "Letter" entry (file+headline "letters.org" "Letter")
      "* %^{topic} %T %^g\n   :CATEGORY: Letter\n   %i%?\n")
     ("w" "Work Log" entry (file+headline "work.org" "Work Log")
      "* %^{topic} %T %^g\n   :CATEGORY: Log\n   %i%?\n")
     ("a" "Article" entry (file+headline "articles.org" "Article")
      "* %^{topic} %T %^g\n   :CATEGORY: Article\n   %i%?\n")
     ("e" "Event" entry (file+headline "agenda.org" "Events")
      "* %^{title} %^g\n     SCHEDULED: %^{when}t\n   %i%?\n")
     ("c" "Contact" entry (file+headline "addresses.org" "Addresses")
      "* %(org-contacts-template-name)\n   :PROPERTIES:\n   :EMAIL: %(org-contacts-template-email)\n   :END:\n   %i%?\n"))
   org-modules
   '(org-bbdb org-bibtex org-docview org-gnus org-info org-jsinfo org-habit org-irc org-mew org-mhe org-rmail org-special-blocks org-vm org-wl org-w3m org-mouse org-bookmark org-drill org-eshell org-invoice org-registry org-contacts))  
  (defun my-custom-org-hook ()
    (interactive)
    (visual-line-mode t))
  (add-hook 'org-mode-hook 'my-custom-org-hook))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Finish
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Enable GC
(setq gc-cons-threshold 16777216
      gc-cons-percentage 0.1)

;; Enable file handler
(setq file-name-handler-alist default-file-name-handler-alist)

(garbage-collect)

(when (file-exists-p custom-file)
  (load custom-file))
