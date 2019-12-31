;;; init.el --- Startup Script

;;; Commentary:

;; Emacs configuration of Dan Leslie.
;; dan@ironoxide.ca

;;; Code:

;; Disable GC during load
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

;; Windows performance tweaks
(when (boundp 'w32-pipe-read-delay)
  (setq w32-pipe-read-delay 0))

;; Make custom file not this one
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Disable file handler search during load
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; org-directory
(defvar org-directory "~/org")

(add-hook 'c++-mode-hook (lambda () (eldoc-mode 1) (c-set-style "java")))
(add-hook 'c-mode-hook (lambda () (eldoc-mode 1) (c-set-style "java")))

;; Arduino ino files
(add-to-list 'auto-mode-alist '("\\.ino?\\'" . c++-mode))

;; Where are we finding things?
(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")
        ("elpy" . "https://jorgenschaefer.github.io/packages/")))
                                        ;(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(package-initialize)

;; Always show imenu
(defun my-try-to-add-imenu ()
  (condition-case nil (imenu-add-to-menubar "Imenu") (error nil)))
(add-hook 'font-lock-mode-hook 'my-try-to-add-imenu)

;; Dired Omit Mode
(require 'dired-x)
(setq-default dired-omit-files-p t) ; Buffer-local variable
(setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))
(defvar v-dired-omit t
  "If dired-omit-mode enabled by default. Don't setq me.")
(defun dired-omit-switch ()
  "This function is a small enhancement for `dired-omit-mode', which will
   \"remember\" omit state across Dired buffers."
  (interactive)
  (if (eq v-dired-omit t)
      (setq v-dired-omit nil)
    (setq v-dired-omit t))
  (dired-omit-caller)
  (revert-buffer))

(defun dired-omit-caller ()
  (if v-dired-omit
      (setq dired-omit-mode t)
    (setq dired-omit-mode nil)))

(define-key dired-mode-map (kbd "C-x M-o") 'dired-omit-switch)
(add-hook 'dired-mode-hook 'dired-omit-caller)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custom functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun flatten(x)
  "Flattens a list"
  (cond ((null x) nil)
        ((listp x) (append (flatten (car x)) (flatten (cdr x))))
        (t (list x))))

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

(defun dos2unix (buffer)
  "Remove carriage returns"
  (interactive "*b")
  (save-excursion
    (goto-char (point-min))
    (while (search-forward (string ?\C-m) nil t)
      (replace-match "" nil t))))

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

(use-package use-package-ensure-system-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package auto-package-update
  :init
  (setq auto-package-update-delete-old-versions t
        auto-package-update-hide-results t)
  (auto-package-update-maybe))

(use-package f
  :functions (f-join))

(use-package dash)
(use-package geiser)
(use-package toml)
(use-package csharp-mode)
(use-package restclient)
(use-package constant-theme)
(use-package doom-themes)
(use-package sexy-monochrome-theme)
(use-package meson-mode)
(use-package flycheck)

(use-package company
  :init
  (global-company-mode)
  :config
  (setq company-tooltip-align-annotations t
	      company-idle-delay 0.25)
  (defun my-company-ispell-hook ()
    (make-local-variable 'company-backends)
    (push 'company-ispell company-backends))
  (add-hook 'text-mode-hook 'my-company-ispell-hook))

(when (executable-find "git")
  (use-package magit
    :bind (("C-c g" . magit-status))
    :init
    (setq magit-last-seen-setup-instructions "1.4.0")))

(use-package projectile
  :bind
  (:map projectile-mode-map
        ("C-c p f" . projectile-find-file)
        ("C-c p p" . projectile-switch-project)
        ("C-c p d" . projectile-find-dir)
        ("C-c p g" . projectile-grep))
  :init
  (setq projectile-switch-project-action 'projectile-find-dir
        projectile-find-dir-includes-top-level t)
  (projectile-mode t))

(use-package lsp-mode
  :config
  (setq lsp-enable-snippet nil))

(use-package lsp-ui
  :after (lsp-mode)
  :init
  (require 'lsp-ui-imenu)
  (setq lsp-ui-sideline-ignore-duplicate t
        lsp-ui-flycheck-enable t
        lsp-ui-sideline-enable nil)
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))

(use-package company-lsp
  :after (lsp-mode company)
  :init
  (push 'company-lsp company-backends)
  (setq
   company-transformers nil
   company-lsp-async t
   company-lsp-cache-candidates nil))

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

(when (executable-find "go")
  (use-package go-mode
    :ensure-system-package
    ((go)
     (gometalinter . "go get -u github.com/alecthomas/gometalinter; gometalinter --install --update")
     (gocode . "go get -u github.com/nsf/gocode"))))

(use-package go-eldoc
  :after (go-mode)
  :init
  (add-hook 'go-mode-hook 'go-eldoc-setup))

(use-package company-go
  :after (go-mode company)
  :init
  (push 'company-go company-backends))

(when (executable-find "stack")
  (use-package haskell-mode))

(use-package ghc
  :functions (ghc-init)
  :after (haskell-mode)
  :init
  (autoload 'ghc-init "ghc" nil t)
  (autoload 'ghc-debug "ghc" nil t)
  (add-hook 'haskell-mode-hook #'ghc-init))

(use-package company-ghc
  :after (haskell-mode company-mode)
  :init
  (push 'company-haskell company-backends))

(when (executable-find "sbcl")
  (use-package slime
    :functions (slime-setup)
    :init
    (require 'slime-autoloads)
    (setq slime-contribs '(slime-fancy))
    (slime-setup)
    :config
    (add-to-list 'slime-lisp-implementations '(sbcl ("sbcl") :coding-system utf-8-unix))))

(use-package paredit
  :bind
  (:map paredit-mode-map
        ("{" . paredit-open-curly)
        ("}" . paredit-close-curly))
  :init
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
  :mode ("\\.markdown\\'" "\\.md\\'")
  :init
  (defun my-custom-markdown-mode ()
    (interactive)
    (visual-line-mode 1))
  (advice-add 'markdown-mode :after #'my-custom-markdown-mode))

(when (executable-find "npm")
  (use-package js2-mode
    :after (lsp-mode)
    :ensure-system-package
    ((npm)
     (html-languageserver . "npm install -g vscode-html-languageserver-bin"))
    :init
    (add-hook 'js2-mode-hook #'lsp)
    (add-hook 'typescript-mode-hook #'lsp)
    (add-hook 'css-mode-hook #'lsp)))

(when (executable-find "npm")
  (use-package typescript-mode
    :after (js2-mode)
    :ensure-system-package
    ((npm)
     (tsc . "npm install -g typescript"))))

(when (executable-find "cargo")
  (use-package rust-mode
    :after (lsp-mode)
    :ensure-system-package
    ((cargo) (rustup) (rls . "rustup component add rls"))
    :init
    (add-hook 'rust-mode-hook #'lsp)))

(when (executable-find "pip3")
  (use-package python-mode
    :after (projectile lsp-mode)
    :ensure-system-package
    ((pip) (python) (pyls . "pip3 install --user 'python-language-server'"))
    :init
    (require 'lsp-pyls)
    (add-hook 'python-mode-hook #'lsp-pyls-enable)))

(when (executable-find "gem")
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
    (add-hook 'ruby-mode-hook 'my-ruby-mode-hook)))

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

(when (executable-find "global")
  (use-package ggtags
    :after (company)
    :bind
    (:map ggtags-mode-map
          ("C-c g s" . ggtags-find-other-symbol)
          ("C-c g h" . ggtags-view-tag-history)
          ("C-c g r" . ggtags-find-reference)
          ("C-c g f" . ggtags-find-file)
          ("C-c g c" . ggtags-create-tags)
          ("C-c g u" . ggtags-update-tags)
          ("M-," . pop-tag-mark))
    :init
    (add-hook 'c-mode-common-hook
              (lambda ()
                (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
                  (ggtags-mode 1))))
    :config
    (push 'company-gtags company-backends)))

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

(use-package smex
  :bind (("M-x" . smex)))

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
  :bind
  (("C-c j" . dumb-jump-go)
   ("C-c J" . dumb-jump-quick-look)
   ("C-x j" . dumb-jump-back))
  :init
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
  :init
  (define-key-after global-map [menu-bar tools apps dictionary-search]
    '(menu-item "Dictionary" dictionary-search :help "Search dictionary") t))

(use-package quiz
  :bind (("C-c q" . quiz))
  :init
  (define-key global-map [menu-bar tools games quiz]
    '(menu-item "Quiz" quiz :help "Be quizzed")))

(when (executable-find "clang-format")
  (use-package clang-format
    :init
    (defun my-clang-format-on-save ()
      (make-local-variable 'before-save-hook)
      (add-hook 'before-save-hook 'clang-format-buffer))
    (add-hook 'c-mode-hook 'my-clang-format-on-save)))

(when (executable-find "cquery")
  (use-package cquery
    :after (lsp-mode lsp-ui)
    :init
    (setq cquery-extra-init-params '(:completion (:detailedLabel t)))))

(use-package org
  :after (f)
  :bind
  (("C-c t" . org-todo-list)
   ("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c b" . org-iswitchb)
   ("C-c c" . org-capture))
  :init
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


;; General Emacs Sanity
(setq-default
 c-basic-offset 2
 debug-on-error nil
 indent-tabs-mode nil
 make-backup-files nil
 tab-stop-list (number-sequence 2 120 2)
 tab-width 2
 truncate-lines t
 inhibit-startup-screen t
 scroll-step 2
 scroll-conservatively 10000
 auto-window-vscroll nil
 delete-selection-mode t
 show-paren-mode t)
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

(add-hook 'emacs-startup-hook
          (lambda ()
            ;; Sanitize company-backends
            (setq company-backends
                  (delq nil
                        (delete-dups
                         (remove 'company-semantic (flatten company-backends)))))

            ;; Enable GC
            (setq gc-cons-threshold 16777216
                  gc-cons-percentage 0.1)

            ;; Enable file handler
            (setq file-name-handler-alist default-file-name-handler-alist)

            (when (file-exists-p custom-file)
              (load custom-file))

            (garbage-collect)

            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)

            (delete-other-windows)))

(load "server")
(unless (server-running-p) (server-start))
