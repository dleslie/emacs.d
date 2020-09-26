;;; init.el --- Startup Script

;;; Commentary:

;; Emacs configuration of Dan Leslie.
;; dan@ironoxide.ca

;;; Code:

(require 'cl-lib)

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

;; Projectile search paths
(setq projectile-project-search-path '("~/Workspace/" "~/org/"))

(add-hook 'c++-mode-hook (lambda () (eldoc-mode 1) (c-set-style "java")))
(add-hook 'c-mode-hook (lambda () (eldoc-mode 1) (c-set-style "java")))

;; Arduino ino files
(add-to-list 'auto-mode-alist '("\\.ino?\\'" . c++-mode))

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

(when  
    (not (or (string-match-p "Microsoft" (shell-command-to-string "uname -a"))
	     (eq 'windows-nt system-type)))
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

;;(package-initialize 'no-activate)
(package-initialize)

;; Always show imenu
(defun my-try-to-add-imenu ()
  "Attempt to add imenu."
  (condition-case
      nil
      (imenu-add-to-menubar "Imenu")
    (error nil)))
(add-hook 'font-lock-mode-hook 'my-try-to-add-imenu)

;; Dired Omit Mode
(require 'dired-x)
(setq-default dired-omit-files-p t) ; Buffer-local variable
(setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))
(defvar v-dired-omit t
  "If dired-omit-mode enabled by default.  Don't setq me.")
(defun dired-omit-switch ()
  "This function is a small enhancement for `dired-omit-mode'.
It will \"remember\" omit state across Dired buffers."
  (interactive)
  (if (eq v-dired-omit t)
      (setq v-dired-omit nil)
    (setq v-dired-omit t))
  (dired-omit-caller)
  (revert-buffer))

(defun dired-omit-caller ()
  "Ensures dired-omit is working."
  (if v-dired-omit
      (setq dired-omit-mode t)
    (setq dired-omit-mode nil)))

(define-key dired-mode-map (kbd "C-x M-o") 'dired-omit-switch)
(add-hook 'dired-mode-hook 'dired-omit-caller)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custom functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun flatten(x)
  "Flattens list X."
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
  "Remove all carriage return from BUFFER."
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
  "Enable visual line mode."
  (visual-line-mode 1))
(defun my-colorize-compilation-buffer ()
  "Enable ansi colors."
  (read-only-mode nil)
  (ansi-color-apply-on-region (point-min) (point-max))
  (read-only-mode t))

(add-hook 'compilation-mode-hook 'my-compilation-custom-hook)
(add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utf8
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8) 
(when (boundp 'set-w32-system-coding-system)
  (set-w32-system-coding-system 'utf-8))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; straight
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(require 'use-package)
(require 'bind-key)
(require 'use-package-ensure)
(setq use-package-always-ensure t)

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
(use-package geiser
  :config
  (setq geiser-chicken-binary (or (executable-find "chicken-csi") (executable-find "csi"))))
(use-package toml)
(use-package csharp-mode)
;(use-package restclient)
(use-package constant-theme)
(use-package doom-themes)
(use-package sexy-monochrome-theme)
(use-package meson-mode)
(use-package flycheck
  :hook (prog-mode . flycheck-mode))

(use-package selectrum
  :init
  (selectrum-mode +1))
(use-package selectrum-prescient
  :after selectrum
  :init
  (selectrum-prescient-mode +1)
  (prescient-persist-mode +1))

(use-package company
  :hook
  (prog-mode . company-mode)
  :config
  (setq company-backends
	'(company-capf company-gtags company-etags company-files))
  (setq company-tooltip-align-annotations t
	company-idle-delay 0.25
	company-minimum-prefix-length 1))

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
  :commands lsp
  :hook
  ((rust-mode . lsp)
   (ruby-mode . lsp)
   (python-mode . lsp)
   (js3-mode . lsp)))

(use-package lsp-ui)

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

;; (use-package omnisharp
;;   :after (csharp-mode company flycheck)
;;   :bind
;;   (:map csharp-mode-map
;;         ("M-." . omnisharp-go-to-definition)
;;         ("C-c C-c" . recompile))
;;   :init
;;   (add-hook 'csharp-mode-hook 'omnisharp-mode)
;;   (add-hook 'csharp-mode-hook 'flycheck-mode)
;;   (push 'company-omnisharp company-backends))

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

(use-package sly
  :after (company)
  :hook ((sly-mode . company-mode))
  :init
  (setq sly-lisp-implementations
	(cl-remove-if-not (lambda (imp) (caadr imp))
		       `((sbcl (,(executable-find "sbcl")) :coding-system utf-8-unix)
			 (armcl (,(executable-find "armcl")))
			 (cmucl (,(executable-find "cmucl") "-quiet"))
			 (ecl (,(executable-find "ecl")))
			 ))))
(use-package sly-asdf
  :after (sly)
  :init
  (add-to-list 'sly-contribs 'sly-asdf 'append))
(use-package sly-quicklisp
  :after (sly))
(use-package sly-macrostep
  :after (sly))
(use-package sly-repl-ansi-color
  :after (sly))

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
     sly-mode-hook
     clojure-mode-hook)))

(use-package ace-jump-mode
  :bind (("C-c SPC" . ace-jump-mode))
  :init
  (autoload 'ace-jump-mode "ace-jump-mode" "Emacs quick move minor mode" t))

(use-package org
  :ensure org-plus-contrib
  :after (f)
  :bind
  (("C-c C-o t" . org-todo-list)
   ("C-c C-o l" . org-store-link)
   ("C-c C-o a" . org-agenda)
   ("C-c C-o b" . org-iswitchb)
   ("C-c C-o c" . org-capture)
   ("C-c C-o v" . my-org-show-all-inline-images))
  :init
  (defun my-org-show-all-inline-images ()
    (interactive)
    (org-display-inline-images t t))
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
  (add-hook 'org-mode-hook 'my-custom-org-hook)
  
  ;; (use-package org-roam
  ;;   :hook
  ;;   (after-init . org-roam-mode)
  ;;   :custom
  ;;   (org-roam-directory org-directory)
  ;;   :bind (:map org-roam-mode-map
  ;; 		(("C-c n l" . org-roam)
  ;; 		 ("C-c n f" . org-roam-find-file)
  ;; 		 ("C-c n g" . org-roam-show-graph))
  ;; 		:map org-mode-map
  ;; 		(("C-c n i" . org-roam-insert))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Menus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar my-mode-map
  (let ((map (make-sparse-keymap)))
    (easy-menu-define my-menu map
      "My Menu"
      '("Mine"
	["Todo" org-todo-list]
	["Agenda" org-agenda]
	["Capture" org-capture]
	"--"))
    map))
(define-minor-mode my-mode
  "Minor mode to provide my custom menu items."
  :keymap my-mode-map
  :global t)
(my-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Finish
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
(global-set-key (kbd "C-;") 'hippie-expand)
(global-set-key (kbd "C-,") 'company-complete)

;; Enable GC
(setq gc-cons-threshold 16777216
      gc-cons-percentage 0.1)

;; Enable file handler
(setq file-name-handler-alist default-file-name-handler-alist)

;; General Emacs Sanity
(setq
 auto-window-vscroll nil
 c-basic-offset 2
 column-number-mode t
 debug-on-error nil
 delete-selection-mode t
 indent-tabs-mode nil
 inhibit-startup-screen t
 make-backup-files nil
 scroll-bar-mode nil
 scroll-conservatively 10000
 scroll-step 2
 show-paren-mode t
 tab-stop-list (number-sequence 2 120 2)
 tab-width 2
 tool-bar-mode nil
 truncate-lines t)

(global-eldoc-mode t)
(global-hl-line-mode t)

(when (file-exists-p custom-file)
  (load custom-file))

(reset-theme)
(override-theme 'sexy-monochrome)

(show-paren-mode nil)
(show-paren-mode t)

(garbage-collect)

(message "Emacs ready in %s with %d garbage collections."
         (format "%.2f seconds"
                 (float-time
                  (time-subtract after-init-time before-init-time)))
         gcs-done)

(provide 'init)
;;; init.el ends here
