;; local.el --- local configurations

;;; Commentary:

;; Emacs configuration of Dan Leslie.
;; dan@ironoxide.ca

;;; Code:

;; Useful bindings
(global-set-key "\C-w" 'clipboard-kill-region)
(global-set-key "\M-w" 'clipboard-kill-ring-save)
(global-set-key "\C-y" 'clipboard-yank)
(global-set-key "\C-c," 'scroll-bar-mode)
(global-set-key "\C-c." 'tool-bar-mode)
(global-set-key "\C-c?" 'menu-bar-mode)
(global-set-key "\C-c\\" 'comment-or-uncomment-region)
(global-set-key "\C-cs" 'eshell-here)
(global-set-key "\C-cp" 'toggle-window-dedication)
(global-set-key [f12] 'toggle-frame-fullscreen)
(global-set-key (kbd "C-;") 'hippie-expand)
(global-set-key "\C-c\C-t" 'next-theme)

;; General Emacs Sanity
(custom-set-variables
 '(auto-window-vscroll nil)
 '(c-basic-offset 2)
 '(column-number-mode t)
 '(css-indent-offset 2)
 '(debug-on-error nil)
 '(electric-indent-mode nil)
 '(kill-whole-line t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(js-indent-level 2)
 '(make-backup-files nil)
 '(scroll-bar-mode nil)
 '(scroll-conservatively 10000)
 '(scroll-step 2)
 '(show-paren-delay 0)
 '(show-trailing-whitespace t)
 '(tab-stop-list (number-sequence 2 120 2))
 '(tab-width 2)
 '(tool-bar-mode nil)
 '(backup-by-copying nil)
 '(require-final-newline t)
 '(frame-inhibit-implied-resize t)
 '(switch-to-buffer-obey-display-actions t))

(delete-selection-mode 1)
(global-eldoc-mode t)
(show-paren-mode t)
(global-hl-line-mode t)
(global-prettify-symbols-mode +1)
(prefer-coding-system 'utf-8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; My Menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar my-mode-map
  (let ((map (make-sparse-keymap)))
    (easy-menu-define my-menu map "MyMenu"
      '("Mine"
	      ;; ("Org"
	      ;;  ["Todo" org-todo-list]
	      ;;  ["Agenda" org-agenda]
	      ;;  ["Capture" org-capture])

	      ["Todo" org-todo-list]
	      ["Agenda" org-agenda]
	      ["Capture" org-capture]
	      "---"
	      ["Magit" magit-status]
        "---"
        ["Shell Here" eshell-here]
        ["dos2unix" dos2unix]
        "---"
        ["Pin Window" toggle-window-dedication]
        "---"
        ("Writing"
         ["Dictionary" dictionary-search]
         ["Thesaurus" powerthesaurus-lookup-word]
         ["Writegood" writegood-mode
          :style toggle :selected (and (boundp 'writegood-mode) writegood-mode) :enable t]
         "---"
         ["Darkroom" darkroom-mode
          :style toggle :selected (and (boundp 'darkroom-mode) darkroom-mode) :enable t]
         ["Olivetti" olivetti-mode
          :style toggle :selected (and (boundp 'olivetti-mode) olivetti-mode) :enable t]
         ["Writeroom" writeroom-mode
          :style toggle :selected (and (boundp 'writeroom-mode) writeroom-mode) :enable t])
        ("Themes"
         ["Change Theme" change-theme]
	       ["Next Theme" next-theme]
	       ["Random Theme" random-theme]
	       ["Random Dark Theme" random-dark-theme]
	       ["Random Light Theme" random-light-theme]
         ["Reset Theme" reset-theme])))
    map))

(define-minor-mode my-mode
  "Minor mode to provide my custom menu items."
  :keymap my-mode-map
  :group 'menu
  :global t)

(my-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpful Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun eshell-here ()
  "Opens up a new shell in the directory of the current buffer's file."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (height (/ (window-total-height) 3))
         (name   (car (last (split-string parent "/" t)))))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))))

(defun dos2unix (buffer)
  "Remove all carriage return from BUFFER."
  (interactive "*b")
  (save-excursion
    (goto-char (point-min))
    (while (search-forward (string ?\C-m) nil t)
      (replace-match "" nil t))))

(require 'ansi-color)
(defun display-ansi-colors ()
  "Enables ANSI color for the entire buffer."
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

(defun toggle-window-dedication ()
  "Toggle whether or not Emacs is allowed to display another buffer in current window."
  (interactive)
  (set-window-dedicated-p
   (selected-window)
   (not (window-dedicated-p (selected-window)))))

(defun mode-visible-p (mode)
  "Return t if MODE is used by any window."
  (seq-some
   (lambda (win)
     (let ((buf (window-buffer win)))
       (eq (buffer-local-value 'major-mode buf) mode)))
   (window-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compilation Mode Improvements
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dired Mode Improvements
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IMenu Improvements
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Always show imenu
(defun my-try-to-add-imenu ()
  "Attempt to add imenu."
  (condition-case
      nil
      (imenu-add-to-menubar "Imenu")
    (error nil)))

(add-hook 'font-lock-mode-hook 'my-try-to-add-imenu)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package Management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Configure package manager
(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("elpy" . "https://jorgenschaefer.github.io/packages/")
	      ( "jcs-elpa" . "https://jcs-emacs.github.io/jcs-elpa/packages/"))
      package-archive-priorities
      '(("elpy" . 90)
	      ("melpa" . 100)
	      ("gnu" . 80)
	      ("jcs-elpa" . 70)))

;; Configure straight
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
(setq package-native-compile t)

(use-package auto-package-update
  :init
  (setq auto-package-update-delete-old-versions t
        auto-package-update-hide-results t)
  (auto-package-update-maybe))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Useful Elisp Extensions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package dash)
(use-package f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ace Jump
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ace-jump-mode
  :bind (("C-." . ace-jump-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dumb Jump
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package dumb-jump
  :hook (xref-backend-functions . dumb-jump-xref-activate))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Neotree
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package neotree
  :bind (("<f8>" . neotree-project-dir)
         ("C-x p C-d" . neotree-project-dir))
  :init

  (defun neotree-project-dir ()
    "Open NeoTree using the project.el root."
    (interactive)
    (if (mode-visible-p 'neotree-mode)
        (neotree-hide)
      (let ((project-dir (project-root (project-current)))
            (file (buffer-file-name)))
        (if project-dir
            (progn
              (neotree-dir project-dir)
              (neotree-find file))
          (message "No project is active."))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Paredit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package paredit
  :hook ((emacs-lisp-mode . enable-paredit-mode)
         (ielm-mode . enable-paredit-mode)
         (lisp-interaction-mode . enable-paredit-mode)
         (lisp-mode . enable-paredit-mode)
         (sly-mode . enable-paredit-mode)
         (janet-mode . enable-paredit-mode)
         (scheme-mode . enable-paredit-mode)
         (geiser-repl-mode . enable-paredit-mode)
         (geiser-mode . enable-paredit-mode))
  :init
  (advice-add
   'paredit-RET
   :after
   (lambda ()
     (when (string-prefix-p
	          "*sly-mrepl for"
	          (buffer-name (current-buffer)))
       (sly-mrepl-return)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rainbow Delimiters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package rainbow-delimiters
  :hook
  ((emacs-lisp-mode . rainbow-delimiters-mode)
   (ielm-mode . rainbow-delimiters-mode)
   (lisp-interaction-mode . rainbow-delimiters-mode)
   (lisp-mode . rainbow-delimiters-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ag
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (executable-find "ag")
  (use-package ag
    :init
    (define-key-after global-map [menu-bar tools ag]
      '(menu-item "Search Files (ag)..." ag :help "Search files for strings or regexps (with ag)...")
      'grep)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ido
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ido
  :config
  (setq
   ido-create-new-buffer 'always
   ido-enable-flex-matching t
   ido-everywhere t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Minions (Minor Modes in Modeline)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package minions
  :config
  (minions-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Marginalia (Minibuffer Sigils)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package marginalia
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :config
  (marginalia-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Vertico (Minibuffer Interaction)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package vertico
  :config
  (vertico-mode)
  (setq vertico-resize nil
        vertico-cycle t))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Save History (Minibuffer)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package savehist
  :init
  (savehist-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Spacious Padding
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package spacious-padding
  :hook
  ((after-make-frame-functions . #'my/after-make-frame-spacious))
  :init
  (defun my/after-make-frame-spacious (frame)
    "Make FRAME spacious."
    (with-selected-frame frame
      (spacious-padding-mode 1)))
  (spacious-padding-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Corfu (Completions)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-separator ?\s)          ;; Orderless field separator
  (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  (corfu-preview-current t)      ;; Disable current candidate preview
  (corfu-preselect-first nil)    ;; Disable candidate preselection
  (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-excluded-modes'.
  :init
  (global-corfu-mode)
	(corfu-popupinfo-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3
				corfu-auto t
				corfu-quit-no-match 'separator
				corfu-popupinfo-delay 0.5
				corfu-preselect-first t)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Orderless
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package orderless
  :init
  (icomplete-mode)
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))
				                           (eglot (styles orderless)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flycheck
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package flycheck
  :hook
  (prog-mode . flycheck-mode)
  :init
	(setq flycheck-idle-change-delay 0.5))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Magit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (executable-find "git")
  (use-package magit
    :bind
    ("C-c g" . magit-status)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Eglot
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package eglot
  :init
  (setq eglot-connect-timeout 240))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CoPilot
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (executable-find "node")
  (use-package copilot
    :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
    :bind (("C-TAB" . 'copilot-accept-completion-by-word)
           ("C-<tab>" . 'copilot-accept-completion-by-word)
	         :map copilot-completion-map
           ("<tab>" . 'copilot-accept-completion)
           ("TAB" . 'copilot-accept-completion))
    :init
    (global-copilot-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Visual Regexp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package visual-regexp
  :bind
  (("C-c r" . vr/replace)
   ("C-c q" . vr/query-replace)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Which Key?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package which-key
  :init
  (setq which-key-show-early-on-C-h t)
  (setq which-key-idle-delay 2)
  (setq which-key-idle-secondary-delay 0.05)
  (which-key-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tree-Sitter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package tree-sitter
  :hook (tree-sitter-after-on . tree-sitter-hl-mode)
  :init
  (global-tree-sitter-mode))
(use-package tree-sitter-langs
  :after tree-sitter)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C/C++
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Default C style
(add-hook 'c++-mode-hook (lambda () (c-set-style "java")))
(add-hook 'c-mode-hook (lambda () (c-set-style "java")))

;; Extra C/C++ Mode Hooks
(add-to-list 'auto-mode-alist '("\\.ino?\\'" . c++-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (executable-find "dotnet")
  (use-package csharp-mode
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
    (define-key csharp-mode-map (kbd "C-c C-z") 'my-csharp-repl)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Janet
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (executable-find "janet")
  (when (and (fboundp 'treesit-available-p) (treesit-available-p))
      (progn
        (setq treesit-language-source-alist
              (if (eq 'windows-nt system-type)
                  '((janet-simple
                     . ("https://github.com/sogaiu/tree-sitter-janet-simple"
                        nil nil "gcc.exe")))
                '((janet-simple
                   . ("https://github.com/sogaiu/tree-sitter-janet-simple")))))

        (when (not (treesit-language-available-p 'janet-simple))
          (treesit-install-language-grammar 'janet-simple))))

  (if (treesit-language-available-p 'janet-simple)
      (progn
        (use-package janet-ts-mode
          :straight (:type git :host github :repo "sogaiu/janet-ts-mode" :files ("*.el")))
        (use-package ajrepl
          :hook (janet-ts-mode . ajrepl-interaction-mode)
          :straight (:type git :host github :repo "sogaiu/ajrepl" :files ("*.el" "ajrepl"))))
    (progn
      (use-package janet-mode
        :hook (janet-mode . paredit-mode))
      (use-package inf-janet
        :hook (janet-mode . inf-janet-minor-mode)
        :straight (:type git :host github :repo "velkyel/inf-janet")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Common Lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package sly
  :bind (:map sly-mode-map
              ("C-x 3 ." . xref-find-definitions)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Nim
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package nim-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ruby
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (executable-find "rvm")
  (use-package ruby-mode
    :hook (ruby-mode . inf-ruby-keys)
    :init
    (autoload 'ruby-mode "ruby-mode" "Ruby Mode" t ".rb"))

  (defun launch-ruby ()
    "Launches a ruby process in a buffer named *ruby*."
    (interactive)
    (unless (get-buffer "*ruby*")
      (let ((buf (current-buffer)))
	      (inf-ruby)
	      (set-buffer buf))))

  (defun kill-ruby ()
    "Kills a ruby process in a buffer named *ruby*."
    (interactive)
    (when (get-buffer "*ruby*")
      (kill-buffer "*ruby*")))

  (use-package inf-ruby)
  (use-package enh-ruby-mode)

  (use-package robe
    :init
    (advice-add 'launch-ruby :after #'robe-start)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rust
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (executable-find "cargo")
  (use-package rust-mode)
  (use-package flycheck-rust
    :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scheme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (-any? #'executable-find
             '("chicken" "gambit" "racket" "chez" "gauche" "chibi"))
  (use-package geiser
    :config
    (setq geiser-chicken-binary (or (executable-find "chicken-csi") (executable-find "csi")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Markdown
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package markdown-mode
  :init
  (autoload 'markdown-mode "markdown-mode" "Markdown Mode" t ".md")
  (autoload 'markdown-mode "markdown-mode" "Markdown Mode" t ".markdown"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc Development Modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package flatbuffers-mode)
(use-package meson-mode)
(use-package dockerfile-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Web
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package web-mode
  :init
  (autoload 'web-mode "web-mode" "Web Mode" t ".phtml")
  (autoload 'web-mode "web-mode" "Web Mode" t ".tpl")
  (autoload 'web-mode "web-mode" "Web Mode" t ".php")
  (autoload 'web-mode "web-mode" "Web Mode" t ".asp")
  (autoload 'web-mode "web-mode" "Web Mode" t ".gsp")
  (autoload 'web-mode "web-mode" "Web Mode" t ".jsp")
  (autoload 'web-mode "web-mode" "Web Mode" t ".aspx")
  (autoload 'web-mode "web-mode" "Web Mode" t ".ascx")
  (autoload 'web-mode "web-mode" "Web Mode" t ".erb")
  (autoload 'web-mode "web-mode" "Web Mode" t ".mustache")
  (autoload 'web-mode "web-mode" "Web Mode" t ".djhtml")
  (autoload 'web-mode "web-mode" "Web Mode" t ".html")
  (autoload 'web-mode "web-mode" "Web Mode" t ".tsx")
  (autoload 'web-mode "web-mode" "Web Mode" t ".jsx"))
(use-package css-mode)
(use-package js2-mode)
(use-package json-mode)
(use-package restclient)
(when (executable-find "tsc")
  (use-package typescript-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Writing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package writegood-mode
  :hook (text-mode . writegood-mode))
(use-package olivetti)
(use-package writeroom-mode)
(use-package darkroom)

;; Enable truncate lines for all text mode buffers
(add-hook 'text-mode-hook 'toggle-truncate-lines)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dictionary
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package dictionary
  :init
  (global-set-key "\C-c d" 'dictionary-search)
  (define-key-after global-map [menu-bar tools apps dictionary-search]
    '(menu-item "Dictionary" dictionary-search :help "Search dictionary") t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Thesaurus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package powerthesaurus
  :init
  (global-set-key "\C-c t" 'powerthesaurus-lookup-word-dwim)
  (define-key-after global-map [menu-bar tools apps powerthesaurus-lookup-word]
    '(menu-item "Thesaurus" powerthesaurus-lookup-word :help "Search thesaurus") t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org
  :straight nil
  :init
  (defun my-org-show-all-inline-images ()
    (interactive)
    (org-display-inline-images t t))
  (defun my-org-directory ()
    (interactive)
    (find-file org-directory))
  (defcustom org-directory (file-truename "~/org") "Location of org documents")
  (setq
   org-default-notes-file (concat (file-truename org-directory) "notes.org")
   org-agenda-files `(,(concat (file-truename org-directory) "todo.org") ,(concat (file-truename org-directory) "agenda.org"))
   org-agenda-diary-file (concat (expand-file-name org-directory) "diary.org")
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
      "* %(org-contacts-template-name)\n   :PROPERTIES:\n   :EMAIL: %(org-contacts-template-email)\n   :END:\n   %i%?\n")))
  :bind
  (("C-c o c" . org-capture)
   ("C-c o a" . org-agenda)
   ("C-c o l" . org-store-link)
   ("C-c o b" . org-iswitchb)
   ("C-c o t" . org-todo-list)
   ("C-c o s" . org-search-view)
   ("C-c o d" . my-org-directory)
   ("C-c o i" . my-org-show-all-inline-images)))

(defvar org--inhibit-version-check t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Themes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq custom-safe-themes t)

(use-package afternoon-theme :defer t)
(use-package alect-themes :defer t)
(use-package ample-theme :defer t)
(use-package clues-theme :defer t)
(use-package color-theme-sanityinc-tomorrow :defer t)
(use-package constant-theme :defer t)
(use-package cyberpunk-theme :defer t)
(use-package flatland-theme :defer t)
(use-package gruber-darker-theme :defer t)
(use-package gruvbox-theme :defer t)
(use-package moe-theme :defer t)
(use-package sexy-monochrome-theme :defer t)
(use-package solarized-theme :defer t)
(use-package zenburn-theme :defer t)
(use-package almost-mono-themes :defer t)
(use-package quasi-monochrome-theme :defer t)
(use-package monochrome-theme :defer t)
(use-package modus-themes :defer t)

(defun reset-theme ()
  "Disable all active themes."
  (interactive)
  (dolist (theme custom-enabled-themes)
    (disable-theme theme)))

(defun change-theme (theme)
  "Disable all enabled themes and then load the provided theme THEME."
  (interactive
   (list
    (intern (completing-read "Load custom theme: "
                             (mapcar 'symbol-name (custom-available-themes))))))
  (reset-theme)
  (load-theme theme))

(defun random-theme ()
  "Change to a random theme."
  (interactive)
  (let ((success nil)
	      (current (car custom-enabled-themes))
	      (all-themes (custom-available-themes)))
    (let ((new-theme (seq-random-elt all-themes)))
      (message (format "Switching to theme %s" new-theme))
      (ignore-errors (change-theme new-theme)))))

(defun random-dark-theme ()
  "Change to a random dark theme."
  (interactive)
  (while
      (progn
	      (random-theme)
	      (not (background-is-dark)))))

(defun random-light-theme ()
  "Change to a random light theme."
  (interactive)
  (while
      (progn
	      (random-theme)
	      (background-is-dark))))

(defun next-theme ()
  "Cycle through all available themes."
  (interactive)
  (let* ((current (car custom-enabled-themes))
	       (all-themes (custom-available-themes)))
    (if (not current)
	      (change-theme (car all-themes))
      (let* ((remaining
	            (seq-drop all-themes
			                  (+ 1 (seq-position all-themes current))))
	           (next (or (car remaining)
		                   (car all-themes))))
	      (change-theme next)))))

(defun background-is-dark ()
  "Return t if the current theme background is dark."
  (interactive)
  (let ((dark 0.33))
    (seq-every-p (lambda (x) (<= x dark))
		             (color-name-to-rgb (face-attribute 'default :background)))))

(defcustom default-theme 'constant
  "The default theme to load."
  :type 'symbol
  :group 'local)

(defun load-default-theme ()
  "Load the default theme."
  (interactive)
  (change-theme default-theme))

(add-hook 'after-init-hook 'load-default-theme)

(provide 'local)
;;; local.el ends here
