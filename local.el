;; local.el --- local configurations
;;; Commentary:

;; Emacs configuration of Dan Leslie.
;; dan@ironoxide.ca

;;; Code:

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
        ["SmartParens Guide" sp-cheat-sheet]
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
         ["Writeroom" writeroom-mode
          :style toggle :selected (and (boundp 'writeroom-mode) writeroom-mode) :enable t])
        ("Themes"
         ["Change Theme" change-theme]
	       ["Next Theme" next-theme]
	       ["Random Theme" random-theme]
	       ["Random Dark Theme" random-dark-theme]
	       ["Random Light Theme" random-light-theme]
         ["Reset Theme" reset-theme]
         ["Save Current as Default" set-current-theme-as-default])))
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

(defun my-org-directory ()
  "Opens `my/org-directory' in a new buffer."
  (interactive)
  (find-file my/org-directory))

(defun my-emacs-directory ()
  "Opens the Emacs configuration directory in a new buffer."
  (interactive)
  (find-file user-emacs-directory))

(defun my-home-directory ()
  "Opens the home directory in a new buffer."
  (interactive)
  (find-file "~/"))

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
    (let ((new-name (generate-new-buffer-name (concat "*eshell: " name "*"))))
      (rename-buffer new-name))))

(defun dos2unix (buffer)
  "Remove all carriage return from BUFFER."
  (interactive "*b")
  (save-excursion
    (goto-char (point-min))
    (while (search-forward (string ?\C-m) nil t)
      (replace-match "" nil t))))

(require 'seq)
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
  (read-only-mode -1)
  (ansi-color-apply-on-region (point-min) (point-max))
  (read-only-mode 1))

(add-hook 'compilation-mode-hook 'my-compilation-custom-hook)
(add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dired Mode Improvements
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'dired-x)
(setq-default dired-omit-files-p t) ; Buffer-local variable
(setq dired-omit-files (concat "^\\..+\\|" (default-value 'dired-omit-files)))
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
      (dired-omit-mode 1)
    (dired-omit-mode -1)))

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

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(defcustom my/package-refresh-interval (* 7 24 60 60)
  "Minimum seconds between automatic package archive refreshes.
Defaults to one week (604800 seconds)."
  :type 'integer
  :group 'package)

(defun my/package-archives-stale-p ()
  "Return t if the package archive cache is absent or older than
`my/package-refresh-interval' seconds."
  (let ((cache (expand-file-name "archives/melpa/archive-contents" package-user-dir)))
    (or (not (file-exists-p cache))
        (> (float-time (time-since (file-attribute-modification-time
                                    (file-attributes cache))))
           my/package-refresh-interval))))

(when (my/package-archives-stale-p)
  (package-refresh-contents))

(require 'use-package)
(require 'bind-key)
(require 'use-package-ensure)
(setopt use-package-always-ensure t)
(setopt package-native-compile t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(use-package emacs
  :bind
  ("\C-w" . clipboard-kill-region)
  ("\M-w" . clipboard-kill-ring-save)
  ("\C-y" . clipboard-yank)
  ("\C-c," . scroll-bar-mode)
  ("\C-c." . tool-bar-mode)
  ("\C-c?" . menu-bar-mode)
  ("\C-c\\" . comment-or-uncomment-region)
  ("\C-cs" . eshell-here)
  ("\C-cp" . toggle-window-dedication)
  ([f12] . toggle-frame-fullscreen)
  ("\C-ch" . hippie-expand)
  ("\C-c\C-t" . next-theme)

  :custom
  (read-process-output-max (* 1024 1024))
  (auto-window-vscroll nil)
  (c-basic-offset 2)
  (column-number-mode t)
  (css-indent-offset 2)
  (debug-on-error nil)
  (electric-indent-mode nil)
  (kill-whole-line t)
  (indent-tabs-mode nil)
  (inhibit-startup-screen t)
  (js-indent-level 2)
  (make-backup-files nil)
  (scroll-bar-mode nil)
  (scroll-conservatively 10000)
  (scroll-step 2)
  (show-paren-delay 0)
  (show-trailing-whitespace t)
  (tab-stop-list (number-sequence 2 120 2))
  (tab-width 2)
  (tool-bar-mode nil)
  (require-final-newline t)
  (frame-inhibit-implied-resize t)
  (switch-to-buffer-obey-display-actions t)
  (warning-minimum-level :warning)

  :hook
  (prog-mode . hl-line-mode)
  (text-mode . visual-line-mode)

  :init
  (delete-selection-mode 1)
  (global-eldoc-mode t)
  (show-paren-mode t)
  (global-prettify-symbols-mode +1)
  (prefer-coding-system 'utf-8)

  (when (fboundp 'set-message-beep)
    (set-message-beep 'silent))

  ;; Do not allow the cursor in the minibuffer prompt
  (setopt minibuffer-prompt-properties
          '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Enable recursive minibuffers
  (setopt enable-recursive-minibuffers t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Useful Elisp Extensions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package dash)
(use-package f)

(use-package smerge-mode
  :ensure nil
  :hook
  ;; Activate only when a file actually contains conflict markers,
  ;; rather than scanning every prog-mode buffer unconditionally.
  (find-file . smerge-start-session))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Avy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package avy
  :bind (("C-." . avy-goto-char)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dumb Jump
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package dumb-jump
  :init
  (remove-hook 'xref-backend-functions #'etags--xref-backend)
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setopt xref-show-definitions-function #'xref-show-definitions-completing-read))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Neotree
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package neotree
  :bind (("<f8>" . neotree-project-dir)
         ("C-x p C-d" . neotree-project-dir))
  :config

  (defun neotree-dir-prompt ()
    "Prompt for directory and open NeoTree."
    (interactive)
    (let ((dir (read-directory-name "NeoTree directory: ")))
      (neotree-dir dir)))

  (defun neotree-project-dir ()
    "Open NeoTree using the project.el root."
    (interactive)
    (if (mode-visible-p 'neotree-mode)
        (neotree-hide)
      (let ((project-dir (or (and (project-current) (project-root (project-current)))
                             (neotree-dir-prompt)))
            (file (buffer-file-name)))
        (if project-dir
            (progn
              (neotree-dir project-dir)
              (neotree-find file))
          (message "No project is active."))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Smart Parens
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package smartparens
  :hook
  ((emacs-lisp-mode . smartparens-strict-mode)
   (ielm-mode . smartparens-strict-mode)
   (lisp-interaction-mode . smartparens-strict-mode)
   (lisp-mode . smartparens-strict-mode)
   (sly-mode . smartparens-strict-mode)
   (janet-mode . smartparens-strict-mode)
   (scheme-mode . smartparens-strict-mode)
   (geiser-repl-mode . smartparens-strict-mode)
   (geiser-mode . smartparens-strict-mode)
   (minibuffer-setup . show-smartparens-mode))
  :config
  (require 'smartparens-config)
  (sp-use-smartparens-bindings))

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
;; Ripgrep
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (executable-find "rg")
  (use-package rg
    :config
    (rg-enable-default-bindings)
    (define-key-after global-map [menu-bar tools rg]
      '(menu-item "Search Files (ripgrep)..." rg :help "Search files for strings or regexps (with ripgrep)...")
      'grep)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Minions (Minor Modes in Modeline)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package minions
  :config
  (minions-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Vertico (Minibuffer Interaction)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package vertico
  :config
  (vertico-mode 1)
  (setopt vertico-resize nil
          vertico-cycle t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Save History (Minibuffer)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package savehist
  :init
  (savehist-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Corfu (Completions)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package corfu
  :config
  (require 'corfu-info)
  (require 'corfu-history)
  (global-corfu-mode)
	(corfu-popupinfo-mode)
  (corfu-history-mode t)
  :custom
  (global-corfu-modes '((not org-mode) (not text-mode) t))
	(corfu-popupinfo-delay 0.5)
  (completion-cycle-threshold 3)
  (corfu-auto t)
  (corfu-cycle t)
  (corfu-on-exact-match nil)
  (corfu-preselect-first t)
  (corfu-preview-current t)
  (corfu-quit-at-boundary nil)
  (corfu-quit-no-match 'separator)
  (corfu-scroll-margin 5)
  (corfu-separator ?\s)
  (tab-always-indent 'complete)
  (corfu-excluded-modes '(org-mode text-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Orderless
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))
			                           (eglot (styles orderless)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flyspell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package flyspell
  :hook
  (text-mode . flyspell-mode)
  (prog-mode . flyspell-prog-mode))

(use-package ispell
  :init
  (when (and (eq 'windows-nt system-type) (executable-find "hunspell"))
    (setenv "DICTIONARY" "en_CA,en_US")
    (let ((dict-dir "C:\\Hunspell\\"))
      (setq
       ispell-hunspell-dict-paths-alist `(("en_CA" ,(concat dict-dir "en_CA.dic"))
                                          ("en_US" ,(concat dict-dir "en_US.dic")))
       ispell-really-hunspell t
       ispell-dictionary "en_CA"
       ispell-program-name (executable-find "hunspell")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flycheck
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package flycheck
  :hook
  (prog-mode . flycheck-mode)
  :init
	(setopt flycheck-idle-change-delay 0.5))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Magit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package magit
  :defer t
  :bind
  ("C-c g" . magit-status)
  :config
  ;; Windows performance tweaks
  (when (eq system-type 'windows-nt)
    (setq magit-process-connection-type nil))
  (setq magit-auto-revert-mode nil)
  (setq magit-revision-show-gravatars nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Eglot
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package eglot
  :demand t
  :init
  (setopt eglot-connect-timeout 240))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DAPE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package dape
  :preface
  ;; By default dape shares the same keybinding prefix as `gud'
  ;; If you do not want to use any prefix, set it to nil.
  ;; (setopt dape-key-prefix "\C-x\C-a")

                                        ;:hook
  ;; Save breakpoints on quit
  ;;((kill-emacs . dape-breakpoint-save)
  ;; Load breakpoints on startup
  ;; (after-init . dape-breakpoint-load))

  :config
  ;; Turn on global bindings for setting breakpoints with mouse
  (dape-breakpoint-global-mode)

  ;; Info buffers to the right
  (setopt dape-buffer-window-arrangement 'right)

  ;; Info buffers like gud (gdb-mi)
  ;; (setopt dape-buffer-window-arrangement 'gud)
  ;; (setopt dape-info-hide-mode-line nil)

  ;; Pulse source line (performance hit)
  (add-hook 'dape-display-source-hook 'pulse-momentary-highlight-one-line)

  ;; Showing inlay hints
  (setopt dape-inlay-hints t)

  ;; Save buffers on startup, useful for interpreted languages
  (add-hook 'dape-start-hook (lambda () (save-some-buffers t t)))

  ;; Kill compile buffer on build success
  (add-hook 'dape-compile-hook 'kill-buffer)

  ;; Projectile users
  ;; (setopt dape-cwd-fn 'projectile-project-root)
  )

;; Enable repeat mode for more ergonomic `dape' use
(use-package repeat
  :config
  (repeat-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AI
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom copilot-models '(claude-sonnet-4.6 claude-sonnet-4.5 claude-haiku-4.5 claude-opus-4.6 claude-opus-4.6-fast claude-opus-4.5 claude-sonnet-4 gemini-3-pro-preview gpt-5.3-codex gpt-5.2-codex gpt-5.2 gpt-5.1-codex-max gpt-5.1-codex gpt-5.1 gpt-5.1-codex-mini gpt-5-mini gpt-4.1)
  "List of available copilot models."
  :type '(repeat symbol)
  :group 'gptel)

(defcustom ollama-models nil
  "List of models available from the ollama CLI."
  :type '(repeat symbol)
  :group 'ollama)

(defcustom ollama-host "localhost:11434"
  "Host for the ollama server."
  :type 'string
  :group 'ollama)
(setopt ollama-host (or (getenv "OLLAMA_HOST") ollama-host))

(when (executable-find "ollama")
  (defun ollama-list-models ()
    "List all available models from the ollama CLI."
    (interactive)
    (let ((output (shell-command-to-string "ollama list")))
      (with-temp-buffer
        (insert output)
        (goto-char (point-min))
        ;; Skip the header line ("NAME   ID   SIZE   MODIFIED")
        (forward-line 1)
        (let ((models '()))
          (while (re-search-forward "^\\(\\S-+\\)\\s-+" nil t)
            (let ((model (intern (match-string 1))))
              (unless (member model models)
                (push model models))))
          models))))
  (if (or (not ollama-models) (not (listp ollama-models)))
      (setopt ollama-models (ollama-list-models))
    (setopt ollama-models (append ollama-models (ollama-list-models))))
  ;; Remove duplicates
  (setopt ollama-models (delete-dups ollama-models)))

(when (executable-find "copilot-language-server")
  (use-package copilot
    :hook ((prog-mode . copilot-mode)
           (text-mode . copilot-mode)
           (conf-mode . copilot-mode)
           (yaml-mode . copilot-mode)
           (json-mode . copilot-mode)
           (markdown-mode . copilot-mode)
           (org-mode . copilot-mode)
           (latex-mode . copilot-mode))
    :bind (("C-c <tab>" . 'copilot-accept-completion-by-paragraph)
           ("C-c S-<tab>" . 'copilot-accept-completion))
    :config
    (setopt copilot-indent-offset-warning-disable t)
    (setopt copilot-max-char -1)
    (mapc (lambda (entry)
            (add-to-list 'copilot-indentation-alist entry))
          '((org-mode 2)
            (text-mode 2)
            (conf-mode 2)
            (yaml-mode 2)
            (json-mode 2)
            (markdown-mode 2)
            (latex-mode 2)
            (prog-mode 2)))))

(use-package gptel
  :bind (("C-c a g" . gptel-send)
         ("C-c a G" . gptel-menu)
         ("C-c a a" . gptel-add)
         ("C-c a f" . gptel-add-file)
         ("C-c a e c" . 'gptel-enable-copilot)
         ("C-c a e o" . 'gptel-enable-ollama))
  :init
  (defun gptel-enable-copilot ()
    "Enables Copilot for GPTel."
    (interactive)
    (let ((copilot-backend (gptel-make-gh-copilot "Copilot") ))
      (setopt gptel-backend copilot-backend)
      (setopt gptel-model (car copilot-models)))
    (message "Copilot enabled for GPTel."))

  (when ollama-models
    (defun gptel-enable-ollama ()
      "Enables Ollama for GPTel."
      (interactive)
      (let ((ollama-backend
             (gptel-make-ollama "Ollama"
               :host ollama-host
               :stream t
               :models ollama-models)))
        (setopt gptel-backend ollama-backend)
        (setopt gptel-model (car ollama-models)))
      (message "Ollama enabled for GPTel."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Which Key?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package which-key
  :custom
  (which-key-show-early-on-C-h t)
  (which-key-idle-delay 2.0)
  (which-key-idle-secondary-delay 0.05)
  :config
  (which-key-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C/C++
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Default C style
(add-hook 'c++-mode-hook (lambda () (c-set-style "java")))
(add-hook 'c-mode-hook (lambda () (c-set-style "java")))

;; Extra C/C++ Mode Hooks
(add-to-list 'auto-mode-alist '("\\.ino?\\'" . c++-mode))

(when-let (clangd (executable-find "clangd"))
  (with-eval-after-load 'eglot
    (add-to-list
     'eglot-server-programs
     `((c-mode c++-mode)
       . (,clangd
          "--background-index"
          "--clang-tidy"
          "--completion-style=detailed"
          "--all-scopes-completion"
          "--pch-storage=memory")))
    (add-hook 'c-mode-hook 'eglot-ensure)
    (add-hook 'c++-mode-hook 'eglot-ensure)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when-let (dotnet (executable-find "dotnet"))
  ;; dotnet global tools install to ~/.dotnet/tools which is not on exec-path by default
  (let ((dotnet-tools (expand-file-name "~/.dotnet/tools")))
    (when (file-directory-p dotnet-tools)
      (add-to-list 'exec-path dotnet-tools)
      (setenv "PATH" (concat dotnet-tools path-separator (getenv "PATH")))))

  ;; Ensure language server tools are installed at startup
  (unless (executable-find "dotnet-script")
    (shell-command (concat "\"" dotnet "\" tool install -g dotnet-script")))
  (unless (or (executable-find "csharp-ls") (executable-find "OmniSharp"))
    (shell-command (concat "\"" dotnet "\" tool install -g csharp-ls")))

  (use-package csharp-mode
    :ensure t
    :mode ("\\.cs\\'" . csharp-mode))

  ;; Eglot is loaded eagerly (:demand t), so register server and hook at startup.
  (let* ((csharp-ls (executable-find "csharp-ls"))
         (omnisharp (executable-find "OmniSharp"))
         (alternatives
          (append
           (when csharp-ls `((,csharp-ls "-l" "error")))
           (when omnisharp `((,omnisharp "-lsp"))))))
    (when alternatives
      (add-to-list 'eglot-server-programs
                   `(csharp-mode . ,(eglot-alternatives alternatives)))))

  (add-hook 'csharp-mode-hook 'eglot-ensure)

  (with-eval-after-load 'csharp-mode
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
      ;; C# uses 4-space indent (overrides the global c-basic-offset of 2)
      (setq-local indent-tabs-mode nil)
      (setq-local comment-column 40)
      (setq-local c-basic-offset 4))
    (add-hook 'csharp-mode-hook #'my/csharp-mode-hook)

    (define-key csharp-mode-map (kbd "C-c C-z") 'my-csharp-repl))

  (use-package dotnet
    :config
    (add-hook 'csharp-mode-hook 'dotnet-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Janet
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (executable-find "janet")
  (use-package janet-mode
    :hook (janet-mode . smartparens-mode))
  (when (>= emacs-major-version 30)
    (use-package inf-janet
      :hook (janet-mode . inf-janet-minor-mode)
      :vc (:url "https://github.com/velkyel/inf-janet")))

  (when-let (janet-lsp (executable-find "janet-lsp"))
    (add-to-list 'eglot-server-programs
                 `(janet-mode . (,janet-lsp)))
    (add-hook 'janet-mode-hook 'eglot-ensure))

  (when (>= emacs-major-version 30)
    (use-package flycheck-janet
      :after (flycheck janet-mode)
      :demand t
      :vc (:url "https://github.com/sogaiu/flycheck-janet"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Common Lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (or (executable-find "sbcl")
          (executable-find "ccl")
          (executable-find "ecl"))
  (use-package sly
    :bind (:map sly-mode-map
                ("C-x 3 ." . xref-find-definitions))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Nim
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (executable-find "nim")
  (use-package nim-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Zig
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when-let (zig (executable-find "zig"))
  (use-package zig-mode
    :init
    (when-let (zls (executable-find "zls"))
      (with-eval-after-load 'eglot
        (add-hook 'zig-mode-hook 'eglot-ensure)
        (add-to-list 'eglot-server-programs
                     `(zig-mode . (,zls
                                   :initializationOptions
                                   (:zig_exe_path ,zig
                                                  :enable_build_on_save t))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ruby
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when-let (ruby (executable-find "ruby"))
  (use-package ruby-mode
    :hook (ruby-mode . inf-ruby-keys)
    :mode ("\\.rb\\'" . ruby-mode))

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

  (use-package robe
    :init
    (advice-add 'launch-ruby :after #'robe-start))

  (when-let (ruby-lsp (executable-find "ruby-lsp"))
    (with-eval-after-load 'eglot
      (add-to-list 'eglot-server-programs
                   `(ruby-mode . (,ruby-lsp)))
      (add-hook 'ruby-mode-hook 'eglot-ensure))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rust
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (executable-find "rustc")
  (use-package rust-mode)
  (when-let (rust-analyzer (executable-find "rust-analyzer"))
    (with-eval-after-load 'eglot
      (add-to-list 'eglot-server-programs
                   `(rust-mode . (,rust-analyzer)))
      (add-hook 'rust-mode-hook 'eglot-ensure))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TypeScript / JavaScript
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package typescript-mode
  :mode ("\\.ts\\'" "\\.tsx\\'"))

(when-let (tsls (executable-find "typescript-language-server"))
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 `((typescript-mode js-mode javascript-mode)
                   . (,tsls "--stdio")))
    (add-hook 'typescript-mode-hook 'eglot-ensure)
    (add-hook 'js-mode-hook 'eglot-ensure)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JSON
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package json-mode
  :mode ("\\.json\\'"))
(when-let (jsonls (executable-find "vscode-json-language-server"))
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 `(json-mode . (,jsonls "--stdio")))
    (add-hook 'json-mode-hook 'eglot-ensure)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CMake
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package cmake-mode
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode)))
(when-let (cmake-ls (executable-find "cmake-language-server"))
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 `(cmake-mode . (,cmake-ls)))
    (add-hook 'cmake-mode-hook 'eglot-ensure)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GLSL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package glsl-mode
  :mode (("\\.vert\\'" . glsl-mode)
         ("\\.frag\\'" . glsl-mode)
         ("\\.geom\\'" . glsl-mode)
         ("\\.comp\\'" . glsl-mode)
         ("\\.glsl\\'" . glsl-mode)
         ("\\.glslh\\'" . glsl-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scheme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package geiser)
(when (executable-find "chez")
  (use-package geiser-chez
    :after geiser))
(when (executable-find "chibi-scheme")
  (use-package geiser-chibi
    :after geiser))
(when (or (executable-find "chicken-csi") (executable-find "csi"))
  (use-package geiser-chicken
    :after geiser
    :config
    (setopt geiser-chicken-binary (or (executable-find "chicken-csi") (executable-find "csi")))))
(when (executable-find "gsi")
  (use-package geiser-gambit
    :after geiser))
(when (executable-find "gosh")
  (use-package geiser-gauche
    :after geiser))
(when (executable-find "guile")
  (use-package geiser-guile
    :after geiser))
(when (executable-find "kawa")
  (use-package geiser-kawa
    :after geiser))
(when (or (executable-find "mit-scheme") (executable-find "scheme"))
  (use-package geiser-mit
    :after geiser))
(when (executable-find "racket")
  (use-package geiser-racket
    :after geiser))
(when (executable-find "stklos")
  (use-package geiser-stklos
    :after geiser))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Markdown
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package markdown-mode
  :hook (markdown-mode . visual-line-mode)
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

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
  :mode (("\\.phtml\\'" . web-mode)
         ("\\.tpl\\'" . web-mode)
         ("\\.php\\'" . web-mode)
         ("\\.asp\\'" . web-mode)
         ("\\.gsp\\'" . web-mode)
         ("\\.jsp\\'" . web-mode)
         ("\\.aspx\\'" . web-mode)
         ("\\.ascx\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.djhtml\\'" . web-mode)
         ("\\.html\\'" . web-mode)
         ("\\.tsx\\'" . web-mode)
         ("\\.jsx\\'" . web-mode)))
(use-package css-mode)
(use-package verb)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Writing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package writegood-mode
  :hook (text-mode . writegood-mode))
(use-package writeroom-mode
  :config
  (setopt writeroom-fringes-outside-margins nil
          writeroom-extra-line-spacing 0.1)
  :init
  (defun my/writeroom-config ()
    (setq-local word-wrap t)
    (fringe-mode 0))
  (defun my/writeroom-disable ()
    (fringe-mode nil))
  (add-hook 'writeroom-mode-hook #'my/writeroom-config)
  (add-hook 'writeroom-mode-disable-hook #'my/writeroom-disable))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dictionary
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package dictionary
  :bind
  (("C-c d" . dictionary-search))
  :init
  (define-key-after global-map [menu-bar tools apps dictionary-search]
    '(menu-item "Dictionary" dictionary-search :help "Search dictionary") t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Thesaurus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package powerthesaurus
  :bind
  (("\C-c t" . powerthesaurus-lookup-word-dwim))
  :init
  (define-key-after global-map [menu-bar tools apps powerthesaurus-lookup-word]
    '(menu-item "Thesaurus" powerthesaurus-lookup-word :help "Search thesaurus") t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom my/org-directory (file-truename "~/org")
  "Location of org documents."
  :type 'directory
  :group 'org)

(use-package org
  :ensure t
  :hook
  (org-mode . org-num-mode)
  (org-mode . visual-line-mode)

  :bind
  ("C-c o c" . org-capture)
  ("C-c o a" . org-agenda)
  ("C-c o l" . org-store-link)
  ("C-c o b" . org-iswitchb)
  ("C-c o t" . org-todo-list)
  ("C-c o s" . org-search-view)
  ("C-c o m" . org-move-tree)
  ("C-c o d" . my-org-directory)
  ("C-c o i" . my-org-show-all-inline-images)

  :custom
  (org-directory my/org-directory)
  (org-default-notes-file
   (expand-file-name "notes.org" my/org-directory))
  (org-agenda-files
   `(,(expand-file-name "todo.org" my/org-directory)
     ,(expand-file-name "agenda.org" my/org-directory)))
  (org-agenda-diary-file
   (expand-file-name "diary.org" my/org-directory))
  (org-todo-keywords
   '((sequence "TODO(t)" "PROG(p)" "BLCK(b)" "STAL(s)" "|" "DONE(d)" "WONT(w)")))
  (org-todo-keyword-faces
   '(("TODO" . (:foreground "white" :weight bold))
     ("DOIN" . (:foreground "green" :weight bold))
     ("BLCK" . (:foreground "red" :weight bold))
     ("STAL" . (:foreground "yellow" :weight bold))
     ("WONT" . (:foreground "grey" :weight bold))
     ("DONE" . (:foreground "grey" :weight bold))))
  (org-capture-templates
   `(("n" "Note" entry (file+headline "notes.org" "Notes")
      "\n* %^{topic} %T %^g\n   :CATEGORY: %^{category}\n%i%?\n")
     ("t" "To Do" entry (file+headline "todo.org" "To Do")
      "\n* TODO %^{todo} %^g\n   DEADLINE: %^{due}t\n   :CATEGORY: %^{category}\n")
     ("d" "Daily review" entry (file+headline "diary.org" "Daily Review")
      (format
       "* %%T %%^g\n   :CATEGORY: Review\n   %%?%%[%s/template_daily_review.org]\n"
       my/org-directory))
     ("i" "Idea" entry (file+headline "ideas.org" "Ideas")
      "\n* %^{topic} %T %^g\n   :CATEGORY: Idea\n%i%?\n")
     ("j" "Journal" entry (file+headline "diary.org" "Journal")
      "\n* %^{topic} %T %^g\n   :CATEGORY: Journal\n%i%?\n")
     ("w" "Work Log" entry (file+headline "work.org" "Work Log")
      "\n* %^{topic} %T %^g\n   :CATEGORY: Log\n%i%?\n")
     ("e" "Event" entry (file+headline "agenda.org" "Events")
      "\n* %^{title} %^g\n     SCHEDULED: %^{when}t\n%i%?\n")
     ("c" "Contact" entry (file+headline "addresses.org" "Addresses")
      "\n* %(org-contacts-template-name)\n   :PROPERTIES:\n   :EMAIL: %(org-contacts-template-email)\n   :END:\n%i%?\n")))

  :init
  (defun my-org-show-all-inline-images ()
    (interactive)
    (org-display-inline-images t t))

  (defun org-move-tree (file-name)
    "Move the sub-tree to FILE-NAME and replace it with a link."
    (interactive "F")
    (org-mark-subtree)
    (let*
        ((title    (car (last (org-get-outline-path t))))
         (content  (buffer-substring (region-beginning) (region-end))))
      (delete-region (region-beginning) (region-end))
      (insert (format "** [[file:%s][%s]]\n" file-name title))
      (with-temp-buffer
        (insert content)
        (write-file file-name)))))

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
;; Themes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setopt custom-safe-themes t)

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
(use-package monochrome-theme :defer t)
(use-package modus-themes :defer t)

(defcustom default-theme 'modus-operandi-tinted
  "The default theme to load."
  :type 'symbol
  :group 'local)

(defun set-current-theme-as-default ()
  "Save the current theme as the default theme."
  (interactive)
  (when custom-enabled-themes
    (setopt default-theme (car custom-enabled-themes))
    (message "Set %s as default theme. Remember to save options before exiting." default-theme)))

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
  (let ((new-theme (seq-random-elt (custom-available-themes))))
    (message (format "Switching to theme %s" new-theme))
    (ignore-errors (change-theme new-theme))))

(defun random-dark-theme ()
  "Change to a random dark theme."
  (interactive)
  (let ((attempts 0)
        (max-attempts 20))
    (while (and (progn (random-theme) (not (background-is-dark)))
                (< attempts max-attempts))
      (setq attempts (1+ attempts)))
    (when (= attempts max-attempts)
      (message "Could not find a dark theme after %d attempts." max-attempts))))

(defun random-light-theme ()
  "Change to a random light theme."
  (interactive)
  (let ((attempts 0)
        (max-attempts 20))
    (while (and (progn (random-theme) (background-is-dark))
                (< attempts max-attempts))
      (setq attempts (1+ attempts)))
    (when (= attempts max-attempts)
      (message "Could not find a light theme after %d attempts." max-attempts))))

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
  "Return t if the current theme background is dark based on luminance."
  (interactive)
  (let ((rgb (color-name-to-rgb (face-attribute 'default :background))))
    (when rgb
      (let ((luminance (+ (* 0.299 (nth 0 rgb))
                          (* 0.587 (nth 1 rgb))
                          (* 0.114 (nth 2 rgb)))))
        (< luminance 0.5)))))

(defun load-default-theme ()
  "Load the default theme."
  (interactive)
  (custom-set-faces
   '(show-paren-match ((t (:background "gray15")))))
  (change-theme default-theme))

(add-hook 'after-init-hook 'load-default-theme)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extra Miscellaneous
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'auto-mode-alist '("\\.info\\'" . Info-on-current-buffer))

(provide 'local)
;;; local.el ends here
