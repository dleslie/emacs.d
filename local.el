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

(defun my-org-directory ()
  "Opens ORG-DIRECTORY in a new buffer."
  (interactive)
  (find-file org-directory))

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
(setopt dired-omit-files (concat dired-omit-files "\\|^\\..+$"))
(defvar v-dired-omit t
  "If dired-omit-mode enabled by default.  Don't setq me.")
(defun dired-omit-switch ()
  "This function is a small enhancement for `dired-omit-mode'.
It will \"remember\" omit state across Dired buffers."
  (interactive)
  (if (eq v-dired-omit t)
      (setopt v-dired-omit nil)
    (setopt v-dired-omit t))
  (dired-omit-caller)
  (revert-buffer))

(defun dired-omit-caller ()
  "Ensures dired-omit is working."
  (if v-dired-omit
      (setopt dired-omit-mode t)
    (setopt dired-omit-mode nil)))

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
(setopt package-archives
        '(("melpa" . "https://melpa.org/packages/") 
	        ("gnu" . "https://elpa.gnu.org/packages/"))
        package-archive-priorities
        '(("melpa" . 100)
          ("gnu" . 80)))

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
(setopt use-package-always-ensure t)
(setopt package-native-compile t)

(use-package auto-package-update
  :init
  (setopt auto-package-update-delete-old-versions t
          auto-package-update-hide-results t)
  (auto-package-update-maybe))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  ("\C-\\;" . hippie-expand)
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
  (backup-by-copying nil)
  (require-final-newline t)
  (frame-inhibit-implied-resize t)
  (switch-to-buffer-obey-display-actions t)
  (warning-minimum-level :emergency)

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
    (set-message-beep 'silent)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Useful Elisp Extensions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package dash)
(use-package f)

(use-package smerge-mode
  :ensure nil
  :hook
  (prog-mode . smerge-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ace Jump
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ace-jump-mode
  :bind (("C-." . ace-jump-mode)))

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
;; Ag
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ag
  :init
  (define-key-after global-map [menu-bar tools ag]
    '(menu-item "Search Files (ag)..." ag :help "Search files for strings or regexps (with ag)...")
    'grep))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ido
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ido
  :config
  (setopt
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
;; Vertico (Minibuffer Interaction)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package vertico
  :config
  (vertico-mode 1)
  (setopt vertico-resize nil
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
  (setopt minibuffer-prompt-properties
          '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Enable recursive minibuffers
  (setopt enable-recursive-minibuffers t))

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
  :init
  (global-corfu-mode)
	(corfu-popupinfo-mode)
  (corfu-history-mode t)
  :config
  (require 'corfu-info)
  (require 'corfu-history)
  :custom
  (global-corfu-modes '((not org-mode) (not text-mode) t))
	(corfu-auto t)
	(corfu-popupinfo-delay 0.5)
	(corfu-quit-no-match t)
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
  :init
  (icomplete-mode)
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
  (prog-mode . flyspell-prog-mode)
  (org-mode . flyspell-mode))

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
  :bind
  ("C-c g" . magit-status))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Eglot
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package eglot
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
    "List all available models from the ollama CLI"
    (interactive)
    (let ((output (shell-command-to-string "ollama list")))
      (with-temp-buffer
        (insert output)
        (goto-char (point-min))
        (let ((models '()))
          ;; Split output by whitespace, model is first column
          (while (re-search-forward "^\\(\\S-+\\)\\s-+" nil t)
            ;; Skip if model is "NAME"
            (when (string= (match-string 1) "NAME")
              (forward-line))
            ;; Intern model name as symbol
            (let ((model (intern (match-string 1))))
              ;; Add model to list if not already present
              (unless (member model models)
                (push model models))))
          models))))
  (if (or (not ollama-models) (not (listp ollama-models)))
      (setopt ollama-models (ollama-list-models))
    (setopt ollama-models (append ollama-models (ollama-list-models))))
  ;; Remove duplicates
  (setopt ollama-models (delete-dups ollama-models)))

(when (and (executable-find "copilot-language-server"))
  (use-package copilot
    :bind (("C-c <tab>" . 'copilot-accept-completion)
           ("C-c S-<tab>" . 'copilot-accept-completion-by-word))
    :hook ((prog-mode . copilot-mode)
           (text-mode . copilot-mode)
           (conf-mode . copilot-mode)
           (yaml-mode . copilot-mode)
           (json-mode . copilot-mode)
           (markdown-mode . copilot-mode)
           (org-mode . copilot-mode)
           (latex-mode . copilot-mode))
    :config
    (add-to-list 'copilot-indentation-alist
                 '(org-mode 2)))

  (use-package copilot-chat
    :after copilot
    :bind
    (:map global-map
          ("C-c a c c" . copilot-chat-display)
          ("C-c a c a" . copilot-chat-add-current-buffer)
          ("C-c a c S-a" . copilot-chat-del-current-buffer)
          ("C-c a c e" . copilot-chat-explain)
          ("C-c a c d" . copilot-chat-doc)
          ("C-c a c r" . copilot-chat-review)
          ("C-c a c f" . copilot-chat-fix)
          ("C-c a c o" . copilot-chat-optimize)
          ("C-c a c t" . copilot-chat-test)
          ("C-c a c m" . copilot-chat-insert-commit-message)
          ("C-c C-y" . copilot-chat-yank)
          ("C-c M-y" . copilot-chat-yank-pop)
          ("C-c C-M-y" . (lambda () (interactive) (copilot-chat-yank-pop -1))))))

(use-package elysium
  :after gptel
  :bind (("C-c a e q" . elysium-query)
         ("C-c a e c" . elysium-add-context)
         ("C-c a e w" . elysium-toggle-window))
  :custom
  (elysium-window-size 0.33)
  (elysium-window-style 'vertical))

(use-package mcp
  :after gptel
  :ensure t
  :hook (after-init-hook . mcp-hub-start-all-server)
  :config
  (require 'gptel-integrations)
  (require 'mcp-hub)
  ;(require 'gptel-integrations)
  :custom
  (mcp-hub-servers
   `(("github" . (:command "npx" :args ("-y" "@modelcontextprotocol/server-github")))
     ("fetch" . (:command "uv" :args ("tool" "run" "mcp-server-fetch"))))))

(use-package gptel
  :bind (("C-c a g c" . gptel)
         ("C-c a g a" . gptel-add)
         ("C-c a g f" . gptel-add-file)
         ("C-c a g m" . gptel-menu))
  :init
  (when ollama-models
    (setopt gptel-backend
            (gptel-make-ollama "Ollama"
              :host ollama-host
              :stream t
              :models ollama-models))
    (when (or (not (boundp gptel-model))
              (not gptel-model)
              (not (member gptel-model ollama-models)))
      (setopt gptel-model (car ollama-models)))))

;; To try: ancilla, ellama

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
;; Treesit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (and (fboundp 'treesit-available-p)
           (treesit-available-p))

  (use-package treesit-langs
    :straight (:type git :host github :repo "emacs-tree-sitter/treesit-langs")
    :config
    (treesit-langs-major-mode-setup)

    (dolist
        (mapping
         '((python-mode . python-ts-mode)
           (c-mode . c-ts-mode)
           (cpp-mode . cpp-ts-mode)
           (css-mode . css-ts-mode)
           (rust-mode . rust-ts-mode)
           (csharp-mode . csharp-ts-mode)
           (typescript-mode . typescript-ts-mode)
           (js2-mode . js-ts-mode)
           (bash-mode . bash-ts-mode)
           (conf-toml-mode . toml-ts-mode)
           (go-mode . go-ts-mode)
           (css-mode . css-ts-mode)
           (janet-mode . janet-ts-mode)
           (json-mode . json-ts-mode)
           (js-json-mode . json-ts-mode)))
      (add-to-list 'major-mode-remap-alist mapping))))

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
     `((c-mode c++-mode c-ts-mode c++-ts-mode)
       . (,clangd
          "--background-index"
          "--clang-tidy"
          "--completion-style=detailed"
          "--all-scopes-completion"
          "--pch-storage=memory")))
    (add-hook 'c-mode-hook 'eglot-ensure)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when-let (dotnet (executable-find "dotnet"))
  (use-package csharp-mode
    :init
    (let* ((dotnet-script (executable-find "dotnet-script"))
           (omnisharp (executable-find "OmniSharp"))
           (csharp-ls (executable-find "csharp-ls"))
           (alternatives '()))

      (when (not dotnet-script)
        (shell-command (concat "\"" dotnet "\" tool install -g dotnet-script")))
      (when (and (not csharp-ls) (not omnisharp))
        (shell-command (concat "\"" dotnet "\" tool install -g csharp-ls"))
        (setopt csharp-ls (executable-find "csharp-ls")))

      (when csharp-ls
        (add-to-list 'alternatives `(,csharp-ls "-l" "error")))
      (when omnisharp
        (add-to-list 'alternatives `(,omnisharp "-lsp")))
      (when alternatives
        (add-to-list 'eglot-server-programs `(csharp-mode . ,(eglot-alternatives alternatives)))
        (add-hook 'csharp-mode-hook 'eglot-ensure)))

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

  (use-package dotnet
    :after csharp-mode
    :config
    (add-hook 'csharp-mode-hook 'dotnet-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Janet
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (executable-find "janet")
  (if (and (not (eq 'windows-nt system-type))
           (fboundp 'treesit-available-p)
           (treesit-language-available-p 'janet-simple))
      (progn
        (use-package janet-ts-mode
          :hook (janet-ts-mode . smartparens-mode)
          :straight (:type git :host github :repo "sogaiu/janet-ts-mode" :files ("*.el")))
        (use-package ajrepl
          :hook (janet-ts-mode . ajrepl-interaction-mode)
          :straight (:type git :host github :repo "sogaiu/ajrepl" :files ("*.el" "ajrepl"))))
    (progn
      (use-package janet-mode
        :hook (janet-mode . smartparens-mode))
      (use-package inf-janet
        :hook (janet-mode . inf-janet-minor-mode)
        :straight (:type git :host github :repo "velkyel/inf-janet"))))

  (when-let (janet-lsp (executable-find "janet-lsp"))
    (add-to-list 'eglot-server-programs
                 `((janet-ts-mode janet-mode) . (,janet-lsp)))
    (add-hook 'janet-mode-hook 'eglot-ensure))

  (use-package flycheck-janet
    :straight (:type git :host github :repo "sogaiu/flycheck-janet" :files ("*.el"))))

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

(when (executable-find "rustc")
  (use-package rust-mode)
  (use-package flycheck-rust
    :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scheme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package geiser)
(use-package geiser-chez
  :after geiser)
(use-package geiser-chibi
  :after geiser)
(use-package geiser-chicken
  :after geiser
  :config
  (setopt geiser-chicken-binary (or (executable-find "chicken-csi") (executable-find "csi"))))
(use-package geiser-gambit
  :after geiser)
(use-package geiser-gauche
  :after geiser)
(use-package geiser-guile
  :after geiser)
(use-package geiser-kawa
  :after geiser)
(use-package geiser-mit
  :after geiser)
(use-package geiser-racket
  :after geiser)
(use-package geiser-stklos
  :after geiser)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Markdown
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package markdown-mode
  :hook (markdown-mode . visual-line-mode)
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
(use-package typescript-mode)
(use-package tide
  :after typescript-mode
  :hook (typescript-mode . tide-setup)
  :hook (tide-mode . tide-hl-identifier-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Writing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package writegood-mode
  :hook (text-mode . writegood-mode))
(use-package writeroom-mode
  :config
  (setopt writeroom-fringes-outside-margins 0
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

(defcustom org-directory (file-truename "~/org") "Location of org documents."
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
  (org-default-notes-file
   (concat (file-truename org-directory) "notes.org"))
  (org-agenda-files
   `(,(concat (file-truename org-directory) "todo.org") ,(concat (file-truename org-directory) "agenda.org")))
  (org-agenda-diary-file
   (concat (expand-file-name org-directory) "diary.org"))
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
     ("w" "Work Log" entry (file+headline "work.org" "Work Log")
      "* %^{topic} %T %^g\n   :CATEGORY: Log\n   %i%?\n")
     ("e" "Event" entry (file+headline "agenda.org" "Events")
      "* %^{title} %^g\n     SCHEDULED: %^{when}t\n   %i%?\n")
     ("c" "Contact" entry (file+headline "addresses.org" "Addresses")
      "* %(org-contacts-template-name)\n   :PROPERTIES:\n   :EMAIL: %(org-contacts-template-email)\n   :END:\n   %i%?\n")))

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

(defcustom default-theme 'modus-operandi-tinted
  "The default theme to load."
  :type 'symbol
  :group 'local)

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
