;;; init.el --- Startup Script

;;; Commentary:

;; Emacs configuration of Dan Leslie.
;; dan@ironoxide.ca

;;; Code:

(defvar my-init-start-time (float-time))

(defvar enable-company t)
(defvar enable-global t)

(defvar lisp-implementations
  '((sbcl "sbcl" run-sbcl)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custom functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-exe (name)
  "Locate an executable with NAME on the system."
  (let ((found
         (if (string= system-type "windows-nt")
             (or
              (executable-find name)
              (executable-find	(format "%s.exe" name))
              (executable-find	(format "%s.bat" name)))
           (executable-find name))))
    (when (not found)
      (message (format "%s could not be located." name)))
    found))

(defmacro when-find-exe (exe &rest body)
  "When EXE is found execute BODY."
  (declare (indent 1) (debug t))
  `(if (find-exe ,exe) ,(cons 'progn body) (warn (format "Could not locate %s" ,exe))))

(defun execute-cmd (name args)
  "Execute NAME with ARGS."
  (shell-command (format "%s %s" (find-exe name) args)))

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

(defmacro when-set-and-true (sym &rest body)
  "When SYM exists and is t execute BODY."
  (declare (indent 1) (debug t))
  `(when (and ,(boundp sym) ,sym)
     ,(cons 'progn body)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-perf-metrics "packages"
  (package-initialize)
  (require 'package)
  (setq package-archives
        '(("gnu" . "https://elpa.gnu.org/packages/")
          ("melpa" . "https://melpa.org/packages/")
          ("org" . "https://orgmode.org/elpa/")
          ("elpy" . "https://jorgenschaefer.github.io/packages/"))))

(dolist (dir (directory-files  (expand-file-name "lisp" user-emacs-directory) t "[^\\.]"))
  (add-to-list 'load-path dir))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; use-package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-perf-metrics "use-package"
  (require 'use-package)
  (require 'bind-key)

  (setq use-package-always-ensure t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; miscellaneous tools
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-perf-metrics "tools"
  (use-package f)
  (use-package dash)
  (when (or (find-exe "ispell") (find-exe "aspell"))
    (add-hook 'text-mode-hook 'flyspell-mode))
  (define-key-after global-map [menu-bar tools apps]
    (cons "Apps" (make-sparse-keymap "applications")) 'games))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compilation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-perf-metrics "compilation"
  (require 'ansi-color)
  
  (defun my-compilation-custom-hook ()
    (visual-line-mode 1))
  (defun my-colorize-compilation-buffer ()
    (read-only-mode nil)
    (ansi-color-apply-on-region (point-min) (point-max))
    (read-only-mode t))

  (add-hook 'compilation-mode-hook 'my-compilation-custom-hook)
  (add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; company
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-perf-metrics "company"
  (when-set-and-true enable-company
    (use-package company
      :bind
      (:map company-mode-map ("<C-tab>" . company-complete))
      :init
      (setq company-tooltip-align-annotations t)
      
      (global-company-mode)

      (setq company-idle-delay 0.25)
      (setq company-backends (remove 'company-clang company-backends))
      (setq company-backends (remove 'company-semantic company-backends))

      (defun my-company-ispell-hook ()
        (make-local-variable 'company-backends)
        (push 'company-ispell company-backends))
      
      (add-hook 'text-mode-hook 'my-company-ispell-hook))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; git
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-perf-metrics "git"
  (when-find-exe "git"
    (use-package gist)
    
    (use-package magit
      :bind (("C-c g" . magit-status))
      :init
      (setq magit-last-seen-setup-instructions "1.4.0"))

    (use-package git-gutter
      :init
      (global-git-gutter-mode t)
      (setq git-gutter:visual-line t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; projectile
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-perf-metrics "projectile"
  (use-package projectile
    :init
    (setq
     projectile-switch-project-action 'projectile-find-dir
     projectile-find-dir-includes-top-level t)

    (projectile-global-mode t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parenthesis
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-perf-metrics "parenthesis"
  (use-package highlight-parentheses
    :init
    (define-globalized-minor-mode global-highlight-parentheses-mode
      highlight-parentheses-mode
      (lambda ()
        (highlight-parentheses-mode t)))
    (global-highlight-parentheses-mode t))

  (defvar-local paren-modes
    '(emacs-lisp-mode-hook
      eval-expression-minibuffer-setup-hook
      ielm-mode-hook
      lisp-mode-hook
      lisp-interaction-mode-hook
      scheme-mode-hook
      slime-repl-mode-hook
      clojure-mode-hook))
  
  (use-package rainbow-delimiters
    :init
    (mapc
     (lambda (mode-hook)
       (add-hook mode-hook #'rainbow-delimiters-mode))
     (append paren-modes '(c-mode-hook c++-mode-hook))))
  
  (use-package paredit
    :init
    (mapc
     (lambda (mode-hook)
       (add-hook mode-hook #'paredit-mode))
     paren-modes)
    (with-eval-after-load "eldoc"
      (eldoc-add-command
       'paredit-backward-delete
       'paredit-close-round))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; docker
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-perf-metrics "docker"
  (require 'dockerfile-mode)
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; markdown
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-perf-metrics "markdown"
  (when-find-exe "markdown"
    (use-package markdown-mode
      :init
      (defun my-custom-markdown-mode ()
        (interactive)
        (visual-line-mode 1)
        (markdown-mode))
      (add-to-list 'auto-mode-alist '("\\.markdown\\'" . my-custom-markdown-mode))
      (add-to-list 'auto-mode-alist '("\\.md\\'" . my-custom-markdown-mode)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-perf-metrics "org"
  (use-package org
    :bind
    (("C-c t" . org-todo-list)
     ("C-c l" . org-store-link)
     ("C-c a" . org-agenda)
     ("C-c b" . org-iswitchb)
     ("C-c c" . org-capture))

    :init
    (require 'org)
    
    (with-perf-metrics "org addons"
      (use-package ox-asciidoc)
      (use-package ox-epub)
      (use-package ox-gfm)
      (use-package ox-html5slide)
      (use-package ox-impress-js)
      (use-package ox-jira)
      (use-package ox-mediawiki)
      (use-package ox-minutes)
      (use-package ox-nikola)
      (use-package ox-reveal)
      (use-package ox-rst)
      (use-package ox-textile)
      (use-package ox-trac)
      (use-package ox-tufte)
      (use-package ox-twbs)
      (use-package ox-twiki))

    (setq
     org-mobile-directory (f-join (file-truename org-directory) "mobile")
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
        "* %T %^g\n   :CATEGORY: Review\n   %?%[~/ownCloud/org/template_daily_review.org]\n")
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

    (mkdir org-mobile-directory t)
    
    (let ((gcal-settings (expand-file-name "gcal-settings.el" user-emacs-directory)))
      (when (file-exists-p gcal-settings)
        (use-package org-gcal
          :init
          (load gcal-settings)
          (defun update-gcal ()
            (interactive)
            (message "Updating Calendar")
            (org-gcal-fetch))
          (update-gcal))))

    (defun my-org-save-hook ()
      (when (eq major-mode 'org-mode)
        (dolist (file (org-mobile-files-alist))
          (when (string= (file-truename (expand-file-name (car file)))
                         (file-truename (buffer-file-name)))
            (org-mobile-push)
            (with-eval-after-load "magit"
              (dolist (gitfile (magit-unstaged-files))
                (magit-stage-file gitfile))
              (magit-commit (list "-m" "org auto-commit"))
              (magit-git-push (magit-get-current-branch)
                              (magit-get-upstream-branch)
                              (magit-push-arguments)))))))

    (defun my-org-load-hook ()
      (when (eq major-mode 'org-mode)
        (dolist (file (org-mobile-files-alist))
          (when (string= (file-truename (expand-file-name (car file)))
                         (file-truename (buffer-file-name)))
            (with-eval-after-load "magit"
              (magit-git-pull (magit-get-current-branch)
                              (magit-pull-arguments)))
            (org-mobile-pull)
            (revert-buffer)))))
    
    (defun my-custom-org-hook ()
      (interactive)
      (visual-line-mode t))
    
    (add-hook 'org-mode-hook 'my-custom-org-hook)
    (add-hook 'after-save-hook 'my-org-save-hook)
    (add-hook 'after-find-file 'my-org-load-hook)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LSP Languages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-perf-metrics "lsp"
  (use-package lsp-mode
    :init
    (use-package lsp-ui
      :init
      (add-hook 'lsp-mode-hook 'lsp-ui-mode))
    (with-eval-after-load "company"
      (use-package company-lsp
        :init
        (push 'company-lsp company-backends)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; javascript
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-perf-metrics "javascript"
  (use-package typescript-mode
    :init
    (when (not (find-exe "tsc"))
      (execute-cmd (find-exe "npm") "install -g typescript")))
  (use-package js2-mode
    :init
    (add-to-list 'auto-mode-alist '("\\.js?\\'" . js2-mode)))
  (with-eval-after-load "lsp"
    (when (not (find-exe "html-languageserver"))
      (execute-cmd (find-exe "npm") "install -g vscode-html-languageserver-bin"))
    (add-hook 'js2-mode-hook #'lsp)
    (add-hook 'typescript-mode-hook #'lsp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; css
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-perf-metrics "css"
  (with-eval-after-load "lsp"
    (when (find-exe "npm")
      (when (not (find-exe "css-languageserver"))
        (execute-cmd (find-exe "npm") "install -g vscode-css-languageserver-bin")))
    (add-hook 'css-mode-hook #'lsp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bash
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-perf-metrics "bash"
  (with-eval-after-load "lsp"
    (when (find-exe "npm")
      (when (not (find-exe "bash-language-server"))
        (execute-cmd (find-exe "npm") "install -g bash-language-server")))
    (add-hook 'sh-mode-hook #'lsp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rust
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-perf-metrics "rust"
  (when (and (find-exe "cargo") (find-exe "rustup"))
    (use-package rust-mode
      :init

      (use-package toml-mode)
      (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
      (with-eval-after-load "lsp"
        (when (not (find-exe "rls"))
          (execute-cmd "rustup" "component add rls"))
        (add-hook 'rust-mode-hook #'lsp)))))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; non-LSP Languages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; go
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-perf-metrics "go"
  (when-find-exe "go"
    (use-package go-mode
      :init
      (when (not (find-exe "gometalinter"))
        (execute-cmd "go" "get -u github.com/alecthomas/gometalinter")
        (execute-cmd "gometalinter" "--install --update"))

      (when (not (find-exe "gocode"))
        (execute-cmd "go" "get -u github.com/nsf/gocode"))
      
      (use-package go-eldoc
        :init
        (add-hook 'go-mode-hook 'go-eldoc-setup))

      (with-eval-after-load "company"
        (use-package company-go
          :init
          (push 'company-go company-backends))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; meson
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-perf-metrics "meson"
  (when-find-exe "meson"
    (use-package meson-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; c-sharp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-perf-metrics "c-sharp"
  (use-package csharp-mode
    :init
    ;; Run omnisharp-install-server
    (use-package omnisharp
      :bind
      (:map csharp-mode-map
            ("M-." . omnisharp-go-to-definition)
            ("C-c C-c" . recompile))
      :init
      (add-hook 'csharp-mode-hook 'omnisharp-mode)
      (with-eval-after-load 'company
        (push 'company-omnisharp company-backends)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; haskell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-perf-metrics "haskell"
  (when-find-exe "stack"
    (use-package haskell-mode
      :init
      (use-package ghc
        :init
        (autoload 'ghc-init "ghc" nil t)
        (autoload 'ghc-debug "ghc" nil t)
        (add-hook 'haskell-mode-hook #'ghc-init))

      (with-eval-after-load "company"
        (use-package company-ghc
          :init
          (push 'company-haskell company-backends))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-perf-metrics "lisp"
  (use-package slime
    :init
    (require 'slime-autoloads)
    
    (setq slime-contribs '(slime-fancy))

    (dolist (impl lisp-implementations)
      (let ((sym (first impl))
            (exe (find-exe (second impl)))
            (run (third impl)))
        (when exe
          (when (or (not (boundp 'inferior-lisp-program)) (not inferior-lisp-program))
            (setq inferior-lisp-program exe))

          (when (or (not (boundp 'slime-lisp-implementations))
                    (not (listp slime-lisp-implementations)))
            (setq slime-lisp-implementations '()))
          (add-to-list 'slime-lisp-implementations `(,sym (,exe) :coding-system utf-8-unix))
          (eval `(defun ,run ()
                   (interactive)
                   (slime ',sym))))))

    (slime-setup)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; python
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-perf-metrics "python"
  (use-package anaconda-mode
    :init
    (add-hook 'python-mode-hook 'anaconda-mode)
    (add-hook 'python-mode-hook 'anaconda-eldoc-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ruby
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-perf-metrics "ruby"
  (when-find-exe "gem"
    (use-package inf-ruby
      :init
      (use-package enh-ruby-mode)
      
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

      (with-eval-after-load "projectile"
        (use-package projectile-rails
          :init
          (advice-add 'projectile-rails-console :before #'kill-ruby)
          (advice-add 'launch-ruby :after #'projectile-rails-on)
          (advice-add 'kill-ruby :after #'projectile-rails-off)))

      (use-package robe
        :init
        (when (not (find-exe "pry"))
          (execute-cmd "gem" "install pry pry-doc"))
        (advice-add 'launch-ruby :after #'robe-start))
      
      (with-eval-after-load "company"
        (use-package company-inf-ruby
          :init
          (push 'company-jedi company-backends))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; web-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-perf-metrics "web"
  (use-package restclient
    :init
    (with-eval-after-load "company"
      (use-package company-restclient
        :init
        (push 'company-restclient company-backends))))

  (use-package web-mode
    :init
    (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))

    (defun my-tsx-web-mode-hook ()
      (when (string-equal "tsx" (file-name-extension buffer-file-name))
        (setup-tide-mode)))

    (defun my-jsx-web-mode-hook ()
      (when (string-equal "jsx" (file-name-extension buffer-file-name))
        (setup-tide-mode)))

    (add-hook 'web-mode-hook #'my-tsx-web-mode-hook)
    (add-hook 'web-mode-hook #'my-jsx-web-mode-hook)

    (with-eval-after-load "company"
      (use-package company-web
        :init
        (push 'company-web-html company-backends)
        (push 'company-web-jade company-backends)
        (push 'company-web-slim company-backends)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; scheme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-perf-metrics "scheme"
  (use-package geiser))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; toml
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-perf-metrics "toml"
  (use-package toml-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; c and c++
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-perf-metrics "c and c++"
  (add-hook 'c++-mode-hook (lambda () (eldoc-mode 1)))
  (add-hook 'c-mode-hook (lambda () (eldoc-mode 1)))
  (add-to-list 'auto-mode-alist '("\\.ino?\\'" . c++-mode))

  (when (find-exe "global")
    (use-package ggtags
      :init
      (add-hook 'c-mode-common-hook
                (lambda ()
                  (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
                    (ggtags-mode 1))))

      (push 'company-gtags company-backends )
      
      (define-key ggtags-mode-map (kbd "C-c g s") 'ggtags-find-other-symbol)
      (define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-tag-history)
      (define-key ggtags-mode-map (kbd "C-c g r") 'ggtags-find-reference)
      (define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
      (define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
      (define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags)

      (define-key ggtags-mode-map (kbd "M-,") 'pop-tag-mark))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; look and feel
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-perf-metrics "look and feel"
  (define-key-after global-map [menu-bar tools apps eww]
    '(menu-item "Browse the Web (eww)" eww :help "Browse the web with eww") t)
  (define-key-after global-map [menu-bar tools apps erc]
    '(menu-item "IRC (erc)" erc :help "Use IRC via erc") t)
  (define-key-after global-map [menu-bar tools apps rcirc]
    '(menu-item "IRC (rcirc)" erc :help "Use IRC via rcirc") t)
  
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
    (add-hook 'text-mode-hook 'writegood-mode)
    (define-key text-mode-map [menu-bar text writeroom-mode]
      '(menu-item "Writegood" writegood-mode
                  :button (:toggle . (and (boundp 'writegood-mode) writegood-mode)))))
  
  (use-package smex
    :bind (("M-x" . smex)))

  (use-package ido
    :init
    (setq
     ido-create-new-buffer 'always
     ido-enable-flex-matching t
     ido-everywhere t))

  (use-package nyan-mode
    :init
    (nyan-mode t)
    (setq nyan-animate-nyancat t))

  (when-find-exe "ag"
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

  (use-package ace-jump-mode
    :bind
    (("C-c SPC" . ace-jump-mode)
     ("C-x SPC" . ace-jump-mode-pop-mark))
    :init
    (use-package ace-link)
    
    (autoload 'ace-jump-mode "ace-jump-mode" "Emacs quick move minor mode" t)
    (autoload 'ace-jump-mode-pop-mark "ace-jump-mode" "Ace jump back:-)" t)

    (ace-link-setup-default)

    (define-key-after global-map [menu-bar edit ace-menu]
      (cons "Ace Jump" (make-sparse-keymap "ace jump")) 'goto)
    (define-key global-map [menu-bar edit ace-menu jump]
      '(menu-item "Jump" ace-jump-mode :help "Ace Jump"))
    (define-key global-map [menu-bar edit ace-menu back]
      '(menu-item "Back" ace-jump-mode-pop-mark :help "Ace Jump Back")))
  
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
  
  (use-package doom-themes)
  
  (defun my-try-to-add-imenu ()
    (condition-case nil (imenu-add-to-menubar "Imenu") (error nil)))
  (add-hook 'font-lock-mode-hook 'my-try-to-add-imenu))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; customizable values
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-perf-metrics "customizations"
  (reset-theme)
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (unless (file-exists-p custom-file)
    (shell-command (concat "touch " custom-file)))
  (load custom-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Enable GC
(setq gc-cons-threshold 16777216
      gc-cons-percentage 0.1)

;; Enable file handler
(setq file-name-handler-alist default-file-name-handler-alist)

(garbage-collect)

(message (format "Init completed in %s seconds." (- (float-time) my-init-start-time)))

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; init.el ends here
