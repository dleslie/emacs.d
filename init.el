(load (expand-file-name "configuration.el" user-emacs-directory))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Init File Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d/lisp/")

(setq my-optional-init
      '(company
	compilation
	elisp
	email
	;; clojure
	geiser
	;; chicken
	js2
	ruby
	python
	c++
	haskell
	ocaml
	;; lisp
	markdown
	magit
	mu4e
	org
	paredit
	projectile
	semantic
	smex
	text
	rust
	racer
	web))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customizable Values
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(unless (file-exists-p custom-file)
  (shell-command (concat "touch " custom-file)))
(load custom-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Defining Custom Functions")

(defun reset-theme ()
  (interactive)
  (while custom-enabled-themes
    (disable-theme (car custom-enabled-themes))))

(defun override-theme (arg)
  "Disables all enabled themes and then loads the provided theme."
  (interactive
   (list
    (intern (completing-read "Load custom theme: "
                             (mapcar 'symbol-name (custom-available-themes))))))
  (reset-theme)
  (load-theme arg t))

(setq my-first-boot-package-inited nil)

(defun require-package (package-name &rest remaining-packages)
  "Loads and imports packages, installing from ELPA if necessary"
  (with-demoted-errors
      "Error: %S"
    
    (unless (package-installed-p package-name)
      (unless my-first-boot-package-inited
	(package-refresh-contents)
	(setq my-first-boot-package-inited t))

      (package-install package-name))

    ;; Some themes auto-apply themselves when required, which is uncool
    (unless (string-match "-theme" (symbol-name package-name))
      (require package-name nil 'noerror))
    
    (cons package-name
	  (cond
	   ((equal remaining-packages nil) nil)
	   (t (apply 'require-package remaining-packages))))))

;; From:
;; http://www.howardism.org/Technical/Emacs/eshell-fun.html
(defun eshell-here ()
  "Opens up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier."
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

;; Because eval-after-load is always sufficient
(defmacro with-optional-init (feature &rest body)
  `(when
       (if (listp ,feature)
	   (every (lambda (v) (memq v my-optional-init)) ,feature)
	 (memq ,feature my-optional-init))
     (with-demoted-errors
	 "Error: %S"
       ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Email
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-optional-init
 'email
 (require 'smtpmail)

 (setq
  message-send-mail-function 
  'smtpmail-send-it
  smtpmail-stream-type
  'starttls
  smtpmail-starttls-credentials
  '((mail-smtp-server mail-smtp-port nil nil))
  smtpmail-auth-credentials
  '((mail-smtp-server mail-smtp-port
		      user-mail-login nil))
  smtpmail-default-smtp-server
  mail-smtp-server
  smtpmail-smtp-server
  mail-smtp-server
  smtpmail-smtp-service
  mail-smtp-port
  gnus-ignored-newsgroups
  "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Semantic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-optional-init
 'semantic 
 (message "Configuring Semantic and CEDET")

 ;; (require 'srecode)
 ;; (require 'srecode/map)
 ;; (require 'eieio-opt)

 (require 'semantic)
 (require 'semantic/bovine/gcc)
 ;; (require 'semantic/ia)
 ;; (require 'semantic/sb)

 (mapc #'(lambda (s) (semantic-add-system-include s))
       system-include-paths)

 (semanticdb-enable-gnu-global-databases 'c-mode)
 (semanticdb-enable-gnu-global-databases 'c++-mode))

(defun enable-semantic-mode ()
  (interactive)
  (with-optional-init
   'semantic
   (semanticdb-minor-mode 1)
   (semantic-idle-scheduler-mode 1)
   (semantic-idle-summary-mode 1)
   (semantic-idle-scheduler-mode 1)
   (semantic-mode 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fix TLS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; From:
;; https://glyph.twistedmatrix.com/2015/11/editor-malware.html

(setq tls-checktrust 1)

(let ((trustfile
       (replace-regexp-in-string
        "\\\\" "/"
        (replace-regexp-in-string
         "\n" ""
         (shell-command-to-string "python -m certifi")))))
  (setq tls-program
        (list
         (format "gnutls-cli%s --x509cafile %s -p %%p %%h"
                 (if (eq window-system 'w32) ".exe" "") trustfile)))
  (setq gnutls-verify-error t)
  (setq gnutls-trustfiles (list trustfile)))

(if (condition-case e
        (progn
          (url-retrieve "https://wrong.host.badssl.com/"
                        (lambda (retrieved) t))
          (url-retrieve "https://self-signed.badssl.com/"
                        (lambda (retrieved) t))
          t)
      ('error nil))
    (error "tls misconfigured")
  (url-retrieve "https://badssl.com"
                (lambda (retrieved) t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring Packages")

(require 'package)
(package-initialize)

(setq package-archives 
      '(("melpa" . "https://melpa.org/packages/")
        ("org" . "http://orgmode.org/elpa/")))

(message "Check for packages")

(setq my-package-list
      (list 'gist
            'nyan-mode
	    'parenface
	    'dictionary))

(setq my-theme-list
      (list 'tronesque-theme
            'anti-zenburn-theme
            'ample-theme
            'ample-zen-theme
            'twilight-theme
            'ubuntu-theme
            'zen-and-art-theme
            'zenburn-theme
            'bubbleberry-theme))

(with-optional-init
 'text
 (add-to-list 'my-package-list 'writegood-mode))

(with-optional-init
 'paredit
 (add-to-list 'my-package-list 'paredit))

(with-optional-init
 'markdown
 (add-to-list 'my-package-list 'markdown-mode))

(with-optional-init
 'projectile
 (add-to-list 'my-package-list 'projectile))

(with-optional-init
 'smex
 (add-to-list 'my-package-list 'smex))

(with-optional-init
 'web
 (add-to-list 'my-package-list 'web-mode)
 (add-to-list 'my-package-list 'restclient))

(with-optional-init
 'org
 (add-to-list 'my-package-list 'org-plus-contrib)
 (add-to-list 'my-package-list 'org-gcal))

(with-optional-init
 'magit
 (add-to-list 'my-package-list 'magit)
 (add-to-list 'my-package-list 'magit-svn))

(with-optional-init
 'rust
 (add-to-list 'my-package-list 'rust-mode))

(with-optional-init
 'clojure
 (add-to-list 'my-package-list 'clojure-mode)
 (add-to-list 'my-package-list 'cider)
 (add-to-list 'my-package-list 'clojure-cheatsheet)

 (add-to-list 'package-pinned-packages '(cider . "melpa-stable") t))

(with-optional-init
 'chicken
 (add-to-list 'my-package-list 'chicken-scheme))

(with-optional-init
 'company
 (add-to-list 'my-package-list 'company)
 (with-optional-init
  'c++
  (add-to-list 'my-package-list 'company-c-headers))
 (with-optional-init
  'haskell
  (add-to-list 'my-package-list 'company-ghc))
 (with-optional-init
  'ruby
  (add-to-list 'my-package-list 'company-inf-ruby))
 (with-optional-init
  'python
  (add-to-list 'my-package-list 'company-jedi))
 (with-optional-init
  'web
  (add-to-list 'my-package-list 'company-restclient)
  (add-to-list 'my-package-list 'company-web))
 (with-optional-init
  'js2
  (add-to-list 'my-package-list 'company-tern)))

(when (not my-load-debug-geiser)
  (with-optional-init
   'geiser
   (add-to-list 'my-package-list 'geiser)))

(with-optional-init
 'js2
 (add-to-list 'my-package-list 'js2-mode)
 (add-to-list 'my-package-list 'tern))

(with-optional-init
 'ruby
 (add-to-list 'my-package-list 'enh-ruby-mode)
 (add-to-list 'my-package-list 'inf-ruby)
 (add-to-list 'my-package-list 'projectile-rails)
 (add-to-list 'my-package-list 'robe))

(with-optional-init
 'python
 (add-to-list 'my-package-list 'jedi)
 (add-to-list 'my-package-list 'python-environment))

(with-optional-init
 'c++
 (add-to-list 'my-package-list 'function-args)
 (add-to-list 'my-package-list 'ggtags))

(with-optional-init
 'haskell
 (add-to-list 'my-package-list 'ghc)
 (add-to-list 'my-package-list 'haskell-mode))

(with-optional-init
 'ocaml
 (add-to-list 'my-package-list 'tuareg))

(with-optional-init
 'lisp
 (add-to-list 'my-package-list 'slime))

(let ((loaded (eval (cons 'require-package (mapcar (lambda (x) `(quote ,x)) (append my-package-list my-theme-list))))))
  (message (format "Installed %s" loaded)))

(reset-theme)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; elisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-optional-init
 'elisp
 (message "Configuring elisp")

 (require 'eldoc)
 (defun custom-elisp-prog-hook ()
   (eldoc-mode 1))

 (add-hook 'emacs-lisp-mode-hook 'custom-elisp-prog-hook))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parens
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-optional-init
 'paredit
 (defun paredit-prog-mode-hook ()
   (paredit-mode t))

 (mapc #'(lambda (m) (add-hook m 'paredit-prog-mode-hook))
       '(scheme-mode-hook 
	 lisp-mode-hook
	 cider-mode-hook
	 clojure-mode-hook
	 emacs-lisp-mode-hook 
	 lisp-interaction-mode))

 (put 'paredit-forward-delete 'delete-selection 'supersede))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compilation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-optional-init
 'compilation
 (message "Configuring Compilation Mode")

 (defun compilation-custom-hook ()
   (visual-line-mode 1))

 (add-hook 'compilation-mode-hook 'compilation-custom-hook)

 (require 'ansi-color)
 (defun colorize-compilation-buffer ()
   (toggle-read-only)
   (ansi-color-apply-on-region (point-min) (point-max))
   (toggle-read-only))
 (add-hook 'compilation-filter-hook 'colorize-compilation-buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scheme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when
    (and
     my-load-debug-geiser
     (file-exists-p "/home/dleslie/Workspace/code/dleslie/geiser/elisp/geiser.el"))
  (load "/home/dleslie/Workspace/code/dleslie/geiser/elisp/geiser.el")
  (require 'geiser))

(with-optional-init
 'chicken
 (message "Configuring Chicken Scheme")

 (defun custom-scheme-hook ()
   (interactive)
   (setup-chicken-scheme))

 (add-hook 'scheme-mode-hook 'custom-scheme-hook))

(require 'multi-mode)
(require 'scheme-c-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LISP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-optional-init
 'lisp

 (message "Configuring LISP")

 (setq slime-lisp-implementations
       '((sbcl ("/usr/bin/sbcl" "--core" "/usr/lib/sbcl/sbcl.core")
               :coding-system utf-8-unix
               :env ("SBCL_HOME=/usr/lib/sbcl"))))

 (require 'slime-autoloads)
 (slime-setup)

 (defun sbcl-slime ()
   (interactive)
   (slime 'sbcl))

 (defun my-lisp-mode-hook ()
   (slime-mode t))

 (add-hook 'lisp-mode-hook 'my-lisp-mode-hook))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C/C++
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-optional-init
 'c++
 (message "Configuring C and C++")

 (with-optional-init
  'company
  (add-to-list 'company-backends 'company-c-headers)

  (defun custom-cc-prog-hook ()
    ;; (enable-semantic-mode)
    (ggtags-mode 1)))

 (add-hook 'c++-mode-hook 'custom-cc-prog-hook)
 (add-hook 'c-mode-hook 'custom-cc-prog-hook)

 ;; Fixes missing C++11 fontlocking in cc-mode
 (defun c++-font-lock-fix ()
   (font-lock-add-keywords 
    nil 
    '(("\\<\\(void\\|unsigned\\|signed\\|char\\|short\\|bool\\|int\\|long\\|float\\|double\\)\\>" . font-lock-keyword-face)
      ;; add the new C++11 keywords
      ("\\<\\(alignof\\|alignas\\|constexpr\\|decltype\\|noexcept\\|nullptr\\|static_assert\\|thread_local\\|override\\|final\\)\\>" . font-lock-keyword-face)
      ("\\<\\(char[0-9]+_t\\)\\>" . font-lock-keyword-face)
      ;; PREPROCESSOR_CONSTANT
      ("\\<[A-Z]+\\([A-Z_]+\\|[0-9]+\\)\\>" . font-lock-constant-face)
      ;; hexadecimal numbers
      ("\\<0[xX][0-9A-Fa-f]+\\>" . font-lock-constant-face)
      ;; integer/float/scientific numbers
      ("\\<[\\-+]*[0-9]*\\.?[0-9]+\\([ulUL]+\\|[eE][\\-+]?[0-9]+\\)?\\>" . font-lock-constant-face)
      ;; user-types
      ("\\<[A-Za-z_]+[A-Za-z_0-9]*_\\(t\\|ptr\\|c\\|e\\)\\>" . font-lock-type-face)
      )))

 (add-hook 'c++-mode-hook 'c++-font-lock-fix)
					;(add-hook 'c-mode-common-hook 'fa-auto)
 (add-hook 'c-mode-common-hook 'fa-config-default))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Haskell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-optional-init
 'haskell
 (with-optional-init
  'company
  (add-to-list 'company-backends 'company-ghc))
 (autoload 'ghc-init "ghc" nil t)
 (autoload 'ghc-debug "ghc" nil t)
 (add-hook 'haskell-mode-hook
           (lambda () (ghc-init))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ocaml
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-optional-init
 'ocaml
 (setq opam-share (substring (shell-command-to-string "opam config var share 2> /dev/null") 0 -1))
 (add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))
 (autoload 'merlin-mode "Merlin" "Merlin Mode" t)
 
 (setq merlin-command 'opam)

 (add-hook 'tuareg-mode-hook 'merlin-mode)
 (add-hook 'caml-mode-hook 'merlin-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Markdown
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-optional-init
 'markdown
 (defun custom-markdown-mode ()
   (interactive)
   (visual-line-mode 1)
   (flyspell-mode 1)
   (writegood-mode 1)
   (markdown-mode))

 (add-to-list 'auto-mode-alist '("\\.markdown\\'" . custom-markdown-mode))
 (add-to-list 'auto-mode-alist '("\\.md\\'" . custom-markdown-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Text
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-optional-init
 'text
 (defun custom-text-mode-hook ()
   (visual-line-mode 1)
   (flyspell-mode 1)
   (writegood-mode 1))

 (add-hook 'text-mode-hook 'custom-text-mode-hook))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-optional-init
 'org
 (message "Configuring Org Mode")

 (setq
  org-default-notes-file 
  (concat org-directory "notes.org")
  org-agenda-files 
  `(,(concat org-directory "todo.org") 
    ,(concat org-directory "agenda.org")
    ,(concat org-directory "gcal-main.org")
    ,(concat org-directory "gcal-appointments.org")
    ,(concat org-directory "gcal-vacations.org"))
  org-agenda-diary-file 
  (concat org-directory "diary.org")
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
  '(("n" "Note" entry (file+headline (concat org-directory "notes.org") "Notes")
     "* %^{topic} %T %^g\n   :CATEGORY: %^{category}\n%i%?\n")
    ("t" "To Do" entry (file+headline (concat org-directory "todo.org") "To Do")
     "* TODO %^{todo} %^g\n   DEADLINE: %^{due}t\n   :CATEGORY: %^{category}\n")
    ("d" "Daily review" entry (file+headline (concat org-directory "diary.org") "Daily Review") 
     "* %T %^g\n   :CATEGORY: Review\n   %?%[~/ownCloud/org/template_daily_review.org]\n") 
    ("i" "Idea" entry (file+headline (concat org-directory "ideas.org") "Ideas") 
     "* %^{topic} %T %^g\n   :CATEGORY: Idea\n   %i%?\n") 
    ("j" "Journal" entry (file+headline (concat org-directory "diary.org") "Journal") 
     "* %^{topic} %T %^g\n   :CATEGORY: Journal\n   %i%?\n")
    ("l" "Letter" entry (file+headline (concat org-directory "letters.org") "Letter") 
     "* %^{topic} %T %^g\n   :CATEGORY: Letter\n   %i%?\n")
    ("w" "Work Log" entry (file+headline (concat org-directory "work.org") "Work Log") 
     "* %^{topic} %T %^g\n   :CATEGORY: Log\n   %i%?\n")
    ("a" "Article" entry (file+headline (concat org-directory "articles.org") "Article")
     "* %^{topic} %T %^g\n   :CATEGORY: Article\n   %i%?\n")
    ("e" "Event" entry (file+headline (concat org-directory "agenda.org") "Events")
     "* %^{title} %^g\n     SCHEDULED: %^{when}t\n   %i%?\n")
    ("c" "Contact" entry (file+headline (concat org-directory "addresses.org") "Addresses")
     "* %(org-contacts-template-name)\n   :PROPERTIES:\n   :EMAIL: %(org-contacts-template-email)\n   :END:\n   %i%?\n"))
  org-modules
  '(org-bbdb org-bibtex org-docview org-gnus org-info org-jsinfo org-habit org-irc org-mew 
	     org-mhe org-rmail org-special-blocks org-vm org-wl org-w3m org-mouse org-bookmark 
	     org-drill org-eshell org-invoice org-registry org-contacts))

 (defun custom-org-hook ()
   (interactive)
   (visual-line-mode 1)
   (flyspell-mode 1)
   (writegood-mode 1))

 (add-hook 'org-mode-hook 'custom-org-hook))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-optional-init
 'python
 (message "Configuring Python Mode")

 (jedi:install-server)

 (with-optional-init
  'company
  (add-to-list 'company-backends 'company-jedi))

 (defun python-custom-hook ()
   (jedi:setup))

 (add-hook 'python-mode-hook 'python-custom-hook))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ruby
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-optional-init
 'ruby
 (message "Configuring Ruby Mode")

 (with-optional-init
  'company
  (add-to-list 'company-backends 'company-inf-ruby))

 (defun launch-ruby ()
   (interactive)

   (projectile-rails-on)
   
   (unless (get-buffer "*ruby*")
     (let ((buf (current-buffer)))
       (inf-ruby)
       (robe-start)
       (set-buffer buf))))

 (defun kill-ruby ()
   (interactive)
   (when (get-buffer "*ruby*")
     (kill-buffer "*ruby*")))

 (advice-add 'projectile-rails-console :before #'kill-ruby)

 (add-hook 'ruby-mode-hook 'launch-ruby)
 (add-hook 'enh-ruby-mode-hook 'launch-ruby)

 (add-hook 'ruby-mode-hook 'robe-mode)
 (add-hook 'enh-ruby-mode-hook 'robe-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clojure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-optional-init 'clojure
                    (add-hook 'clojure-mode-hook 'cider-mode)
                    (add-hook 'cider-mode-hook 'eldoc-mode)
                    
                    (add-to-list 'auto-mode-alist '("\\.clj\\'" . clojure-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Web
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-optional-init
 'web
 (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
 (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
 (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
 (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
 (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
 (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
 (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
 (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
 (with-optional-init
  'company
  (add-to-list 'company-backends 'company-web-html)
  (add-to-list 'company-backends 'company-web-jade)
  (add-to-list 'company-backends 'company-web-slim)
  (add-to-list 'company-backends 'company-restclient)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Javascript
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-optional-init
 'js2
 (defun js2-mode-custom-hook ()
   (tern-mode t))

 (add-hook 'js2-mode-hook 'js2-mode-custom-hook)

 (with-optional-init
  'company
  (add-to-list 'company-backends 'company-web-slim))
 
 (add-to-list 'auto-mode-alist '("\\.js?\\'" . js2-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rust
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-optional-init
 (list 'racer 'rust)
 (when (and (file-exists-p racer-cmd)
            (file-exists-p racer-load-path))
   (add-to-list 'load-path racer-load-path)
   (eval-after-load "rust-mode" '(require 'racer))
   (add-to-list 'company-backends 'racer-company-complete)
   (define-key rust-mode-map (kbd "M-.") #'racer-find-definition)
   (define-key rust-mode-map (kbd "<C-tab>") #'racer-complete-or-indent)
   (add-hook 'rust-mode-hook #'racer-turn-on-eldoc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-gcal
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-optional-init
 'org
 (defun update-gcal ()
   (interactive)
   (org-gcal-refresh-token)
   (org-gcal-fetch))

 (defun force-gcal-category ()
   (let ((category (assoc buffer-file-name org-gcal-forced-category-file-alist)))
     (when category
       (goto-char (point-min))
       (insert (format "#+CATEGORY: %s" (cdr category)))
       (newline))))

 (add-hook 'before-save-hook 'force-gcal-category)

 (let ((gcal-config (expand-file-name "gcal-settings.el" user-emacs-directory)))
   (when (file-exists-p gcal-config)
     (load gcal-config)
     (run-with-timer 3600 3600 'update-gcal))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mu4e
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-optional-init
 'mu4e
 (when (file-exists-p "/usr/local/share/emacs/site-lisp/mu4e")
   (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e"))

 (require 'mu4e)

 (setq 
  mu4e-maildir 
  "~/Maildir"

  mu4e-drafts-folder
  mail-folder-drafts

  mu4e-sent-folder
  mail-folder-sent

  mu4e-trash-folder
  mail-folder-trash

  mu4e-sent-messages-behavior
  'sent

  mu4e-maildir-shortcuts
  `((,mail-folder-inbox . ?i)
    (,mail-folder-sent  . ?s)
    (,mail-folder-trash . ?t))

  mu4e-get-mail-command
  "offlineimap"

  message-kill-buffer-on-exit
  t

  mu4e-attachment-dir
  user-mail-attachment-directory

  mu4e-compose-reply-to-address
  user-mail-address

  mu4e-headers-include-related
  nil

  mu4e-headers-results-limit
  -1

  mu4e-sent-messages-behavior
  (quote sent)

  mu4e-update-interval
  -1

  mu4e-user-mail-address-list
  `(,user-mail-login ,user-mail-address)

  mu4e-view-show-addresses
  t

  mu4e-view-show-images
  t

  mu4e-org-contacts-file
  (concat org-directory "addresses.org"))

 (when (package-installed-p 'eww)
   (require 'mu4e-contrib)
   (setq mu4e-html2text-command 'mu4e-shr2text))

 (add-to-list 'mu4e-headers-actions
              '("org-contact-add" . mu4e-action-add-org-contact) t)
 (add-to-list 'mu4e-view-actions
              '("org-contact-add" . mu4e-action-add-org-contact) t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc Custom
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring Miscellaneous")

(with-optional-init
 'smex
 (smex-initialize))

(with-optional-init
 'projectile
 (projectile-global-mode t))

(nyan-mode t)

(setq gc-cons-threshold 20000000)
(add-hook 'prog-mode-hook 'show-paren-mode)

(setq make-backup-files nil)
(setq indent-tabs-mode nil)

;; From http://stackoverflow.com/a/20788581
(with-demoted-errors
    "Error: %S"
  (require 'ansi-color)
  (defun my-colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer))

(delete-selection-mode t)

(with-optional-init
 'company
 (add-to-list 'company-backends 'company-ispell)
 (add-to-list 'company-backends 'company-elisp)
 (defun my-anti-ispell-prog-hook ()
   (make-local-variable 'company-backends)
   (setq company-backends (remove 'company-ispell company-backends)))
 (add-hook 'prog-mode-hook 'my-anti-ispell-prog-hook)
 (add-hook 'eshell-mode-hook 'my-anti-ispell-prog-hook)
 (global-company-mode))

(require 'parenface)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keys
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring custom keys")

(global-set-key [mouse-2] '(lambda () (interactive) (message "mouse-2 paste disabled")))

(global-set-key "\C-w" 'clipboard-kill-region)
(global-set-key "\M-w" 'clipboard-kill-ring-save)
(global-set-key "\C-y" 'clipboard-yank)

(with-optional-init
 'smex
 (global-set-key "\M-x" 'smex))

(with-optional-init
 'org
 (global-set-key "\C-ct" 'org-todo-list)
 (global-set-key "\C-cl" 'org-store-link)
 (global-set-key "\C-ca" 'org-agenda)
 (global-set-key "\C-cb" 'org-iswitchb)
 (global-set-key "\C-cc" 'org-capture))

(with-optional-init
 'mu4e
 (global-set-key "\C-cm" 'mu4e))

(with-optional-init
 'magit
 (global-set-key "\C-cg" 'magit-status))

(with-optional-init
 'company
 (global-set-key (kbd "<C-tab>") 'company-complete))

(global-set-key "\C-c," 'scroll-bar-mode)
(global-set-key "\C-c." 'tool-bar-mode)
(global-set-key "\C-c?" 'menu-bar-mode)
(global-set-key "\C-c\\" 'comment-or-uncomment-region)
(global-set-key "\C-cs" 'eshell-here)
(global-set-key "\C-cd" 'dictionary-search)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; End
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Init Complete.")
