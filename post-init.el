(message "Post Init Started")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Semantic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring Semantic and CEDET")

; Semantic and CEDET have to come first because they're dinks

(require 'cedet)
(require 'srecode)
(require 'srecode/map)
(require 'advice)
(require 'eieio-opt)

(require 'semantic)
(require 'semantic/bovine/gcc)
(require 'semantic/ia)
(require 'semantic/imenu)
(require 'semantic/sb)

(defvar system-include-paths
  '("/usr/local/include" 
    "/usr/include"
    "/usr/include/c++/4.4/" 
    "/usr/include/c++/4.7/" 
    "/usr/include/c++/4.8/" 
    "/usr/include/c++/4.9/" 
    "/usr/include/x86_64-linux-gnu"
    "/usr/include/x86_64-linux-gnu/c++/4.7/"
    "/usr/include/x86_64-linux-gnu/c++/4.8/"
    "/usr/include/x86_64-linux-gnu/c++/4.9/"
    "/usr/lib/gcc/x86_64-linux-gnu/4.7/include/"
    "/usr/lib/gcc/x86_64-linux-gnu/4.8/include/"
    "/usr/lib/gcc/x86_64-linux-gnu/4.9/include/"))

(global-ede-mode 1)
(global-semanticdb-minor-mode 1)
(global-semantic-idle-summary-mode 1)
(global-semantic-idle-scheduler-mode 1)

(mapc #'(lambda (s) (semantic-add-system-include s))
      system-include-paths)

(semanticdb-enable-gnu-global-databases 'c-mode)
(semanticdb-enable-gnu-global-databases 'c++-mode)

(semantic-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Defining Custom Functions")

(defun override-theme (arg)
  "Disables all enabled themes and then loads the provided theme."
  (interactive
   (list
    (intern (completing-read "Load custom theme: "
			     (mapcar 'symbol-name (custom-available-themes))))))
  (while custom-enabled-themes
    (disable-theme (car custom-enabled-themes)))
  (load-theme arg t)
  t)

(defun require-package (package-name &rest remaining-packages)
  "Loads and imports packages, installing from ELPA if necessary"
  (unless (package-installed-p package-name)
    (package-install package-name))
  (require package-name)
  
  (cons package-name
	(cond
	 ((equal remaining-packages nil) nil)
	 (t (apply 'require-package remaining-packages)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring Packages")

(add-to-list 'load-path "~/.emacs.d/")

(require 'package)
(package-initialize)

(setq package-archives 
      '(
        ("melpa" . "http://melpa.milkbox.net/packages/")
        ("gnu" . "http://elpa.gnu.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")
	))

(message "Check for packages")

(package-refresh-contents)

(message 
 (format "Installed %s"
	 (require-package
	  'ac-capf
	  'ac-slime
	  'auto-complete
	  'auto-complete-exuberant-ctags
	  'cl-lib
	  'chicken-scheme
	  'dired+
	  'doremi
	  'enh-ruby-mode
	  'function-args
	  'ggtags
	  'ghc
	  'gist
	  'help+
	  'help-fns+
	  'help-mode+
	  'inf-ruby
	  'jedi
	  'magit
	  'magit-gh-pulls
	  'magit-svn
	  'menu-bar+
	  'moe-theme
	  'nyan-mode
	  'paredit 
	  'parenface 
	  'popup
	  'projectile 
	  'python-environment 
	  'rainbow-delimiters 
	  'rainbow-mode 
	  'robe
	  'slime 
	  'smex 
	  'sublime-themes 
	  'web-mode 
	  'zenburn-theme)))

; Additional that require force loading
(require 'cl)
(require 'imenu)
(require 'org-remember)
(require 'auto-complete-config)
(require 'cc-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring Menu")

(defun try-to-add-imenu ()
  (condition-case nil (imenu-add-to-menubar "TAGS") (error nil)))
(add-hook 'font-lock-mode-hook 'try-to-add-imenu)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eldoc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring eldoc")

(require 'eldoc)
(defun custom-eldoc-prog-hook ()
  (eldoc-mode 1))

(add-hook 'emacs-lisp-mode-hook 'custom-eldoc-prog-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ggtags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring ggtags")

(defun custom-ggtags-prog-hook ()
  (ggtags-mode 1))

(add-hook 'c++-mode-hook 'custom-ggtags-prog-hook)
(add-hook 'c-mode-hook 'custom-ggtags-prog-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rainbow Modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring Rainbow Modes")

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'after-find-file 'rainbow-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parens
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun paredit-prog-mode-hook ()
  (paredit-mode t))

(mapc #'(lambda (m) (add-hook m 'paredit-prog-mode-hook))
      '(scheme-mode-hook 
	lisp-mode-hook 
	emacs-lisp-mode-hook 
	lisp-interaction-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compilation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring Compilation Mode")

(defun compilation-custom-hook ()
  (visual-line-mode 1))

(add-hook 'compilation-mode-hook 'compilation-custom-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SLIME
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring SLIME")

(setq inferior-lisp-program "/usr/bin/sbcl")

(setq slime-lisp-implementations
      '((sbcl ("/usr/bin/sbcl" "--core" "/usr/lib/sbcl/sbcl.core")
              :coding-system utf-8-unix
              :env ("SBCL_HOME=/usr/lib/sbcl"))))

(require 'slime-autoloads)
(setq slime-contribs '(slime-fancy slime-autodoc slime-banner))
(slime-setup)

(defun sbcl-slime ()
  (interactive)
  (slime 'sbcl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scheme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring Scheme")

(add-hook 'scheme-mode-hook 'setup-chicken-scheme)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LISP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring LISP")

(defun my-lisp-mode-hook ()
  (slime-mode t))

(add-hook 'lisp-mode-hook 'my-lisp-mode-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C++
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Extending C++11 fontlocking")

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

(fa-config-default)

(add-hook 'c++-mode-hook 'c++-font-lock-fix)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Haskell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Remember
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring Org Mode")

(setq org-directory "~/Dropbox/org/")
(setq org-default-notes-file (concat org-directory "notes.org"))
(setq org-agenda-files '((concat org-directory "todo.org") (concat org-directory "agenda.org") (concat org-directory "remember.org")))
(setq org-agenda-diary-file (concat org-directory "remember.org"))

(setq remember-annotation-functions '(org-remember-annotation))
(setq remember-handler-functions '(org-remember-handler))
(add-hook 'remember-mode-hook 'org-remember-apply-template)
(setq org-remember-templates
      '(("Daily review" ?d "* %T %^g \n:CATEGORY: Review\n%?%[~/Dropbox/org/template_daily_review.org]\n" (concat org-directory "remember.org") "Daily Review")
        ("Idea" ?i "* %^{topic} %T %^g\n%i%?\n:CATEGORY: Idea\n" (concat org-directory "remember.org") "Ideas")
        ("Journal" ?j "* %^{topic} %T %^g\n%i%?\n:CATEGORY: Journal\n" (concat org-directory "remember.org") "Journal")
        ("Letter" ?l "* %^{topic} %T %^g\n:CATEGORY: Letter\n%i%?\n" (concat org-directory "remember.org") "Letter")
        ("Work Log" ?w "* %^{topic} %T %^g\n:CATEGORY: Log\n%i%?\n" (concat org-directory "remember.org") "Work Log")
	("Article" ?a "* %^{topic} %T %^g\n%i%?\n:CATEGORY: Article\n" (concat org-directory "remember.org") "Article")))

(setq org-todo-keywords
      '((sequence "TODO" "DOIN" "BLCK" "STAL" "|" "WONT" "DONE")))

(setq org-todo-keyword-faces
      '(
        ("DOIN" . (:foreground "green" :weight bold))
        ("BLCK" . (:foreground "red" :weight bold))
        ("STAL" . (:foreground "yellow" :weight bold))
        ))

(defun custom-org-hook ()
  (visual-line-mode 1)
  (flyspell-mode 1))

(add-hook 'org-mode-hook 'custom-org-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring Python Mode")

(jedi:install-server)

(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)
(setq py-python-command "/usr/bin/python")
(setq jedi:server-command (quote ("python" "/home/dleslie/.emacs.d/elpa/jedi-20140321.1323/jediepcserver.py")))
(setq python-shell-interpreter "python")
(setq python-indent-offset 4)

(defun python-custom-hook ()
  (jedi:setup))

(add-hook 'python-mode-hook 'python-custom-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ruby
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring Ruby Mode")

(setq enh-ruby-program "/usr/bin/ruby")

(defun launch-ruby ()
  (interactive)
  (unless (get-buffer "*ruby*")
    (let ((buf (current-buffer)))
      (inf-ruby)
      (robe-start)
      (set-buffer buf))))

(add-hook 'ruby-mode-hook 'launch-ruby)
(add-hook 'enh-ruby-mode-hook 'launch-ruby)

(add-hook 'ruby-mode-hook 'robe-mode)
(add-hook 'enh-ruby-mode-hook 'robe-mode)

(add-hook 'robe-mode-hook 'ac-robe-setup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Web
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto-complete
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring Auto-Complete")

(ac-config-default)

(add-to-list 'ac-dictionary-directories (expand-file-name "~/.emacs.d/ac-dict"))
(setq ac-comphist-file (expand-file-name "~/.emacs.d/ac-comphist.dat"))
(setq ac-modes 
      '(java-mode clojure-mode scala-mode 
	emacs-lisp-mode
	lisp-mode
	lisp-interaction-mode 
	c-mode cc-mode c++-mode
	scheme-mode
	slime-repl-mode
	ocaml-mode tuareg-mode 
	perl-mode cperl-mode 
	python-mode 
	ruby-mode enh-ruby-mode
	ecmascript-mode javascript-mode js-mode js2-mode 
	php-mode 
	css-mode 
	makefile-mode 
	sh-mode 
	fortran-mode f90-mode 
	ada-mode 
	xml-mode sgml-mode 
	lua-mode
	slime-repl-mode
	web-mode))

(setq ac-quick-help-delay 0.5)
(setq ac-delay 0.5)
(setq ac-auto-start 0.25)
(setq ac-ignore-case nil)

(add-to-list 'ac-sources 'ac-capf)

(defun ac-no-semantic-setup ()
  (make-local-variable 'ac-sources)
  (add-to-list 'ac-sources 'ac-source-words-in-same-mode-buffers))

(defun ac-semantic-setup ()
  (make-local-variable 'ac-sources)
  (add-to-list 'ac-sources 'ac-source-semantic))

(mapc #'(lambda (m) (add-hook m 'ac-no-semantic-setup))
      '(scheme-mode-hook
	ocaml-mode-hook	tuareg-mode-hook
	python-mode-hook
	ruby-mode-hook
	php-mode-hook
	css-mode-hook
	perl-mode-hook cperl-mode-hook
	ecmascript-mode-hook javascript-mode-hook js-mode-hook js2-mode-hook
	makefile-mode-hook sh-mode-hook
	fortran-mode-hook f90-mode-hook
	ada-mode-hook
	xml-mode-hook sgml-mode-hook
	lua-mode-hook
	c-mode-hook 
	c++-mode-hook
	java-mode-hook))

(mapc #'(lambda (m) (add-hook m 'ac-semantic-setup))
      '(emacs-lisp-mode-hook
	lisp-mode-hook
	lisp-interaction-mode-hook
	c-mode-common-hook))

(defun ac-css-setup ()
  (make-local-variable 'ac-sources)
  (add-to-list 'ac-sources 'ac-source-css-property))

(defun ac-haskell-setup ()
  (make-local-variable 'ac-sources)
  (add-to-list 'ac-sources 'ac-source-ghc-mod))

(defun ac-elisp-setup ()
  (make-local-variable 'ac-sources)
  (add-to-list 'ac-sources ac-source-functions)
  (add-to-list 'ac-sources ac-source-symbols)
  (add-to-list 'ac-sources ac-source-features)
  (add-to-list 'ac-sources ac-source-variables))

(defun ac-scheme-setup ()
  (make-local-variable 'ac-sources)
  (add-to-list 'ac-sources ac-source-r7rs-symbols)
  (add-to-list 'ac-sources ac-source-r5rs-symbols)
  (add-to-list 'ac-sources ac-source-chicken-symbols)
  (add-to-list 'ac-sources ac-source-chicken-symbols-prefixed))

(defun ac-c-common-mode-setup ()
  (make-local-variable 'ac-sources)
  (add-to-list 'ac-sources 'ac-source-gtags))

(defun ac-lisp-setup ()
  (make-local-variable 'ac-sources)
  (add-to-list 'ac-sources 'ac-source-slime))

(defun ac-slime-setup ()
  (make-local-variable 'ac-sources)
  (add-to-list 'ac-sources 'ac-source-slime))

(add-hook 'haskell-mode-hook 'ac-haskell-setup)
(add-hook 'css-mode-hook 'ac-css-setup)
(add-hook 'emacs-lisp-mode-hook 'ac-elisp-setup)
(add-hook 'scheme-mode-hook 'ac-scheme-setup)
(add-hook 'c-mode-common-hook 'ac-c-common-mode-setup)
(add-hook 'lisp-mode-hook 'ac-lisp-setup)
(add-hook 'lisp-interaction-mode-hook 'ac-lisp-setup)

(add-hook 'slime-mode-hook 'ac-slime-setup)
(add-hook 'slime-repl-mode-hook 'ac-slime-setup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keys
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring custom keys")

(global-set-key "\C-w" 'clipboard-kill-region)
(global-set-key "\M-w" 'clipboard-kill-ring-save)
(global-set-key "\C-y" 'clipboard-yank)

(global-set-key "\M-x" 'smex)

(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "TAB") 'indent-according-to-mode)

(global-set-key [f10] 'tool-bar-mode)
(global-set-key [f11] 'speedbar)
(global-set-key [f12] 'menu-bar-mode)

(global-set-key "\C-cr" 'org-remember)
(global-set-key "\C-ct" 'org-todo-list)
(global-set-key "\C-ca" 'org-agenda)

(define-key scheme-mode-map (kbd "C-?") 'chicken-show-help)

(define-key c-mode-map  [(control tab)] 'moo-complete)
(define-key c++-mode-map  [(control tab)] 'moo-complete)
(define-key c-mode-map (kbd "M-o")  'fa-show)
(define-key c++-mode-map (kbd "M-o")  'fa-show)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc Custom
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring Miscellaneous")

(smex-initialize)
(projectile-global-mode t)

(setq nyan-wavy-trail t)
(nyan-mode t)

(override-theme 'moe-dark)

; Cycle this, somehow it gets gubered
(show-paren-mode nil)
(show-paren-mode t)

(message "Post Init Complete.")
