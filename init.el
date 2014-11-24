;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Local Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Setting Local Configuration")

(setq org-directory "~/Dropbox/org/")
(setq tern-command '("~/node_modules/.bin/tern"))

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

(global-set-key "\C-cg" 'magit-status)

(require 'cc-mode)
(define-key c-mode-map  [(control tab)] 'moo-complete)
(define-key c++-mode-map  [(control tab)] 'moo-complete)
(define-key c-mode-map (kbd "M-o")  'fa-show)
(define-key c++-mode-map (kbd "M-o")  'fa-show)

(global-set-key [(control return)] 'auto-complete)

(global-set-key (kbd "C-!") 'eshell-here)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Semantic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring Semantic and CEDET")

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
;; Utility Functions
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
  
  (require package-name nil 'noerror)
  
  (cons package-name
	(cond
	 ((equal remaining-packages nil) nil)
	 (t (apply 'require-package remaining-packages)))))

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
        ("marmalade" . "http://marmalade-repo.org/packages/")))

(message "Check for packages")

(package-refresh-contents)

(message 
 (format "Installed %s"
	 (require-package
	  'ac-capf
      'ac-js2
      'ac-geiser
      'ac-inf-ruby
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
      'geiser
	  'ghc
	  'gist
	  'help+
	  'help-fns+
	  'help-mode+
	  'inf-ruby
	  'jedi
      'js2-mode
	  'magit
	  'magit-gh-pulls
	  'magit-svn
      'markdown-mode
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
      'tern
      'tern-auto-complete
	  'web-mode 
      'writegood-mode
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
(add-hook 'prog-mode-hook 'show-paren-mode)

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

(setq slime-lisp-implementations
      '((sbcl ("/usr/bin/sbcl" "--core" "/usr/lib/sbcl/sbcl.core")
              :coding-system utf-8-unix
              :env ("SBCL_HOME=/usr/lib/sbcl"))))

(require 'slime-autoloads)
(slime-setup)

(defun sbcl-slime ()
  (interactive)
  (slime 'sbcl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scheme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring Scheme")

(defun custom-scheme-hook ()
  (interactive)
  (setup-chicken-scheme))

(add-hook 'scheme-mode-hook 'custom-scheme-hook)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Markdown
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun custom-markdown-mode ()
  (interactive)
  (visual-line-mode 1)
  (flyspell-mode 1)
  (writegood-mode 1)
  (markdown-mode))

(add-to-list 'auto-mode-alist '("\\.markdown\\'" . custom-markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . custom-markdown-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Text
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun custom-text-mode-hook ()
  (visual-line-mode 1)
  (flyspell-mode 1)
  (writegood-mode 1))

(add-hook 'text-mode-hook 'custom-text-mode-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring Org Mode")

(setq org-default-notes-file (concat org-directory "notes.org"))
(setq org-agenda-files `(,(concat org-directory "todo.org") ,(concat org-directory "agenda.org") ,(concat org-directory "remember.org")))
(setq org-agenda-diary-file (concat org-directory "remember.org"))

(setq remember-annotation-functions '(org-remember-annotation))
(setq remember-handler-functions '(org-remember-handler))
(add-hook 'remember-mode-hook 'org-remember-apply-template)
(setq org-remember-templates
      `(("Daily review" ?d "* %T %^g \n:CATEGORY: Review\n%?%[~/Dropbox/org/template_daily_review.org]\n" ,(concat org-directory "remember.org") "Daily Review")
        ("Idea" ?i "* %^{topic} %T %^g\n%i%?\n:CATEGORY: Idea\n" ,(concat org-directory "remember.org") "Ideas")
        ("Journal" ?j "* %^{topic} %T %^g\n%i%?\n:CATEGORY: Journal\n" ,(concat org-directory "remember.org") "Journal")
        ("Letter" ?l "* %^{topic} %T %^g\n:CATEGORY: Letter\n%i%?\n" ,(concat org-directory "remember.org") "Letter")
        ("Work Log" ?w "* %^{topic} %T %^g\n:CATEGORY: Log\n%i%?\n" ,(concat org-directory "remember.org") "Work Log")
	("Article" ?a "* %^{topic} %T %^g\n%i%?\n:CATEGORY: Article\n" ,(concat org-directory "remember.org") "Article")))

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
  (flyspell-mode 1)
  (writegood-mode 1))

(add-hook 'org-mode-hook 'custom-org-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TeX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(unless (package-installed-p 'auctex)
  (package-install 'auctex))

(let* ((version-list (elt (cdr (assq 'auctex package-alist)) 0))
       (version (package-version-join version-list))
       (path (package--dir "auctex" version)))
  (add-to-list 'load-path path)

  (autoload 'TeX-load-hack
    (expand-file-name "tex-site.el"
                      (file-name-directory load-file-name)))
  (TeX-load-hack)

  (load "preview.el" nil t t)
  (setq load-path (remove-if (lambda (val) (equal path val)) load-path)))

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(setq reftex-plug-into-AUCTeX t)

(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring Python Mode")

(jedi:install-server)

(defun python-custom-hook ()
  (jedi:setup))

(add-hook 'python-mode-hook 'python-custom-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ruby
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring Ruby Mode")

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
;; Javascript
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun js2-mode-custom-hook ()
  (tern-mode t))

(eval-after-load 'tern
   '(progn
      (require 'tern-auto-complete)
      (tern-ac-setup)))

(setq tern-ac-on-dot t)

(add-hook 'js2-mode-hook 'js2-mode-custom-hook)

(add-to-list 'auto-mode-alist '("\\.js?\\'" . js2-mode))

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
        scheme-mode geiser-repl-mode
        slime-repl-mode
        ocaml-mode tuareg-mode 
        perl-mode cperl-mode 
        python-mode 
        ruby-mode enh-ruby-mode inf-ruby-mode
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

(defun ac-js2-setup ()
  (ac-js2-mode))

(add-hook 'haskell-mode-hook 'ac-haskell-setup)
(add-hook 'css-mode-hook 'ac-css-setup)
(add-hook 'emacs-lisp-mode-hook 'ac-elisp-setup)
(add-hook 'scheme-mode-hook 'ac-scheme-setup)
(add-hook 'c-mode-common-hook 'ac-c-common-mode-setup)
(add-hook 'lisp-mode-hook 'ac-lisp-setup)
(add-hook 'lisp-interaction-mode-hook 'ac-lisp-setup)
(add-hook 'js2-mode-hook 'ac-js2-setup)
(add-hook 'slime-mode-hook 'ac-slime-setup)
(add-hook 'slime-repl-mode-hook 'ac-slime-setup)
(add-hook 'inf-ruby-mode-hook 'ac-inf-ruby-enable)

(add-hook 'geiser-mode-hook 'ac-geiser-setup)
(add-hook 'geiser-repl-mode-hook 'ac-geiser-setup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc Custom
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring Miscellaneous")

(smex-initialize)
(projectile-global-mode t)

(nyan-mode t)

(override-theme 'moe-dark)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-auto-start 0.25)
 '(ac-delay 0.5)
 '(ac-ignore-case nil)
 '(ac-quick-help-delay 0.5)
 '(auto-fill-mode t)
 '(auto-save-default nil)
 '(c-basic-offset 2)
 '(c-set-offset 2)
 '(c-set-style "BSD")
 '(column-number-mode t)
 '(custom-safe-themes (quote ("8ada1f0bcfc2d8662b74fb21bd1830eaacb5d29e3c99a5ea7fd7a417b7a9b708" "88e56f2e676c8828e08b128c74f2818cbfc77b79f8ebbae955db6098d0001473" default)))
 '(debug-on-error nil)
 '(debug-on-signal nil)
 '(delete-selection-mode 1)
 '(display-battery-mode t)
 '(display-time-mode t)
 '(enh-ruby-program "/usr/bin/ruby")
 '(fill-column 80)
 '(indent-tabs-mode nil)
 '(inferior-lisp-program "/usr/bin/sbcl" t)
 '(jedi:complete-on-dot t)
 '(jedi:setup-keys t)
 '(make-backup-files nil)
 '(nyan-wavy-trail t)
 '(py-python-command "/usr/bin/python")
 '(python-indent-offset 4)
 '(python-shell-interpreter "python")
 '(redisplay-dont-pause t t)
 '(scroll-bar-mode nil)
 '(scroll-margin 0)
 '(scroll-step 1)
 '(semanticdb-find-default-throttle (quote (local project unloaded system recursive omniscience)))
 '(show-paren-mode t)
 '(slime-contribs (quote (slime-fancy slime-autodoc slime-banner)) t)
 '(standard-indent 2)
 '(tab-stop-list (number-sequence 2 200 2))
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(tool-bar-style (quote image))
 '(truncate-lines t)
 '(visible-bell t)
 '(visual-line-mode t t)
 '(word-wrap t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom Faces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil))))
 '(mode-line ((t (:background "#191919" :foreground "#BBBBBB" :box nil))))
 '(mode-line-highlight ((t (:box nil))))
 '(rainbow-delimiters-depth-1-face ((t (:inherit font-lock-function-name-face))))
 '(rainbow-delimiters-depth-2-face ((t (:inherit font-lock-variable-name-face))))
 '(rainbow-delimiters-depth-3-face ((t (:inherit font-lock-keyword-face))))
 '(rainbow-delimiters-depth-4-face ((t (:inherit font-lock-comment-face))))
 '(rainbow-delimiters-depth-5-face ((t (:inherit font-lock-type-face))))
 '(rainbow-delimiters-depth-6-face ((t (:inherit font-lock-constant-face))))
 '(rainbow-delimiters-depth-7-face ((t (:inherit font-lock-builtin-face))))
 '(rainbow-delimiters-depth-8-face ((t (:inherit font-lock-string-face))))
 '(rainbow-delimiters-depth-9-face ((t (:inherit font-lock-doc-face))))
 '(rainbow-delimiters-unmatched-face ((((class color) (min-colors 89)) (:foreground "#ffffff" :background "#a40000" :bold t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; End
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Init Complete.")
