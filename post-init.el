(message "Post Init Started")

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

(defvar project-include-paths
  '("/home/dleslie/Workspace/c++_engine/src/"
    "/home/dleslie/Workspace/c++_engine/build/debug/include"))

(defvar prefixed-include-paths
  (mapcar #'(lambda (s) (concat "-I" s)) (append project-include-paths system-include-paths)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Semantic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'semantic)

(global-semanticdb-minor-mode 1)
(global-semantic-idle-summary-mode 1)
(global-semantic-idle-scheduler-mode 1)

(semantic-mode 1)

(mapc #'(lambda (s) (semantic-add-system-include s))
      (append system-include-paths project-include-paths))

(require 'cc-mode)
(require 'function-args)
(fa-config-default)
(define-key c-mode-map  [(control tab)] 'moo-complete)
(define-key c++-mode-map  [(control tab)] 'moo-complete)
(define-key c-mode-map (kbd "M-o")  'fa-show)
(define-key c++-mode-map (kbd "M-o")  'fa-show)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring package archives")

(setq package-archives 
      '(
        ("melpa" . "http://melpa.milkbox.net/packages/")
        ("gnu" . "http://elpa.gnu.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extra Modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring extra modes")

(add-to-list 'load-path "~/.emacs.d/")

(require 'gist)

(require 'nyan-mode)
(setq nyan-wavy-trail t)
(nyan-mode t)

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

(require 'ggtags)

(defun custom-ggtags-prog-hook ()
  (ggtags-mode 1))

(add-hook 'c++-mode-hook 'custom-ggtags-prog-hook)
(add-hook 'c-mode-hook 'custom-ggtags-prog-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto-complete
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring Auto-Complete")
(require 'auto-complete-config)
(require 'auto-complete-etags)

(ac-config-default)

(add-to-list 'ac-dictionary-directories (expand-file-name "~/.emacs.d/ac-dict"))
(setq ac-comphist-file (expand-file-name "~/.emacs.d/ac-comphist.dat"))
(setq ac-modes 
      '(emacs-lisp-mode 
	lisp-interaction-mode 
	c-mode cc-mode c++-mode
	java-mode clojure-mode scala-mode 
	scheme-mode 
	ocaml-mode tuareg-mode 
	perl-mode cperl-mode 
	python-mode 
	ruby-mode 
	ecmascript-mode javascript-mode js-mode js2-mode 
	php-mode 
	css-mode 
	makefile-mode 
	sh-mode 
	fortran-mode f90-mode 
	ada-mode 
	xml-mode sgml-mode 
	lua-mode))

(setq ac-etags-use-document t)
(setq ac-quick-help-delay 0.15)
(setq ac-delay 0.25)
(setq ac-auto-start 1)

(setq ac-sources 
      '(ac-source-functions 
	ac-source-features 
	ac-source-variables 
	ac-source-yasnippet 
	ac-source-abbrev 
	ac-source-entity))

(defun ac-no-semantic-setup ()
  (make-local-variable 'ac-sources)
  (add-to-list 'ac-sources 'ac-source-words-in-same-mode-buffers)
  (add-to-list 'ac-sources 'ac-source-dictionary)
  (add-to-list 'ac-sources 'ac-source-symbols))

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
	lua-mode-hook))

(mapc #'(lambda (m) (add-hook m 'ac-semantic-setup))
      '(emacs-lisp-mode-hook
	lisp-interaction-mode-hook
	c-mode-hook 
	c++-mode-hook
	java-mode-hook))

(defun ac-css-setup ()
  (make-local-variable 'ac-sources)
  (add-to-list 'ac-sources 'ac-source-css-property))

(defun ac-haskell-setup ()
  (make-local-variable 'ac-sources)
  (add-to-list 'ac-sources 'ac-source-ghc-mod))

(defun ac-elisp-setup ()
  (make-local-variable 'ac-sources)
  (add-to-list 'ac-sources 'ac-source-emacs-lisp-features))

(add-hook 'haskell-mode-hook 'ac-haskell-setup)
(add-hook 'css-mode-hook 'ac-css-setup)
(add-hook 'emacs-lisp-mode-hook 'ac-elisp-setup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AC-Clang
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring ac-clang")

(require 'auto-complete-clang)

(setq ac-clang-flags 
      (append prefixed-include-paths
	      '("-std=c++11"
		"-pthread"
		"-D__GCC_HAVE_SYNC_COMPARE_AND_SWAP_1"
		"-D__GCC_HAVE_SYNC_COMPARE_AND_SWAP_2"
		"-D__GCC_HAVE_SYNC_COMPARE_AND_SWAP_4"
		"-D__GCC_HAVE_SYNC_COMPARE_AND_SWAP_8"
		"-Wno-write-strings" 
		"-Wno-implicit-function-declaration" 
		"-Wno-deprecated")))

(defun ac-clang-at-will ()
  (interactive)
  (auto-complete '(ac-source-clang)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rainbow Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring Rainbow Mode")

(require 'rainbow-mode)
(require 'rainbow-delimiters)

(global-rainbow-delimiters-mode)

(add-hook 'after-find-file 'rainbow-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parens
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'paredit)
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
;; Scheme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring Scheme")

(require 'scheme-c-mode)
(require 'chicken-scheme)

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

(add-hook 'c++-mode-hook 'c++-font-lock-fix)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Remember
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring Org Mode")

(require 'org-remember)

(setq org-directory "~/Workspace/org/")
(setq org-default-notes-file "~/Workspace/org/notes.org")
(setq org-agenda-files '("~/Workspace/org/todo.org" "~/Workspace/org/agenda.org" "~/Workspace/org/remember.org"))
(setq org-agenda-diary-file "~/Workspace/org/remember.org")

(define-key global-map "\C-cr" 'org-remember)
(define-key global-map "\C-ct" 'org-todo-list)
(define-key global-map "\C-ca" 'org-agenda)

(setq remember-annotation-functions '(org-remember-annotation))
(setq remember-handler-functions '(org-remember-handler))
(add-hook 'remember-mode-hook 'org-remember-apply-template)
(setq org-remember-templates
      '(("Daily review" ?d "* %T %^g \n:CATEGORY: Review\n%?%[~/Workspace/org/template_daily_review.org]\n" "~/Workspace/org/remember.org" "Daily Review")
        ("Idea" ?i "* %^{topic} %T %^g\n%i%?\n:CATEGORY: Idea\n" "~/Workspace/org/remember.org" "Ideas")
        ("Journal" ?j "* %^{topic} %T %^g\n%i%?\n:CATEGORY: Journal\n" "~/Workspace/org/remember.org" "Journal")
        ("Letter" ?l "* %^{topic} %T %^g\n:CATEGORY: Letter\n%i%?\n" "~/Workspace/org/remember.org" "Letter")
        ("Work Log" ?w "* %^{topic} %T %^g\n:CATEGORY: Log\n%i%?\n" "~/Workspace/org/remember.org" "Work Log")
	("Article" ?a "* %^{topic} %T %^g\n%i%?\n:CATEGORY: Article\n" "~/Workspace/org/remember.org" "Article")))

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

(setq jedi:setup-keys t)                      ; optional
(setq jedi:complete-on-dot t)                 ; optional
(setq py-python-command "/usr/bin/python")
(setq jedi:server-command (quote ("python" "/home/dleslie/.emacs.d/elpa/jedi-20140321.1323/jediepcserver.py")))
(setq python-shell-interpreter "python")
(setq python-indent-offset 4)

(defun python-custom-hook ()
  (jedi:setup))

(add-hook 'python-mode-hook 'python-custom-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc Custom
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring Miscellaneous")

(setq auto-fill-mode t)
(setq c-basic-offset 2)
(setq c-set-offset 2)
(setq c-set-style "BSD")
(setq column-number-mode t)
(setq display-battery-mode t)
(setq display-time-mode t)
(setq fill-column 80)
(setq indent-tabs-mode nil)
(setq line-number-mode t)
(setq redisplay-dont-pause t)
(setq scroll-bar-mode nil)
(setq scroll-margin 0)
(setq scroll-step 1)
(setq show-paren-mode t)
(setq show-paren-style 'expression)
(setq standard-indent 2)
(setq tab-stop-list (number-sequence 2 200 2))
(setq tab-width 4)
(setq truncate-lines t)
(setq visual-line-mode t)
(setq word-wrap t)
(setq make-backup-files nil)
(delete-selection-mode 1)
(setq auto-save-default nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keys
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring custom keys")

(global-set-key "\C-w" 'clipboard-kill-region)
(global-set-key "\M-w" 'clipboard-kill-ring-save)
(global-set-key "\C-y" 'clipboard-yank)
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "TAB") 'indent-according-to-mode)

(global-set-key [f10] 'minimap-toggle)
(global-set-key [f11] 'speedbar)
(global-set-key [f12] 'menu-bar-mode)

(define-key ac-mode-map  [(control return)] 'ac-clang-at-will)

(message "Post Init Complete.")

