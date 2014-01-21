(message "Post Init Started")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring package archives")

(setq package-archives 
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extra Modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring extra modes")

(add-to-list 'load-path "~/.emacs.d/")

(require 'ditz-mode)
(require 'gist)
(require 'seclusion-mode)
;; (require 'cmake-mode)

;; (add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-mode))
;; (add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Irony
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (add-to-list 'load-path (expand-file-name "~/.emacs.d/irony-mode/elisp"))

;; (require 'irony)
;; (irony-enable 'ac)

;; (defun irony-mode-hooks ()
;;   (irony-mode 1))

;; (add-hook 'c++-mode-hook 'irony-mode-hooks)
;; (add-hook 'c-mode-hook 'irony-mode-hooks) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CEDET
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (message "Configuring CEDET")

;; (semantic-mode t)

;; (require 'semantic/ia)
;; (require 'semantic/bovine)

;; (global-semantic-decoration-mode t)
;; (global-semantic-stickyfunc-mode 0)
;; (global-semantic-highlight-edits-mode 0)
;; (global-semantic-highlight-func-mode t)
;; (global-semantic-idle-completions-mode t)
;; (global-semantic-idle-summary-mode 0)
;; (global-semantic-show-parser-state-mode t)
;; (global-semantic-show-unmatched-syntax-mode 0)

;; (global-srecode-minor-mode t)

;; (semantic-add-system-include "/usr/local/include/" 'c-mode)
;; (semantic-add-system-include "/usr/include/c++/4.7/" 'c-mode)
;; (semantic-add-system-include "/usr/local/include/" 'c++-mode)
;; (semantic-add-system-include "/usr/include/c++/4.7/" 'c++-mode)

;; (defun cedet-local-config () 
;;   (local-set-key [(control return)] 'semantic-ia-complete-symbol)
;;   (local-set-key "\C-c?" 'semantic-ia-complete-symbol-menu)
;;   (local-set-key "\C-c>" 'semantic-complete-analyze-inline)
;;   (local-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle))

;; (add-hook 'emacs-lisp-mode-hook 'cedet-local-config)
;; (add-hook 'c-mode-common-hook 'cedet-local-config)
;; (add-hook 'lisp-mode-hook 'cedet-local-config)
;; (add-hook 'java-mode-hook 'cedet-local-config)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto-complete
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring Auto-Complete")
(require 'auto-complete-config)

(add-to-list 'ac-dictionary-directories (expand-file-name "~/.emacs.d/ac-dict"))
(setq ac-comphist-file (expand-file-name "~/.emacs.d/ac-comphist.dat"))
(setq ac-modes (quote (emacs-lisp-mode 
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
                       lua-mode)))

(ac-config-default)

(add-to-list 'ac-sources 'ac-source-gtags)

(setq ac-quick-help-delay 0.5)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AC-Clang
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'auto-complete-clang)

(setq ac-clang-flags '("-I/usr/local/include" 
		       "-I/usr/include/c++/4.7/" 
		       "-I/usr/include/x86_64-linux-gnu/c++/4.7/"
		       "-I/usr/include"
		       "-I/usr/include/x86_64-linux-gnu"
		       "-I/usr/include/clang/3.2/include"
		       "-std=c++11"
		       "-pthread"
		       "-D__GCC_HAVE_SYNC_COMPARE_AND_SWAP_1"
		       "-D__GCC_HAVE_SYNC_COMPARE_AND_SWAP_2"
		       "-D__GCC_HAVE_SYNC_COMPARE_AND_SWAP_4"
		       "-D__GCC_HAVE_SYNC_COMPARE_AND_SWAP_8"))

(defun ac-clang-at-will ()
  (interactive)
  (auto-complete (cons 'ac-source-clang 'ac-sources)))

;; (add-hook 'c-mode-hook 'ac-clang-hook)
;; (add-hook 'c++-mode-hook 'ac-clang-hook)
;; (add-hook 'objc-mode-hook 'ac-clang-hook)
(define-key ac-mode-map  [(control return)] 'ac-clang-at-will)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rainbow Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring Rainbow Mode")

(require 'rainbow-mode)
(require 'rainbow-delimiters)

(custom-set-faces
 '(rainbow-delimiters-depth-1-face ((t (:foreground "dark violet"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "royal blue"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "dark turquoise"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "dark violet"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "royal blue"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "dark turquoise"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "dark violet"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "royal blue"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "dark turquoise"))))
 '(rainbow-delimiters-unmatched-face ((t (:background "red" :foreground "white")))))

(global-rainbow-delimiters-mode)

(add-hook 'after-find-file 'rainbow-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Paredit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring paredit")

(require 'parenface)
(require 'paredit-everywhere)

(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)

(add-hook 'scheme-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-mode-hook 'enable-paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)

(add-hook 'prog-mode-hook 'paredit-everywhere-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C++
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Fixes missing C++11 fontlocking in cc-mode
(defun c++-font-lock-fix ()
  (font-lock-add-keywords 
   nil 
   '(("\\<\\(void\\|unsigned\\|signed\\|char\\|short\\|bool\\|int\\|long\\|float\\|double\\)\\>" . font-lock-keyword-face)
     ;; add the new C++11 keywords
     ("\\<\\(alignof\\|alignas\\|constexpr\\|decltype\\|noexcept\\|nullptr\\|static_assert\\|thread_local\\|override\\|final\\)\\>" . font-lock-keyword-face)
     ("\\<\\(char[0-9]+_t\\)\\>" . font-lock-keyword-face)
     ;; PREPROCESSOR_CONSTANT
     ("\\<[A-Z]+[A-Z_]+\\>" . font-lock-constant-face)
     ;; hexadecimal numbers
     ("\\<0[xX][0-9A-Fa-f]+\\>" . font-lock-constant-face)
     ;; integer/float/scientific numbers
     ("\\<[\\-+]*[0-9]*\\.?[0-9]+\\([ulUL]+\\|[eE][\\-+]?[0-9]+\\)?\\>" . font-lock-constant-face)
     ;; user-types
     ("\\<[A-Za-z_]+[A-Za-z_0-9]*_\\(t\\|ptr\\|c\\|e\\)\\>" . font-lock-type-face)
     )))

(add-hook 'c++-mode-hook 'c++-font-lock-fix)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compilation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring Compilation  Mode")

(defun compilation-custom-hook ()
  (visual-line-mode 1))

(add-hook 'compilation-mode-hook 'compilation-custom-hook)

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
        ("Letter" ?l "* %^{topic} %T %^g\n:CATEGORY: Letter\n%i%?\n" "~/Crypt/letters.org" "Letter")
        ("Work Log" ?w "* %^{topic} %T %^g\n:CATEGORY: Log\n%i%?\n" "~/Workspace/org/remember.org" "Work Log")))

(setq org-todo-keywords
      '((sequence "TODO" "DOIN" "BLCK" "STAL" "|" "WONT" "DONE")))

(setq org-todo-keyword-faces
      '(
        ("DOIN" . (:foreground "green" :weight bold))
        ("BLCK" . (:foreground "red" :weight bold))
        ("STAL" . (:foreground "yellow" :weight bold))
        ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring Python  Mode")

(setq jedi:setup-keys t)                      ; optional
(setq jedi:complete-on-dot t)                 ; optional
;; (setq py-python-command "/usr/bin/python3")
;; (setq jedi:server-command (quote ("python3" "/home/dleslie/.emacs.d/elpa/jedi-20130714.1415/jediepcserver.py")))
;; (setq python-shell-interpreter "python3")
(setq py-python-command "/usr/bin/python")
(setq jedi:server-command (quote ("python" "/home/dleslie/.emacs.d/elpa/jedi-20130714.1228/jediepcserver.py")))
(setq python-shell-interpreter "python")
(setq python-indent-offset 4)

(add-hook 'python-mode-hook 'jedi:setup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keys
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring custom keys")

(global-set-key "\C-w" 'clipboard-kill-region)
(global-set-key "\M-w" 'clipboard-kill-ring-save)
(global-set-key "\C-y" 'clipboard-yank)
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "TAB") 'indent-according-to-mode)

(global-set-key [f11] 'speedbar)
(global-set-key [f12] 'menu-bar-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scheme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring Scheme")

(require 'multi-mode)
(require 'scheme-c-mode)
(require 'chicken-scheme)

(defun scheme-ac-hook () 
  (append ac-sources
	  '(ac-source-chicken-symbols
	    ac-source-chicken-symbols-prefixed
	 )))

(add-hook 'scheme-mode-hook 'scheme-ac-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc Custom
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring Miscellaneous")

(setq ac-auto-show-menu 0.8)
(setq ac-candidate-limit 50)
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
(setq make-backup-files 0) 
(setq redisplay-dont-pause t)
(setq scroll-bar-mode nil)
(setq scroll-margin 0)
(setq scroll-step 1)
(setq show-paren-mode t)
(setq standard-indent 2)
(setq tab-stop-list (number-sequence 4 200 4))
(setq tab-width 4)
(setq truncate-lines t)
(setq visual-line-mode t)
(setq word-wrap t)

(require 'nyan-mode)
(setq nyan-wavy-trail t)
(nyan-mode t)

(message "Post Init Complete.")
