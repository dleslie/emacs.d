(message "Post Init Started")

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

(require 'ditz-mode)
(require 'gist)
(require 'seclusion-mode)
(require 'main-line)
(setq main-line-separator-style 'arrow)


;; (require 'nyan-mode)
;; (setq nyan-wavy-trail t)
;; (nyan-mode t)

(require 'ggtags)

(defun custom-prog-hook ()
  (ggtags-mode 1))

(add-hook 'prog-mode-hook 'custom-prog-hook)

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

(require 'auto-complete-etags)
(add-to-list 'ac-sources 'ac-source-etags)
(add-to-list 'ac-sources 'ac-source-gtags)

(setq ac-etags-use-document t)

(setq ac-quick-help-delay 0.15)
(setq ac-delay 0.25)
(setq ac-auto-start 1)

(ac-config-default)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AC-Clang
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring ac-clang")

(require 'auto-complete-clang)

(setq ac-clang-flags '("-I/usr/local/include" 
		       "-I/usr/include"
		       "-I/usr/include/clang/3.2/include"
		       "-I/usr/include/clang/3.4/include"
		       "-I/usr/lib/llvm-3.2/lib/clang/3.2/include/"
		       "-I/usr/lib/llvm-3.4/lib/clang/3.4/include/"
		       "-I/usr/include/c++/4.4/" 
		       "-I/usr/include/c++/4.7/" 
		       "-I/usr/include/c++/4.8/" 
		       "-I/usr/include/x86_64-linux-gnu"
		       "-I/usr/include/x86_64-linux-gnu/c++/4.7/"
		       "-I/usr/include/x86_64-linux-gnu/c++/4.8/"
		       "-I/usr/lib/gcc/x86_64-linux-gnu/4.7/include/"
		       "-I/usr/lib/gcc/x86_64-linux-gnu/4.8/include/"
		       "-std=c++11"
		       "-pthread"
		       "-D__GCC_HAVE_SYNC_COMPARE_AND_SWAP_1"
		       "-D__GCC_HAVE_SYNC_COMPARE_AND_SWAP_2"
		       "-D__GCC_HAVE_SYNC_COMPARE_AND_SWAP_4"
		       "-D__GCC_HAVE_SYNC_COMPARE_AND_SWAP_8"
		       "-Wno-write-strings" 
		       "-Wno-implicit-function-declaration" 
		       "-Wno-deprecated"))

;; Project includes
(setq my-c++-engine-flags '("-I/home/dleslie/Workspace/c++_engine/src/"
			    "-I/home/dleslie/Workspace/c++_engine/build/debug/include"))
(setq ac-clang-flags (append ac-clang-flags my-c++-engine-flags))

(defun ac-clang-at-will ()
  (interactive)
  (auto-complete (cons 'ac-source-clang 'ac-sources)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rainbow Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring Rainbow Mode")

(require 'rainbow-mode)
(require 'rainbow-delimiters)

(custom-set-faces
 '(rainbow-delimiters-depth-1-face ((t (:inherit font-lock-function-name-face))))
 '(rainbow-delimiters-depth-2-face ((t (:inherit font-lock-variable-name-face))))
 '(rainbow-delimiters-depth-3-face ((t (:inherit font-lock-keyword-face))))
 '(rainbow-delimiters-depth-4-face ((t (:inherit font-lock-comment-face))))
 '(rainbow-delimiters-depth-5-face ((t (:inherit font-lock-type-face))))
 '(rainbow-delimiters-depth-6-face ((t (:inherit font-lock-constant-face))))
 '(rainbow-delimiters-depth-7-face ((t (:inherit font-lock-builtin-face))))
 '(rainbow-delimiters-depth-8-face ((t (:inherit font-lock-string-face))))
 '(rainbow-delimiters-depth-9-face ((t (:inherit font-lock-doc-face))))
 '(rainbow-delimiters-unmatched-face ((((class color) (min-colors 89)) (:foreground "#ffffff" :background "#a40000" :bold t))))
)

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
     ("\\<[A-Z]+[A-Z_]+\\>" . font-lock-constant-face)
     ;; hexadecimal numbers
     ("\\<0[xX][0-9A-Fa-f]+\\>" . font-lock-constant-face)
     ;; integer/float/scientific numbers
     ("\\<[\\-+]*[0-9]*\\.?[0-9]+\\([ulUL]+\\|[eE][\\-+]?[0-9]+\\)?\\>" . font-lock-constant-face)
     ;; user-types
     ("\\<[A-Za-z_]+[A-Za-z_0-9]*_\\(t\\|ptr\\|c\\|e\\)\\>" . font-lock-type-face)
     )))

(add-hook 'c++-mode-hook 'c++-font-lock-fix)

(defun custom-c-hook ()
  (interactive)
  "Function to be called when entering into c-mode."
  (auto-complete-mode t)
  (make-local-variable 'ac-auto-start)
  (setq ac-auto-start 2)
  (make-local-variable 'ac-sources)
  (add-to-list 'ac-sources 'ac-source-words-in-same-mode-buffers)
  (add-to-list 'ac-sources 'ac-source-dictionary)
  (add-to-list 'ac-sources 'ac-source-etags))

(add-hook 'c-mode-common-hook 'custom-c-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compilation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring Compilation Mode")

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
;; (setq py-python-command "/usr/bin/python3")
;; (setq jedi:server-command (quote ("python3" "/home/dleslie/.emacs.d/elpa/jedi-20130714.1415/jediepcserver.py")))
;; (setq python-shell-interpreter "python3")
(setq py-python-command "/usr/bin/python")
(setq jedi:server-command (quote ("python" "/home/dleslie/.emacs.d/elpa/jedi-20140321.1323/jediepcserver.py")))
(setq python-shell-interpreter "python")
(setq python-indent-offset 4)

(defun python-custom-hook ()
  (jedi:setup))

(add-hook 'python-mode-hook 'python-custom-hook)

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

(defun custom-scheme-hook ()
  (scheme-ac-hook))

(add-hook 'scheme-mode-hook 'custom-scheme-hook)

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
(setq standard-indent 2)
(setq tab-stop-list (number-sequence 2 200 2))
(setq tab-width 4)
(setq truncate-lines t)
(setq visual-line-mode t)
(setq word-wrap t)
(setq make-backup-files nil)
(setq ac-etags-use-document t)

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

(global-set-key (kbd "C-c h") 'helm-mini)
(global-set-key (kbd "C-c g") 'helm-git-grep)

(define-key ac-mode-map  [(control return)] 'ac-clang-at-will)

(message "Post Init Complete.")

