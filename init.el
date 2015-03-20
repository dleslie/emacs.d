;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Local Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Setting Local Configuration")

(setq my-optional-init
  '(auto-complete
    geiser
    ;; chicken
    ;; ido
    js2
    ruby
    python
    ;; tex
    c++
    haskell
    lisp
    mu4e))

(setq 
 org-directory 
 "~/Dropbox/org/")

(setq 
 tern-command 
 '("~/node_modules/.bin/tern"))

(setq
 user-mail-address 
 "dan@ironoxide.ca"
 user-full-name 
 "Dan Leslie"
 user-mail-login 
 "dleslie@gmail.com"
 user-mail-attachment-directory
 "/home/dleslie/Downloads/Attachments"
 mail-smtp-server
 "smtp.gmail.com"
 mail-smtp-port
 587
 mail-folder-inbox
 "/INBOX"
 mail-folder-drafts
 "/[Gmail].Drafts"
 mail-folder-sent
 "/[Gmail].Sent Mail"
 mail-folder-trash
 "/[Gmail].Trash"
 gnus-select-method
 '(nntp "GMane"
        (nntp-address "news.gmane.org")))

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

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Email
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
 "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

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

(global-set-key "\C-ct" 'org-todo-list)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cb" 'org-iswitchb)

(global-set-key "\C-cm" 'mu4e)

(global-set-key "\C-cg" 'magit-status)

(global-set-key (kbd "C-!") 'eshell-here)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Semantic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring Semantic and CEDET")

;; (require 'cedet)
(require 'srecode)
(require 'srecode/map)
(require 'advice)
(require 'eieio-opt)

(require 'semantic)
(require 'semantic/bovine/gcc)
(require 'semantic/ia)
;; (require 'semantic/imenu)
(require 'semantic/sb)

(mapc #'(lambda (s) (semantic-add-system-include s))
      system-include-paths)

(semanticdb-enable-gnu-global-databases 'c-mode)
(semanticdb-enable-gnu-global-databases 'c++-mode)

(defun enable-semantic-mode ()
  (interactive)

  ; (ede-mode 1)
  (semanticdb-minor-mode 1)
  (semantic-idle-scheduler-mode 1)
  (semantic-idle-summary-mode 1)
  (semantic-idle-scheduler-mode 1)

  (semantic-mode 1))

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

  ;; Some themes auto-apply themselves when required, which is uncool
  (unless (string-match "-theme" (symbol-name package-name))
    (require package-name nil 'noerror))
  
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

(require 'package)
(package-initialize)

(setq package-archives 
      '(("melpa" . "http://melpa.milkbox.net/packages/")
        ("gnu" . "http://elpa.gnu.org/packages/")
        ;; ("marmalade" . "http://marmalade-repo.org/packages/")
        ("org" . "http://orgmode.org/elpa/")))

(message "Check for packages")

(package-refresh-contents)

(setq my-package-list
      (list 'cl-lib
            'camcorder
            'dictionary
            'dired+
            'doremi
            'flx-ido
            'gist
            'help+
            'help-fns+
            'help-mode+
            'ido-ubiquitous
            'magit
            'magit-gh-pulls
            'magit-svn
            'markdown-mode
            'menu-bar+
            'nyan-mode
            'org-plus-contrib
            'paredit 
            'parenface 
            'popup
            'projectile
            'purpose
            'rainbow-mode 
            'smex 
            'web-mode
            'writegood-mode))

(setq my-theme-list
      (list 'moe-theme
            'tronesque-theme
            'anti-zenburn-theme
            'ample-theme
            'ample-zen-theme
            'twilight-theme
            'ubuntu-theme
            'zen-and-art-theme
            'zenburn-theme
            'bubbleberry-theme))

(when (memq 'auto-complete my-optional-init)
  (add-to-list 'my-package-list 'ac-capf)
  (add-to-list 'my-package-list 'auto-complete))

(when (memq 'chicken my-optional-init)
  (add-to-list 'my-package-list 'chicken-scheme))

(when (memq 'geiser my-optional-init)
  (add-to-list 'my-package-list 'geiser)
  (when (memq 'auto-complete my-optional-init)
    (add-to-list 'my-package-list 'ac-geiser)))

(when (memq 'js2 my-optional-init)
  (add-to-list 'my-package-list 'js2-mode)
  (when (memq 'auto-complete my-optional-init)
    (add-to-list 'my-package-list 'ac-js2))
  (add-to-list 'my-package-list 'tern)
  (add-to-list 'my-package-list 'tern-auto-complete))

(when (memq 'ruby my-optional-init)
  (when (memq 'auto-complete my-optional-init)
    (add-to-list 'my-package-list 'ac-inf-ruby))
  (add-to-list 'my-package-list 'enh-ruby-mode)
  (add-to-list 'my-package-list 'inf-ruby)
  (add-to-list 'my-package-list 'projectile-rails)
  (add-to-list 'my-package-list 'robe))

(when (memq 'python my-optional-init)
  (add-to-list 'my-package-list 'jedi)
  (add-to-list 'my-package-list 'python-environment))

(when (memq 'c++ my-optional-init)
  (add-to-list 'my-package-list 'function-args)
  (add-to-list 'my-package-list 'ggtags)
  (add-to-list 'my-package-list 'auto-complete-exuberant-ctags))

(when (memq 'haskell my-optional-init)
  (add-to-list 'my-package-list 'ghc))

(when (memq 'lisp my-optional-init)
  (when (memq 'auto-complete my-optional-init)
    (add-to-list 'my-package-list 'ac-slime))
  (add-to-list 'my-package-list 'slime))

(let ((loaded (eval (cons 'require-package (mapcar (lambda (x) `(quote ,x)) (append my-package-list my-theme-list))))))
  (message (format "Installed %s" loaded)))

;; Additional that require force loading
(require 'cl)
(require 'imenu)
(require 'cc-mode)
(require 'org-contacts)

(when (memq 'auto-complete my-optional-init)
  (require 'auto-complete-config))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ido
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (memq 'ido my-optional-init)
  (ido-mode t)
  (ido-everywhere t)
  (flx-ido-mode t)

  (setq ido-enable-flex-matching t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; elisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring elisp")

(require 'eldoc)
(defun custom-elisp-prog-hook ()
  (eldoc-mode 1))

(add-hook 'emacs-lisp-mode-hook 'custom-elisp-prog-hook)

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

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scheme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (memq 'chicken my-optional-init)

  (message "Configuring Chicken Scheme")

  (defun custom-scheme-hook ()
    (interactive)
    (setup-chicken-scheme))

  (add-hook 'scheme-mode-hook 'custom-scheme-hook))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LISP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (memq 'lisp my-optional-init)

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

(when (memq 'c++ my-optional-init)
  (message "Configuring C and C++")

  (defun custom-cc-prog-hook ()
    (enable-semantic-mode)
    (ggtags-mode 1))

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

(when (memq 'haskell my-optional-init)
  (autoload 'ghc-init "ghc" nil t)
  (autoload 'ghc-debug "ghc" nil t)
  (add-hook 'haskell-mode-hook (lambda () (ghc-init))))

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

(setq
 org-default-notes-file 
 (concat org-directory "notes.org")
 org-agenda-files 
 `(,(concat org-directory "todo.org") 
   ,(concat org-directory "agenda.org") )
 org-agenda-diary-file 
 (concat org-directory "diary.org")
 org-agenda-files 
 '("~/Dropbox/org/todo.org" "~/Dropbox/org/agenda.org")
 org-todo-keywords
 '((sequence "TODO(t)" "PROG(p)" "BLCK(b)" "STAL(s)" "|" "DONE(d)" "WONT(w)"))
 org-todo-keyword-faces
 '(("TODO" . (:foreground "white" :weight bold))
   ("DOIN" . (:foreground "green" :weight bold))
   ("BLCK" . (:foreground "red" :weight bold))
   ("STAL" . (:foreground "yellow" :weight bold))
   ("WONT" . (:foreground "grey" :weight bold))
   ("DONE" . (:foreground "black" :weight bold)))
 org-capture-templates
 '(("n" "Note" entry (file+headline (concat org-directory "notes.org") "Notes")
    "* %^{topic} %T %^g\n:CATEGORY: %^{category}\n%i%?\n")
   ("t" "To Do" entry (file+headline (concat org-directory "todo.org") "To Do")
    "* TODO %^{todo} %^{due} %^g\n:CATEGORY: %^{category}")
   ("d" "Daily review" entry (file+headline (concat org-directory "diary.org") "Daily Review") 
    "* %T %^g\n:CATEGORY: Review\n%?%[~/Dropbox/org/template_daily_review.org]") 
   ("i" "Idea" entry (file+headline (concat org-directory "ideas.org") "Ideas") 
    "* %^{topic} %T %^g\n:CATEGORY: Idea\n%i%?\n") 
   ("j" "Journal" entry (file+headline (concat org-directory "diary.org") "Journal") 
    "* %^{topic} %T %^g\n:CATEGORY: Journal\n%i%?\n")
   ("l" "Letter" entry (file+headline (concat org-directory "letters.org") "Letter") 
    "* %^{topic} %T %^g\n:CATEGORY: Letter\n%i%?\n")
   ("w" "Work Log" entry (file+headline (concat org-directory "work.org") "Work Log") 
    "* %^{topic} %T %^g\n:CATEGORY: Log\n%i%?\n")
   ("a" "Article" entry (file+headline (concat org-directory "articles.org") "Article")
    "* %^{topic} %T %^g\n:CATEGORY: Article\n%i%?\n")
   ("e" "Event" entry (file+headline (concat org-directory "agenda.org") "Events")
    "* %^{title} %^g\n%^{when}\n%i%?\n")
   ("c" "Contact" entry (file+headline (concat org-directory "addresses.org") "Addresses")
    "* %(org-contacts-template-name)\n:PROPERTIES:\n:EMAIL: %(org-contacts-template-email)\n:END:\n%i%?\n"))
 org-modules
 '(org-bbdb org-bibtex org-docview org-gnus org-info org-jsinfo org-habit org-irc org-mew 
   org-mhe org-rmail org-special-blocks org-vm org-wl org-w3m org-mouse org-bookmark 
   org-drill org-eshell org-invoice org-registry org-contacts))

(defun custom-org-hook ()
  (interactive)
  (visual-line-mode 1)
  (flyspell-mode 1)
  (writegood-mode 1))

(add-hook 'org-mode-hook 'custom-org-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TeX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (memq 'tex my-optional-init)
  (message "Configuring TeX")

  (unless (package-installed-p 'auctex)
    (package-install 'auctex))

  (let* ((path (elt (cadr (assq 'auctex package-alist)) 7)))
    (when (not (file-exists-p path))
      (message "Error loading AucTeX"))
    (when (file-exists-p path)
      (let ((path (concat path "/")))
        (add-to-list 'load-path path)

        (autoload 'TeX-load-hack
          (expand-file-name "tex-site.el" path))
        (TeX-load-hack)

        (load "preview.el" nil t t)
        (setq load-path (remove-if (lambda (val) (equal path val)) load-path)))))

  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (setq reftex-plug-into-AUCTeX t)

  (add-hook 'LaTeX-mode-hook 'visual-line-mode)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

  (add-hook 'LaTeX-mode-hook 'turn-on-reftex))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (memq 'python my-optional-init)
  (message "Configuring Python Mode")

  (jedi:install-server)

  (defun python-custom-hook ()
    (jedi:setup))

  (add-hook 'python-mode-hook 'python-custom-hook))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ruby
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (memq 'ruby my-optional-init)
  (message "Configuring Ruby Mode")

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
  (add-hook 'enh-ruby-mode-hook 'robe-mode)

  (when (memq 'auto-complete my-optional-init)
    (add-hook 'robe-mode-hook 'ac-robe-setup)))

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

(when (memq 'js2 my-optional-init)
  (defun js2-mode-custom-hook ()
    (tern-mode t))

  (when (memq 'auto-complete my-optional-init)
    (eval-after-load 'tern
      '(progn
         (require 'tern-auto-complete)
         (tern-ac-setup)))
    (setq tern-ac-on-dot t))

  (add-hook 'js2-mode-hook 'js2-mode-custom-hook)

  (add-to-list 'auto-mode-alist '("\\.js?\\'" . js2-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto-complete
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (memq 'auto-complete my-optional-init)
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

  (add-to-list 'ac-sources 'ac-source-capf)

  (defun ac-no-semantic-setup ()
    (make-local-variable 'ac-sources)
    (add-to-list 'ac-sources 'ac-source-words-in-same-mode-buffers))

  (defun ac-semantic-setup ()
    (make-local-variable 'ac-sources)
    (add-to-list 'ac-sources 'ac-source-semantic))

  (mapc #'(lambda (m) (add-hook m 'ac-no-semantic-setup))
        '(emacs-lisp-mode-hook
          lisp-mode-hook
          lisp-interaction-mode-hook
          scheme-mode-hook
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
        '(c-mode-common-hook))

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
    (when (memq 'chicken my-optional-init)
      (make-local-variable 'ac-sources)
      (add-to-list 'ac-sources ac-source-r7rs-symbols)
      (add-to-list 'ac-sources ac-source-r5rs-symbols)
      (add-to-list 'ac-sources ac-source-chicken-symbols)
      (add-to-list 'ac-sources ac-source-chicken-symbols-prefixed)))

  (defun ac-c-common-mode-setup ()
    (make-local-variable 'ac-sources)
    (add-to-list 'ac-sources 'ac-source-gtags))

  (defun ac-lisp-setup ()
    (make-local-variable 'ac-sources))

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
  (add-hook 'inf-ruby-mode-hook 'ac-inf-ruby-enable))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mu4e
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when
    (and (file-exists-p "/usr/local/share/emacs/site-lisp/mu4e")
         (memq 'mu4e my-optional-init))
  (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")

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

   mu4e-compose-signature
   (concat "-" user-full-name "\n")

   message-kill-buffer-on-exit
   t

   mu4e-attachment-dir
   user-mail-attachment-directory

   mu4e-compose-reply-to-address
   user-mail-address

   mu4e-headers-include-related
   t

   mu4e-headers-results-limit
   -1

   mu4e-sent-messages-behavior
   (quote sent)

   mu4e-update-interval
   350

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

(smex-initialize)
(projectile-global-mode t)

(nyan-mode t)

(add-hook 'prog-mode-hook 'show-paren-mode)
(add-hook 'prog-mode-hook 'rainbow-mode)

(setq gc-cons-threshold 20000000)

(winner-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; End
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Init Complete.")
