;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc Custom
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring Miscellaneous")

(require-package 'gist)
(require-package 'dictionary)
(require-package 'menu-bar+)
(require-package 'quiz)
(require-package 'elfeed)
(require-package 'smex)
(require-package 'nyan-mode)
(require-package 'projectile)
(require-package 'dumb-jump)

(when (find-exe "ag")
  (require-package 'ag))

(require-package 'git-gutter)
(with-eval-after-load "git-gutter"
  (global-git-gutter-mode t)
  (setq git-gutter:visual-line t))

(require-package 'fuzzy)
(with-eval-after-load "fuzzy"
  (turn-on-fuzzy-isearch))

(defun my-try-to-add-imenu ()
  (condition-case nil (imenu-add-to-menubar "Imenu") (error nil)))
(add-hook 'font-lock-mode-hook 'my-try-to-add-imenu)

(eval-after-load "menu-bar" '(require 'menu-bar+))

;; From https://stackoverflow.com/questions/4012321/how-can-i-access-the-path-to-the-current-directory-in-an-emacs-directory-variabl
(defun my-dir-locals-dir ()
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

(global-eldoc-mode t)
(global-flycheck-mode t)
(global-hl-line-mode t)
(ido-mode t)
(nyan-mode t)
(projectile-global-mode t)
(show-paren-mode t)

(reset-theme)
