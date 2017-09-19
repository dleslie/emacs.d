;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc Custom
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring Miscellaneous")

(require-package 'gist)
(require-package 'dictionary)
(require-package 'menu-bar+)
(require-package 'fuzzy)

(with-eval-after-load "fuzzy"
  (turn-on-fuzzy-isearch))

;; From http://stackoverflow.com/a/20788581
(with-demoted-errors
    "Error: %S"
  (require 'ansi-color)
  (defun my-colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer))

(show-paren-mode 1)

(defun my-try-to-add-imenu ()
  (condition-case nil (imenu-add-to-menubar "Imenu") (error nil)))
(add-hook 'font-lock-mode-hook 'my-try-to-add-imenu)

(eval-after-load "menu-bar" '(require 'menu-bar+))

(require-package 'borland-blue-theme nil t)

(when (find-exe "ag")
  (require-package 'ag))

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

(require-package 'git-gutter)
(global-git-gutter-mode t)
(setq git-gutter:visual-line t)

(global-hl-line-mode t)
(global-flycheck-mode t)

(require-package 'quiz)

(when (find-exe "ag")
  (require-package 'ag))

(reset-theme)
