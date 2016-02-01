;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc Custom
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring Miscellaneous")

(require-package 'gist)
(require-package 'dictionary)
(require-package 'menu-bar+)

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
(require-package 'paper-theme nil t)
(require-package 'white-sand-theme nil t)

(reset-theme)
