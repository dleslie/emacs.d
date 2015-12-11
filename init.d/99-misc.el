;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc Custom
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring Miscellaneous")

(require-package 'gist)
(require-package 'dictionary)

(setq gc-cons-threshold 20000000
      make-backup-files nil
      indent-tabs-mode nil)

;; From http://stackoverflow.com/a/20788581
(with-demoted-errors
    "Error: %S"
  (require 'ansi-color)
  (defun my-colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer))

(reset-theme)
