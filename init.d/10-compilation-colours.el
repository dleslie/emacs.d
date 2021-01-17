;;; 10-compilation-colours.el --- imenu

;;; Commentary:

;; Emacs configuration of Dan Leslie.
;; dan@ironoxide.ca

;;; Code:

(require 'ansi-color)

(defun my-compilation-custom-hook ()
  "Enable visual line mode."
  (visual-line-mode 1))
(defun my-colorize-compilation-buffer ()
  "Enable ansi colors."
  (read-only-mode nil)
  (ansi-color-apply-on-region (point-min) (point-max))
  (read-only-mode t))

(add-hook 'compilation-mode-hook 'my-compilation-custom-hook)
(add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer)

(provide '10-compilation-colours)
;;; 10-compilation-colours.el ends here
