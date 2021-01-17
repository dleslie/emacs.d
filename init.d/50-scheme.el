;;; 50-scheme.el --- imenu

;;; Commentary:

;; Emacs configuration of Dan Leslie.
;; dan@ironoxide.ca

;;; Code:

(use-package geiser
  :config
  (setq geiser-chicken-binary (or (executable-find "chicken-csi") (executable-find "csi"))))

(provide '50-scheme)
;;; 50-scheme.el ends here
