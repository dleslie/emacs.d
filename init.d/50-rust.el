;;; 50-rust.el --- rust

;;; Commentary:

;; Emacs configuration of Dan Leslie.
;; dan@ironoxide.ca

;;; Code:

(use-package rust-mode
  :config
  (setq rust-format-on-save t))

(use-package flycheck-rust
  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(provide '50-rust)
;;; 50-rust.el ends here
