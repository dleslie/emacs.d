;;; 50-c.el --- c

;;; Commentary:

;; Emacs configuration of Dan Leslie.
;; dan@ironoxide.ca

;;; Code:

(when (executable-find "ccls")
  (use-package ccls
    :after lsp-mode
    :config
    (add-hook 'c-mode-hook 'lsp)
    (add-hook 'c++-mode-hook 'lsp)
    (setq ccls-executable (executable-find "ccls"))))

(when (executable-find "clang-format")
  (use-package clang-format
    :init
    (defun my-clang-format-on-save ()
      (make-local-variable 'before-save-hook)
      (add-hook 'before-save-hook 'clang-format-buffer))
    (add-hook 'c-mode-hook 'my-clang-format-on-save)))

;; Arduino ino files
(add-to-list 'auto-mode-alist '("\\.ino?\\'" . c++-mode))

(provide '50-c)
;;; 50-c.el ends here
