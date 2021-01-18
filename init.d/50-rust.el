;;; 50-rust.el --- rust

;;; Commentary:

;; Emacs configuration of Dan Leslie.
;; dan@ironoxide.ca

;;; Code:

(use-package rust-mode
  :after lsp-mode
  :config
  (setq rust-format-on-save t
	lsp-rust-all-features t
	lsp-rust-build-on-save nil)
  (when (executable-find "racer")
    (setq lsp-rust-racer-completion t))
  (when (executable-find "ra_lsp_server")
    (setq
     lsp-rust-racer-completion nil
     lsp-rust-server 'rust-analyzer))
  (defun my-ra-hack ()
    (let ((clients (lsp--filter-clients
		    (lambda (client)
		      (equalp 'rust-analyzer (lsp--client-server-id client))))))
      (when clients
	(mapcar (lambda (client)
		  (setf (lsp--client-priority client) 2))
		clients))))
  :hook
  ((rust-mode . my-ra-hack)))

(use-package flycheck-rust
  :after rust-mode
  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(provide '50-rust)
;;; 50-rust.el ends here
