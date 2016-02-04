;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rust
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (find-exe "rustc")
  (message "Configuring Rust")

  (require-package 'rust-mode)

  (when (find-exe "racer")
    (require-package 'racer)
    (require-package 'toml-mode)

    (setq racer-cmd (find-exe "racer"))
    (setq racer-rust-src-path (getenv "RUST_SRC_PATH"))

    (defun my-rust-hook ()
      (interactive)
      (company-mode 1)
      (racer-mode 1)
      (eldoc-mode 1))
    (add-hook 'rust-mode-hook #'my-rust-hook))

  (eval-after-load "flycheck"
    '(progn
       (require-package 'flycheck-rust)
       (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))))
