;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rust
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (find-exe "rustc")
  (message "Configuring Rust")

  (require-package 'rust-mode)

  (when (find-exe "racer")
    (require-package 'racer)
    (require-package 'toml-mode)

    (add-hook 'rust-mode-hook #'racer-mode)
    (add-hook 'racer-mode-hook #'eldoc-mode)

    (with-eval-after-load "company"
      (add-hook 'racer-mode-hook #'company-mode))
    
    (setq racer-cmd (find-exe "racer"))
    (setq racer-rust-src-path (getenv "RUST_SRC_PATH")))

  (eval-after-load "flycheck"
    '(progn
       (require-package 'flycheck-rust)
       (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))))
