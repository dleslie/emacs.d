;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rust
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when-find-exe/error "rustc" "Could not locate rustc."
  (message "Configuring Rust")

  (require-package 'rust-mode)

  (when-find-exe/error "racer" "Could not locate racer."
    (require-package 'racer)
    (require-package 'toml-mode)

    (add-hook 'rust-mode-hook #'racer-mode)
    (add-hook 'racer-mode-hook #'eldoc-mode)

    (with-eval-after-load "company"
      (add-hook 'racer-mode-hook #'company-mode))
    
    (setq racer-cmd (find-exe "racer"))
    (setq racer-rust-src-path (getenv "RUST_SRC_PATH"))))
