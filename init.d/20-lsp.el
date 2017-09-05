;; (when (require-package 'lsp-mode)
;;   (require 'lsp-flycheck)

;;   (when (and (not (find-exe "hie")) (find-exe "stack") (find-exe "git"))
;;     (with-temp-buffer
;;       (and
;;        (message "Installing Haskell IDE Engine")
;;        (cd temporary-file-directory)
;;        (message "Cloning HIE Git Repo")
;;        (shell-command "git clone https://github.com/haskell/haskell-ide-engine")
;;        (cd (expand-file-name "haskell-ide-engine" temporary-file-directory))
;;        (shell-command "stack install"))))
;;   (when (find-exe "hie")
;;     (require-package 'haskell-mode)
;;     (require-package 'lsp-haskell)
;;     (add-hook 'haskell-mode-hook #'lsp-mode))

;;   ; check pip list
;;   (when (and (find-exe "pip3") (not (find-exe "python-language-server")) )
;;     (message "Installing Python Language Server")
;;     (shell-command "pip install python-language-server"))
;;   (when (find-exe "python-language-server")
;;     (require-package 'lsp-python)
;;     (add-hook 'python-mode-hook #'lsp-mode))

;;   (when (and (not (find-exe "go-langserver")) (find-exe "go"))
;;     (message "Installing Go Language Server")
;;     (shell-command "go get github.com/sourcegraph/go-langserver"))
;;   (when (find-exe "go-langserver")
;;     (require-package 'lsp-go)
;;     (add-hook 'go-mode-hook #'lsp-mode))

;;   (when (and (not (find-exe "rls")) (find-exe "rustup"))
;;     (and
;;      (message "Updating Rust")
;;      (shell-command "rustup self update")
;;      (shell-command "rustup update nightly")
;;      (message "Installing Rust Language Server")
;;      (shell-command "rustup component add rls --toolchain nightly")
;;      (shell-command "rustup component add rust-analysis --toolchain nightly")
;;      (shell-command "rustup component add rust-src --toolchain nightly")))
;;   (when (find-exe "rls")
;;     (require-package 'lsp-rust)
;;     (add-hook 'rust-mode-hook #'lsp-mode))

;;   (add-hook 'prog-major-mode #'lsp-mode))

