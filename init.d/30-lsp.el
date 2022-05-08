;;; 30-lsp.el --- lsp

;;; Commentary:

;; Emacs configuration of Dan Leslie.
;; dan@ironoxide.ca

;;; Code:

(use-package lsp-mode
  :init
  (require 'lsp-clangd)
  (require 'lsp-dockerfile)
  (require 'lsp-javascript)
  (require 'lsp-css)
  (require 'lsp-bash)
  (require 'lsp-html)
  (require 'lsp-rust)
  (require 'lsp-racket)
  (require 'lsp-json)
  (require 'lsp-pyls)

  (add-hook 'rust-mode-hook 'lsp)
  (add-hook 'ruby-mode-hook 'lsp)
  (add-hook 'python-mode-hook 'lsp)
  (add-hook 'js2-mode-hook 'lsp)
  (add-hook 'c-mode-hook 'lsp)
  (add-hook 'c++-mode-hook 'lsp)
  (add-hook 'js2-mode-hook 'lsp)
  (add-hook 'go-mode-hook 'lsp)
  (add-hook 'bash-mode-hook 'lsp)
  (add-hook 'dockerfile-mode-hook 'lsp)
  (add-hook 'csharp-mode-hook 'lsp)
  
  (setq lsp-modeline-code-actions-segments '(count icon)))

(use-package lsp-ui
  :after lsp-mode
  :init
  (setq lsp-ui-sideline-update-mode 'line
				lsp-ui-sideline-delay 0.2
				lsp-ui-imenu-auto-refresh t
				lsp-ui-sideline-show-code-actions nil)
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))

(use-package lsp-treemacs
  :after lsp-mode)

(use-package lsp-docker
  :after lsp-mode
	:init
	(defvar lsp-docker-client-packages
		'(lsp-css lsp-clients lsp-bash lsp-go lsp-pyls lsp-html lsp-typescript
							lsp-terraform lsp-cpp))
	(defvar lsp-docker-client-configs
		(list
		 (list :server-id 'bash-ls :docker-server-id 'bashls-docker :server-command "bash-language-server start")
		 (list :server-id 'clangd :docker-server-id 'clangd-docker :server-command "ccls")
		 (list :server-id 'css-ls :docker-server-id 'cssls-docker :server-command "css-languageserver --stdio")
		 (list :server-id 'dockerfile-ls :docker-server-id 'dockerfilels-docker :server-command "docker-langserver --stdio")
		 (list :server-id 'gopls :docker-server-id 'gopls-docker :server-command "gopls")
		 (list :server-id 'html-ls :docker-server-id 'htmls-docker :server-command "html-languageserver --stdio")
		 (list :server-id 'pyls :docker-server-id 'pyls-docker :server-command "pyls")
		 (list :server-id 'ts-ls :docker-server-id 'tsls-docker :server-command "typescript-language-server --stdio"))))

(provide '30-lsp)
;;; 30-lsp.el ends here
