;;; 30-lsp.el --- lsp

;;; Commentary:

;; Emacs configuration of Dan Leslie.
;; dan@ironoxide.ca

;;; Code:

(use-package lsp-mode
  :commands lsp
  :hook
  ((rust-mode . lsp)
   (ruby-mode . lsp)
   (python-mode . lsp)
   (js2-mode . lsp)
   (c-mode . lsp)
   (c++-mode . lsp)
   (js2-mode . lsp)
   (go-mode . lsp)
   (bash-mode . lsp)
   (dockerfile-mode . lsp))
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
  (require 'lsp-pyls))

(use-package lsp-ui
  :after lsp-mode)

(use-package lsp-treemacs
  :after lsp-mode)

(use-package lsp-docker
  :defer t)

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
   (list :server-id 'ts-ls :docker-server-id 'tsls-docker :server-command "typescript-language-server --stdio")))

(use-package lsp-docker)
(lsp-docker-init-clients
 :path-mappings '(("path-to-projects-you-want-to-use" . "/projects"))
 :client-packages lsp-docker-client-packages
 :client-configs lsp-docker-client-configs)

(provide '30-lsp)
;;; 30-lsp.el ends here
