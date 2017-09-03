;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Semantic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring Semantic and CEDET")

(require 'semantic)
(require 'semantic/bovine)
(require 'semantic/bovine/c)
(require 'semantic/bovine/el)
(require 'semantic/bovine/gcc)
(require 'semantic/bovine/make)
(require 'semantic/ia)
(require 'semantic/senator)
(require 'semantic/analyze)
(require 'srecode)

(when my-system-include-paths
  (mapc #'(lambda (s) (semantic-add-system-include s)) my-system-include-paths))

(add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode t)
(add-to-list 'semantic-default-submodes 'global-semantic-decoration-mode t)
(add-to-list 'semantic-default-submodes 'global-semantic-highlight-func-mode t)
(add-to-list 'semantic-default-submodes 'global-semantic-highlight-edits-mode t)
(add-to-list 'semantic-default-submodes 'global-semantic-show-unmatched-syntax-mode t)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-local-symbol-highlight-mode t)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode t)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-completions-mode t)
(add-to-list 'semantic-default-submodes 'global-semantic-show-parser-state-mode t)
(add-to-list 'semantic-default-submodes 'global-cedet-m3-minor-mode t)

(global-ede-mode 1)

(setq semanticdb-find-default-throttle
      '(local file unloaded project system recursive omniscience))

(semantic-mode 1)

(with-eval-after-load "company"
  (add-to-list 'company-backends 'company-semantic))

(defun my-semantic-jump ()
  (local-set-key "\M-." 'semantic-ia-fast-jump))

(add-hook 'semantic-mode-hook 'my-semantic-jump)
