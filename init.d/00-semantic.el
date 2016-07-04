;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Semantic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring Semantic and CEDET")

(require 'semantic)
(require 'semantic/bovine/el)
(require 'semantic/bovine/gcc)
(require 'semantic/bovine/c)
;(require 'semantic/senator)

;(require 'srecode)

(when my-system-include-paths
  (mapc #'(lambda (s) (semantic-add-system-include s)) my-system-include-paths))

(global-semantic-idle-scheduler-mode 1)
(global-semantic-idle-summary-mode nil)
(global-semantic-idle-breadcrumbs-mode nil)
(global-semantic-idle-completions-mode nil)
(global-semantic-idle-local-symbol-highlight-mode nil)

(semantic-mode 1)

(with-eval-after-load "company"
  (add-to-list 'company-backends 'company-semantic))
