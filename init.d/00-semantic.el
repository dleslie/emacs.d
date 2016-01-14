;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Semantic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring Semantic and CEDET")

(require 'semantic)
(require 'semantic/bovine/gcc)
(require 'srecode)
(require-package 'srefactor)

(when my-system-include-paths
  (mapc #'(lambda (s) (semantic-add-system-include s)) my-system-include-paths))

(semantic-mode 1)
(global-semantic-idle-scheduler-mode 1)
(global-semantic-idle-summary-mode 1)
(global-semantic-idle-breadcrumbs-mode 1)
(global-semantic-idle-completions-mode nil)

(with-eval-after-load "company"
  (add-to-list 'company-backends 'company-semantic))
