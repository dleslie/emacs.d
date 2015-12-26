;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Semantic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring Semantic and CEDET")

(require 'semantic)
(require 'semantic/bovine/gcc)

(when my-system-include-paths
  (mapc #'(lambda (s) (semantic-add-system-include s)) my-system-include-paths))

(semantic-mode 1)
(global-semantic-idle-completions-mode 1)
(global-semantic-idle-summary-mode 1)
(global-semantic-idle-scheduler-mode 1)
(global-semantic-idle-breadcrumbs-mode 1)
