;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Semantic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring Semantic and CEDET")

(require 'semantic)
(require 'semantic/bovine/gcc)

(when my-system-include-paths
  (mapc #'(lambda (s) (semantic-add-system-include s)) my-system-include-paths))

(semantic-mode 1)

(defun my-semantic-mode-hook ()
  (semantic-idle-completions-mode 1)
  (semantic-idle-summary-mode 1)
  (semantic-idle-scheduler-mode 1)
  (semantic-idle-breadcrumbs-mode 1))

(add-hook 'c-mode-common-hook #'my-semantic-mode-hook)
