;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Semantic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring Semantic and CEDET")

(require 'semantic)
(require 'semantic/bovine/gcc)

(when my-system-include-paths
  (mapc #'(lambda (s) (semantic-add-system-include s)) my-system-include-paths))

(semanticdb-enable-gnu-global-databases 'c-mode)
(semanticdb-enable-gnu-global-databases 'c++-mode)

(defun enable-semantic-mode ()
  (interactive)
  (semanticdb-minor-mode 1)
  (semantic-idle-scheduler-mode 1)
  (semantic-idle-summary-mode 1)
  (semantic-idle-scheduler-mode 1)
  (semantic-mode 1))
