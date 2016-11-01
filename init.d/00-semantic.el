;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Semantic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring Semantic and CEDET")

(require 'semantic)
(require 'semantic/ia)
(require 'semantic/bovine/el)
(require 'semantic/bovine/gcc)
(require 'semantic/senator)
(require 'srecode)

(when my-system-include-paths
  (mapc #'(lambda (s) (semantic-add-system-include s)) my-system-include-paths))

(semantic-mode 1)

(with-eval-after-load "company"
  (add-to-list 'company-backends 'company-semantic))

(defun my-semantic-jump ()
  (local-set-key "\M-." 'semantic-ia-fast-jump))

(add-hook 'semantic-mode-hook 'my-semantic-jump)
