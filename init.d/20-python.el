;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (and (find-exe "pip") (find-exe "python"))
  (message "Configuring Python Mode")

  (require-package 'elpy)
  (elpy-enable))
