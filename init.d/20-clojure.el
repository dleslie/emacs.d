;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clojure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (or (find-exe "lein") (find-exe "clojure"))
  (message "Configuring Clojure")

  (require-package 'clojure-mode)
  (require-package 'cider)

  (add-hook 'clojure-mode-hook 'cider-mode)
  (add-hook 'cider-mode-hook 'eldoc-mode)
  
  (add-to-list 'auto-mode-alist '("\\.clj\\'" . clojure-mode))

  (eval-after-load 'flycheck
    '(progn
       (require-package 'flycheck-clojure)
       (flycheck-clojure-setup))))
