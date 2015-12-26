;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clojure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (find-exe "clojure")
  (message "Configuring Clojure")

  (require-package 'clojure-mode)
  (require-package 'cider)
  (require-package 'clojure-cheatsheet)

  (add-hook 'clojure-mode-hook 'cider-mode)
  (add-hook 'cider-mode-hook 'eldoc-mode)
  
  (add-to-list 'auto-mode-alist '("\\.clj\\'" . clojure-mode))

  (eval-after-load "paredit"
    '(progn
       (add-hook 'clojure-mode-hook 'paredit-mode)
       (add-hook 'cider-mode-hook 'paredit-mode)))

  (eval-after-load 'flycheck
    '(progn
       (require-package 'flycheck-clojure)
       (flycheck-clojure-setup))))
