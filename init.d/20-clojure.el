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

  (with-eval-after-load "auto-complete"
    (require-package 'ac-cider)
    (add-hook 'cider-mode-hook
	      (lambda ()
		(add-to-list 'ac-sources 'ac-source-cider))))

  (eval-after-load 'flycheck
    '(progn
       (require-package 'flycheck-clojure)
       (flycheck-clojure-setup))))
