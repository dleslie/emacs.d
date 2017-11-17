;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clojure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when-find-exe/error
    "clojure" "Could not locate clojure."
    (message "Configuring Clojure")
    
    (require-package 'clojure-mode)
    (require-package 'cider)
    
    (add-hook 'clojure-mode-hook 'cider-mode)
    (add-hook 'cider-mode-hook 'eldoc-mode)
		       
    (add-to-list 'auto-mode-alist '("\\.clj\\'" . clojure-mode))
		       
    (with-eval-after-load "auto-complete"
      (require-package 'ac-cider)
      (defun ac-cider-enable-hook ()
	(make-local-variable 'ac-sources)
	(add-to-list 'ac-sources 'ac-source-cider))
      (add-hook 'cider-mode-hook 'ac-cider-enable-hook))

    (with-eval-after-load "flycheck"
      (require-package 'flycheck-clojure)
      (with-eval-after-load "flycheck-clojure"
	(add-hook 'clojure-mode-hook 'flycheck-clojure-setup))))
