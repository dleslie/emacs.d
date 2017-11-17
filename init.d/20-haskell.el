;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Haskell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require-package 'haskell-mode)

(when-find-exe/error "ghc-mod" "Could not locate ghc-mod."
  (message "Configuring Haskell")

  (require-package 'ghc)

  (with-eval-after-load "company"
    (require-package 'company-ghc)
    (defun ghc-company-fix ()
      (make-local-variable 'company-backends)
      (setq company-backends (list 'company-haskell)))
    (add-hook 'haskell-mode-hook 'haskell-company-fix))

  (with-eval-after-load "auto-complete"
    (add-hook 'haskell-mode-hook
	      (lambda () (add-to-list 'ac-sources 'ac-source-ghc-mod))))

  (autoload 'ghc-init "ghc" nil t)
  (autoload 'ghc-debug "ghc" nil t)
  (add-hook 'haskell-mode-hook (lambda () (ghc-init)))

  (eval-after-load 'flycheck
    '(progn
       (require-package 'flycheck-ghcmod)
       (require-package 'flycheck-haskell)
       (add-hook 'flycheck-mode-hook #'flycheck-haskell-setup)
       (require 'flycheck-ghcmod)))
  )
