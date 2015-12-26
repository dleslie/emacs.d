;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Haskell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (find-exe "ghc")
  (message "Configuring Haskell")

  (require-package 'ghc)
  (require-package 'haskell-mode)

  (eval-after-load "company"
    '(progn
       (require-package 'company-ghc)
       (add-to-list 'company-backends 'company-ghc)))

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
