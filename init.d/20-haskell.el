;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Haskell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
