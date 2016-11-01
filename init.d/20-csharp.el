;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring C# Mode")

(require-package 'omnisharp)

(with-eval-after-load "company"
  (defun omni-company-fix ()
    (make-local-variable 'company-backends)
    (setq company-backends (list 'company-omnisharp)))
  (add-hook 'csharp-mode-hook 'omni-company-fix))
(add-hook 'csharp-mode-hook 'omnisharp-mode)
(add-hook 'csharp-mode-hook 'eldoc-mode)
