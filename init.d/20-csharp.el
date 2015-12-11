;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring C# Mode")

(require-package 'omnisharp)

(eval-after-load "company"
  '(add-to-list 'company-backends 'company-omnisharp))
(add-hook 'csharp-mode-hook 'omnisharp-mode)
