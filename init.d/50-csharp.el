;;; 50-csharp.el --- csharp

;;; Commentary:

;; Emacs configuration of Dan Leslie.
;; dan@ironoxide.ca

;;; Code:

(use-package csharp-mode)

(use-package omnisharp
  :after (csharp-mode company flycheck)
  :bind
  (:map csharp-mode-map
        ("M-." . omnisharp-go-to-definition)
        ("C-c C-c" . recompile))
  :init
  (add-hook 'csharp-mode-hook 'omnisharp-mode)
  (add-hook 'csharp-mode-hook 'flycheck-mode)
  (push 'company-omnisharp company-backends))

(when (not omnisharp-server-executable-path)
  (let ((omnisharp (executable-find "omnisharp")))
    (when omnisharp
      (setq omnisharp-server-executable-path omnisharp))))

(provide '50-csharp)
;;; 50-csharp.el ends here
