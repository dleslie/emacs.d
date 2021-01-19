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
  (push 'company-omnisharp company-backends)

  (when (not (boundp 'c-default-style))
    (setq c-default-style '()))
  (if (not (assoc 'csharp-mode c-default-style))
      (pushnew '(csharp-mode . "csharp") c-default-style)
    (setcdr (assoc 'csharp-mode c-default-style) "csharp"))

  (let* ((dotnet (executable-find "dotnet"))
	 (dotnet-script (executable-find "dotnet-script")))
    (when (and dotnet (not dotnet-script))
      (shell-command (concat dotnet " tool install -g dotnet-script"))))
  
  (defun my-csharp-repl ()
    "Switch to the CSharpRepl buffer, creating it if necessary."
    (interactive)
    (let ((repl (or (executable-find "dotnet-script") (executable-find "csharp"))))
      (when repl
	(if-let ((buf (get-buffer "*CSharpRepl*")))
            (pop-to-buffer buf)
	  (when-let ((b (make-comint "CSharpRepl" repl)))
            (switch-to-buffer-other-window b))))))
  (define-key csharp-mode-map (kbd "C-c C-z") 'my-csharp-repl)

  (when (or (not (boundp 'omnisharp-server-executable-path)) (not omnisharp-server-executable-path))
    (let ((omnisharp (executable-find "omnisharp")))
      (when omnisharp
	(setq omnisharp-server-executable-path omnisharp)))))

(provide '50-csharp)
;;; 50-csharp.el ends here
