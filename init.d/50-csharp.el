;;; 50-csharp.el --- csharp

;;; Commentary:

;; Emacs configuration of Dan Leslie.
;; dan@ironoxide.ca

;;; Code:

(use-package csharp-mode
  :ensure t
  :init
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
  (defun my/csharp-mode-hook ()
    (setq-local lsp-auto-guess-root t)
    (setq-local indent-tabs-mode nil)
    (setq-local comment-column 40)
    (setq-local c-basic-offset 4)
    (lsp))
  (add-hook 'csharp-mode-hook #'my/csharp-mode-hook))

(provide '50-csharp)
;;; 50-csharp.el ends here
