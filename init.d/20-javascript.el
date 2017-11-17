;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Javascript
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring Javascript")

(require-package 'js2-mode)
(require-package 'tide)
(require-package 'web-mode)

(add-to-list 'auto-mode-alist '("\\.js?\\'" . js2-mode))

(when-find-exe/error "tern" "Could not locate tern."
  (message "Configuring Tern")
  
  (require-package 'tern)

  (defun js2-mode-custom-hook ()
    (tern-mode t))

  (add-hook 'js2-mode-hook 'js2-mode-custom-hook)

  (with-eval-after-load "company"
    (require-package 'company-tern)))

(with-eval-after-load "auto-complete"
  (require-package 'ac-js2))

(when-find-exe/error "tsc" "Could not locate tsc."
  (message "Configuring Typescript")

  (add-to-list 'auto-mode-alist '("\\.ts?\\'" . typescript-mode))
  
  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1))

  ;; formats the buffer before saving
  (add-hook 'before-save-hook 'tide-format-before-save)
  (add-hook 'js2-mode-hook #'setup-tide-mode)
  (add-hook 'typescript-mode-hook #'setup-tide-mode)

  ;; format options
  (setq tide-format-options '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t :placeOpenBraceOnNewLineForFunctions nil))

  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (add-hook 'web-mode-hook
	    (lambda ()
	      (when (string-equal "tsx" (file-name-extension buffer-file-name))
		(setup-tide-mode))))
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
  (add-hook 'web-mode-hook
	    (lambda ()
	      (when (string-equal "jsx" (file-name-extension buffer-file-name))
		(setup-tide-mode)))))

