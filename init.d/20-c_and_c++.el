;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C/C++
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring C and C++")

(add-hook 'c++-mode-hook (lambda () (eldoc-mode 1)))
(add-hook 'c-mode-hook (lambda () (eldoc-mode 1)))

(when (and (find-exe "clang") (find-exe "cmake"))
  (message "Configuring Irony")
  (require-package 'irony)
  (with-eval-after-load "irony"
    ;; Set the buffer size to 64K on Windows (from the original 4K)
    (when (boundp 'w32-pipe-buffer-size)
      (setq irony-server-w32-pipe-buffer-size (* 64 1024)))
    (add-hook 'c++-mode-hook 'irony-mode)
    (add-hook 'c-mode-hook 'irony-mode)
    (add-hook 'objc-mode-hook 'irony-mode)

    (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

    (with-eval-after-load "flycheck"
      (require-package 'flycheck-irony)
      (with-eval-after-load "flycheck-irony"
	(add-hook 'flycheck-mode-hook #'flycheck-irony-setup)))

    (require-package 'irony-eldoc)
    (with-eval-after-load "irony-eldoc"
      (add-hook 'irony-mode-hook #'irony-eldoc))

    (with-eval-after-load "company"
      (require-package 'company-irony)
      (with-eval-after-load "company-irony"
	(add-to-list 'company-backends 'company-irony)))))

(with-eval-after-load "company"
  (require-package 'company-c-headers)
  (with-eval-after-load "company-c-headers"
    (add-to-list 'company-backends 'company-c-headers)))

(with-eval-after-load "auto-complete"
  (require-package 'ac-c-headers)
  (defun ac-source-c-headers-enable-hook ()
    (make-local-variable 'ac-sources)
    (add-to-list 'ac-sources 'ac-source-c-headers)
    (add-to-list 'ac-sources 'ac-source-c-header-symbols t))
  (with-eval-after-load "ac-c-headers"
    (add-hook 'c-mode-hook 'ac-source-c-headers-enable-hook)))
