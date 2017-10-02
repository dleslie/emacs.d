(when (find-exe "global")
  (message "Configuring gtags")
  (require-package 'ggtags)
  (with-eval-after-load "ggtags"
    (add-hook 'c-mode-hook #'ggtags-mode)
    (add-hook 'c++-mode-hook #'ggtags-mode)

    (with-eval-after-load "company"
      (add-to-list 'company-backends 'company-gtags))

    (with-eval-after-load "auto-complete"
      (defun ac-source-gtags-enable-hook ()
	(make-local-variable 'ac-sources)
	(add-to-list 'ac-sources 'ac-source-gtags))
      (add-hook 'ggtags-mode-hook 'ac-source-gtags-enable-hook))))
