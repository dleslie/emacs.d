
(require-package 'ggtags)

(add-hook 'c-mode-hook #'ggtags-mode)
(add-hook 'c++-mode-hook #'ggtags-mode)

(with-eval-after-load "company"
  (add-to-list 'company-backends 'company-gtags))

(with-eval-after-load "auto-complete"
  (add-to-list 'ac-sources 'ac-source-gtags))
