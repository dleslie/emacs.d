
(require-package 'ggtags)

(add-hook 'c-mode-hook #'ggtags-mode)
(add-hook 'c++-mode-hook #'ggtags-mode)

(with-eval-after-load "company-mode"
  (add-to-list 'company-backends 'company-gtags))
