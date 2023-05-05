(use-package janet-mode)
(straight-use-package
 '(inf-janet
   :type git
   :host github
   :repo "velkyel/inf-janet"))
(add-hook 'janet-mode-hook #'inf-janet-minor-mode)
(add-hook 'janet-mode-hook #'paredit-mode)
