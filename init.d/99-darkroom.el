(use-package darkroom
  :init
  ;; Enable darkroom-tentative-mode in all text mode buffers
  (add-hook 'text-mode-hook #'darkroom-tentative-mode)
  ;; Hide menubar when darkroom is active
  (add-hook 'darkroom-tentative-mode-hook #'menu-bar-mode)
  ;; Hide linewrap arrows and other indicators when in darkroom mode
  (add-hook 'darkroom-tentative-mode-hook #'visual-line-mode))
