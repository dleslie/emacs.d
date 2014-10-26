(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(visible-bell t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil :family "Inconsolata" :foundry "unknown" :slant normal :weight normal :height 83 :width normal))))
 '(mode-line ((t (:background "#191919" :foreground "#BBBBBB" :box nil))))
 '(mode-line-highlight ((t (:box nil))))
 '(rainbow-delimiters-depth-1-face ((t (:inherit font-lock-function-name-face))))
 '(rainbow-delimiters-depth-2-face ((t (:inherit font-lock-variable-name-face))))
 '(rainbow-delimiters-depth-3-face ((t (:inherit font-lock-keyword-face))))
 '(rainbow-delimiters-depth-4-face ((t (:inherit font-lock-comment-face))))
 '(rainbow-delimiters-depth-5-face ((t (:inherit font-lock-type-face))))
 '(rainbow-delimiters-depth-6-face ((t (:inherit font-lock-constant-face))))
 '(rainbow-delimiters-depth-7-face ((t (:inherit font-lock-builtin-face))))
 '(rainbow-delimiters-depth-8-face ((t (:inherit font-lock-string-face))))
 '(rainbow-delimiters-depth-9-face ((t (:inherit font-lock-doc-face))))
 '(rainbow-delimiters-unmatched-face ((((class color) (min-colors 89)) (:foreground "#ffffff" :background "#a40000" :bold t)))))

(load-file (concat user-emacs-directory "/cedet/cedet-devel-load.el"))
(load-file (concat user-emacs-directory "/cedet/contrib/cedet-contrib-load.el"))

(message "Adding Post Init Hook")

(defun post-init-hook ()
  (load "~/.emacs.d/post-init.el"))

(add-hook 'after-init-hook 'post-init-hook)

(put 'downcase-region 'disabled nil)
