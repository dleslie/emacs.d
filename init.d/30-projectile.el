;;; 30-projectile.el --- projectile

;;; Commentary:

;; Emacs configuration of Dan Leslie.
;; dan@ironoxide.ca

;;; Code:

(use-package projectile
  :init
  (projectile-mode +1)
  :config
  
  (when (string-match-p "Microsoft" (shell-command-to-string "uname -a"))
    (setq projectile-indexing-method 'native))
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(provide '30-projectile)
;;; 30-projectile.el ends here
