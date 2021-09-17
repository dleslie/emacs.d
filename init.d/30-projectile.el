;;; 30-projectile.el --- projectile

;;; Commentary:

;; Emacs configuration of Dan Leslie.
;; dan@ironoxide.ca

;;; Code:

(use-package projectile
  :init
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))

(provide '30-projectile)
;;; 30-projectile.el ends here
