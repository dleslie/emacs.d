;;; 30-projectile.el --- imenu

;;; Commentary:

;; Emacs configuration of Dan Leslie.
;; dan@ironoxide.ca

;;; Code:

(use-package projectile
  :bind
  (:map projectile-mode-map
        ("C-c p f" . projectile-find-file)
        ("C-c p p" . projectile-switch-project)
        ("C-c p d" . projectile-find-dir)
        ("C-c p g" . projectile-grep))
  :init
  (setq projectile-switch-project-action 'projectile-find-dir
        projectile-find-dir-includes-top-level t)
  (projectile-mode t))

(provide '30-projectile)
;;; 30-projectile.el ends here
