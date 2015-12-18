;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Projectile
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring Projectile")

(require-package 'projectile)
(projectile-global-mode t)

(eval-after-load "helm"
  '(progn
     (require-package 'helm-projectile)
     (helm-projectile-on)))
