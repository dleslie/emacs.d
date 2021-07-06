;;; 70-minimap.el --- minimap

;;; Commentary:

;; Emacs configuration of Dan Leslie.
;; dan@ironoxide.ca

;;; Code:

(use-package minimap
  :init
  (setq minimap-update-delay 0)
  (setq minimap-window-location 'right)
  (setq minimap-hide-fringes t)
  (setq minimap-always-recenter nil)
  (setq minimap-major-modes '(prog-mode text-mode org-mode)))

(provide '70-minimap)
;;; 70-minimap.el ends here
