;;; 99-themes.el --- themes

;;; Commentary:

;; Emacs configuration of Dan Leslie.
;; dan@ironoxide.ca

;;; Code:

(defun acquire-themes (&optional defer)
  "Acquires themes.
Will not defer loading if IMMEDIATE is true."
  (let ((active-theme (car custom-enabled-themes)))
    (use-package afternoon-theme :defer defer)
    (use-package alect-themes :defer defer)
    (use-package ample-theme :defer defer)
    (use-package clues-theme :defer defer)
    (use-package color-theme-sanityinc-tomorrow :defer defer)
    (use-package constant-theme :defer defer)
    (use-package cyberpunk-theme :defer defer)
    (use-package flatland-theme :defer defer)
    (use-package gruber-darker-theme :defer defer)
    (use-package gruvbox-theme :defer defer)
    (use-package moe-theme :defer defer)
    (use-package sexy-monochrome-theme :defer defer)
    (use-package solarized-theme :defer defer)
    (use-package zenburn-theme :defer defer)
    (when (not (ignore-errors (change-theme active-theme)))
      (change-theme 'tango))))

;; Running Emacs as a daemon doesn't properly configure default faces
;; until after the first window is created. So we attach a hook to the
;; cache faces method in order to wait for that to occur.
(acquire-themes t)
(add-hook 'after-init-hook 'acquire-themes)

(provide '99-themes)
;;; 99-themes.el ends here
