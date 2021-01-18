;;; 99-themes.el --- themes

;;; Commentary:

;; Emacs configuration of Dan Leslie.
;; dan@ironoxide.ca

;;; Code:

(defun acquire-themes (&optional immediate)
  "Acquires themes.
Will not defer loading if IMMEDIATE is true."
  (let ((active-theme (car custom-enabled-themes)))
    (use-package afternoon-theme :defer (not immediate))
    (use-package alect-themes :defer (not immediate))
    (use-package ample-theme :defer (not immediate))
    (use-package clues-theme :defer (not immediate))
    (use-package color-theme-sanityinc-tomorrow :defer (not immediate))
    (use-package constant-theme :defer (not immediate))
    (use-package cyberpunk-theme :defer (not immediate))
    (use-package flatland-theme :defer (not immediate))
    (use-package gruber-darker-theme :defer (not immediate))
    (use-package gruvbox-theme :defer (not immediate))
    (use-package moe-theme :defer (not immediate))
    (use-package sexy-monochrome-theme :defer (not immediate))
    (use-package solarized-theme :defer (not immediate))
    (use-package zenburn-theme :defer (not immediate))
    (when (not (ignore-errors (change-theme active-theme)))
      (change-theme 'tango))))

;; Running Emacs as a daemon doesn't properly configure default faces
;; until after the first window is created. So we attach a hook to the
;; cache faces method in order to wait for that to occur.
(if (daemonp)
    (add-hook 'after-cache-faces-hook 'acquire-themes)
  (progn
    (acquire-themes)
    (add-hook 'after-init-hook 'acquire-themes)))

(provide '99-themes)
;;; 99-themes.el ends here
