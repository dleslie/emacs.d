;;; 99-themes.el --- themes

;;; Commentary:

;; Emacs configuration of Dan Leslie.
;; dan@ironoxide.ca

;;; Code:

(defun acquire-themes ()
  "Acquires themes."
  (let ((active-theme (car custom-enabled-themes)))
    (use-package afternoon-theme)
    (use-package alect-themes)
    (use-package ample-theme)
    (use-package clues-theme)
    (use-package color-theme-sanityinc-tomorrow)
    (use-package constant-theme)
    (use-package cyberpunk-theme)
    (use-package flatland-theme)
    (use-package gruber-darker-theme)
    (use-package gruvbox-theme)
    (use-package moe-theme)
    (use-package sexy-monochrome-theme)
    (use-package solarized-theme)
    (use-package zenburn-theme)
    (reset-theme)
    (load-theme active-theme t)))

;; Running Emacs as a daemon doesn't properly configure default faces
;; until after the first window is created. So we attach a hook to the
;; cache faces method in order to wait for that to occur.
(if (daemonp)
    (add-hook 'after-cache-faces-hook #'acquire-themes)
  (acquire-themes))

(provide '99-themes)
;;; 99-themes.el ends here
