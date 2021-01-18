;;; 99-themes.el --- themes

;;; Commentary:

;; Emacs configuration of Dan Leslie.
;; dan@ironoxide.ca

;;; Code:

(defun reset-theme ()
  "Disable all active themes."
  (interactive)
  (while custom-enabled-themes
    (disable-theme (car custom-enabled-themes)))
  (dolist (face (face-list))
    (when (face-differs-from-default-p face)
      (copy-face 'default face))))

(defun change-theme (theme)
  "Disable all enabled themes and then load the provided theme THEME."
  (interactive
   (list
    (intern (completing-read "Load custom theme: "
                             (mapcar 'symbol-name (custom-available-themes))))))
  (reset-theme)
  (load-theme theme t))

(defun next-theme ()
  "Cycles through all 'custom-known'themes'."
  (interactive)
  (let* ((custom-available-themes (custom-available-themes))
	 (current (or (car custom-enabled-themes)
		      (car custom-available-themes)))
	 (next (cadr (memq current custom-available-themes))))
    (when (memq next '(use-package user changed))
      (setq next (car custom-known-themes)))
    (reset-theme)
    (load-theme next t)
    (reset-theme)
    (load-theme next t)
    (message "Using \"%S\"" next)))

(use-package afternoon-theme)
(use-package alect-themes)
(use-package ample-theme)
(use-package clues-theme)
(use-package color-theme-sanityinc-tomorrow)
(use-package constant-theme)
(use-package cyberpunk-theme)
;(use-package doom-themes)
(use-package flatland-theme)
(use-package gruber-darker-theme)
(use-package gruvbox-theme)
(use-package moe-theme)
(use-package sexy-monochrome-theme)
(use-package solarized-theme)
(use-package zenburn-theme)

(load-theme 'constant t)

(provide '99-themes)
;;; 99-themes.el ends here
