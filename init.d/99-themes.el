;;; 99-themes.el --- themes

;;; Commentary:

;; Emacs configuration of Dan Leslie.
;; dan@ironoxide.ca

;;; Code:

(defun reset-face (face)
  "Sets FACE to a monotone black on white."
  (set-face-foreground face "black")
  (set-face-background face "white")
  (set-face-stipple face nil)
  (set-face-bold-p face nil)
  (set-face-italic-p face nil)
  (set-face-underline-p face nil))

(defun reset-theme ()
  "Disable all active themes."
  (interactive)
  (while custom-enabled-themes
    (disable-theme (car custom-enabled-themes))))

(defun change-theme (theme)
  "Disable all enabled themes and then load the provided theme THEME."
  (interactive
   (list
    (intern (completing-read "Load custom theme: "
                             (mapcar 'symbol-name (custom-available-themes))))))
  (reset-theme)
  (load-theme theme t))

(defun next-theme ()
  "Cycles through all available themes."
  (interactive)
  (let* ((all-themes (delete-dups (sort (append (custom-available-themes) custom-known-themes) (lambda (a b) (string< (symbol-name a) (symbol-name b))))))
	 (next (or (car custom-enabled-themes)
		   (car all-themes)))
	 (success nil))
    (while (not success)
      (let ((next (cadr (memq next all-themes))))
	(when (not next)
	  (setq next (car all-themes)))
	(reset-theme)
	(unless (ignore-errors (load-theme next t))
	    (setq success t)
	    (message "Using \"%S\"" next))))))

(use-package afternoon-theme)
(use-package alect-themes)
(use-package ample-theme)
(use-package clues-theme)
(use-package color-theme-sanityinc-tomorrow)
(use-package constant-theme)
(use-package cyberpunk-theme)
(use-package doom-themes)
(use-package flatland-theme)
(use-package gruber-darker-theme)
(use-package gruvbox-theme)
(use-package moe-theme)
(use-package sexy-monochrome-theme)
(use-package solarized-theme)
(use-package zenburn-theme)

(ignore-errors
  (load-theme 'tango t))

(provide '99-themes)
;;; 99-themes.el ends here
