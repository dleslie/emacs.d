;;; 99-themes.el --- themes

;;; Commentary:

;; Emacs configuration of Dan Leslie.
;; dan@ironoxide.ca

;;; Code:

(setq custom-safe-themes t)

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
(use-package almost-mono-themes)
(use-package quasi-monochrome-theme)
(use-package monochrome-theme)

(defun reset-theme ()
  "Disable all active themes."
  (interactive)
  (cl-loop
   for theme in custom-enabled-themes do
   (disable-theme theme)))

(defun change-theme (theme)
  "Disable all enabled themes and then load the provided theme THEME."
  (interactive
   (list
    (intern (completing-read "Load custom theme: "
                             (mapcar 'symbol-name (custom-available-themes))))))
  (reset-theme)
  (load-theme theme))

(defun random-theme ()
  "Changes to a random theme."
  (interactive)
  (let ((success nil)
	(current (car custom-enabled-themes))
	(all-themes (custom-available-themes)))
    (let ((new-theme (seq-random-elt all-themes)))
      (message (format "Switching to theme %s" new-theme))
      (ignore-errors (change-theme new-theme)))))

(defun random-dark-theme ()
  "Changes to a random dark theme."
  (interactive)
  (while
      (progn
	(random-theme)
	(not (background-is-dark)))))

(defun random-light-theme ()
  "Changes to a random light theme."
  (interactive)
  (while
      (progn
	(random-theme)
	(background-is-dark))))

(defun next-theme ()
  "Cycles through all available themes."
  (interactive)
  (let* ((current (car custom-enabled-themes))
	 (all-themes (custom-available-themes)))
    (if (not current)
	(change-theme (car all-themes))
      (let* ((remaining 
	      (seq-drop all-themes 
			(+ 1 (seq-position all-themes current))))
	     (next (or (car remaining)
		       (car all-themes))))
	(change-theme next)))))

(defun background-is-dark ()
  "Returns t if the current theme background is dark"
  (interactive)
  (let ((dark 0.33))
    (seq-every-p (lambda (x) (<= x dark))
		 (color-name-to-rgb (face-attribute 'default :background)))))

(provide '99-themes)
;;; 99-themes.el ends here
