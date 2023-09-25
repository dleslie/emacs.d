;;; 99-themes.el --- themes

;;; Commentary:

;; Emacs configuration of Dan Leslie.
;; dan@ironoxide.ca

;;; Code:

;; This is considered dangerous, but I think the practice of approving themes individually pre-dates the accepted use of use-package. So long as they're coming from trusted package sources I don't see where there's much benefit to _also_ approving individual themes.
(setq custom-safe-themes t)

(use-package afternoon-theme :defer t)
(use-package alect-themes :defer t)
(use-package ample-theme :defer t)
(use-package clues-theme :defer t)
(use-package color-theme-sanityinc-tomorrow :defer t)
(use-package constant-theme :defer t)
(use-package cyberpunk-theme :defer t)
(use-package flatland-theme :defer t)
(use-package gruber-darker-theme :defer t)
(use-package gruvbox-theme :defer t)
(use-package moe-theme :defer t)
(use-package sexy-monochrome-theme :defer t)
(use-package solarized-theme :defer t)
(use-package zenburn-theme :defer t)
(use-package almost-mono-themes :defer t)
(use-package quasi-monochrome-theme :defer t)
(use-package monochrome-theme :defer t)

(defun reset-theme ()
  "Disable all active themes."
  (interactive)
  (dolist (theme custom-enabled-themes)
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
  (do
      ())
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
