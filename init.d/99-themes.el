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
		(use-package almost-mono-themes :defer defer)
		(use-package quasi-monochrome-theme :defer defer)
		(use-package monochrome-theme :defer defer)
    (when (not defer)
      (reset-theme)
      (when (not (ignore-errors (change-theme active-theme)))
	(change-theme 'tango)))))

;; Running Emacs as a daemon doesn't properly configure default faces
;; until after the first window is created. So we attach a hook to the
;; cache faces method in order to wait for that to occur.
(acquire-themes t)
(add-hook 'after-init-hook 'acquire-themes)

(defun find-all-custom-themes ()
  "Locates all known custom themes"
  (delete-dups (sort (append (custom-available-themes) custom-known-themes) 
		     (lambda (a b) (string< (symbol-name a) (symbol-name b))))))

(defun reset-theme ()
  "Disable all active themes."
  (interactive)
  (cl-loop
   for theme in (find-all-custom-themes) do
   (when (-contains? custom-enabled-themes theme)
     (disable-theme theme))))

(defun change-theme (theme)
  "Disable all enabled themes and then load the provided theme THEME."
  (interactive
   (list
    (intern (completing-read "Load custom theme: "
                             (mapcar 'symbol-name (custom-available-themes))))))
  (reset-theme)
  (load-theme theme t))

(defun random-theme ()
  "Changes to a random theme."
  (interactive)
  (let ((success nil)
	(current (car custom-enabled-themes))
	(all-themes (find-all-custom-themes)))
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
  (let* ((all-themes (delete-dups (sort (append (custom-available-themes) custom-known-themes) (lambda (a b) (string< (symbol-name a) (symbol-name b))))))
	 (next (or (car custom-enabled-themes)
		   (car all-themes)))
	 (success nil))
    (while (not success)
      (setq next (cadr (memq next all-themes)))
      (when (not next)
	(setq next (car all-themes)))
      (reset-theme)
      (when (ignore-errors (load-theme next t))
	(setq success t)
	(message "Using \"%S\"" next)))))

(defun background-is-dark ()
  "Returns t if the current theme background is dark"
  (interactive)
  (let ((dark 0.33))
    (seq-every-p (lambda (x) (<= x dark))
		 (color-name-to-rgb (face-attribute 'default :background)))))

(provide '99-themes)
;;; 99-themes.el ends here
