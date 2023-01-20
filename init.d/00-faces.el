;;; 00-faces.el --- faces utilities

;;; Commentary:

;; Emacs configuration of Dan Leslie.
;; dan@ironoxide.ca

;;; Code:
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

(defun random-theme ()
  "Changes to a random theme."
  (interactive)
  (let ((success nil)
	(current (car custom-enabled-themes))
	(all-themes (delete-dups (sort (append (custom-available-themes) custom-known-themes) 
				       (lambda (a b) (string< (symbol-name a) (symbol-name b)))))))
    (while 
      (let ((new-theme (seq-random-elt all-themes)))
	(or (string-equal current new-theme)
	    (not (ignore-errors (change-theme new-theme))))))))

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

(provide '00-faces)
;;; 00-faces.el ends here
