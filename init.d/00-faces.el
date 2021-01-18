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
	(all-themes (delete-dups (sort (append (custom-available-themes) custom-known-themes) (lambda (a b) (string< (symbol-name a) (symbol-name b)))))))
    (while (not success)
      (let ((new-theme (seq-random-elt all-themes)))
	(when (ignore-errors (change-theme new-theme))
	  (setq success t)
	  (message "Using \"%S\"" new-theme))))))

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

(provide '00-faces)
;;; 00-faces.el ends here
