;;; 00-faces.el --- faces utilities

;;; Commentary:

;; Emacs configuration of Dan Leslie.
;; dan@ironoxide.ca

;;; Code:
(defvar default-faces-cache '())
(defvar after-cache-faces-hook '())

(defun cache-faces ()
  "Caches all faces."
  (when (not default-faces-cache)
    (setq default-faces-cache
	  (mapcar (lambda (face)
		    `(,face .
			    ((foreground . ,(face-foreground face))
			     (background . ,(face-background face))
			     (stipple . ,(face-stipple face))
			     (bold . ,(face-bold-p face))
			     (italic . ,(face-italic-p face))
			     (underline . ,(face-underline-p face))
			     (font . ,(face-font face))
			     (attributes . ,(face-all-attributes face)))))
		  (face-list)))
    (run-hooks after-cache-faces-hook)))

(if (daemonp)
    (add-hook 'server-after-make-frame-hook #'cache-faces)
  (cache-faces))

(defun reset-face (face)
  "Reset FACE to what its value was on load, if possible."
  (ignore-errors
    (let ((data (assoc face default-faces-cache)))
      (when data
	(let ((foreground (cdr (assoc 'foreground (cdr data))))
	      (background (cdr (assoc 'background (cdr data))))
	      (stipple (cdr (assoc 'stipple (cdr data))))
	      (bold (cdr (assoc 'bold (cdr data))))
	      (italic (cdr (assoc 'italic (cdr data))))
	      (underline (cdr (assoc 'underline (cdr data))))
	      (font (cdr (assoc 'font (cdr data))))
	      (attributes (cdr (assoc 'attributes (cdr data)))))
	  (set-face-foreground face foreground)
	  (set-face-background face background)
	  (set-face-stipple face stipple)
	  (set-face-italic face italic)
	  (set-face-underline face underline)
	  (set-face-font face font)
	  (mapc
	   (lambda (attr)
	     (set-face-attribute face (car attr) (cdr attr)))
	   attributes))))))

(defun reset-theme ()
  "Disable all active themes."
  (interactive)
  (while custom-enabled-themes
    (disable-theme (car custom-enabled-themes)))
  (mapc #'reset-face (face-list)))

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
      (setq next (cadr (memq next all-themes)))
      (when (not next)
	(setq next (car all-themes)))
      (reset-theme)
      (when (ignore-errors (load-theme next t))
	(setq success t)
	(message "Using \"%S\"" next)))))

(provide '00-faces)
;;; 00-faces.el ends here
