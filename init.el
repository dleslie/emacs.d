;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Disable Emacs SPAM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq inhibit-startup-screen t)
(setq my-init-start-time (float-time))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions used during init
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-exe (name)
  (if (string= system-type "windows-nt")
      (or
       (executable-find name)
       (executable-find	(format "%s.exe" name))
       (executable-find	(format "%s.bat" name)))
    (executable-find name)))

(setq packages-refreshed-at-least-once nil)

(defun require-package (package-name &optional require-name dont-load)
  "Loads and imports packages, installing from ELPA if necessary"
  (with-demoted-errors
      "Error: %S"
    
    (unless (package-installed-p package-name)
      (unless packages-refreshed-at-least-once
	(package-refresh-contents)
	(setq packages-refreshed-at-least-once t))

      (package-install package-name))
    (if (and (not dont-load) (package-installed-p package-name))
	(if require-name
	    (require require-name)
	  (require package-name))
      nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Base configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load (expand-file-name "configuration.el" user-emacs-directory))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load Init Files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Loading init files")

(setq my-init-directory (expand-file-name "init.d/" user-emacs-directory))
(add-to-list 'load-path my-init-directory)

(setq my-init-files
      (if (file-exists-p my-init-directory)
	  (sort (directory-files my-init-directory nil ".*\.el$") 'string<)
	'()))

(mapcar	#'(lambda (file)
	    (let ((begin-time (float-time)))
	      (with-demoted-errors "Load Error: %S"
		(load file nil t))
	      (message (format "Processed %s in %s seconds" file (- (float-time) begin-time)))))
	my-init-files)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customizable Values
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Loading custom file")

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(unless (file-exists-p custom-file)
  (shell-command (concat "touch " custom-file)))
(load custom-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; End
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message (format "Init completed in %s seconds." (- (float-time) my-init-start-time)))
