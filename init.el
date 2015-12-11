;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Init File Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Loading base configuration")

(setq my-init-directory (expand-file-name "init.d/" user-emacs-directory))
(add-to-list 'load-path my-init-directory)

(setq my-init-files
      (if (file-exists-p my-init-directory)
	  (sort (directory-files my-init-directory nil ".*\.el") 'string<)
	'()))

(load (expand-file-name "configuration.el" user-emacs-directory))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customizable Values
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Loading custom file")

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(unless (file-exists-p custom-file)
  (shell-command (concat "touch " custom-file)))
(load custom-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring Packages")

(require 'package)
(setq package-archives 
      '(("gnu" . "https://elpa.gnu.org/packages/")
	("melpa" . "https://melpa.org/packages/")
        ("org" . "http://orgmode.org/elpa/")))

(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load Init Files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Loading remaining init files")

(defun safe-load (file)
  (with-demoted-errors "Load Error: %S"
    (load file nil t)))

(mapcar 'safe-load my-init-files)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; End
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Init Complete.")
