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
            (message (format "Processing %s" file))
            (with-demoted-errors "Load Error: %S"
              (load file nil t)))
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

(message "Init Complete.")
