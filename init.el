;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Init File Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Loading base configuration")

(setq my-init-directory (expand-file-name "init.d/" user-emacs-directory))
(add-to-list 'load-path my-init-directory)

(setq my-init-files
      (if (file-exists-p my-init-directory)
	  (sort (directory-files my-init-directory nil ".*\.el$") 'string<)
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
;; Fix TLS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((gnutls-exe (format "gnutls-cli%s" (if (eq window-system 'w32) ".exe" ""))))
  (when
      (executable-find gnutls-exe)

    (message "Fixing TLS")
    
    ;; From:
    ;; https://glyph.twistedmatrix.com/2015/11/editor-malware.html

    (setq tls-checktrust 1)
    
    (let ((trustfile
	   (replace-regexp-in-string
	    "\\\\" "/"
	    (replace-regexp-in-string
	     "\n" ""
	     (shell-command-to-string "python -m certifi")))))
      (setq tls-program
	    (list
	     (format "%s --x509cafile %s -p %%p %%h" gnutls-exe trustfile)))
      (setq gnutls-verify-error t)
      (setq gnutls-trustfiles (list trustfile)))

    (if (condition-case e
	    (progn
	      (url-retrieve "https://wrong.host.badssl.com/"
			    (lambda (retrieved) t))
	      (url-retrieve "https://self-signed.badssl.com/"
			    (lambda (retrieved) t))
	      t)
	  ('error nil))
	(error "tls misconfigured")
      (url-retrieve "https://badssl.com"
		    (lambda (retrieved) t)))))

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
