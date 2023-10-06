;;; init.el --- Startup Script

;;; Commentary:

;; Emacs configuration of Dan Leslie.
;; dan@ironoxide.ca

;;; Code:

;; Loads of legacy packages require 'cl
(require 'cl-lib)

;; Disable GC during load
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

;; Windows performance tweaks
(when (boundp 'w32-pipe-read-delay)
  (setq w32-pipe-read-delay 0))

;; Fix TLS on Windows
(when  
    (not (or (string-match-p "Microsoft" (shell-command-to-string "uname -a"))
	     (eq 'windows-nt system-type)))
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

;; Disable file handler search during load
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Compilation helper
;; (defun byte-compile-if-newer (file)
;;   (let ((el-file (concat file ".el"))
;;         (elc-file (concat file ".elc")))
;;     (if (or (not (file-exists-p elc-file))
;;             (file-newer-than-file-p el-file elc-file))
;;         (byte-compile-file el-file))))

;; Load local configurations
(let ((localel (expand-file-name (concat user-emacs-directory "local"))))
  ;; (byte-compile-if-newer localel)
  (load localel))

;; Make custom file not this one
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Enable file handler
(setq file-name-handler-alist default-file-name-handler-alist)

;; Load custom.el
(when (file-exists-p custom-file)
  (load custom-file))

;; Enable GC
(setq gc-cons-threshold 16777216
      gc-cons-percentage 0.1)
(garbage-collect)

;; Friendly message
(message "Emacs ready in %s with %d garbage collections."
         (format "%.2f seconds"
                 (float-time
                  (time-subtract after-init-time before-init-time)))
         gcs-done)

(provide 'init)
;;; init.el ends here
