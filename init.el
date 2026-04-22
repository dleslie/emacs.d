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
(when (boundp 'w32-pipe-buffer-size)
  (setq w32-pipe-buffer-size (* 64 1024)))

;; Fix TLS on Windows
(when  
    (not (or (string-match-p "Microsoft" (shell-command-to-string "uname -a"))
	     (eq 'windows-nt system-type)))
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

;; Memoize executable-find during init
(defvar my/executable-find-cache (make-hash-table :test 'equal))
(advice-add 'executable-find :around
            (defun my/memoize-executable-find (orig-fun &rest args)
              (let ((cmd (car args)))
                (if-let ((result (gethash cmd my/executable-find-cache)))
                    result
                  (let ((result (apply orig-fun args)))
                    (puthash cmd result my/executable-find-cache)
                    result))))
            '((name . "my/memoize-executable-find")))

;; Cleanup and restore after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (advice-remove 'executable-find "my/memoize-executable-find")
            (setq my/executable-find-cache nil)
            (setq file-name-handler-alist default-file-name-handler-alist)
            (setq gc-cons-threshold 104857600
                  gc-cons-percentage 0.1)
            (garbage-collect)
            (message "Emacs ready in %s with %d garbage collections."
                     (emacs-init-time)
                     gcs-done)))

;; Disable file handler search during load
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Load local configurations
(let ((localel (expand-file-name "local" user-emacs-directory)))
  (load localel))

;; Make custom file not this one
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Ensure it exists
(when (not (file-exists-p custom-file))
  (with-temp-buffer
    (write-file custom-file)))

;; Function to prompt for unsaved custom options
(add-hook 'kill-emacs-query-functions
          'custom-prompt-customize-unsaved-options)

;; Load custom.el
(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)
;;; init.el ends here
