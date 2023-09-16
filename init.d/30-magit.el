;;; 30-magit.el --- magit

;;; Commentary:

;; Emacs configuration of Dan Leslie.
;; dan@ironoxide.ca

;;; Code:

(when (executable-find "git")
  (use-package magit
    :init
    (global-set-key "\C-c g" 'magit-status)))

(provide '30-magit)
;;; 30-magit.el ends here
