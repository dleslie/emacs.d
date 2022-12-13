;;; 30-magit.el --- magit

;;; Commentary:

;; Emacs configuration of Dan Leslie.
;; dan@ironoxide.ca

;;; Code:

(when (executable-find "git")
  (use-package magit
    :defer t
    :init
    (global-set-key "\C-c g" 'magit-status)
    (setq magit-last-seen-setup-instructions "1.4.0")))

(provide '30-magit)
;;; 30-magit.el ends here
