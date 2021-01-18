;;; 30-magit.el --- magit

;;; Commentary:

;; Emacs configuration of Dan Leslie.
;; dan@ironoxide.ca

;;; Code:

(when (executable-find "git")
  (use-package magit
    :bind (("C-c g" . magit-status))
    :init
    (setq magit-last-seen-setup-instructions "1.4.0")))

(provide '30-magit)
;;; 30-magit.el ends here
