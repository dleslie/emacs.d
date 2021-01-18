;;; 20-auto-package-update.el --- automatically update packages

;;; Commentary:

;; Emacs configuration of Dan Leslie.
;; dan@ironoxide.ca

;;; Code:

(use-package auto-package-update
  :init
  (setq auto-package-update-delete-old-versions t
        auto-package-update-hide-results t)
  (auto-package-update-maybe))

(provide '20-auto-package-update)
;;; 20-auto-package-update.el ends here
