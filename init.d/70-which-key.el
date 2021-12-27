;;; 70-which-key.el --- which-key

;;; Commentary:

;; Emacs configuration of Dan Leslie.
;; dan@ironoxide.ca

;;; Code:

(use-package which-key
  :init
  ;; Allow C-h to trigger which-key before it is done automatically
  (setq which-key-show-early-on-C-h t)
  ;; make sure which-key doesn't show normally but refreshes quickly after it is
  ;; triggered.
  (setq which-key-idle-delay 1)
  (setq which-key-idle-secondary-delay 0.05)
  (which-key-mode))

(provide '70-which-key)
;;; 70-which-key.el ends here
