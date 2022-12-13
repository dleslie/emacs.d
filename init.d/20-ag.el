;;; 20-ag.el --- ag

;;; Commentary:

;; Emacs configuration of Dan Leslie.
;; dan@ironoxide.ca

;;; Code:

(when (executable-find "ag")
  (use-package ag
    :defer t
    :init
    (define-key-after global-map [menu-bar tools ag]
      '(menu-item "Search Files (ag)..." ag :help "Search files for strings or regexps (with ag)...")
      'grep)))

(provide '20-ag)
;;; 20-ag.el ends here
