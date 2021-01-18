;;; 99-themes.el --- themes

;;; Commentary:

;; Emacs configuration of Dan Leslie.
;; dan@ironoxide.ca

;;; Code:

;; disable other themes before loading new one
(defadvice load-theme (before theme-dont-propagate activate)
  "Disable theme before loading new one."
  (mapc #'disable-theme custom-enabled-themes))

(use-package constant-theme)
(use-package doom-themes)
(use-package sexy-monochrome-theme)

(load-theme 'tango)

(provide '99-themes)
;;; 99-themes.el ends here
