;;; 00-utf8.el --- utf8 configuration

;;; Commentary:

;; Emacs configuration of Dan Leslie.
;; dan@ironoxide.ca

;;; Code:

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(when (boundp 'set-w32-system-coding-system)
  (set-w32-system-coding-system 'utf-8))

(provide '00-utf8)
;;; 00-utf8.el ends here
