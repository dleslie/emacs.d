;;; 20-ace-jump.el --- ace jump

;;; Commentary:

;; Emacs configuration of Dan Leslie.
;; dan@ironoxide.ca

;;; Code:

(use-package ace-jump-mode
  :init
  (global-set-key "\C-c SPC" 'ace-jump-mode)
  (autoload 'ace-jump-mode "ace-jump-mode" "Emacs quick move minor mode" t))

(provide '20-ace-jump)
;;; 20-ace-jump.el ends here
