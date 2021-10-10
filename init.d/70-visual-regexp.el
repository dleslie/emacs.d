;;; 70-visual-regexp.el --- visual-regexp

;;; Commentary:

;; Emacs configuration of Dan Leslie.
;; dan@ironoxide.ca

;;; Code:

(use-package visual-regexp
  :init
  (global-set-key "\C-c r" 'vr/replace)
  (global-set-key "\C-c q" 'vr/query-replace))

(when (executable-find "python2")
  (use-package visual-regexp-steroids
    :init
    (global-set-key "\C-r" 'vr/isearch-backward)
    (global-set-key "\C-s" 'vr/isearch-forward)))

(provide '70-visual-regexp)
;;; 70-visual-regexp.el ends here
