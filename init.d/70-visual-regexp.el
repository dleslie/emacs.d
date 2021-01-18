;;; 70-visual-regexp.el --- visual-regexp

;;; Commentary:

;; Emacs configuration of Dan Leslie.
;; dan@ironoxide.ca

;;; Code:

(use-package visual-regexp
  :bind
  (("C-c r" . vr/replace)
   ("C-c q" . vr/query-replace)))

(when (executable-find "python2")
  (use-package visual-regexp-steroids
    :after visual-regexp
    :bind
    (("C-r" . vr/isearch-backward)
     ("C-s" . vr/isearch-forward))))

(provide '70-visual-regexp)
;;; 70-visual-regexp.el ends here
