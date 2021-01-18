;;; 70-org.el --- org

;;; Commentary:

;; Emacs configuration of Dan Leslie.
;; dan@ironoxide.ca

;;; Code:

(defvar org-directory '())

(use-package org
  :ensure org-plus-contrib
  :after (f)
  :bind
  (("C-c C-o t" . org-todo-list)
   ("C-c C-o l" . org-store-link)
   ("C-c C-o a" . org-agenda)
   ("C-c C-o b" . org-iswitchb)
   ("C-c C-o c" . org-capture)
   ("C-c C-o v" . my-org-show-all-inline-images))
  :init
  (defun my-org-show-all-inline-images ()
    (interactive)
    (org-display-inline-images t t)))

(provide '70-org)
;;; 70-org.el ends here
