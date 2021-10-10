;;; 70-org.el --- org

;;; Commentary:

;; Emacs configuration of Dan Leslie.
;; dan@ironoxide.ca

;;; Code:

(defvar org-directory '())

(use-package org
  :ensure org-plus-contrib
  :init
  (global-set-key "\C-c \C-o t" 'org-todo-list)
  (global-set-key "\C-c \C-o t" 'org-store-link)
  (global-set-key "\C-c \C-o t" 'org-agenda)
  (global-set-key "\C-c \C-o t" 'org-iswitchb)
  (global-set-key "\C-c \C-o t" 'org-capture)
  (global-set-key "\C-c \C-o t" 'my-org-show-all-inline-images)
  (defun my-org-show-all-inline-images ()
    (interactive)
    (org-display-inline-images t t)))

(provide '70-org)
;;; 70-org.el ends here
