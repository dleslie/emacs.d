;;; 10-imenu.el --- imenu

;;; Commentary:

;; Emacs configuration of Dan Leslie.
;; dan@ironoxide.ca

;;; Code:

;; Always show imenu
(defun my-try-to-add-imenu ()
  "Attempt to add imenu."
  (condition-case
      nil
      (imenu-add-to-menubar "Imenu")
    (error nil)))
(add-hook 'font-lock-mode-hook 'my-try-to-add-imenu)

(provide '10-imenu)
;;; 10-imenu.el ends here
