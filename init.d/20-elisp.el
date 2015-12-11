;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; elisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring elisp")

(require 'eldoc)
(defun custom-elisp-prog-hook ()
  (eldoc-mode 1))

(add-hook 'emacs-lisp-mode-hook 'custom-elisp-prog-hook)
