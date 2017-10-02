;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Text
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring Text Mode")

(require-package 'writegood-mode)

(defun custom-text-mode-hook ()
  (visual-line-mode t)
  (flyspell-mode t)
  (writegood-mode t))

(add-hook 'text-mode-hook 'custom-text-mode-hook)
