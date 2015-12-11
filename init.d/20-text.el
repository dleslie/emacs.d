;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Text
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring Text Mode")

(require-package 'writegood-mode)

(defun custom-text-mode-hook ()
  (visual-line-mode 1)
  (flyspell-mode 1)
  (writegood-mode 1))

(add-hook 'text-mode-hook 'custom-text-mode-hook)
