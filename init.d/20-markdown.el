;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Markdown
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring Markdown Mode")

(require-package 'markdown-mode)
(require-package 'writegood-mode)

(defun custom-markdown-mode ()
  (interactive)
  (visual-line-mode 1)
  (flyspell-mode 1)
  (writegood-mode 1)
  (markdown-mode))

(add-to-list 'auto-mode-alist '("\\.markdown\\'" . custom-markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . custom-markdown-mode))
