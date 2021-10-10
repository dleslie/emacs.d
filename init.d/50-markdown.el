;;; 70-markdown.el --- markdown

;;; Commentary:

;; Emacs configuration of Dan Leslie.
;; dan@ironoxide.ca

;;; Code:

(use-package markdown-mode
  :init
  (autoload 'markdown-mode "markdown-mode" "Markdown Mode" t ".md")
  (autoload 'markdown-mode "markdown-mode" "Markdown Mode" t ".markdown"))

(provide '70-markdown)
;;; 70-markdown.el ends here
