;;; 50-web.el --- web

;;; Commentary:

;; Emacs configuration of Dan Leslie.
;; dan@ironoxide.ca

;;; Code:

(use-package web-mode
  :mode ("\\.phtml\\'"
	 "\\.tpl\\.php\\'"
	 "\\.[agj]sp\\'"
	 "\\.as[cp]x\\'"
	 "\\.erb\\'"
	 "\\.mustache\\'"
	 "\\.djhtml\\'"
	 "\\.html?\\'"
	 "\\.tsx\\'"
	 "\\.jsx\\'"))

(use-package company-web
  :after (company web-mode)
  :init
  (push 'company-web-html company-backends)
  (push 'company-web-jade company-backends)
  (push 'company-web-slim company-backends))

(provide '50-web)
;;; 50-web.el ends here
