;;; 50-web.el --- web

;;; Commentary:

;; Emacs configuration of Dan Leslie.
;; dan@ironoxide.ca

;;; Code:

(use-package web-mode
  :init
  (autoload 'web-mode "web-mode" "Web Mode" t ".phtml")
  (autoload 'web-mode "web-mode" "Web Mode" t ".tpl")
  (autoload 'web-mode "web-mode" "Web Mode" t ".php")
  (autoload 'web-mode "web-mode" "Web Mode" t ".asp")
  (autoload 'web-mode "web-mode" "Web Mode" t ".gsp")
  (autoload 'web-mode "web-mode" "Web Mode" t ".jsp")
  (autoload 'web-mode "web-mode" "Web Mode" t ".aspx")
  (autoload 'web-mode "web-mode" "Web Mode" t ".ascx")
  (autoload 'web-mode "web-mode" "Web Mode" t ".erb")
  (autoload 'web-mode "web-mode" "Web Mode" t ".mustache")
  (autoload 'web-mode "web-mode" "Web Mode" t ".djhtml")
  (autoload 'web-mode "web-mode" "Web Mode" t ".html")
  (autoload 'web-mode "web-mode" "Web Mode" t ".tsx")
  (autoload 'web-mode "web-mode" "Web Mode" t ".jsx"))

(provide '50-web)
;;; 50-web.el ends here
