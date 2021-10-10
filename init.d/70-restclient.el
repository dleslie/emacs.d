;;; 70-restclient.el --- restclient

;;; Commentary:

;; Emacs configuration of Dan Leslie.
;; dan@ironoxide.ca

;;; Code:

(use-package restclient)

(use-package company-restclient
  :init
  (push 'company-restclient company-backends))

(provide '70-restclient)
;;; 70-restclient.el ends here
