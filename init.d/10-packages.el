;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring Packages")

(require 'package)
(setq package-archives 
      '(("gnu" . "http://elpa.gnu.org/packages/")
	("melpa" . "http://melpa.org/packages/")
        ("org" . "http://orgmode.org/elpa/")
	("elpy" . "http://jorgenschaefer.github.io/packages/")))

(package-initialize)
