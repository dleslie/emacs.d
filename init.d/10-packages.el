;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring Packages")

(require 'package)
(setq package-archives 
      '(("gnu" . "https://elpa.gnu.org/packages/")
	("melpa" . "https://melpa.org/packages/")
        ("org" . "http://orgmode.org/elpa/")
	("elpy" . "http://jorgenschaefer.github.io/packages/")))

(package-initialize)
