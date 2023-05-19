;;; package.el --- Package configuration

;;; Commentary:

;; Emacs configuration of Dan Leslie.
;; dan@ironoxide.ca

;;; Code:

;; Where are we finding things?
(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("elpy" . "https://jorgenschaefer.github.io/packages/")
	( "jcs-elpa" . "https://jcs-emacs.github.io/jcs-elpa/packages/"))
      package-archive-priorities
      '(("elpy" . 90)
	("melpa" . 100)
	("gnu" . 80)
	("jcs-elpa" . 70)))

;;(package-initialize 'no-activate)
(package-initialize)

;;; package.el ends here
