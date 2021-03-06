;;; package.el --- Package configuration

;;; Commentary:

;; Emacs configuration of Dan Leslie.
;; dan@ironoxide.ca

;;; Code:

;; Where are we finding things?
(require 'package)
(setq package-archives
      '(("org" . "https://orgmode.org/elpa/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("elpy" . "https://jorgenschaefer.github.io/packages/"))
      package-archive-priorities
      '(("elpy" . 90)
	("melpa" . 100)
	("org" . 70)
	("gnu" . 80)))

;;(package-initialize 'no-activate)
(package-initialize)

;;; package.el ends here
