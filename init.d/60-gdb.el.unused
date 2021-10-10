;;; 60-gdb.el --- gdb

;;; Commentary:

;; Emacs configuration of Dan Leslie.
;; dan@ironoxide.ca

;;; Code:

;; Replaces the built-in gdb-mi with something better
(when (executable-find "gdb")
  (use-package gdb-mi
    ;;  :after hydra
    :straight (:host github :repo "weirdNox/emacs-gdb" :files ("*.el" "*.c" "*.h" "Makefile"))
    :init
    (fmakunbound 'gdb)
    (fmakunbound 'gdb-enable-debug)))

(provide '60-gdb)
;;; 60-gdb.el ends here
