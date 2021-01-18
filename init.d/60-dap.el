;;; 60-dap.el --- dap

;;; Commentary:

;; Emacs configuration of Dan Leslie.
;; dan@ironoxide.ca

;;; Code:

(use-package dap-mode
  :hook
  ((web-mode . dap-mode)
   (python-mode . dap-mode)
   (ruby-mode . dap-mode)
   (c-mode . dap-mode)
   (js3-mode . dap-mode))
  :config
  (dap-ui-mode 1)
  ;; enables mouse hover support
  (dap-tooltip-mode 1)
  ;; use tooltips for mouse hover
  ;; if it is not enabled `dap-mode' will use the minibuffer.
  (tooltip-mode 1)
  (require 'dap-ruby)
  (require 'dap-dap-lldb)
  (require 'dap-node)
  (require 'dap-python)
  (require 'dap-firefox))

(provide '60-dap)
;;; 60-dap.el ends here
