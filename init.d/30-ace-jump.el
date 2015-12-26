(require-package 'ace-jump-mode)
(require-package 'ace-link)

(add-to-list 'load-path "/full/path/where/ace-jump-mode.el/in/")
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)

(autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Ace jump back:-)"
  t)

(with-eval-after-load "ace-jump-mode"
  (ace-link-setup-default)
  (ace-jump-mode-enable-mark-sync))
