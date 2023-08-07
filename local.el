;;; local.el --- local configurations

;;; Commentary:

;; Emacs configuration of Dan Leslie.
;; dan@ironoxide.ca

;;; Code:

;; Where are we storing org files?

;; Useful bindings
(global-set-key "\C-w" 'clipboard-kill-region)
(global-set-key "\M-w" 'clipboard-kill-ring-save)
(global-set-key "\C-y" 'clipboard-yank)
(global-set-key "\C-c," 'scroll-bar-mode)
(global-set-key "\C-c." 'tool-bar-mode)
(global-set-key "\C-c?" 'menu-bar-mode)
(global-set-key "\C-c\\" 'comment-or-uncomment-region)
(global-set-key "\C-cs" 'eshell-here)
(global-set-key [f12] 'toggle-frame-fullscreen)
(global-set-key (kbd "C-;") 'hippie-expand)
(global-set-key (kbd "C-,") 'company-complete)
(global-set-key "\C-c\C-t" 'next-theme)

;; Default C style
(add-hook 'c++-mode-hook (lambda () (eldoc-mode 1) (c-set-style "java")))
(add-hook 'c-mode-hook (lambda () (eldoc-mode 1) (c-set-style "java")))

;; My menu
(defvar my-mode-map
  (let ((map (make-sparse-keymap)))
    (easy-menu-define my-menu map
      "My Menu"
      '("Mine"
	;; ("Org"
	;;  ["Todo" org-todo-list]
	;;  ["Agenda" org-agenda]
	;;  ["Capture" org-capture])
	
	["Todo" org-todo-list]
	["Agenda" org-agenda]
	["Capture" org-capture]
	"--"
	["Magit" magit-status]
        "---"
        ["Shell Here" eshell-here]
        ["dos2unix" dos2unix]
        "--"
        ["Smartparens Cheat Sheet" s-cheat-sheet]
        "---"
        ["Change Theme" change-theme]
	["Next Theme" next-theme]
	["Random Theme" random-theme]
	["Random Dark Theme" random-dark-theme]
	["Random Light Theme" random-light-theme]
        ["Reset Theme" reset-theme]))
    map))

(define-minor-mode my-mode
  "Minor mode to provide my custom menu items."
  :keymap my-mode-map
  :global t)

(my-mode t)

;; Enable truncate lines for all text mode buffers
(add-hook 'text-mode-hook 'toggle-truncate-lines)

;; General Emacs Sanity
(setq
 auto-window-vscroll nil
 c-basic-offset 2
 column-number-mode t
 css-indent-offset 2
 debug-on-error nil
 electric-indent-mode nil
 indent-tabs-mode nil
 indicate-buffer-boundaries 'left
 indicate-empty-lines t
 inhibit-startup-screen t
 js-indent-level 2
 make-backup-files nil
 scroll-bar-mode nil
 scroll-conservatively 10000
 scroll-step 2
 show-paren-delay 0
 show-trailing-whitespace t
 tab-stop-list (number-sequence 2 120 2)
 tab-width 2
 tool-bar-mode nil)

(delete-selection-mode 1)
(global-eldoc-mode t)
(show-paren-mode t)

;(global-display-line-numbers-mode)
;(global-hl-line-mode t)
(global-prettify-symbols-mode +1)

(change-theme 'leuven-dark)

(provide 'local)
;;; local.el ends here
