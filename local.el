;;; local.el --- local configurations

;;; Commentary:

;; Emacs configuration of Dan Leslie.
;; dan@ironoxide.ca

;;; Code:

;; org-directory
(setq org-directory "~/org")

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

;; Projectile search paths
(setq projectile-project-search-path '())
(dolist (p `(,org-directory "~/Workspace/" "~/Workspace/GitHub" "~/Workspace/github" "~/Workspace/GitLab" "~/Workspace/gitlab" "~/Workspace/other" "~/Workspace/Other" "/c/work/" "/d/work/" "/mnt/c/work/" "/mnt/d/work/" "C:/work/" "D:/work/"))
  (when (file-exists-p p)
    (push (expand-file-name p) projectile-project-search-path)))

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
        "--"
        ["Shell Here" eshell-here]
        ["dos2unix" dos2unix]
        "--"
        ["Override Theme" override-theme]
        ["Reset Theme" reset-theme]))
    map))

(define-minor-mode my-mode
  "Minor mode to provide my custom menu items."
  :keymap my-mode-map
  :global t)

(my-mode t)

;; General Emacs Sanity
(setq
 auto-window-vscroll nil
 column-number-mode t
 debug-on-error nil
 indent-tabs-mode nil
 inhibit-startup-screen t
 make-backup-files nil
 scroll-bar-mode nil
 scroll-conservatively 10000
 scroll-step 2
 c-basic-offset 2
 tab-stop-list (number-sequence 2 120 2)
 tab-width 2
 tool-bar-mode nil
 truncate-lines t)

(delete-selection-mode 1)
(global-eldoc-mode t)
(global-hl-line-mode t)
(show-paren-mode t)
(global-display-line-numbers-mode t)

(provide 'local)
;;; local.el ends here
