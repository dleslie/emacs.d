;;; local.el --- local configurations

;;; Commentary:

;; Emacs configuration of Dan Leslie.
;; dan@ironoxide.ca

;;; Code:

;; Where are we storing org files?
(defvar org-directory)
(setq org-directory (expand-file-name "c:/personal/dleslie/org/"))

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

;; Projectile search paths
(setq projectile-project-search-path '())
(dolist (p `(,org-directory "~/Workspace/" "~/Workspace/GitHub" "~/Workspace/github" "~/Workspace/GitLab" "~/Workspace/gitlab" "~/Workspace/other" "~/Workspace/Other" "/c/work/" "/d/work/" "/mnt/c/work/" "/mnt/d/work/" "C:/work/" "D:/work/"))
  (when p
    (let ((expanded (expand-file-name p)))
      (when (and expanded (file-exists-p expanded))
	(push expanded projectile-project-search-path)))))

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
        ["Reset Theme" reset-theme]))
    map))

(define-minor-mode my-mode
  "Minor mode to provide my custom menu items."
  :keymap my-mode-map
  :global t)

(my-mode t)

;; org stuff
(setq
 org-default-notes-file (concat (expand-file-name org-directory) "notes.org")
 org-agenda-files `(,(concat (expand-file-name org-directory) "todo.org") ,(concat (expand-file-name org-directory) "agenda.org"))
 org-agenda-diary-file (concat (expand-file-name org-directory) "diary.org")
 org-todo-keywords
 '((sequence "TODO(t)" "PROG(p)" "BLCK(b)" "STAL(s)" "|" "DONE(d)" "WONT(w)"))
 org-todo-keyword-faces
 '(("TODO" . (:foreground "white" :weight bold))
   ("DOIN" . (:foreground "green" :weight bold))
   ("BLCK" . (:foreground "red" :weight bold))
   ("STAL" . (:foreground "yellow" :weight bold))
   ("WONT" . (:foreground "grey" :weight bold))
   ("DONE" . (:foreground "grey" :weight bold)))
 org-capture-templates
 '(("n" "Note" entry (file+headline "notes.org" "Notes")
    "* %^{topic} %T %^g\n   :CATEGORY: %^{category}\n%i%?\n")
   ("t" "To Do" entry (file+headline "todo.org" "To Do")
    "* TODO %^{todo} %^g\n   DEADLINE: %^{due}t\n   :CATEGORY: %^{category}\n")
   ("d" "Daily review" entry (file+headline "diary.org" "Daily Review")
    (format
     "* %%T %%^g\n   :CATEGORY: Review\n   %%?%%[%s/template_daily_review.org]\n"
     org-directory))
   ("i" "Idea" entry (file+headline "ideas.org" "Ideas")
    "* %^{topic} %T %^g\n   :CATEGORY: Idea\n   %i%?\n")
   ("j" "Journal" entry (file+headline "diary.org" "Journal")
    "* %^{topic} %T %^g\n   :CATEGORY: Journal\n   %i%?\n")
   ("l" "Letter" entry (file+headline "letters.org" "Letter")
    "* %^{topic} %T %^g\n   :CATEGORY: Letter\n   %i%?\n")
   ("w" "Work Log" entry (file+headline "work.org" "Work Log")
    "* %^{topic} %T %^g\n   :CATEGORY: Log\n   %i%?\n")
   ("a" "Article" entry (file+headline "articles.org" "Article")
    "* %^{topic} %T %^g\n   :CATEGORY: Article\n   %i%?\n")
   ("e" "Event" entry (file+headline "agenda.org" "Events")
    "* %^{title} %^g\n     SCHEDULED: %^{when}t\n   %i%?\n")
   ("c" "Contact" entry (file+headline "addresses.org" "Addresses")
    "* %(org-contacts-template-name)\n   :PROPERTIES:\n   :EMAIL: %(org-contacts-template-email)\n   :END:\n   %i%?\n")))

;; General Emacs Sanity
(setq
 auto-window-vscroll nil
 c-basic-offset 2
 column-number-mode t
 css-indent-offset 2
 debug-on-error nil
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
 tool-bar-mode nil
 truncate-lines t
 electric-indent-mode nil)
(setq-default tab-width 2)

(delete-selection-mode 1)
(global-eldoc-mode t)
(show-paren-mode t)

(global-display-line-numbers-mode t)
;(global-hl-line-mode t)
(global-prettify-symbols-mode +1)

(provide 'local)
;;; local.el ends here
