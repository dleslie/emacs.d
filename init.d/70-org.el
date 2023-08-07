;;; 70-org.el --- org

;;; Commentary:

;; Emacs configuration of Dan Leslie.
;; dan@ironoxide.ca

;;; Code:

(use-package org
  :ensure t
  :straight t
  :custom
  (org-directory (file-truename "~/org/")
   org-default-notes-file (concat (file-truename org-directory) "notes.org")
   org-agenda-files `(,(concat (file-truename org-directory) "todo.org") ,(concat (file-truename org-directory) "agenda.org"))
   org-agenda-diary-file (concat (expand-file-name org-directory) "diary.org"))
  :init
  (defun my-org-show-all-inline-images ()
    (interactive)
    (org-display-inline-images t t))
  (setq
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
  :bind
  (("C-c o c" . org-capture)
   ("C-c o a" . org-agenda)
   ("C-c o l" . org-store-link)
   ("C-c o b" . org-iswitchb)
   ("C-c o t" . org-todo-list)
   ("C-c o s" . org-search-view)
   ("C-c o i" . my-org-show-all-inline-images)))

;; (use-package org-bullets
;;   :after org
;;   :straight t
;;   :init
;;   (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(defvar org-roam-directory '())
(defvar org--inhibit-version-check t)

(use-package org-roam
  :straight t
  :ensure t
  :after org
  :hook
  (after-init . org-roam-mode)
  :custom
  (org-roam-directory (file-truename "~/org/roam"))
  :bind
  (("C-c o r" . org-roam)
   ("C-c o f" . org-roam-find-file)
   ("C-c o g" . org-roam-graph))
  :config
  (setq org-roam-capture-templates
	'(("d" "default" plain (function org-roam--capture-get-point)
	   "%?"
	   :file-name "%<%Y%m%d%H%M%S>-${slug}"
	   :head "#+title: ${title}\n#+roam_tags: \n\n"
	   :unnarrowed t)))
  (org-roam-db-autosync-mode))

(provide '70-org)
;;; 70-org.el ends here
