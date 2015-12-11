;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring Org Mode")

(require-package 'org-plus-contrib)
(require-package 'writegood-mode)

(setq
 org-default-notes-file 
 (concat org-directory "notes.org")
 org-agenda-files 
 `(,(concat org-directory "todo.org") 
   ,(concat org-directory "agenda.org")
   ,(concat org-directory "gcal-main.org")
   ,(concat org-directory "gcal-appointments.org")
   ,(concat org-directory "gcal-vacations.org"))
 org-agenda-diary-file 
 (concat org-directory "diary.org")
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
 '(("n" "Note" entry (file+headline (concat org-directory "notes.org") "Notes")
    "* %^{topic} %T %^g\n   :CATEGORY: %^{category}\n%i%?\n")
   ("t" "To Do" entry (file+headline (concat org-directory "todo.org") "To Do")
    "* TODO %^{todo} %^g\n   DEADLINE: %^{due}t\n   :CATEGORY: %^{category}\n")
   ("d" "Daily review" entry (file+headline (concat org-directory "diary.org") "Daily Review") 
    "* %T %^g\n   :CATEGORY: Review\n   %?%[~/ownCloud/org/template_daily_review.org]\n") 
   ("i" "Idea" entry (file+headline (concat org-directory "ideas.org") "Ideas") 
    "* %^{topic} %T %^g\n   :CATEGORY: Idea\n   %i%?\n") 
   ("j" "Journal" entry (file+headline (concat org-directory "diary.org") "Journal") 
    "* %^{topic} %T %^g\n   :CATEGORY: Journal\n   %i%?\n")
   ("l" "Letter" entry (file+headline (concat org-directory "letters.org") "Letter") 
    "* %^{topic} %T %^g\n   :CATEGORY: Letter\n   %i%?\n")
   ("w" "Work Log" entry (file+headline (concat org-directory "work.org") "Work Log") 
    "* %^{topic} %T %^g\n   :CATEGORY: Log\n   %i%?\n")
   ("a" "Article" entry (file+headline (concat org-directory "articles.org") "Article")
    "* %^{topic} %T %^g\n   :CATEGORY: Article\n   %i%?\n")
   ("e" "Event" entry (file+headline (concat org-directory "agenda.org") "Events")
    "* %^{title} %^g\n     SCHEDULED: %^{when}t\n   %i%?\n")
   ("c" "Contact" entry (file+headline (concat org-directory "addresses.org") "Addresses")
    "* %(org-contacts-template-name)\n   :PROPERTIES:\n   :EMAIL: %(org-contacts-template-email)\n   :END:\n   %i%?\n"))
 org-modules
 '(org-bbdb org-bibtex org-docview org-gnus org-info org-jsinfo org-habit org-irc org-mew 
	    org-mhe org-rmail org-special-blocks org-vm org-wl org-w3m org-mouse org-bookmark 
	    org-drill org-eshell org-invoice org-registry org-contacts))

(defun custom-org-hook ()
  (interactive)
  (visual-line-mode 1)
  (flyspell-mode 1)
  (writegood-mode 1))

(add-hook 'org-mode-hook 'custom-org-hook)