;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring Org Mode")

(require-package 'org-plus-contrib 'org)
(require-package 'writegood-mode)

(setq
 org-default-notes-file "notes.org"
 org-agenda-files '("todo.org" "agenda.org")
 org-agenda-diary-file "/diary.org"
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
    "* %T %^g\n   :CATEGORY: Review\n   %?%[~/ownCloud/org/template_daily_review.org]\n") 
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
    "* %(org-contacts-template-name)\n   :PROPERTIES:\n   :EMAIL: %(org-contacts-template-email)\n   :END:\n   %i%?\n"))
 org-modules
 '(org-bbdb org-bibtex org-docview org-gnus org-info org-jsinfo org-habit org-irc org-mew 
	    org-mhe org-rmail org-special-blocks org-vm org-wl org-w3m org-mouse org-bookmark 
	    org-drill org-eshell org-invoice org-registry org-contacts))

(defun custom-org-hook ()
  (interactive)
  (visual-line-mode t)
  (with-eval-after-load "flyspell"
    (flyspell-mode t))
  (with-eval-after-load "writegood"
    (writegood-mode t)))

(require-package 'ox-asciidoc)
(require-package 'ox-epub)
(require-package 'ox-gfm)
(require-package 'ox-html5slide)
(require-package 'ox-impress-js)
(require-package 'ox-jira)
(require-package 'ox-mediawiki)
(require-package 'ox-minutes)
(require-package 'ox-nikola)
(require-package 'ox-pandoc)
(require-package 'ox-reveal)
(require-package 'ox-rst)
(require-package 'ox-textile)
(require-package 'ox-tiddly)
(require-package 'ox-trac)
(require-package 'ox-tufte)
(require-package 'ox-twbs)
(require-package 'ox-twiki)

(add-hook 'org-mode-hook 'custom-org-hook)

(with-eval-after-load "org"
  (when (file-exists-p (concat user-emacs-directory "gcal-settings.el"))
    (require-package 'org-gcal)

    (load (expand-file-name (concat user-emacs-directory "gcal-settings.el")))
    
    (defun update-gcal ()
      (interactive)
      (message "Updating Calendar")
      (org-gcal-fetch))))