;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Local Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Setting Local Configuration")

;; Org
(setq 
 org-directory "~/ownCloud/org/")

;; Javascript
(setq 
 tern-command '("~/node_modules/.bin/tern"))

;; Mail
(setq
 user-mail-address "dan@ironoxide.ca"
 user-full-name "Dan Leslie"
 user-mail-login "dan@ironoxide.ca"
 user-mail-attachment-directory "/home/dleslie/Downloads/Attachments"
 mail-smtp-server "boomer.asoshared.com"
 mail-smtp-port 587
 mail-folder-inbox "/INBOX"
 mail-folder-drafts "/INBOX.Drafts"
 mail-folder-sent "/INBOX.Sent"
 mail-folder-trash "/INBOX.Trash")

;; mu4e
(setq 
 my-mu4e-lisp-path "/usr/local/share/emacs/site-lisp/mu4e"
 mail-user-agent 'mu4e-user-agent
 mu4e-compose-signature nil
 mu4e-maildir "~/Maildir"
 mu4e-drafts-folder mail-folder-drafts
 mu4e-sent-folder mail-folder-sent
 mu4e-trash-folder mail-folder-trash
 mu4e-sent-messages-behavior 'sent
 mu4e-maildir-shortcuts `((,mail-folder-inbox . ?i)
			  (,mail-folder-sent  . ?s)
			  (,mail-folder-trash . ?t))
 mu4e-get-mail-command "offlineimap"
 mu4e-attachment-dir user-mail-attachment-directory
 mu4e-compose-reply-to-address user-mail-address
 mu4e-headers-include-related nil
 mu4e-headers-results-limit -1
 mu4e-sent-messages-behavior 'sent
 mu4e-update-interval -1
 mu4e-user-mail-address-list `(,user-mail-login ,user-mail-address)
 mu4e-view-show-addresses t
 mu4e-view-show-images t
 mu4e-org-contacts-file (concat org-directory "addresses.org"))

;; Rust
(setq
 racer-rust-src-path "/usr/local/src/rustc-1.5.0/src"
 my-racer-cmd "/home/dleslie/Workspace/code/dleslie/racer/target/release/racer"
 my-racer-load-path "/home/dleslie/Workspace/code/dleslie/racer/editors/emacs")

;; Semantic
(setq my-system-include-paths
      (append
       (directory-files "/usr/include/c++/" t "[^.][0-9.]+")
       '("/usr/local/include" "/usr/include")))

;; Geiser
(setq my-debug-geiser-path "/home/dleslie/Workspace/code/dleslie/geiser/elisp/geiser.el")

;; Elfeed
(setq elfeed-feeds
      '(("https://news.ycombinator.com/rss" aggregator tech)
	("http://rss.cbc.ca/lineup/technology.xml" news tech)
	("http://rss.cbc.ca/lineup/world.xml" news world)
	("http://rss.cbc.ca/lineup/canada.xml" news canada)
	("http://rss.cbc.ca/lineup/canada-britishcolumbia.xml" news bc)
	("http://www.wired.com/category/science/feed/" news science)
	("http://www.wired.com/category/gear/feed/" news tech)
	("http://www.wired.com/category/science/science-blogs/feed/" blog science)
	("https://www.reddit.com/r/lisp+emacs+scheme.rss" aggregator programming)
	("https://www.reddit.com/r/canada+canadapolitics+environment+science+worldnews.rss" aggregator news)))

;; Lisp
(setq slime-lisp-implementations
      '((sbcl ((find-exe "sbcl"))
	      :coding-system utf-8-unix)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keys
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key [mouse-2]
		'(lambda () (interactive) (message "mouse-2 paste disabled")))

(global-set-key "\C-w" 'clipboard-kill-region)
(global-set-key "\M-w" 'clipboard-kill-ring-save)
(global-set-key "\C-y" 'clipboard-yank)
(global-set-key "\C-c," 'scroll-bar-mode)
(global-set-key "\C-c." 'tool-bar-mode)
(global-set-key "\C-c?" 'menu-bar-mode)
(global-set-key "\C-c\\" 'comment-or-uncomment-region)
(global-set-key "\C-cs" 'eshell-here)
(global-set-key "\C-cw" 'browse-url)

(eval-after-load "org"
  '(progn
     (global-set-key "\C-ct" 'org-todo-list)
     (global-set-key "\C-cl" 'org-store-link)
     (global-set-key "\C-ca" 'org-agenda)
     (global-set-key "\C-cb" 'org-iswitchb)
     (global-set-key "\C-cc" 'org-capture)))

(eval-after-load "smex"
  '(global-set-key "\M-x" 'smex))

(eval-after-load "mu4e"
  '(global-set-key "\C-cm" 'mu4e))

(eval-after-load "magit"
  '(global-set-key "\C-cg" 'magit-status))

(eval-after-load "company"
  '(global-set-key (kbd "<C-tab>") 'company-complete))

(eval-after-load "dictionary"
  '(global-set-key "\C-cd" 'dictionary-search))

(eval-after-load "elfeed"
  '(global-set-key "\C-cf" 'elfeed))
