;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Local Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Setting Local Configuration")

(setq 
 org-directory 
 "~/ownCloud/org/")

(setq 
 tern-command 
 '("~/node_modules/.bin/tern"))

(setq
 user-mail-address 
 "dan@ironoxide.ca"
 user-full-name 
 "Dan Leslie"
 user-mail-login 
 "dleslie@gmail.com"
 user-mail-attachment-directory
 "/home/dleslie/Downloads/Attachments"
 mail-smtp-server
 "smtp.gmail.com"
 mail-smtp-port
 587
 mail-folder-inbox
 "/INBOX"
 mail-folder-drafts
 "/[Gmail].Drafts"
 mail-folder-sent
 "/[Gmail].Sent Mail"
 mail-folder-trash
 "/[Gmail].Trash"
 mu4e-compose-signature
 nil
 ;(concat "-" user-full-name "\n")
 gnus-select-method
 '(nntp "GMane"
        (nntp-address "news.gmane.org")))

(setq
 racer-cmd
 "/home/dleslie/Workspace/code/dleslie/racer/target/release/racer"
 racer-rust-src-path
 "/usr/local/src/rustc-1.1.0/src"
 racer-load-path
 "/home/dleslie/Workspace/code/dleslie/racer/editors/emacs")

(setq my-system-include-paths
  '("/usr/local/include" 
    "/usr/include"
    "/usr/include/c++/4.4/" 
    "/usr/include/c++/4.7/" 
    "/usr/include/c++/4.8/" 
    "/usr/include/c++/4.9/" 
    "/usr/include/x86_64-linux-gnu"
    "/usr/include/x86_64-linux-gnu/c++/4.7/"
    "/usr/include/x86_64-linux-gnu/c++/4.8/"
    "/usr/include/x86_64-linux-gnu/c++/4.9/"
    "/usr/lib/gcc/x86_64-linux-gnu/4.7/include/"
    "/usr/lib/gcc/x86_64-linux-gnu/4.8/include/"
    "/usr/lib/gcc/x86_64-linux-gnu/4.9/include/"))

(setq my-debug-geiser-path "/home/dleslie/Workspace/code/dleslie/geiser/elisp/geiser.el")

(setq elfeed-feeds
      '(("https://news.ycombinator.com/rss" aggregator tech)
	("http://rss.cbc.ca/lineup/technology.xml" news tech)
	("http://rss.cbc.ca/lineup/world.xml" news world)
	("http://rss.cbc.ca/lineup/canada.xml" news canada)
	("http://rss.cbc.ca/lineup/canada-britishcolumbia.xml" news bc)
	("http://www.wired.com/category/science/feed/" news science)
	("http://www.wired.com/category/gear/feed/" news tech)
	("http://www.wired.com/category/science/science-blogs/feed/" blog science)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keys
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key [mouse-2] '(lambda () (interactive) (message "mouse-2 paste disabled")))

(global-set-key "\C-w" 'clipboard-kill-region)
(global-set-key "\M-w" 'clipboard-kill-ring-save)
(global-set-key "\C-y" 'clipboard-yank)

(global-set-key "\C-ct" 'org-todo-list)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-cc" 'org-capture)

(global-set-key "\M-x" 'smex)

(global-set-key "\C-cm" 'mu4e)

(global-set-key "\C-cg" 'magit-status)

(global-set-key (kbd "<C-tab>") 'company-complete)

(global-set-key "\C-c," 'scroll-bar-mode)
(global-set-key "\C-c." 'tool-bar-mode)
(global-set-key "\C-c?" 'menu-bar-mode)
(global-set-key "\C-c\\" 'comment-or-uncomment-region)
(global-set-key "\C-cs" 'eshell-here)
(global-set-key "\C-cd" 'dictionary-search)

(global-set-key "\C-cf" 'elfeed)

(global-set-key "\C-cw" 'browse-url)
