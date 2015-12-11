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
 my-mu4e-lisp-path
 "/usr/local/share/emacs/site-lisp/mu4e"
 user-mail-address 
 "dan@ironoxide.ca"
 user-full-name 
 "Dan Leslie"
 user-mail-login 
 "dan@ironoxide.ca"
 user-mail-attachment-directory
 "/home/dleslie/Downloads/Attachments"
 mail-smtp-server
 "boomer.asoshared.com"
 mail-smtp-port
 465
 mail-folder-inbox
 "/"
 mail-folder-drafts
 "/Drafts"
 mail-folder-sent
 "/Sent"
 mail-folder-trash
 "/Trash"
 mu4e-compose-signature
 nil
 gnus-select-method
 '(nntp "GMane"
        (nntp-address "news.gmane.org")))

(setq
 racer-rust-src-path
 "/usr/local/src/rustc-1.5.0/src"
 my-racer-cmd
 "/home/dleslie/Workspace/code/dleslie/racer/target/release/racer"
 my-racer-load-path
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
	("http://www.wired.com/category/science/science-blogs/feed/" blog science)
	("https://www.reddit.com/r/lisp+emacs+scheme.rss" aggregator programming)
	("https://www.reddit.com/r/canada+canadapolitics+environment+science+worldnews.rss" aggregator news)))

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
