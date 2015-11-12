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
 (concat "-" user-full-name "\n")
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

(setq magit-last-seen-setup-instructions "1.4.0")

(defvar system-include-paths
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

(setq my-load-debug-geiser t)
