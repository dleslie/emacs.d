;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Local Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Setting Local Configuration")

;; Emacs default scrolling behaviour is the worst
(setq
 scroll-step 2
 scroll-conservatively 10000
 auto-window-vscroll nil)

;; Org
(setq 
 org-directory "~/OneDrive/org/")

;; Javascript
(setq 
 tern-command '("~/node_modules/.bin/tern"))

;; Company
(setq company-tooltip-align-annotations t)

;; Semantic
(with-eval-after-load "semantic"
  (global-set-key "\M-." 'semantic-ia-fast-jump))

(setq my-system-include-paths
      (append
       (when (file-exists-p "/usr/include/c++/")
	 (directory-files "/usr/include/c++/" t "[^.][0-9.]+"))
       '("/usr/local/include" "/usr/include")))

;; Geiser
(setq my-debug-geiser-path "/home/dleslie/Workspace/code/github/dleslie/geiser/elisp/geiser-load.el")

;; Elfeed
(setq elfeed-feeds
      '(("http://news.ycombinator.com/rss" aggregator tech)
        ("http://rss.cbc.ca/lineup/world.xml" news world)
        ("http://rss.cbc.ca/lineup/canada.xml" news canada)
        ("http://rss.cbc.ca/lineup/canada-britishcolumbia.xml" news bc)
        ("http://www.reddit.com/r/lisp+emacs+scheme.rss" aggregator programming)
        ("http://www.reddit.com/r/canada+canadapolitics+environment+science+worldnews.rss" aggregator news)))

;; Lisp
(setq slime-lisp-implementations
      `((sbcl (,(find-exe "sbcl"))
	      :coding-system utf-8-unix))
      inferior-lisp-program
      (find-exe "sbcl"))

;; General Emacs Sanity
(setq gc-cons-threshold 20000000
      indent-tabs-mode nil
      make-backup-files nil)
(delete-selection-mode 1)

;; Ido
(setq ido-create-new-buffer 'always
      ido-enable-flex-matching t
      ido-everywhere t)

;; Projectile

(setq projectile-switch-project-action 'projectile-find-dir
      projectile-find-dir-includes-top-level t)

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
(global-set-key "\C-c/" 'speedbar-get-focus)

(eval-after-load "org"
  '(progn
     (global-set-key "\C-ct" 'org-todo-list)
     (global-set-key "\C-cl" 'org-store-link)
     (global-set-key "\C-ca" 'org-agenda)
     (global-set-key "\C-cb" 'org-iswitchb)
     (global-set-key "\C-cc" 'org-capture)))

(eval-after-load "mu4e"
  '(global-set-key "\C-cm" 'mu4e))

(eval-after-load "magit"
  '(global-set-key "\C-cg" 'magit-status))

(eval-after-load "company"
  '(progn
     (global-set-key (kbd "<C-tab>") 'company-indent-or-complete-common)
     (setq company-tooltip-align-annotations t)))

(eval-after-load "dictionary"
  '(global-set-key "\C-cd" 'dictionary-search))

(eval-after-load "elfeed"
  '(global-set-key "\C-cf" 'elfeed))

(eval-after-load "omnisharp"
  '(define-key omnisharp-mode-map "\M-." 'omnisharp-go-to-definition))

(eval-after-load "smex"
  '(global-set-key "\M-x" 'smex))

(eval-after-load "ggtags"
  '(progn
     (define-key ggtags-mode-map (kbd "C-c g s") 'ggtags-find-other-symbol)
     (define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-tag-history)
     (define-key ggtags-mode-map (kbd "C-c g r") 'ggtags-find-reference)
     (define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
     (define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
     (define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags)

     (define-key ggtags-mode-map (kbd "M-,") 'pop-tag-mark)))

; VS-style debug commands
(global-set-key [f5] 'gud-cont)
(global-set-key [f7] 'gud-tbreak)
(global-set-key [S-f11] 'gud-finish)
(global-set-key [f9] 'gud-break)
(global-set-key [f10] 'gud-next)
(global-set-key [f11] 'gud-step)

(global-set-key [f2] 'menu-bar-open)
(global-set-key [f12] 'toggle-frame-fullscreen)

(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)
(define-key global-map (kbd "C-c r") 'srefactor-refactor-at-point)
