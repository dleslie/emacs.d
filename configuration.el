;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Local Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Setting Local Configuration")

(setq
 enable-semantic nil
 enable-company nil
 enable-auto-complete t)

;; Emacs default scrolling behaviour is the worst
(setq
 scroll-step 2
 scroll-conservatively 10000
 auto-window-vscroll nil)

;; General Emacs Sanity
(setq gc-cons-threshold 20000000
      indent-tabs-mode nil
      make-backup-files nil)
(delete-selection-mode 1)

;; Org
(setq org-directory "~/OneDrive/org/")

;; Javascript
(setq tern-command '("~/node_modules/.bin/tern"))

;; Semantic
(setq enable-semantic nil)
(when enable-semantic
  (with-eval-after-load "semantic"
    (global-set-key "\M-." 'semantic-ia-fast-jump))

  (setq my-system-include-paths
	(append
	 (when (file-exists-p "/usr/include/c++/")
	   (directory-files "/usr/include/c++/" t "[^.][0-9.]+"))
	 '("/usr/local/include" "/usr/include"))))

;; Lisp
(setq slime-lisp-implementations
      `((sbcl (,(find-exe "sbcl"))
	      :coding-system utf-8-unix)))
(setq inferior-lisp-program (find-exe "sbcl"))

;; Ido
(setq ido-create-new-buffer 'always)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)

;; Projectile
(setq projectile-switch-project-action 'projectile-find-dir)
(setq projectile-find-dir-includes-top-level t)

(global-set-key "\C-w" 'clipboard-kill-region)
(global-set-key "\M-w" 'clipboard-kill-ring-save)
(global-set-key "\C-y" 'clipboard-yank)
(global-set-key "\C-c," 'scroll-bar-mode)
(global-set-key "\C-c." 'tool-bar-mode)
(global-set-key "\C-c?" 'menu-bar-mode)
(global-set-key "\C-c\\" 'comment-or-uncomment-region)
(global-set-key "\C-cs" 'eshell-here)

(with-eval-after-load "quiz"
  (global-set-key "\C-cq" 'quiz))

(with-eval-after-load "ag"
  (global-set-key "\C-ca" 'ag))

(with-eval-after-load "org"
  (global-set-key "\C-ct" 'org-todo-list)
  (global-set-key "\C-cl" 'org-store-link)
  (global-set-key "\C-ca" 'org-agenda)
  (global-set-key "\C-cb" 'org-iswitchb)
  (global-set-key "\C-cc" 'org-capture))

(with-eval-after-load "magit"
  (global-set-key "\C-cg" 'magit-status))

(with-eval-after-load "company"
  (global-set-key (kbd "<C-tab>") 'company-indent-or-complete-common)
  (setq company-tooltip-align-annotations t))

(with-eval-after-load "dictionary"
  (global-set-key "\C-cd" 'dictionary-search))

(with-eval-after-load "elfeed"
  (setq elfeed-feeds
	'(("http://news.ycombinator.com/rss" aggregator tech)
	  ("http://rss.cbc.ca/lineup/world.xml" news world)
	  ("http://rss.cbc.ca/lineup/canada.xml" news canada)
	  ("http://rss.cbc.ca/lineup/canada-britishcolumbia.xml" news bc)
	  ("http://www.reddit.com/r/lisp+emacs+scheme.rss" aggregator programming)
	  ("http://www.reddit.com/r/canada+canadapolitics+environment+science+worldnews.rss" aggregator news)))
  (global-set-key "\C-cf" 'elfeed))

(with-eval-after-load "omnisharp"
  (define-key omnisharp-mode-map "\M-." 'omnisharp-go-to-definition))

(with-eval-after-load "smex"
  (global-set-key "\M-x" 'smex))

(with-eval-after-load "ggtags"
  (define-key ggtags-mode-map (kbd "C-c g s") 'ggtags-find-other-symbol)
  (define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-tag-history)
  (define-key ggtags-mode-map (kbd "C-c g r") 'ggtags-find-reference)
  (define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
  (define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
  (define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags)
  (define-key ggtags-mode-map (kbd "M-,") 'pop-tag-mark))

(global-set-key [f12] 'toggle-frame-fullscreen)

(with-eval-after-load "ace-jump"
  (define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
  (define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)
  (define-key global-map (kbd "C-c r") 'srefactor-refactor-at-point))
