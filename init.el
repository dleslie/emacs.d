(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-auto-start 0.25)
 '(ac-delay 0.5)
 '(ac-ignore-case nil)
 '(ac-quick-help-delay 0.5)
 '(auto-fill-mode t)
 '(auto-save-default nil)
 '(c-basic-offset 2)
 '(c-set-offset 2)
 '(c-set-style "BSD")
 '(column-number-mode t)
 '(custom-safe-themes (quote ("8ada1f0bcfc2d8662b74fb21bd1830eaacb5d29e3c99a5ea7fd7a417b7a9b708" "88e56f2e676c8828e08b128c74f2818cbfc77b79f8ebbae955db6098d0001473" default)))
 '(debug-on-error nil)
 '(debug-on-signal nil)
 '(delete-selection-mode 1)
 '(display-battery-mode t)
 '(display-time-mode t)
 '(enh-ruby-program "/usr/bin/ruby")
 '(fill-column 80)
 '(indent-tabs-mode nil)
 '(inferior-lisp-program "/usr/bin/sbcl")
 '(jedi:complete-on-dot t)
 '(jedi:setup-keys t)
 '(line-number-mode t)
 '(make-backup-files nil)
 '(nyan-wavy-trail t)
 '(py-python-command "/usr/bin/python")
 '(python-indent-offset 4)
 '(python-shell-interpreter "python")
 '(redisplay-dont-pause t)
 '(scroll-bar-mode nil)
 '(scroll-margin 0)
 '(scroll-step 1)
 '(semanticdb-find-default-throttle (quote (local project unloaded system recursive omniscience)))
 '(slime-contribs '(slime-fancy slime-autodoc slime-banner))
 '(standard-indent 2)
 '(tab-stop-list (number-sequence 2 200 2))
 '(tab-width 4)
 '(tool-bar-style (quote image))
 '(truncate-lines t)
 '(visible-bell t)
 '(visual-line-mode t)
 '(word-wrap t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil))))
 '(mode-line ((t (:background "#191919" :foreground "#BBBBBB" :box nil))))
 '(mode-line-highlight ((t (:box nil))))
 '(rainbow-delimiters-depth-1-face ((t (:inherit font-lock-function-name-face))))
 '(rainbow-delimiters-depth-2-face ((t (:inherit font-lock-variable-name-face))))
 '(rainbow-delimiters-depth-3-face ((t (:inherit font-lock-keyword-face))))
 '(rainbow-delimiters-depth-4-face ((t (:inherit font-lock-comment-face))))
 '(rainbow-delimiters-depth-5-face ((t (:inherit font-lock-type-face))))
 '(rainbow-delimiters-depth-6-face ((t (:inherit font-lock-constant-face))))
 '(rainbow-delimiters-depth-7-face ((t (:inherit font-lock-builtin-face))))
 '(rainbow-delimiters-depth-8-face ((t (:inherit font-lock-string-face))))
 '(rainbow-delimiters-depth-9-face ((t (:inherit font-lock-doc-face))))
 '(rainbow-delimiters-unmatched-face ((((class color) (min-colors 89)) (:foreground "#ffffff" :background "#a40000" :bold t)))))

(setq org-directory "~/Dropbox/org/")
(setq org-default-notes-file (concat org-directory "notes.org"))
(setq org-agenda-files '((concat org-directory "todo.org") (concat org-directory "agenda.org") (concat org-directory "remember.org")))
(setq org-agenda-diary-file (concat org-directory "remember.org"))

(message "Adding Post Init Hook")

(defun post-init-hook ()
  (load "~/.emacs.d/post-init.el")
  (override-theme 'moe-dark))

(add-hook 'after-init-hook 'post-init-hook)

(put 'downcase-region 'disabled nil)
