;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; web-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-perf-metrics "web"
  (use-package restclient
    :init
    (with-eval-after-load "company"
      (use-package company-restclient
        :init
        (push 'company-restclient company-backends))))

  (use-package web-mode
    :init
    (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))

    (defun my-tsx-web-mode-hook ()
      (when (string-equal "tsx" (file-name-extension buffer-file-name))
        (setup-tide-mode)))

    (defun my-jsx-web-mode-hook ()
      (when (string-equal "jsx" (file-name-extension buffer-file-name))
        (setup-tide-mode)))

    (add-hook 'web-mode-hook #'my-tsx-web-mode-hook)
    (add-hook 'web-mode-hook #'my-jsx-web-mode-hook)

    (with-eval-after-load "company"
      (use-package company-web
        :init
        (push 'company-web-html company-backends)
        (push 'company-web-jade company-backends)
        (push 'company-web-slim company-backends)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; scheme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-perf-metrics "scheme"
  (use-package geiser))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; toml
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-perf-metrics "toml"
  (use-package toml-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; c and c++
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-perf-metrics "c and c++"
  (add-hook 'c++-mode-hook (lambda () (eldoc-mode 1)))
  (add-hook 'c-mode-hook (lambda () (eldoc-mode 1)))
  (add-to-list 'auto-mode-alist '("\\.ino?\\'" . c++-mode))

  (when (find-exe "global")
    (use-package ggtags
      :init
      (add-hook 'c-mode-common-hook
                (lambda ()
                  (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
                    (ggtags-mode 1))))

      (push 'company-gtags company-backends )

      :config
      (define-key ggtags-mode-map (kbd "C-c g s") 'ggtags-find-other-symbol)
      (define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-tag-history)
      (define-key ggtags-mode-map (kbd "C-c g r") 'ggtags-find-reference)
      (define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
      (define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
      (define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags)

      (define-key ggtags-mode-map (kbd "M-,") 'pop-tag-mark))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; look and feel
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-perf-metrics "look and feel"
  (define-key-after global-map [menu-bar tools apps eww]
    '(menu-item "Browse the Web (eww)" eww :help "Browse the web with eww") t)
  (define-key-after global-map [menu-bar tools apps erc]
    '(menu-item "IRC (erc)" erc :help "Use IRC via erc") t)
  (define-key-after global-map [menu-bar tools apps rcirc]
    '(menu-item "IRC (rcirc)" erc :help "Use IRC via rcirc") t)
  
  (use-package olivetti
    :init
    (define-key text-mode-map [menu-bar text olivetti-mode]
      '(menu-item "Olivetti" olivetti-mode
                  :button (:toggle . (and (boundp 'olivetti-mode) olivetti-mode)))))

  (use-package writeroom-mode
    :init
    (define-key text-mode-map [menu-bar text writeroom-mode]
      '(menu-item "Writeroom" writeroom-mode
                  :button (:toggle . (and (boundp 'writeroom-mode) writeroom-mode)))))

  (use-package writegood-mode
    :init
    (add-hook 'text-mode-hook 'writegood-mode)
    (define-key text-mode-map [menu-bar text writeroom-mode]
      '(menu-item "Writegood" writegood-mode
                  :button (:toggle . (and (boundp 'writegood-mode) writegood-mode)))))
  
  (use-package smex
    :bind (("M-x" . smex)))

  (use-package ido
    :init
    (setq
     ido-create-new-buffer 'always
     ido-enable-flex-matching t
     ido-everywhere t))

  (use-package nyan-mode
    :init
    (nyan-mode t)
    (setq nyan-animate-nyancat t))

  (when-find-exe "ag"
    (use-package ag
      :init
      (define-key-after global-map [menu-bar tools ag]
        '(menu-item "Search Files (ag)..." ag :help "Search files for strings or regexps (with ag)...")
        'grep)))
  
  (use-package dumb-jump
    :bind
    (("C-c j" . dumb-jump-go)
     ("C-c J" . dumb-jump-quick-look)
     ("C-x j" . dumb-jump-back))
    :init
    (define-key-after global-map [menu-bar edit dj-menu]
      (cons "Dumb Jump" (make-sparse-keymap "dumb jump")) 'goto)
    (define-key global-map [menu-bar edit dj-menu go]
      '(menu-item "Go" dumb-jump-go :help "Jump to definition"))
    (define-key global-map [menu-bar edit dj-menu quick-look]
      '(menu-item "Quick Look" dumb-jump-quick-look :help "Look at definition"))
    (define-key global-map [menu-bar edit dj-menu back]
      '(menu-item "Back" dumb-jump-back :help "Go back")))

  (use-package ace-jump-mode
    :bind
    (("C-c SPC" . ace-jump-mode)
     ("C-x SPC" . ace-jump-mode-pop-mark))
    :init
    (use-package ace-link)
    
    (autoload 'ace-jump-mode "ace-jump-mode" "Emacs quick move minor mode" t)
    (autoload 'ace-jump-mode-pop-mark "ace-jump-mode" "Ace jump back:-)" t)

    (ace-link-setup-default)

    (define-key-after global-map [menu-bar edit ace-menu]
      (cons "Ace Jump" (make-sparse-keymap "ace jump")) 'goto)
    (define-key global-map [menu-bar edit ace-menu jump]
      '(menu-item "Jump" ace-jump-mode :help "Ace Jump"))
    (define-key global-map [menu-bar edit ace-menu back]
      '(menu-item "Back" ace-jump-mode-pop-mark :help "Ace Jump Back")))
  
  (use-package dictionary
    :bind (("C-c d" . dictionary-search))
    :init
    (define-key-after global-map [menu-bar tools apps dictionary-search]
      '(menu-item "Dictionary" dictionary-search :help "Search dictionary") t))
  
  (use-package quiz
    :bind (("C-c q" . quiz))
    :init
    (define-key global-map [menu-bar tools games quiz]
      '(menu-item "Quiz" quiz :help "Be quizzed")))
  
  (use-package doom-themes)
  
  (defun my-try-to-add-imenu ()
    (condition-case nil (imenu-add-to-menubar "Imenu") (error nil)))
  (add-hook 'font-lock-mode-hook 'my-try-to-add-imenu))
