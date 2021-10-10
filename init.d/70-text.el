;;; 70-text.el --- text

;;; Commentary:

;; Emacs configuration of Dan Leslie.
;; dan@ironoxide.ca

;;; Code:

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
  (define-key text-mode-map [menu-bar text writeroom-mode]
    '(menu-item "Writegood" writegood-mode
                :button (:toggle . (and (boundp 'writegood-mode) writegood-mode))))
  :init
  (add-hook 'text-mode-hook 'writegood-mode))

(use-package dictionary
  :init
  (global-set-key "\C-c d" 'dictionary-search)
  (define-key-after global-map [menu-bar tools apps dictionary-search]
    '(menu-item "Dictionary" dictionary-search :help "Search dictionary") t))

(use-package powerthesaurus
  :init
  (global-set-key "\C-c t" 'powerthesaurus-lookup-word-dwim)
  (define-key-after global-map [menu-bar tools apps powerthesaurus-lookup-word]
    '(menu-item "Thesaurus" powerthesaurus-lookup-word :help "Search thesaurus") t))

(provide '70-text)
;;; 70-text.el ends here
