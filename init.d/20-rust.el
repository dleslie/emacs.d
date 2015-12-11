;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rust
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring Rust")

(require-package 'rust-mode)

(when (and (file-exists-p racer-cmd)
	   (file-exists-p racer-load-path))
  (add-to-list 'load-path racer-load-path)

  (eval-after-load "rust"
    '(require 'racer))

  (eval-after-load "company"
    '(add-to-list 'company-backends 'racer-company-complete))

  (define-key rust-mode-map (kbd "M-.") #'racer-find-definition)
  (define-key rust-mode-map (kbd "<C-tab>") #'racer-complete-or-indent)
  (add-hook 'rust-mode-hook #'racer-turn-on-eldoc))

