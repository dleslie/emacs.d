;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C/C++
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring C and C++")

 ;; Fixes missing C++11 fontlocking in cc-mode

(defun c++-font-lock-fix ()
  (font-lock-add-keywords 
   nil 
   '(("\\<\\(void\\|unsigned\\|signed\\|char\\|short\\|bool\\|int\\|long\\|float\\|double\\)\\>" . font-lock-keyword-face)
     ;; add the new C++11 keywords
     ("\\<\\(alignof\\|alignas\\|constexpr\\|decltype\\|noexcept\\|nullptr\\|static_assert\\|thread_local\\|override\\|final\\)\\>" . font-lock-keyword-face)
     ("\\<\\(char[0-9]+_t\\)\\>" . font-lock-keyword-face)
     ;; PREPROCESSOR_CONSTANT
     ("\\<[A-Z]+\\([A-Z_]+\\|[0-9]+\\)\\>" . font-lock-constant-face)
     ;; hexadecimal numbers
     ("\\<0[xX][0-9A-Fa-f]+\\>" . font-lock-constant-face)
     ;; integer/float/scientific numbers
     ("\\<[\\-+]*[0-9]*\\.?[0-9]+\\([ulUL]+\\|[eE][\\-+]?[0-9]+\\)?\\>" . font-lock-constant-face)
     ;; user-types
     ("\\<[A-Za-z_]+[A-Za-z_0-9]*_\\(t\\|ptr\\|c\\|e\\)\\>" . font-lock-type-face)
     )))

(add-hook 'c++-mode-hook 'c++-font-lock-fix)

(with-eval-after-load 'flycheck
  (add-hook 'c++-mode-hook
	    (lambda () (setq flycheck-gcc-language-standard "c++11")))
  (add-hook 'c-mode-hook
	    (lambda () (setq flycheck-gcc-language-standard "c11"))))

;; (require-package 'irony)
;; (require-package 'irony-eldoc)

;; (with-eval-after-load 'irony
;;   (add-hook 'c++-mode-hook 'irony-mode)
;;   (add-hook 'c-mode-hook 'irony-mode)
;;   (add-hook 'objc-mode-hook 'irony-mode)
;;   (add-hook 'irony-mode-hook 'irony-eldoc)
  
;;   ;; replace the `completion-at-point' and `complete-symbol' bindings in
;;   ;; irony-mode's buffers by irony-mode's asynchronous function
;;   (defun my-irony-mode-hook ()
;;     (define-key irony-mode-map [remap completion-at-point]
;;       'irony-completion-at-point-async)
;;     (define-key irony-mode-map [remap complete-symbol]
;;       'irony-completion-at-point-async))
;;   (add-hook 'irony-mode-hook 'my-irony-mode-hook)

;;   (when (eq system-type 'windows-nt)
;;     (setq w32-pipe-read-delay 0)))

