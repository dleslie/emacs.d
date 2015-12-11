;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C/C++
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring C and C++")

(require-package 'function-args)
(require-package 'ggtags)

(eval-after-load "company-mode"
  '(progn
     (require-package 'company-c-headers)
     (add-to-list 'company-backends 'company-c-headers)))

(defun custom-cc-prog-hook ()
  ;; (enable-semantic-mode)
  (ggtags-mode 1))

(add-hook 'c++-mode-hook 'custom-cc-prog-hook)
(add-hook 'c-mode-hook 'custom-cc-prog-hook)

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
					;(add-hook 'c-mode-common-hook 'fa-auto)

(add-hook 'c-mode-common-hook 'fa-config-default)
