;;; yellow-term-theme.el --- Emacs 24 theme with a dark background.

;; Copyright (C) 2014 , Dan Leslie

;; Author: Dan Leslie
;; 
;; Version: 0.1
;; Package-Requires: ((emacs "24"))
;; Created with emacs-theme-generator, https://github.com/mswift42/theme-creator.


;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of Emacs.

;;; Commentary:

;;; Code:

(deftheme yellow-term)
(let ((class '((class color) (min-colors 89)))
      (fg4 "#908000")
      (fg3 "#A09000")
      (fg2 "#D0B000")
      (fg1 "#FFE000")
      (bg1 "#000000")
      (bg2 "#282800")
      (bg3 "#705000")
      (bg4 "#908000")
      (al1 "#8f8f6f")
      (al2 "#afaf88")
      (al3 "#dfdf98")
      (al4 "#fff0bc"))
  (custom-theme-set-faces
   'yellow-term
   `(default ((,class (:background ,bg1 :foreground ,fg3))))
   `(default-italic ((,class (:italic t))))
   `(cursor ((,class (:background ,bg3))))
   `(font-lock-builtin-face ((,class (:foreground ,fg2))))
   `(font-lock-comment-face ((,class (:foreground ,al1 :background ,bg2))))
   `(font-lock-constant-face ((,class (:foreground ,al1))))
   `(font-lock-doc-face ((,class (:foreground ,al2 :background ,bg2))))
   `(font-lock-function-name-face ((,class (:foreground ,fg2 :bold t))))
   `(font-lock-keyword-face ((,class (:bold ,class :foreground ,fg1))))
   `(font-lock-negation-char-face ((,class (:foreground ,al1))))
   `(font-lock-reference-face ((,class (:foreground ,al1))))
   `(font-lock-string-face ((,class (:foreground ,al1))))
   `(font-lock-type-face ((,class (:foreground ,al2 ))))
   `(font-lock-variable-name-face ((,class (:foreground ,fg2))))
   `(font-lock-warning-face ((,class (:foreground ,al4 :background ,bg3))))
   `(fringe ((,class (:background ,bg1 :foreground ,fg4))))
   `(highlight ((,class (:foreground ,fg3 :background ,bg3))))
   `(hl-line ((,class (:background  ,bg2))))
   `(isearch ((,class (:bold t :foreground ,al4 :background ,bg4))))
   `(minibuffer-prompt ((,class (:bold t :foreground ,al3))))
   `(mode-line ((,class (:box (:line-width 1 :color nil) :bold t :foreground ,fg4 :background ,bg2))))
   `(mode-line-buffer-id ((,class (:bold t :foreground ,fg1 :background nil))))
   `(mode-line-emphasis ((,class (:foreground ,fg1))))
   `(mode-line-highlight ((,class (:foreground ,al4 :box nil :weight bold))))
   `(mode-line-inactive ((,class (:box (:line-width 1 :color nil :style pressed-button) :foreground ,al3 :background ,bg1 :weight normal))))
   `(parenface-paren-face ((,class (:foreground ,al1))))
   `(region ((,class (:background ,fg1 :foreground ,bg1))))
   `(show-paren-match-face ((,class (:background ,bg3 :foreground ,al4))))
   `(vertical-border ((,class (:foreground ,fg3))))
   `(link ((,class (:foreground ,al1 :underline t))))
   `(parenthesis ((,class (:foreground ,bg3))))
   ))


;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'yellow-term)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; yellow-term-theme.el ends here

