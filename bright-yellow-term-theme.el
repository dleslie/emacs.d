;;; bright-yellow-term-theme.el --- Emacs 24 theme with a dark background.

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

(deftheme bright-yellow-term)
(let ((class '((class color) (min-colors 89)))
      (fg4 "#E0F000")
      (fg3 "#F0E000")
      (fg2 "#E0F000")
      (fg1 "#F0F000")
      (bg1 "#000000")
      (bg2 "#505000")
      (bg3 "#707000")
      (bg4 "#707000")
      (al1 "#C0C0C0")
      (al2 "#E0E0E0")
      (al3 "#F0F0A0")
      (al4 "#ffffff"))
  (custom-theme-set-faces
   'bright-yellow-term
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
   `(link ((,class (:foreground ,al1 :underline t))))))


;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'bright-yellow-term)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; yellow-term-theme.el ends here

