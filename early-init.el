(setq package-user-dir (locate-user-emacs-file
                        (concat
                         (file-name-as-directory "elpa")
                         emacs-version)))

;; Prevent initial window flicker by disabling UI elements early
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)
