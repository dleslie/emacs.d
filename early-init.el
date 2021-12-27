(setq package-user-dir (locate-user-emacs-file
                        (concat
                         (file-name-as-directory "elpa")
                         emacs-version)))
(setq straight-build-dir (format "build-%s" emacs-version))
