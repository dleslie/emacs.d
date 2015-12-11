;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Defining Custom Functions")

(defun find-exe (name)
  (if (string= system-type "windows-nt")
      (or
       (executable-find name)
       (executable-find	(format "%s.exe" name))
       (executable-find	(format "%s.bat" name)))
    (executable-find name)))

(defun reset-theme ()
  (interactive)
  (while custom-enabled-themes
    (disable-theme (car custom-enabled-themes))))

(defun override-theme (arg)
  "Disables all enabled themes and then loads the provided theme."
  (interactive
   (list
    (intern (completing-read "Load custom theme: "
                             (mapcar 'symbol-name (custom-available-themes))))))
  (reset-theme)
  (load-theme arg t))

(setq my-first-boot-package-inited nil)

(defun require-package (package-name)
  "Loads and imports packages, installing from ELPA if necessary"
  (with-demoted-errors
      "Error: %S"
    
    (unless (package-installed-p package-name)
      (unless my-first-boot-package-inited
	(package-refresh-contents)
	(setq my-first-boot-package-inited t))

      (package-install package-name))))

(defun require-packages (package-list)
  "Loads packages from a list"
  (apply 'require-package package-list))

;; From:
;; http://www.howardism.org/Technical/Emacs/eshell-fun.html
(defun eshell-here ()
  "Opens up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (height (/ (window-total-height) 3))
         (name   (car (last (split-string parent "/" t)))))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))

    (insert (concat "ls"))
    (eshell-send-input)))
