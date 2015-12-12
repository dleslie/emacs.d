;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-gcal
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring org-gcal")

(require-package 'org-gcal)

(defun update-gcal ()
  (interactive)
  (message "Updating Calendar")
  (org-gcal-refresh-token)
  (org-gcal-fetch))

(defun force-gcal-category ()
  (let ((category (assoc buffer-file-name org-gcal-forced-category-file-alist)))
    (when category
      (goto-char (point-min))
      (insert (format "#+CATEGORY: %s" (cdr category)))
      (newline))))

(add-hook 'before-save-hook 'force-gcal-category)
