(require 'multi-mode)

(defun scheme-c-mode ()
  "Mode for editing C embedded in Scheme, using multi-mode."
  (interactive)
  (set (make-local-variable 'multi-alist)
       '((scheme-mode)
	 (c-mode . scheme-c-header-region)
	 (c-mode . scheme-c-inline-region)))
  (multi-mode-install-modes))

(defun scheme-c-header-region (pos)
  "Mode-selecting function for C embedded in Scheme."
  (let ((case-fold-search t)
	foreign-start foreign-end next-foreign)
    (save-excursion
      (save-restriction
	(widen)
	(goto-char pos)
	(save-excursion
	  (let* ((p1 (save-excursion
		       ;; Check whether we're on the processing
		       ;; instruction start.  Skip definitely clear of
		       ;; it and then search backwards.
		       (goto-char (min (point-max) (+ (point) 2)))
		       (search-backward "#>" (- (point) 3) t)))
		 (match-end (if p1 (match-end 0)))
		 ;; Otherwise search backwards simply.
		 (p2 (unless p1 (search-backward "#>" nil t))))
	    (if p2 (setq match-end (match-end 0)))
	    (setq foreign-start (or p1 p2))
	    ;; Ready to search for matching terminator or next
	    ;; processing instruction.
	    (goto-char (or match-end pos)))
	  (if foreign-start
	      ;; Look forward for the FOREIGN terminator.
	      (let* ((p1 (save-excursion
			   ;; Check whether we're on the terminator.
			   (backward-char 1)
			   (search-backward "<#" (- (point) 2) t)))
		     (p2 (unless p1 (search-forward "<#" nil t))))
		(setq foreign-end (or p1 p2 (point-max)))
		(goto-char pos)))
	  (if (and foreign-start foreign-end (< pos foreign-end))
	      ;; We were between FOREIGN start and terminator.
	      (list 'c-mode foreign-start foreign-end)
	    ;; Otherwise, look forward for a FOREIGN to delimit the Scheme
	    ;; region.
	    (setq next-foreign 
		  (if (search-forward "#>" nil t)
		      (match-beginning 0)
		    (point-max)))
	    (if foreign-start
		(list 'scheme-mode (or foreign-end (point-min)) next-foreign)
	      (list 'scheme-mode (point-min) next-foreign))))))))

(defun scheme-c-inline-region (pos)
  "Mode-selecting function for C embedded in Scheme."
  (let ((case-fold-search t)
	foreign-start foreign-end next-foreign)
    (save-excursion
      (save-restriction
	(widen)
	(goto-char pos)
	(save-excursion
	  (let* ((p1 (save-excursion
		       ;; Check whether we're on the processing
		       ;; instruction start.  Skip definitely clear of
		       ;; it and then search backwards.
		       (goto-char (min (point-max) (+ (point) 6)))
		       (search-backward "<<ENDC" (- (point) 11) t)))
		 (match-end (if p1 (match-end 0)))
		 ;; Otherwise search backwards simply.
		 (p2 (unless p1 (search-backward "<<ENDC" nil t))))
	    (if p2 (setq match-end (match-end 0)))
	    (setq foreign-start (or p1 p2))
	    ;; Ready to search for matching terminator or next
	    ;; processing instruction.
	    (goto-char (or match-end pos)))
	  (if foreign-start
	      ;; Look forward for the FOREIGN terminator.
	      (let* ((p1 (save-excursion
			   ;; Check whether we're on the terminator.
			   (backward-char 1)
			   (search-backward "ENDC" (- (point) 4) t)))
		     (p2 (unless p1 (search-forward "ENDC" nil t))))
		(setq foreign-end (or p1 p2 (point-max)))
		(goto-char pos)))
	  (if (and foreign-start foreign-end (< pos foreign-end))
	      ;; We were between FOREIGN start and terminator.
	      (list 'c-mode foreign-start foreign-end)
	    ;; Otherwise, look forward for a FOREIGN to delimit the Scheme
	    ;; region.
	    (setq next-foreign 
		  (if (search-forward "<<ENDC" nil t)
		      (match-beginning 0)
		    (point-max)))
	    (if foreign-start
		(list 'scheme-mode (or foreign-end (point-min)) next-foreign)
	      (list 'scheme-mode (point-min) next-foreign))))))))

(provide 'scheme-c-mode)
