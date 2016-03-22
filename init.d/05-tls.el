;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fix TLS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (and (not (string= system-type "windows-nt")) (find-exe "gnuts-cli"))
  (message "Fixing TLS")
  
  ;; From:
  ;; https://glyph.twistedmatrix.com/2015/11/editor-malware.html

  (setq tls-checktrust 1)
  
  (let ((trustfile
	 (replace-regexp-in-string
	  "\\\\" "/"
	  (replace-regexp-in-string
	   "\n" ""
	   (shell-command-to-string "python -m certifi")))))
    (setq tls-program
	  (list
	   (format "%s --x509cafile %s -p %%p %%h" (find-exe "gnutls-cli") trustfile)))
    (setq gnutls-verify-error t)
    (setq gnutls-trustfiles (list trustfile)))

  (if (condition-case e
	  (progn
	    (url-retrieve "https://wrong.host.badssl.com/"
			  (lambda (retrieved) t))
	    (url-retrieve "https://self-signed.badssl.com/"
			  (lambda (retrieved) t))
	    t)
	('error nil))
      (error "tls misconfigured")
    (url-retrieve "https://badssl.com"
		  (lambda (retrieved) t))))
