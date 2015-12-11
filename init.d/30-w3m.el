;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; w3m
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Configuring w3m")

(setq browse-url-browser-function 'w3m-browse-url)
(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
