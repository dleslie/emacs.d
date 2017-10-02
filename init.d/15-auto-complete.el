(when (and (boundp 'enable-auto-complete) enable-auto-complete)
  (message "Configuring Auto Complete")
  (require-package 'auto-complete)
  (require-package 'ac-capf)
  (ac-config-default)
  (add-to-list 'ac-sources 'ac-source-capf)
  (add-to-list 'ac-sourcs 'ac-source-dictionary)
  (add-to-list 'ac-sourcs 'ac-source-imenu))
