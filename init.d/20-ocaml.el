;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ocaml
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (and (find-exe "ocaml") (find-exe "opam"))
  (message "Configuring O'Caml")

  (require-package 'tuareg)

  (setq opam-share (substring (shell-command-to-string "opam config var share 2> /dev/null") 0 -1))
  (add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))
  (autoload 'merlin-mode "Merlin" "Merlin Mode" t)
  
  (setq merlin-command 'opam)

  (add-hook 'tuareg-mode-hook 'merlin-mode)
  (add-hook 'caml-mode-hook 'merlin-mode))
