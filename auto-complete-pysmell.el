(defvar ac-source-pysmell
  '((candidates
     . (lambda ()
         (require 'pysmell)
         (pysmell-get-all-completions))))
  "Source for PySmell")

(add-hook 'python-mode-hook
          '(lambda ()             
             (set (make-local-variable 'ac-sources) (append ac-sources '(ac-source-pysmell)))))