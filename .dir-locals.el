((emacs-lisp-mode
  . (
     (outline-regexp . "^;;; \\|^;;;; \\|^;;;;; ")
     (outline-it-heading-alist .
                               '(("^;;; " . 1)
                                 ("^;;;; " . 2)
                                 ("^;;;;; " . 3)))
     (eval . (when (fboundp 'my/outline-small-init)
               (my/outline-small-init))))))
