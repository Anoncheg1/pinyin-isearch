((emacs-lisp-mode
  . (
     (outline-regexp . "^;;; \\|^;; -=-=\\|^;;;; ")
     (outline-it-heading-alist .
                               '((";;; " . 1)
                                 (";; -=-=" . 2)
                                 (";;;; " . 3)))
     ;; (eval . (outline-minor-mode 1))
     (eval . (progn (keymap-local-set "C-c k" #'outline-previous-heading)
                    (keymap-local-set "C-c n" #'outline-next-heading)
                    (when (require 'outline-it nil 'noerror)
                      (keymap-local-set "C-c C-e" #'outline-it-hide-other))
                    (keymap-local-set "<backtab>" #'outline-cycle-buffer)
                    (keymap-local-set "C-<tab>" #'outline-toggle-children)
                    (outline-hide-body)
                    ))
     )))
