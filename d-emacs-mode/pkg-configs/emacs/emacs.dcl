((d-emacs-special-read-answer-bindlist
 . (car (d-emacs-dirs-act-on-sexps-in-file
         (concat d-emacs-dirs-pkg-configs-directory
                 "emacs/special-answers.dbl")
         (lambda ()
           (let ((blist (d-emacs-read-region)))
             (append
              ;; We have to replace the bindings of lower levels that have possibly been used with bindings in higher levels that have not because they are not in other keyboard layouts.
              (mapcar (lambda (bind)
                        (cons (car (d-emacs-bind-change-coords-in-binding
                                    bind '((1 5 2 6) nil nil)))
                              (string-to-char
                               (d-emacs-bind-string bind))))
                      blist)
              blist)))))))
