((daselt-special-read-answer-bindlist
  . (car (daselt-dirs-act-on-sexps-in-file
          (concat daselt-dirs-pkg-configs-directory
                  "emacs/special-answers.dbl")
          (lambda ()
            (let ((blist (daselt-base-read-region)))
              (append
               ;; We have to replace the bindings of lower levels that have possibly been used with bindings in higher levels that have not because they are not in other keyboard layouts.
               (mapcar (lambda (bind)
                         (cons (car (daselt-bind-change-coords-in-binding
                                     bind '((1 5 2 6) nil nil)))
                               (string-to-char
                                (daselt-bind-string bind))))
                       blist)
               blist)))))))
