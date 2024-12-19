`(d-emacs-special-read-answer-bindlist
  . ,(car (d--act-on-bindlists-in-file
           (concat (file-name-directory (buffer-file-name))
                   "d-emacs-emacs-special-answers-bindlists.el")
           (lambda ()
             (let ((blist (d--extract-bindlist)))
               (append
                ;; We have to replace the bindings of lower levels that have possibly been used with bindings in higher levels that have not because they are not in other keyboard layouts.
                (mapcar (lambda (bind)
                          (cons (car (d--change-coords-in-binding
                                      bind '((1 5 2 6) nil nil)))
                                (string-to-char
                                 (d--extract-binding-string bind))))
                        blist)
                blist))))))
