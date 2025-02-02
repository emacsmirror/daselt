((cdlatex-math-symbol-alist-default
  . (let* ((filepath (concat d-emacs-dirs-pkg-configs-directory "cdlatex/cdlatex-special-math-symbol.dbl"))
           (buffer (find-file-noselect filepath)))
      (prog1 (car (d-emacs-dirs-act-on-sexps-in-file
                   filepath
                   (lambda () (let ((blist (d-emacs-base-read-region)))
                           (mapcar (lambda (binding)
                                     (list (string-to-char
                                            (d-emacs-bind-string binding))
                                           (cdr binding)))
                                   blist)))))
        (unless d-emacs-dirs-keep-read-buffers
          (kill-buffer buffer)))))

 (cdlatex-math-modify-alist-default
  . (let* ((filepath (concat d-emacs-dirs-pkg-configs-directory "cdlatex/cdlatex-special-math-modify.dbl"))
           (buffer (find-file-noselect filepath)))
      (prog1 (car (d-emacs-dirs-act-on-sexps-in-file
                   filepath
                   (lambda () (let ((blist (d-emacs-base-read-region)))
                           (mapcar (lambda (binding)
                                     (cons (string-to-char
                                            (d-emacs-bind-string binding))
                                           (cdr binding)))
                                   blist)))))
        (unless d-emacs-dirs-keep-read-buffers
          (kill-buffer buffer))))))
