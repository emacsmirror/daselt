((cdlatex-math-symbol-alist-default
    . (let* ((filepath (concat daselt-dirs-pkg-configs-directory "cdlatex/cdlatex-special-math-symbol.dbl"))
           (buffer (find-file-noselect filepath)))
      (prog1 (car (daselt-dirs-act-on-sexps-in-file
                   filepath
                   (lambda () (let ((blist (daselt-base-read-region)))
                           (mapcar (lambda (binding)
                                       (list (string-to-char
                                            (daselt-bind-string binding))
                                           (cdr binding)))
                                   blist)))))
        (unless daselt-dirs-keep-read-buffers
          (kill-buffer buffer)))))
  
  (cdlatex-math-modify-alist-default
  . (let* ((filepath (concat daselt-dirs-pkg-configs-directory "cdlatex/cdlatex-special-math-modify.dbl"))
           (buffer (find-file-noselect filepath)))
      (prog1 (car (daselt-dirs-act-on-sexps-in-file
                   filepath
                   (lambda () (let ((blist (daselt-base-read-region)))
                           (mapcar (lambda (binding)
                                       (cons (string-to-char
                                            (daselt-bind-string binding))
                                           (cdr binding)))
                                   blist)))))
        (unless daselt-dirs-keep-read-buffers
          (kill-buffer buffer))))))
