((cdlatex-math-symbol-alist-default
 . (let ((filepath (concat d-emacs-dirs-pkg-configs-directory "cdlatex/cdlatex-special-math-modify.dbl")))
     (prog1 (d-emacs-dirs-act-on-sexps-in-file
             filepath
             (lambda () (let ((blist (d-emacs-read-region)))
                     (mapcar (lambda (binding)
                               (list (string-to-char
                                      (d-emacs-bind-string binding))
                                     (cdr binding)))
                             blist))))
       (unless (or d-emacs-debug d-emacs-dirs-keep-read-buffers)
         (kill-buffer (get-file-buffer filepath))))))

(cdlatex-math-modify-alist-default
 . (let ((filepath (concat d-emacs-dirs-pkg-configs-directory "cdlatex/cdlatex-special-math-modify.dbl")))
     (prog1 (d-emacs-dirs-act-on-sexps-in-file
             filepath
             (lambda () (let ((blist (d-emacs-read-region)))
                     (mapcar (lambda (binding)
                               (cons (string-to-char
                                      (d-emacs-bind-string binding))
                                     (cdr binding)))
                             blist))))
       (unless (or d-emacs-debug d-emacs-dirs-keep-read-buffers)
         (kill-buffer (get-file-buffer filepath)))))))
