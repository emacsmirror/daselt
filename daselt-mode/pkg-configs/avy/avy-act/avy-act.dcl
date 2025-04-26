((avy-act-recenter-at-cur-line-keys
  . (flatten-list
     (daselt-dirs-act-on-sexps-in-file
      (concat (file-name-directory (buffer-file-name)) "avy-act-special-recenter.dbl")
      (lambda () (let ((blist (daselt-base-read-region)))
                   (mapcar (lambda (bind)
                             (let ((kbdbind (kbd (daselt-bind-string bind t t))))
                               (if (stringp kbdbind) ; Allow non-characters to also be bound.
                                   (string-to-char kbdbind)
                                 kbdbind)))
                           blist)))))))
