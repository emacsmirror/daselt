((org-latex-compiler . "lualatex")
 (org-preview-latex-default-process . 'imagemagic-lua)
 (org-use-speed-commands . t)
 (org-speed-commands
  . (daselt-dirs-act-on-sexps-in-file
     (concat daselt-dirs-pkg-configs-directory "org/org-speed-commands-special.dbl")
     (lambda () (let ((blist (daselt-base-read-region)))
                  (mapcar (lambda (binding)
                            (cons (daselt-bind-string binding)
                                  (cdr binding)))
                          blist))))))

