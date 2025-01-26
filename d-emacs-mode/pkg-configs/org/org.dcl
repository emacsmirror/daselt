((org-latex-compiler . "lualatex")
 (org-preview-latex-default-process . 'imagemagic-lua)
 (org-use-speed-commands . t)
 (org-speed-commands
  . (d-emacs-dirs-act-on-sexps-in-file
     (concat d-emacs-dirs-pkg-configs-directory "org/org-speed-commands-special.dbl")
     (lambda () (let ((blist (d-emacs-base-read-region)))
             (mapcar (lambda (binding)
                       (cons (d-emacs-bind-string binding)
                             (cdr binding)))
                     blist))))))

