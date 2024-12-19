  ;;; d-lua-functions.el --- Daselt's Emacs module              -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Alexander Prähauser

;; Author: Alexander Prähauser <ahprae@protonmail.com>
;; Keywords: tools
;; Version: 0.8

;; This file is part of Daselt.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;  d-lua functions.

;;; Code:

;;;; Prettify
(defun d-lua-add-to-prettify ()
  "Adds symbol pairs to Emacs's prettify-symbols-alist.
To be hooked to `LaTeX-mode-hook'."
  (interactive)
  (mapcar (lambda (symbolpair)
            (add-to-list 'prettify-symbols-alist symbolpair))
          d-lua-prettify-symbols-list))

;;;; Add-Tex-Envs
(defun d-lua-add-cdlatex-envs (inputlist)
  "Adds latex-environments to cdlatex-env-alist."
  (mapcar #'(lambda (envtuple)
              (cond ((some #'identity
                           (mapcar
                            #'(lambda
                                (cdlatexenv)                             
                                (cl-member
                                 (car envtuple)
                                 cdlatexenv
                                 :test #'string=))
                            cdlatex-env-alist))
                     (message "%s is already defined in cdlatex-env-alist." (car envtuple)))
                    
                    ((some #'identity
                           (mapcar
                            #'(lambda
                                (cdlatexenv)
                                (string-match-p
                                 (nth 0 envtuple)
                                 (nth 1 cdlatexenv)
                                 ))
                            cdlatex-env-alist))
                     (message "%s is already taken in cdlatex-env-alist." 
                              (nth 1 envtuple)))
                    
                    (t (add-to-list 'cdlatex-env-alist
                                    (list                           
                                     (car envtuple)                           
                                     (concat
                                      "\\begin{"                           
                                      (nth 0 envtuple)                           
                                      "}\n  AUTOLABEL\n  ?\n\\end{"                            
                                      (nth 0 envtuple)
                                      "}\n")
                                     nil)))))
          inputlist))



(defun d-lua-add-cdlatex-command-list (inputlist)
  "Adds latex-environments to cdlatex-command-alist."
  (mapcar #'(lambda (envtuple)
              (cond ((some #'identity
                           (mapcar
                            #'(lambda
                                (cdlatexcmd)                             
                                (string=
                                 (nth 1 envtuple)
                                 (nth 0 cdlatexcmd)))
                            cdlatex-command-alist))
                     (message "%s is already defined in cdlatex-command-alist." (car envtuple)))
                    ((some #'identity
                           (mapcar
                            #'(lambda
                                (cdlatexcmd)
                                (string-match-p
                                 (nth 1 envtuple)
                                 (car (nth 4 cdlatexcmd))
                                 ))
                            cdlatex-command-alist))
                     (message "%s is already taken in cdlatex-command-alist." (nth 1 envtuple)))
                    (t (add-to-list 'cdlatex-command-alist
                                    (list                           
                                     (nth 1 envtuple)                           
                                     (concat
                                      "Insert "                           
                                      (nth 0 envtuple)                           
                                      " env")
                                     '""
                                     'cdlatex-environment
                                     (list (nth 0 envtuple))
                                     t
                                     nil)))))
          inputlist))


(defun d-lua-add-my-auctex-environments (inputlist)
  "Adds latex-environments to AucTeX environments."
  (interactive)
  (mapcar
   #'(lambda (envtuple)
       (unless (or (mapcar (lambda (environment)
                             (equal (car environment) (car envtuple)))
                           LaTeX-environment-list))
         (LaTeX-add-environments
          (list (nth 0 envtuple) 'LaTeX-env-label))))
   inputlist))


(defun d-lua-add-reftex-envs (inputlist)
  "Adds latex-environments to reftex-label-alist."
  (if (not (boundp 'reftex-label-alist))
      (setq reftex-label-alist (list nil)))
  (mapcar #'(lambda (envtuple)
              (cond ((and (boundp 'reftexenv)
                          (some #'identity
                                (mapcar
                                 #'(lambda
                                     (reftexenv)                             
                                     (string=
                                      (nth 0 envtuple)
                                      (nth 0 reftexenv)
                                      ))
                                 reftex-label-alist)))
                     (message "%s is already defined in reftex-label-alist." (car envtuple)))
                    
                    ((and (boundp 'reftexenv)
                          (some #'identity
                                (mapcar
                                 #'(lambda
                                     (reftexenv)
                                     (if (nth 2 reftexenv)
                                         (string-match-p
                                          (nth 1 envtuple)
                                          (nth 2 reftexenv)
                                          )))
                                 reftex-label-alist)))
                     (message "%s is already taken in reftex-label-alist." (nth 1 envtuple)))
                    
                    ((and (boundp 'reftexenv)
                          (>= (length envtuple) 3)
                          (not (identity (nth 3 envtuple)))
                          (some #'identity
                                (mapcar #'(lambda (reftexenv)
                                            (char-equal
                                             (nth 2 envtuple)
                                             (nth 1 reftexenv)))
                                        reftex-label-alist)))
                     (message "%s is already taken in reftex-label-alist." (nth 2 envtuple)))

                    (t (add-to-list 'reftex-label-alist
                                    (list                           
                                     (nth 0 envtuple)
                                     (nth 2 envtuple) 
                                     (concat (nth 1 envtuple) ":")
                                     '"~\\ref{%s}"
                                     nil
                                     (list
                                      (nth 0 envtuple)
                                      (concat
                                       (nth 0 envtuple)
                                       '".")))))))
          inputlist))

(defun d-lua-add (inputlist)
  "This function adds environments given to it in the form of a list of tuples
of an environment name and an abbreviation to the CDLaTeX, AUCTeX, RefTeX
lists."
  ;;  (setq reftex-label-alist nil)
  (d-lua-add-cdlatex-envs inputlist)
  (d-lua-add-cdlatex-command-list inputlist)
  (d-lua-add-my-auctex-environments inputlist)
  (d-lua-add-reftex-envs inputlist))

(provide 'd-lua-functions)
;;; d-lua-functions ends here
