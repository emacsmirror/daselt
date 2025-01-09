;;; d-emacs-eval-icicles-function.el --- d-emacs-functions for icicles-functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Alexander Prähauser

;; Author: Alexander Prähauser <ahprae@protonmail.com>
;; Keywords: tools

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

;; This file contains functions for functions used in icicles. If daselt-icicles is t, it is parsed automatically when daselt-mode is started or icicles is evaluated, depending on what comes first. Each element in this file should be a function or macro definition.

;;; Code:


(defun d-emacs-icicles--set-key-customs-from-bindlists ()
  "Set icicles customs for keys according to their entries in d-emacs bindlists
in this directory that contain `customs' in their name."
  (d--act-on-pkg-files-by-type-and-maybe-kill
   `(((lambda (filepath)
        (d--act-on-bindlists-in-file
         filepath
         (lambda ()
           (let* ((blist (d--extract-bindlist))
                  (head (d-head-if-exists blist))
                  (body (if head (cdr blist) blist)))
             (if head
                 (let ((bupsymb (intern (concat "d-emacs-" (symbol-name
                                                            head)
                                                "-backup"))))
                   (unless (bound-and-true-p bupsymb)
                     (setq bupsymb head))
                   (setopt--set
                    head
                    (append
                     (d-emacs-filter-list (symbol-value head)
                                          (lambda (ici) (let ((icikey (car ici)))
                                                          (or (symbolp icikey)
                                                              (string-match-p
                                                               "mouse" (key-description
                                                                        icikey))))))
                     (mapcar
                      (lambda (bind)
                        (let ((val (eval (cdr bind))))
                          (cons
                           (kbd (d--extract-binding-string bind t t))
                           (if (atom val)
                               (list val t)
                             val))))
                      body))))
               (mapcar (lambda (bind)
                         (let* ((cust (eval (cdr bind)))
                                (bindstr (d--extract-binding-string bind t t))
                                (bindkbd (kbd bindstr))
                                (custbup (intern (concat "d-emacs-" (symbol-name
                                                                     cust)
                                                         "-backup"))))
                           (if (bound-and-true-p custbup)
                               (add-to-list cust bindkbd)
                             (progn (set custbup cust)
                                    (set cust (list bindkbd))))))
                       body))
             ))))
      . (lambda (filepath) (and (d--bindlists-p filepath)
                                (string-match-p "customs" (file-name-base filepath))))))
   (concat "d-emacs/icicles/")))

(defun d-emacs-icicles--reset-key-customs-from-backups ()
  "Reset icicles customs for keys to their backed-up values."
  (d-recurse-through-directory
   (lambda (filepath) (d--act-on-bindlists-in-file
                  filepath
                  (lambda () (let* ((blist (d--extract-bindlist))
                               (icilist (d-head-if-exists blist))
                               (body (if icilist (cdr blist) blist)))
                          (if icilist
                              (let ((bupsymb (intern (concat "d-emacs-" (symbol-name icilist) "-backup"))))
                                (if (boundp bupsymb)
                                    (setopt--set icilist (symbol-value bupsymb))))
                            (mapcar (lambda (bind)
                                      (let* ((cust (cdr bind))
                                             (custbup (intern (concat "d-emacs-" (symbol-name cust) "-backup"))))
                                        (if (boundp custbup)
                                            (setopt--set cust (symbol-value (symbol-value custbup))))))
                                    body))))))
   (concat d-emacs-directory "pkg-configs/d-emacs/icicles/")
   (lambda (filepath) (and (d--bindlists-p filepath)
                      (string-match-p "customs" (file-name-base filepath))))))

(provide 'd-emacs-eval-icicles-functions)
;;; d-emacs-eval-icicles-function.el ends here
