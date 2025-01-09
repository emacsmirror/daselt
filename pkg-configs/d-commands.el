;;; d-commands.el --- General commands for Daselt.   -*- lexical-binding: t; -*-

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

;; This file houses the general commands for Daselt.

;;; Code:

;; (eval-when-compile (require 'd-constants))
;; (eval-when-compile (require 'd-functions))
;; (eval-when-compile (require 'd-emacs-xkb-constants))
;; (eval-when-compile (require 'd-emacs-xkb-functions))
;; (eval-when-compile (require 'd-emacs-xkb-customs))

(declare-function d--pick-pkg-file-by-type "../d-functions.el"
                  (type &optional subdir nodefault))
(declare-function d--act-on-bindlists-in-file "../d-functions.el"
                  (filepath function &optional untangle))
(declare-function d--act-on-bindlists-in-file "../d-functions.el"
                  (filepath function &optional untangle))
(declare-function d--sort-and-format-marked-bindlist-string "../d-functions.el"
                  (&optional coordsonly prefun modlist))
(declare-function d--delete-duplicate-comment-lines "../d-functions.el"
                  ())
(declare-function d-emacs-filter-obarray "d-functions.el"
                  (predicate))
(declare-function d-emacs-filter-list "d-functions.el"
                  (lst predicate))
(declare-function d--pick-pkg-file-by-type "d-functions.el"
                  (type &optional subdir nodefault))
(declare-function d-emacs-powerlist "d-functions.el"
                  (list &optional elt))
(declare-function d-emacs-complement "d-functions.el"
                  (list1 list2 &optional compfun))
(declare-function d-emacs-coords-placevals-matching-indexed-rx "d-emacs-xkb/d-emacs-xkb-functions.el"
                  (placevals idx coordrx))
(declare-function d-emacs-coords-draw-placevals "d-functions.el"
                  (placevals &optional drawfull runcoords org))
(declare-function d-emacs-with-max-buffer-maybe-return "d-functions.el"
                  (buffername function))
(declare-function d--bindlist-symb-p "d-functions.el"
                  (symb))
(declare-function d-emacs-flatten-until "d-functions.el"
                  (lst cnd))
(declare-function d-emacs-coords-p "d-emacs-xkb/d-emacs-xkb-functions.el"
                  (list))
(declare-function d-string-together-modifiers "d-functions.el"
                  (modifiers))
(declare-function d-emacs-string-exists-and-nonempty "d-functions.el"
                  (str))
(declare-function d-emacs-filter-list "d-functions.el"
                  (lst predicate))
(declare-function d-emacs-coords-extract-value-string "d-emacs-xkb/d-emacs-xkb-functions.el"
                  (val))
(declare-function d-emacs-coords-placevals-matching-coordrx "d-emacs-xkb/d-emacs-xkb-functions.el"
                  (placevals rx))
(declare-function d-emacs-coords-binding "d-emacs-xkb/d-emacs-xkb-functions.el"
                  (coords))
(declare-function d--bindlist-p "d-functions.el"
                  (list))
(declare-function d--format-bindlist-into-string-before-insertion "d-functions.el"
                  (bindlist &optional coordsonly))
(declare-function d--extract-bindlist "d-functions.el"
                  (&optional noconstruct))
(declare-function d--save-bindlist-as-variable "d-functions.el"
                  (bindlist))
(declare-function d-emacs-read-region "d-functions.el"
                  ())

;;;; Bindlists-files
;;;; Navigation
(defun d-find-pkg-file-by-type (type &optional typemodifiers)
    "Pick a file of a Daselt-type TYPE with modifiers TYPEMODIFIERS and visit it.
The only difference to `find-file' is in the interactive completion, which asks
for a filetype in `d-pkg-file-types-list' and some type modifiers in
`d-pkg-type-modifiers-list', then displays all files of that type with those
modifiers."
    (interactive (let* ((type (completing-read "Main type: "
                                             d-pkg-file-types-list))
                      (typemodifiers (cl-loop for repl = (completing-read "Type modifier (empty to exit): "
                                                                          d-pkg-type-modifiers-list)
                                              while (not (string-empty-p repl))
                                              collect repl)))
                 (list type typemodifiers)))
    (let ((filepath (d--pick-pkg-file-by-type (cons type typemodifiers))))
    (find-file filepath)))

(defun d-find-bindlists-file ()
                                                      "Visit a bindlists file.
With a prefix argument, only regular bindlists files are considered."
                                                      (interactive)
                                                      (if current-prefix-arg
                                                                                                              (d-find-pkg-file-by-type "bindlists" '("regular"))
                                                        (d-find-pkg-file-by-type "bindlists" nil)))

;;;; Cleaning
(defun d-recursively-remove-nonstandard-files (&optional dir)
  "Remove files in subdirectories of DIR that are non-standard.
That means they are not directory and don't fulfill
`d--standard-file-p'.
The default for DIR is `d-emacs-pkg-configs-dir'."
  (interactive "DDirectory: ")
  (let ((dir (or dir d-emacs-pkg-configs-dir)))
    (d-recurse-through-directory dir
                                 `(((lambda (fn)
                                      (delete-file fn))
                                    . (lambda (idx lst) (let ((fn (nth idx lst)))
                                                     (string-match-p "newdoc" fn)))))
                                 nil
                                 nil
                                 t)))
;;;; Tutorial
(defun d-generate-tutorial ()
                            "Generate the Daselt-tutorial."
                            (interactive)
                            (let ((tutfile (concat d-emacs-directory "pkg-configs/d-tutorial.el"))
        (display-buffer-alist '((".*" display-buffer-full-frame))))
    (find-file tutfile)
    (goto-char (point-min))
    (search-forward ";;; Code:")
    (search-forward "\(")
    (backward-char)
    (mark-sexp)
    (let ((tuttext (eval (d-emacs-read-region))))
      (pop-to-buffer "*daselt-tutorial*")
      (delete-minibuffer-contents)
      (org-mode)
      (setq visual-line-mode t)
      (insert tuttext)
      (goto-char (point-min)))))


;;;; Provide
(provide 'd-commands)
;;; d-commands.el ends here
