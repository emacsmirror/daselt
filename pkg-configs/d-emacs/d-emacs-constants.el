;;; d-emacs-constants.el --- Constants for Daselt's Emacs module              -*- lexical-binding: t; -*-

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

;;  d-emacs variables.

;;; Code

;; (defconst daselt-latex-math-modify-list
;;   (list "\\mo" "\\mc" "\\mathopen" "\\mathclose")
;;   "List of modifiers daselt considers in daselt-latex-concat-after-modify.")

(defconst d-emacs-pkgs-list
  (flatten-list
   (d-recurse-through-directory
    (concat d-emacs-directory "pkg-configs/d-emacs/")
    `(((lambda (filepath) (intern (file-name-base filepath)))
       .
       (lambda (idx lst) (file-directory-p (nth idx lst)))))))
  "List of all packages for which d-emacs-configurations exist.")

(defconst d-emacs-emacs-standardized-modifiers-list
  (list ?A ?C ?H ?M ?S ?s)
  "List of modifiers in the standard order used by Emacs.
This order differs from the Daselt order, which is specified in
d-modifiers-list.")

;;;; Lists of commands used in macros to create commands
(defconst d-emacs-backward-command-list
  '(sp-kill-hybrid-sexp sp-mark-sexp mark-sexp mark-defun transpose-paragraphs kill-paragraph transpose-sentences delete-char transpose-lines kill-line transpose-words transpose-sexps sp-down-sexp down-list transpose-chars other-frame sp-transpose-sexp sp-transpose-hybrid-sexp mark-line d-emacs-kill-defun delete-all-space delete-horizontal-space sp-up-sexp)
  "List of commands for which backward analogs are created in
  `d-emacs-commands'.")

(defconst d-emacs-new-buffer-command-list
  '(Info-prev Info-next)
  "List of commands for which analogs are created in `d-emacs-commands' that
  open a new buffer.")

(defconst d-emacs-subsentence-command-list
  '(backward-sentence forward-sentence backward-kill-sentence kill-sentence er/mark-sentence d-emacs-backward-er/mark-sentence d-emacs-backward-transpose-sentences)
  "List of commands for which subsentence analogs are created in
  `d-emacs-commands'.")

;;;; Lists for rebinding
(defconst d-emacs-no-shift-list
  '("'")
  "List of strings that should not be replaced by their downcased version with a
shift modifier when `d--extract-binding-string' is called with `csectoshft' set
to t.")

(with-eval-after-load 'd-stump-constants
  (with-eval-after-load 'd-emacs-customs
    (defconst d-emacs-replace-binding-strings-alist
      (remq nil (append (unless (or d-stump d-emacs-translate-C-1-1--2-C-g)
                          `(("C-g" . ,(d--extract-binding-string `(("C-" . (1 1 -2)))))))

                        (unless (or d-emacs-translate-keys
                                    d-emacs-do-not-replace-translate-keys)
                          (mapcar (lambda (cns)
                                    (let ((str (car cns)))
                                      (cons str (string-replace "C-" "A-" str))))
                                  d-emacs-key-translations-alist))))
      "Association list of binding strings and their replacements.
This list allows certain key bindings to be replaced, mostly
to avoid `C-g' from being bound to anything.")))

;;;; Other
(defconst d-emacs-docstring-functions-list
  '(defun defmacro defconst defcustom defun* defalias defgroup)
  "List of definition macros for which `d-emacs-beginning-of-docstring' works.")

(provide 'd-emacs-constants)
;;; d-emacs-constants ends here
