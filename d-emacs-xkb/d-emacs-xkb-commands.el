;;;  d-emacs-xkb-commands.el --- Commands for Daselt's xkb module              -*- lexical-binding: t; -*-

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

;;  d-emacs-xkb commands.

;;; Code:
;;;; Preamble
(declare-function d-emacs-coords-get-layer "d-emacs-xkb-functions" (coordlayout laynum))
(declare-function cl-mapcar "cl-lib" (arg1 arg2 &rest rest))
(declare-function d-emacs-coords-p "d-emacs-xkb-functions" (list))
(declare-function d-emacs-coords-binding "d-emacs-xkb-functions" (coords &optional extlayout))
(declare-function d-emacs-read-multiple-choice-base "d-emacs-functions" (prompt choices &optional help-string show-help long-form))
(declare-function d-emacs-coords-draw-placevals "d-emacs-xkb-functions" (placevals &optional drawfull runcoords org))
(declare-function d-emacs-coords-layer-list "d-emacs-xkb-functions" (placevals &optional drawfull runcoords org))
(declare-function d--binding-p "d-functions" (cns))
(declare-function d-emacs-coords-coordinatize-layout "d-emacs-xkb-functions" (layout &optional extt))
(declare-function d-flatten-until "d-functions" (lst cnd))
(declare-function d-emacs-coords-placevals-matching-coordrx "d-emacs-xkb-functions" (placevals coordrx))

(defvar d-emacs-coords-key-coords)
(defvar d-emacs-xkb-key-0-coords)
(defvar d-emacs-coords-layer-numbers-list)
(defvar d-emacs-xkb-extended-layout)

(defun d-emacs-coords-layer-list (coordrx1 coordrx2 coordrx3)
  "Draw layer matches based on three coordinate regular expressions.

COORDRX1, COORDRX2, and COORDRX3 are regular expressions that specify
which coordinates can take which values. When called interactively,
the user is prompted to enter three regular expressions.

- If one of the COORDRXs is a fixed value (a number), the other two
  are used as row and column specifiers, in that order.
- If two COORDRXs are fixed, matches for the third are drawn in a row.
- If all three COORDRXs are variable, layers containing matches are drawn
  sequentially.

The resulting matches are drawn either in the current buffer or a temporary
buffer, depending on the invocation context."
  (interactive (list (read-regexp "Number1 regexp: ")
                     (read-regexp "Number2 regexp: ")
                     (read-regexp "Number3 regexp: ")))

  (let* ((coordrxlst (list coordrx1 coordrx2 coordrx3))
         (coordrx (mapconcat #'identity coordrxlst " "))
         (placevals (d-emacs-coords-placevals-matching-coordrx
                     (d-flatten-until
                      (d-emacs-coords-coordinatize-layout
                       (symbol-value d-emacs-xkb-extended-layout)
                       t)
                      (lambda (lst) (d--binding-p (car lst))))
                     coordrx)))

    (funcall (if (called-interactively-p 'any)
                 #'d-emacs-xkb-draw-d-emacs-coords-ld-emacs-coords-layer-list-placevals)
             placevald-emacs-coords-layer-lists-layer-list (laynum &optional org)
  "Draw the layer of d-emacs-xkb-main-layout with LAYNUM.
If ORG is t, draw an org-table."
  (interactive (list (string-to-number
                      (nth 1 (d-emacs-read-multiple-choice-base
                              "Layer number: "
                              (append (list (list (string-to-char (d-emacs-coords-binding '(1 0 -2)))
                                                  "0")
                                            (list (string-to-char (d-emacs-coords-binding '(1 0 2)))
                                                  "4"))
                                      (mapcar (lambda (num)
                                                (list (string-to-char (format "%d" num))
                                                      (number-to-string num)))
                                              d-emacs-coords-layer-numbers-list)))))))
  (funcall (if (called-interactively-p 'any)
               #'d-emacs-xkb-draw-placevd-emacs-coords-layer-list-xkb-drd-emacs-coords-layer-listapply #'append (d-emacs-xkb-getd-emacs-coords-layer-listnatize-layout
                                             (symbol-value d-emacs-xkb-extended-layout)
                                             t)
                                            laynum))
           nil
           nil
           org))

  (defun d-emacs-coords-layer-list (&optional extt org)
    "Draw the coordinates of d-emacs-xkb-keys. 
With a prefix argument, draw the coordinates of the 0-th key."
    (interactive "P")
    (let* ((coords (if extt
                       d-emacs-xkb-key-0-coords
                     d-emacs-coords-key-coords))
           (flatcoords (d-flatten-until
                        coords
                        (lambda (lst)
                          (d-emacs-coords-p (car lst)))))
           (placevals (cl-mapcar (lambda (coords1 coords2)
                                   (cons coords1 (substring (format "%s" coords2) 1 -1)))
                                 flatcoords flatcoords)))
      (d-emacs-coords-draw-placevals placevals nil nil org)))

(provide 'd-emacs-xkb-comd-emacs-coords-layer-listmands.el ends here

