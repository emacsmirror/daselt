;;; d-emacs-coords.el --- Tools for the coordinatization and drawing of layouts  -*- lexical-binding: t; -*-

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

;; 

;;; Code:

;;;; Preamble

;;;; Customs
(defgroup d-emacs-coords
  nil
  "Customization group for d-emacs-coords."
  :group 'Daselt
  :prefix "d-emacs-coords-")

(define-widget 'coords 'lazy
  "A list of coordinate numbers."
  :offset 4
  :tag "Coords"
  :type '(repeat number))

(define-widget 'prefix-coords-pair 'lazy
  "A list of coordinate numbers."
  :offset 4
  :tag "Coords"
  :type '(cons string coords))

(defcustom d-emacs-coords-abs-mid
  '(1 4.5)
  "Absolute coordinates of the midpoint of the d-xkb-layouts.
First coordinate is the number of the row counted downward. Second coordinate is
the number of the key counted rightward. If midpoint is between two rows or
keys, use a floating point number between the key numbers."
  :type 'coords
  :group 'd-emacs-coords)

(defcustom d-emacs-coords-layer-shifts-list
                    '((0 1 1))
                    "Shifts of individual layers in coordinates.
Each entry of the list should be a list consisting of three coordinates.

The first coordinate is the absolute coordinate of the layer, i.e. its position in
`d-emacs-coords-layer-list'.

The second coordinate is the row shift, meaning the number of rows that have to
be added or removed from the row coordinate of the midpoint in
`d-emacs-coords-abs-mid' in that particular layer. For instance, Daselt's
midpoint is generally in the second row, but in layer 0 a further row has to be
added on top, so 1 has to be added to the row coordinate of
`d-emacs-coords-abs-mid' for calculations in that layer.

The second coordinate is the column midpoint shift and works similarly to the
row midpoint shift. For instance, in Daselt's layer 0 the outer left row of keys
is used, while it is unused in other layers. Therefore 1 has to be added to the
column coordinate. Therefore, the layer shift coordinates for Daselt layer 0 are
\(0 1 1).

If no shift is necessary, it doesn't have to be specified."
                      :type 'coords
                      :group 'd-emacs-coords)

(defcustom d-emacs-coords-row-shifts-list
  '((2 0.5))
  "List of row shifts in absolute coordinates.

Each entry should consist of two numbers. The first is the absolute coordinate
of the row in an unshifted layer, meaning a layer without an entry in
`d-emacs-coords-layer-shifts-list'.

The second number is the column shift, meaning the number that has to be added
or subtracted from the column coordinate of `d-emacs-coords-abs-mid'.

For instance, Daselt's layout accounts for the shift of the lower key row on
standard keyboards by shifting that row by 0.5 and inserting formal places
around the middle key using `d-emacs-coords-formal-places-list', thus it has a
row shift of (2 0.5)."
  :type '(repeat coords)
  :group 'd-emacs-coords)

(defcustom d-emacs-coords-layer-row-shifts-list
  '((0 0 0.5))
  "Shifts for rows in particular layers.
This constant works similar to `d-emacs-coords-row-shifts-list'.

Generally it is better to use the shifts in `d-emacs-coords-row-shifts-list'
and `d-emacs-coords-layer-shifts-list'.

Setting shifts using this constant is only necessary for shifts of rows that
are not in all layers.

For instance, in Daselt's layout, the number row is only in layer 0. Since
row shifts have to be given in absolute coordinates, shifts in this row cannot
be specified in `d-emacs-coords-row-shifts-list'. Instead they have to be 
specified here.

Each element should be a three-coordinate list. The first coordinate is the layer.
The second coordinate is the row. The third is the row shift.
These should all be given in absolute coordinates.
"
  :type '(repeat coords)
  :group 'd-emacs-coords)

(defcustom d-emacs-coords-formal-places-list
  '((1 -1) (1 1) (-2 -1) (-2 1))
  "Relative coordinates of formal positions in the d-xkb-layouts.
These are positions that do not correspond to keys and are used to align rows
with each other when converting between absolute and relative coordinates."
  :type '(repeat coords)
  :group 'd-emacs-coords)

(defcustom d-emacs-coords-layer-list
  '(0 1 2 3 7 5 6 4 8)
  "Layers in d-emacs-coords-layouts.
The index of a number is supposed to be its level in the xkb-layout, the number
value the relative layer coordinate.
Layer 0 is not supposed to be included."
  :type '(repeat 'natnum)
  :group 'd-emacs-coords)

(defconst d-emacs-coords-layer-numbers-list
  (d-cardinal (length d-emacs-coords-layer-list))
  "Number of layers in d-emacs-coords-layouts.")

(defcustom d-emacs-coords-bad-combinations-list
  nil
  "A list of key combinations that do not work on your main keyboard(s).

Some key combinations may not be registered on specific keyboards. Which ones
varies by model.

When `d-emacs--apply-binding' is executed, it checks each combination in this
list. If a combination is found, a variant binding is created in which

- the C-modifier is replaced with an A-modifier.

- the H-modifier is replaced by an s-M-modifier.

Key combinations should be specified using conses of prefixes and relative
Daselt coordinates."
  :type 'prefix-coords-pair
  :group 'd-emacs-coords)

;;;; Coordinates
;;;;; Transformations
(defun d-emacs-coords-abs-to-rel (coords)
  "Transform absolute coordinate COORDS of a key into relative ones."
  (let* ((layer (nth 0 coords))
         (row (nth 1 coords))
         (col (nth 2 coords))
         (absmid d-emacs-coords-abs-mid)
         (absmidrow (nth 0 absmid))
         (absmidcol (nth 1 absmid))
         (layershift (alist-get layer d-emacs-coords-layer-shifts-list '(0 0)))
         (layerallrowsshift (nth 0 layershift))
         (layerallcolsshift (nth 1 layershift))
         (layermid (list (+ absmidrow layerallrowsshift)
                         (+ absmidcol layerallcolsshift)))
         (layermidrow (nth 0 layermid))
         (layermidcol (nth 1 layermid))
         (shiftedrowshifts (mapcar (lambda (rowshift)
                                     (list (+ layerallrowsshift (nth 0 rowshift))
                                           (nth 1 rowshift)))
                                   d-emacs-coords-row-shifts-list))
         (layerrowshift (or (nth 2 (car (d-filter-by-predicate
                                         d-emacs-coords-layer-row-shifts-list
                                         (lambda (shift) (and (= layer (nth 0 shift))
                                                         (= row (nth 1 shift)))))))
                            0))
         (rowshift (+ (car (alist-get row shiftedrowshifts '(0))) layerrowshift))
         (rowmid (+ layermidcol rowshift))
         (layer (nth layer d-emacs-coords-layer-list))
         (row (d-roundout (- row layermidrow)))
         (col (d-roundout (- col rowmid)))
         ;; We still have to account for formal places and midpoints between two keys.
         (colposp (natnump col))
         (formplacesfrommidtocol
          (d-filter-by-predicate d-emacs-coords-formal-places-list
                                 (lambda (coords)
                                   (let* ((formrow (car coords))
                                          (formcol (nth 1 coords))
                                          (formcolposp (natnump formcol)))
                                     (and (= row formrow)
                                          (if colposp
                                              (and formcolposp
                                                   (<= formcol col))
                                            (and (not formcolposp)
                                                 (>= formcol col))))))))
         (formplacenumtocol (length formplacesfrommidtocol))
         (col (if colposp
                  (+ col formplacenumtocol)
                (- col formplacenumtocol))))
    (list layer row col)))

(defun d-emacs-coords-rel-to-abs (coords)
  "Transform relative coordinate COORDS of a key into absolute ones."
  (let* ((layer (nth 0 coords))
         (row (nth 1 coords))
         (place (nth 2 coords))
         (layer (cl-position layer d-emacs-coords-layer-list))
         (layershift (alist-get layer d-emacs-coords-layer-shifts-list '(0 0)))
         (layerallrowsshift (nth 0 layershift))
         (layerallcolsshift (nth 1 layershift))
         (layermid (list (+ absmidrow layerallrowsshift)
                         (+ absmidcol layerallcolsshift)))
         (layermidrow (nth 0 layermid))
         (layermidcol (nth 1 layermid))
         (row (truncate (+ row layermidrow)))
         (layerrowshift (or (nth 2 (car (d-filter-by-predicate
                                         d-emacs-coords-layer-row-shifts-list
                                         (lambda (shift) (and (= layer (nth 0 shift))
                                                         (= row (nth 1 shift)))))))
                            0))
         (shiftedrowshifts (mapcar (lambda (rowshift)
                                     (list (+ layerallrowsshift (nth 0 rowshift))
                                           (nth 1 rowshift)))
                                   d-emacs-coords-row-shifts-list))
         (rowshift (+ (car (alist-get row shiftedrowshifts '(0))) layerrowshift))
         (rowmid (+ layermidcol rowshift))
         (placeposp (natnump place))
         (formplacestomid (d-filter-by-predicate
                           d-emacs-coords-formal-places-list
                           (lambda (coords)
                             (let* ((formrow (car coords))
                                    (formplace (nth 1 coords))
                                    (formplaceposp (natnump formplace)))
                               (and (= row formrow)
                                    (if placeposp
                                        (and formplaceposp
                                             (<= formplaceposp placeposp))
                                      (and (not formplaceposp)
                                           (<= place formplaceposp))))))))
         (formplacenum (length formplacestomid))
         (place (- (+ place rowmid) formplacenum)))
    (list layer row place)))

;;;;; Coordinatization
(defun d-emacs-coords-coordinatize-layout (layout)
      "Add relative coordinates to every element of the d-emacs-coords-layout LAYOUT.
Resulting layout places are pairs of coordinates and their corresponding key
symbol."
      (mapcar (lambda (laynum)
                (mapcar (lambda (indrow)
                          (mapcar (lambda (indsymb)
                                    (cons (d-emacs-coords-abs-to-rel
                                       (list laynum
                                             (car indrow)
                                             (car indsymb)))
                                      (cdr indsymb)))
                              (d-add-list-indices (cdr indrow))))
                    (d-add-list-indices (nth laynum layout))))
          d-emacs-coords-layer-numbers-list))

;;;;; Layers
(defun d-emacs-coords-get-layer (coordlayout laynum)
  "Get the layer LAYNUM from a coordinatized layout COORDLAYOUT.
Remove the layer coordinate from each coordinate list."
  (mapcar (lambda (row)
            (mapcar (lambda (place)
                      (let ((coords (car place)))
                        (cons (cl-subseq coords 0 (1- (length coords))) (cdr place))))
                    row))
          (alist-get laynum (mapcar (lambda (layer)
                                      (cons (caaaar layer) layer))
                                    coordlayout))))

;;;;; Predicates
(defun d-emacs-coords-p (list)
  "Return t if LIST is a valid d-emacs-coords coordinate list.
This means it consists entirely of numbers."
  (and (proper-list-p list)
       (d-forall-p list #'numberp)))

;;;;; Bindings
(defun d-emacs-coords-binding (coords &optional extlayout wholebinds)
  "Retrieve the value of a layout from COORDS.
Use EXTENDED LAYOUT or `d-emacs-xkb-extended-layout' by default. COORDS should
contain three numbers:

- the layer number.

- the row number, where the middle row is numbered 0 and upper rows are
negative.

- the relative position of the key with respect to the central axis. The B-key
in a QWERTY keyboard is numbered 0, the keys next to it -2 and 2.

The 0-th layer is the value of the symbol bound to `d-emacs-xkb-layer-0', which is not given by scanning the `d-emacs-xkb-file' but has to be provided. It is intended as a layer containing double-bindings (i.e. using `dual-function-keys'). It is assumed that double bindings in this layer are separated by slashes `/'. This is why, by default, `d-emacs-coords-binding' splits the returned string using `/'. To disable this behavior, set WHOLECOORDS to t."
  (let* ((abscoords (d-emacs-coords-rel-to-abs coords t))
         (extlayout (if extlayout extlayout (symbol-value d-emacs-xkb-extended-layout)))
         (unsplitbind (nth (nth 2 abscoords)
                           (nth (nth 1 abscoords)
                                (nth (nth 0 abscoords) extlayout))))
         (lastbindpart (car (last (split-string unsplitbind "/"))))
         (bind (if (and (not wholebinds)
                        (d-string-exists-and-nonempty lastbindpart))
                   lastbindpart
                 unsplitbind)))
    bind))

;;;;; Other
(defun d-emacs-coords-concat (coords)
  "Concatenate a list of coordinates COORDS into a string."
  (if (= (length coords) 1)
      (number-to-string (car coords))
    (let ((runstr ""))
      (cl-loop for coord in (reverse (cdr coords)) ; We shouldn't have a - after the last coord
               do (setq runstr (concat runstr (number-to-string coord) "-"))
               finally (setq runstr (concat runstr (number-to-string (car coords))))
               finally return runstr))))

;;;; Placevals
;;;;; Regexp matching
(defun d-emacs-coords-placevals-matching-coordrx (placevals coordrx)
  "Retrieve all place values in PLACEVALS whose coordinates match COORDRX."
  (d-filter-by-predicate placevals (lambda (placeval)
                                         (let ((coords (car placeval)))
                                       (string-match-p coordrx (format "%s" coords))))))

(defun d-emacs-coords-placevals-matching-indexed-rx (placevals idx coordrx)
  "Retrieve place values among PLACEVALS whose IDX-th coordinate matches COORDRX."
  (let* ((testcoords (caar placevals))
         (coordcard (length testcoords))
         (runrx "")
         (finalrx (if (= coordcard 1)
                                  coordrx
                          (cl-loop for runidx from 0 to (1- coordcard)
                             do (setq runrx (concat runrx
                                                    (if (= idx runidx)
                                                                    coordrx
                                                            "[^ ]*")
                                                    (unless (= runidx (1- coordcard))
                                                      " ")))
                             finally return runrx))))
    (if d-debug (message "Unindexed rx: %s" finalrx))
    (d-emacs-coords-placevals-matching-coordrx
     placevals
     finalrx)))

;;;;; Index manipulation
(defun d-emacs-coords-remove-index-from-placevals (placevals idx)
  "Remove IDX from the coordinates of each placeval in PLACEVALS.
Return the modified list."
  (mapcar (lambda (placeval)
              (let ((coords (car placeval))
                  (val (cdr placeval)))
              (cons (d-remove-list-index coords idx)
                    val)))
          placevals))

(defun d-emacs-coords-find-coord-values-at-index (placevals idx)
  "Find all values the coordinates PLACEVALS have at IDX."
  (mapcar (lambda (placeval)
                  (let ((coords (car placeval)))
              (nth idx coords)))
          placevals))


;;;;; Extraction
(defun d-emacs-coords-extract-value-string (val)
  "Extract the string corresponding to the value VAL of a placevalue."
  (let* ((valname (if (symbolp val)
                      (symbol-name val)))
         (valval (unless (or (stringp val) valname)
                   ;; Proceed if there's an error.
                   (condition-case nil (eval val)
                     (error (lambda ())))))
         (valvalname (if (symbolp valval)
                         (symbol-name valval)))
         (valstr (cond
                  ((stringp val) val)
                  (valname valname)
                  ((stringp valval) valval)
                  (valvalname valvalname)
                  (t ""))))
    valstr))

;;;;; Drawing
(defun d-emacs-coords-draw-placevals (placevals &optional bounds runcoords org)
  "Draw a keyboard layout from PLACEVALS.
PLACEVALS is a list of cons cells containing coordinates and a value.

Each PLACEVAL is a cons cell where the car is a pair of coordinates and the cdr
is a value that can be inserted or evaluated into an insertable form.

If BOUNDS is given, it should be be a cons of two conses, specifying the minimum
and maximum number of rows and columns in the layout. Otherwise, the boundaries
are calculated using the given placevals.

RUNCOORDS is used for recursive calls and typically should not be manually
specified.

If ORG is non-nil, output is formatted as an org-mode table."
  (if placevals
      (let* (;; Test for the number of coordinates.
             (testcoords (caar placevals))
             (coordcard (length testcoords)))

        ;; Recurse for higher dimensions.
        (if (>= coordcard 3)
            (mapcar (lambda (firstcoord)
                      (let ((runcoords (append runcoords (list firstcoord)))) ; Redefine RUNCOORDS to include the coordinate that is recursed upon.
                        (insert (format "\n%s\n" (d-emacs-coords-concat runcoords)))

                        (progn (d-emacs-coords-draw-placevals (d-emacs-coords-remove-index-from-placevals (d-emacs-coords-placevals-matching-indexed-rx placevals 0 (number-to-string firstcoord))
                                                                                                          0)
                                                              bounds runcoords org)
                               (insert "\n"))))
                    
                    (cl-remove-duplicates (d-emacs-coords-find-coord-values-at-index placevals 0)))

          (let* ((coordlist (mapcar (lambda (placeval)
                                      (car placeval))
                                    placevals))

                 ;; Use the last coordinate to calculate columns.
                 (colcoords (unless bounds ; Unneccessary if BOUNDS.
                              (mapcar (lambda (coords)
                                        (car (last coords)))
                                      coordlist)))
                 (firstcol (if bounds
                               (cadr bounds)
                             (apply #'min colcoords)))
                 (lastcol (if bounds
                              (cddr bounds)
                            (apply #'max colcoords)))
                 (colnum (1+ (- lastcol firstcol)))

                 ;; Similar for rows.
                 (rowcoords (unless bounds
                              (if (= 2 coordcard)
                                  (mapcar (lambda (coords)
                                            (car coords))
                                          coordlist)
                                1)))
                 (firstrow (if bounds
                               (caar bounds)
                             (apply #'min rowcoords)))
                 (lastrow (if bounds
                              (cdar bounds)
                            (apply #'max rowcoords)))
                 (rownum (1+ (- lastrow firstrow))))

            (let ((print-level nil)
                  (print-length nil))

              (if org
                  (d-emacs-coords-draw-org-table colnum rownum firstrow lastrow firstcol lastcol placevals)
                (d-emacs-coords-draw-table colnum rownum firstrow lastrow firstcol lastcol placevals))))

          (goto-char (point-max))))))

;;;;;;; Auxiliary
(defun d-emacs-coords-draw-table (colnum rownum firstrow lastrow firstcol lastcol placevals)
  "Draw a daselt-table using table.el.
An auxiliary function to `d-emacs-coords-draw-placevals'. See there for more
documentation."
  (table-insert colnum rownum nil 1)
  (cl-loop for row from firstrow to lastrow
           do (cl-loop for place from firstcol to lastcol
                       do (table-with-cache-buffer
	                    (insert
                             (let* ((val (alist-get (list row place)
                                                    placevals
                                                    ""
                                                    nil
                                                    #'equal))
                                    (valstr (d-emacs-coords-extract-value-string val)))
                               (if (not (string-empty-p valstr))
                                   valstr
                                 (format "%s" val)))))
                       do (table-forward-cell 1))))

(defun d-emacs-coords-draw-org-table (colnum _rownum firstrow lastrow firstcol lastcol placevals)
  "Draw a daselt-table in org-mode.
An auxiliary function to `d-emacs-coords-draw-placevals'. See there for more
documentation."

  ;; (message "Drawing.")
  ;; Insert header for the Org table.
  (insert "|-\n")
  (insert "|")
  (dotimes (i colnum)
    (insert (format " %d |" (+ firstcol i))))
  (insert "|-\n")
  (insert "|-\n")

  ;; Insert rows for the Org table.
  (let ((pos (point)))
    (cl-loop for row from firstrow to lastrow
             do (progn
                  (insert "|")
                  (cl-loop for place from firstcol to lastcol
                           do (let* ((val (alist-get (list row place) placevals "" nil #'equal))
                                     (valstr (d-emacs-coords-extract-value-string val)))
                                (insert (if (not (string-empty-p valstr)) valstr " "))
                                (insert " |")) ; End of cell
                           )
                  (insert "\n|-\n")))
    (set-mark (point))
    (goto-char pos)
    (org-table-align)))

(defun d-emacs-coords-draw-placevals-in-temp-buffer (placevals &optional drawfull runcoords org)
  "Execute `d-emacs-coords-draw-placevals' in a maximized temporary buffer.
All arguments are forwarded to `d-emacs-coords-draw-placevals'. See there for more
documentation."
  (d-execute-in-maximized-maybe-temp-buffer
   "*daselt-layout*" (lambda () (d-emacs-coords-draw-placevals placevals drawfull runcoords org)
                       (org-mode))))

;;;;; Drawing Commands

(defun d-emacs-coords-draw-coordwise-from-coordrxlst (coordrx1 coordrx2 coordrx3)
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
                 #'d-emacs-coords-draw-placevals-in-temp-buffer
               #'d-emacs-coords-draw-placevals)
             placevals)))

(defun d-emacs-coords-draw-keyboard-layer (laynum &optional org)
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
               #'d-emacs-coords-draw-placevals-in-temp-buffer
             #'d-emacs-coords-draw-placevals)
           (apply #'append (d-emacs-coords-get-layer (d-emacs-coords-coordinatize-layout
                                                      (symbol-value d-emacs-xkb-extended-layout))
                                                     laynum))
           nil
           nil
           org))

(defun d-emacs-coords-draw-key-coordinates (&optional extt org)
  "Draw the coordinates of d-emacs-xkb-keys.
With a prefix argument, draw the coordinates of the 0-th key."
  (interactive "P")
  (let* ((coords (if extt
                     d-emacs-xkb-extended-key-coords
                   d-emacs-coords-key-coords))
         (flatcoords (d-flatten-until
                      coords
                      (lambda (lst)
                        (d-emacs-coords-p (car lst)))))
         (placevals (cl-mapcar (lambda (coords1 coords2)
                                 (cons coords1 (substring (format "%s" coords2) 1 -1)))
                               flatcoords flatcoords)))
    (d-emacs-coords-draw-placevals placevals nil nil org)))

(provide 'd-emacs-coords)
;;; d-emacs-coords.el ends here
