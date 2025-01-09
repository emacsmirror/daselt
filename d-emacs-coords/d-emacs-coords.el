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

;;; Commentary:

;; `d-emacs-coords` is a comprehensive Emacs Lisp library designed to facilitate
;; the coordinatization and visualization of keyboard layouts. This package
;; provides a suite of tools for defining, transforming, and rendering
;; multi-layer keyboard configurations, making it ideal for users who seek to
;; customize their keyboard mappings or visualize complex key arrangements.

;; **Key Features:**

;; - **Coordinate Management:**  
;; - **Absolute and Relative Coordinates:** Convert between absolute coordinates
;; - (specific key positions within layers) and relative coordinates
;; - (positions relative to a defined midpoint), allowing for flexible layout
;; - definitions.
;; - **Layer Shifts:** Define and manage shifts for entire layers or individual
;; - rows, accommodating variations across different keyboard layers.

;; - **Layer Handling:**  
;; - **Multi-Layer Support:** Manage multiple keyboard layers, each with its own
;; - set of coordinate shifts and configurations.
;; - **Bad Combinations Filtering:** Specify key combinations that may not be
;; - supported by certain keyboards, ensuring compatibility and preventing
;; - conflicts.

;; - **Layout Visualization:**  
;; - **Drawing Functions:** Render keyboard layouts as Emacs tables or Org-mode
;; - tables, providing a clear and organized view of key arrangements.
;; - **Dynamic Rendering:** Generate visual representations based on coordinate
;; - regular expressions or specific layer selections, enabling users to focus
;; - on particular aspects of their layout.

;; - **Binding Retrieval and Manipulation:**  
;; - **Key Binding Access:** Retrieve the binding associated with specific
;; - coordinates, facilitating the customization of key behaviors.
;; - **Layout Generation:** Automatically generate keyboard layouts from a
;; - collection of place values, streamlining the process of creating and
;; - modifying layouts.

;; - **Customization and Extensibility:**  
;; - **Custom Groups and Widgets:** Define customizable groups and widgets for
;; - fine-tuning layout parameters, ensuring that the package can be tailored
;; - to individual preferences and requirements.
;; - **Integration with Existing Layouts:** Seamlessly integrate with existing
;; - `d-emacs-dfk-layout` or `d-emacs-xkb-layout` configurations, providing
;; - flexibility for users with predefined layouts.

;; **Usage Examples:**

;; - **Defining a Layout:**  
;; Customize the `d-emacs-coords-abs-mid` and `d-emacs-coords-layer-shifts-list`
;; variables to set the midpoint and layer-specific shifts for your keyboard
;; layout.

;; - **Visualizing a Layer:** Use `d-emacs-coords-draw-keyboard-layer` to render
;; a specific layer of your keyboard layout in an Emacs buffer, either as a
;; standard table or an Org-mode table for enhanced readability.

;; - **Use in Bindlists:** `d-emacs-coords' provides the base for
;; `d-emacs-binds', which provides utilities to define bindings by positions in
;; the layout (independently of the symbols they house).

;; - **D-Emacs Mode:** This package is part of the Daselt-project
;; (https://gitlab.com/nameiwillforget/daselt) and provides necessary
;; prerequisites for Daselt's Emacs implementation d-emacs.

;; `d-emacs-coords` empowers Emacs users to meticulously design and visualize
;; their keyboard layouts, offering granular control over key positioning and
;; layer management. Whether you're crafting a custom ergonomic setup or
;; experimenting with multi-layer configurations, this package provides the
;; necessary tools to achieve a tailored and efficient keyboard experience.


;;; Code:

;;;; Preamble
(require 'cl-macs)
(require 'table)
(require 'org-compat)
(require 'd-emacs-base)

(declare-function org-table-align "org-table" nil)
(declare-function d-execute-in-maximized-maybe-temp-buffer "d-emacs-base" (bufname fun))
(declare-function d-remove-list-index "d-emacs-base" (lst idx))
(declare-function d-filter-by-predicate "d-emacs-base" (lst pred))
(declare-function d-string-exists-and-nonempty "d-emacs-base" (str))
(declare-function d-forall-p "d-emacs-base" (list predicate))
(declare-function d-add-list-indices "d-emacs-base" (list &optional fromone))
(declare-function d-cardinal "d-emacs-base" (n &optional fromone))

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
  '((2 0.5) (3 -1.5))
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
  '((0 0 -0.5))
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
with each other when converting between absolute and relative coordinates.

If you have several formal places on the same side in the same row, please put
them in ascending order, otherwise the algorithm will get confused."
  :type '(repeat coords)
  :group 'd-emacs-coords)

(defcustom d-emacs-coords-layer-list
    '(0 1 2 3 4 5 6 7 8)
    "Layers in d-emacs-coords-layouts.
The index of a number is supposed to be its level in the xkb-layout, the number
value the relative layer coordinate.
Layer 0 is not supposed to be included."
    :type '(repeat 'natnum)
    :group 'd-emacs-coords)

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

;;;; Constants
(defconst d-emacs-coords-layer-numbers-list
  (d-cardinal (length d-emacs-coords-layer-list))
  "Number of layers in d-emacs-coords-layouts.")

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
         (formplacesinrow (d-filter-by-predicate d-emacs-coords-formal-places-list
                                                 (lambda (coords)
                                                                                               (let ((formrow (nth 0 coords)))
                                                     (= row formrow)))))
         (formplacesonside (d-filter-by-predicate formplacesinrow
                                                  (lambda (coords)
                                                                                                (let* ((formcol (nth 1 coords))
                                                           (formcolposp (natnump formcol)))
                                                      (equal formcolposp colposp)))))
         (col (cl-loop for formplace in (if colposp formplacesonside ; We have to go from 0
                                          (reverse formplacesonside))
                       do (let ((formcol (nth 1 formplace)))
                            (if (<= (abs formcol) (abs col))
                                                                                                                        (setq col (funcall (if colposp #'1+ #'1-)
                                                   col))
                              (cl-return col)))
                       finally return col)) ; We can ignore places outside the span between the column and 0.
         )
    (list layer row col)))

(defun d-emacs-coords--remove-formal-places (coords)
  "Return COL with the places between COL and the mid of ROW removed.

ROW and COL should be in relative coordinates."
  (if (d-emacs-coords-p coords)
      (if coords
          (let* ((col (car (last coords)))
                 (row (nth (- (length coords) 2) coords))
                 (colposp (natnump col))
                 (formplacesinrow (d-filter-by-predicate d-emacs-coords-formal-places-list
                                                         (lambda (coords)
                                                           (let ((formrow (nth 0 coords)))
                                                             (= row formrow)))))
                 (formplacesonside (d-filter-by-predicate formplacesinrow
                                                          (lambda (coords)
                                                            (let* ((formcol (nth 1 coords))
                                                                   (formcolposp (natnump formcol)))
                                                              (equal formcolposp colposp))))))
            (cl-loop for formplace in (if colposp (reverse formplacesonside) ; We have to go from col to 0 this time.
                                        formplacesonside)
                     do (let ((formcol (nth 1 formplace)))
                          (if (<= (abs formcol) (abs col))
                              (setq col (funcall (if colposp #'1- #'1+)
                                                 col))
                            (cl-return col)))
                     finally return col))
        nil)
    (error "Wrong-type argument d-emacs-coords-p")))

(defun d-emacs-coords-rel-to-abs (coords)
  "Transform relative coordinate COORDS of a key into absolute ones."
  (let* ((layer (nth 0 coords))
         (row (nth 1 coords))
         (col (d-emacs-coords--remove-formal-places coords))
         (absmid d-emacs-coords-abs-mid)
         (absmidrow (nth 0 absmid))
         (absmidcol (nth 1 absmid))
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
         (col
          (funcall (if (natnump col) #'floor #'ceiling)
                   (+ col rowmid))))
    (list layer row col)))

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

;;;;; Boundaries
(defun d-emacs-coords-boundaries (coordslist)
  "Calculate the boundaries of a COORDSLIST.

Returns two coords:

- The first is made from the minima of all coords in COORDSLIST.

- The second is made out of all maxima.

Assumes all coords in COORDSLIST have the same length."
  (d-list-to-cons (mapcar (lambda (extremum)
                            (mapcar (lambda (idx)
                                      (apply extremum (mapcar (lambda (coords)
                                                                (nth idx coords))
                                                              coordslist)))
                                    (d-cardinal (length (car coordslist)))))
                          (list #'min #'max))))

;;;;; Bindings
(defun d-emacs-coords-binding (coords &optional layout wholebinds)
  "Retrieve the value of LAYOUT at COORDS.

The default of LAYOUT is the symbol-value of `d-emacs-dfk-layout' or
`d-emacs-xkb-layout' if the first is undefined.

COORDS should contain three numbers:

- the layer number.

- the row number, where the middle row is numbered 0 and upper rows are
negative.

- the relative position of the key with respect to the central axis. The B-key
in a QWERTY keyboard is numbered 0, the keys next to it -2 and 2.

The 0-th layer is the value of the symbol bound to `d-emacs-xkb-layer-0', which is not given by scanning the `d-emacs-xkb-file' but has to be provided. It is intended as a layer containing double-bindings (i.e. using `dual-function-keys'). It is assumed that double bindings in this layer are separated by slashes `/'. This is why, by default, `d-emacs-coords-binding' splits the returned string using `/'. To disable this behavior, set WHOLECOORDS to t."
  (let* ((abscoords (d-emacs-coords-rel-to-abs coords))
         (layout (if layout layout (symbol-value (d-emacs-coords--dfk-or-xkb-layout))))
         (unsplitbind (nth (nth 2 abscoords)
                           (nth (nth 1 abscoords)
                                (nth (nth 0 abscoords) layout))))
         (lastbindpart (if unsplitbind (car (last (split-string unsplitbind "/")))))
         (bind (if (and (not wholebinds)
                        (d-string-exists-and-nonempty lastbindpart))
                   lastbindpart
                 unsplitbind)))
    bind))

;;;;; Function application
(defun d-emacs-coords-run-through (bounds fun &optional runcoords)
  "Iterate over each coordinate within a specified BOUNDS, applying a function FUN to each coordinate.

BOUNDS can be either:
- A cons cell (MIN . MAX) where MIN and MAX are coordinates (cons cells themselves) defining the rectangular region's opposite corners.
- A list of cons cells each containing a 'running coordinate' and its associated bounds for further iteration. This allows for recursive traversal through nested coordinate spaces.

Each coordinate is represented as a cons cell (X . Y), where X and Y are integers.

FUN is a function that will be applied to each coordinate tuple generated. The tuple will be passed as a single argument to FUN. If RUNCOORDS is provided, these coordinates will be prepended to each tuple before FUN is applied, effectively allowing additional context to be passed through the recursive iterations.

RUNCOORDS is an optional list of 'extra' coordinates that should be included with each coordinate tuple passed to FUN. This is useful for maintaining state or additional information across the recursive traversal. Initially, this should typically be left nil.

The traversal is depth-first, expanding across the X dimension before proceeding along the Y dimension within the defined bounds. Inside the iteration, if BOUNDS is defined as a list of cons cells (implying recursed traversal with additional contextual coordinates), each element's bounds are used for further recursive calls, with the 'running coordinate' appended to RUNCOORDS.

This function is particularly useful for processing or analyzing grid-like structures where an operation needs to be applied across a range of coordinates, optionally utilizing additional context carried through RUNCOORDS."
  (if (consp bounds)
      (if (and (d-emacs-coords-p (car bounds))
               (d-emacs-coords-p (cdr bounds)))
          (let ((minima (car bounds))
                (maxima (cdr bounds)))
            (if (and minima maxima)
                (mapcar
                 (lambda (rightmost)
                   (d-emacs-coords-run-through (cons (cdr minima)
                                                     (cdr maxima))
                                               fun
                                               (append runcoords (list rightmost))))
                 (d-numbers-between (car minima) (car maxima)))
              (funcall fun runcoords)))
        (mapcar (lambda (bboundscons)
                  (let ((runcoord (car bboundscons))
                        (bbounds (cdr bboundscons)))
                    (d-emacs-coords-run-through bbounds fun (append runcoords
                                                                    (list runcoord)))))
                bounds))
    (error "Ill-formatted bounds %s" bounds)))

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

(defun d-emacs-coords--dfk-or-xkb-layout ()
  "Return the value of the symbol in `d-emacs-dfk-layout'.
If it is undefined, use `d-emacs-xkb-layout' instead."
  (if (and (symbolp 'd-emacs-dfk-layout)
           (boundp'd-emacs-dfk-layout))
      d-emacs-dfk-layout
    (if (and (symbolp 'd-emacs-xkb-layout)
             (boundp'd-emacs-xkb-layout))
        d-emacs-xkb-layout
      (error "No layout defined"))))
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
                        (progn (d-emacs-coords-draw-placevals
                                (d-emacs-coords-remove-index-from-placevals
                                 (d-emacs-coords-placevals-matching-indexed-rx
                                  placevals 0 (number-to-string firstcoord))
                                 0)
                                bounds runcoords org)
                               (insert "\n"))))
                    (cl-remove-duplicates (d-emacs-coords-find-coord-values-at-index placevals 0)))

          (let* ((coordslist (mapcar #'car placevals))
                 (bounds (or bounds (d-emacs-coords-boundaries coordslist)))
                 (firstrow (caar bounds))
                 (firstcol (nth 1 (car bounds)))
                 (lastrow (cadr bounds))
                 (lastcol (nth 1 (cdr bounds)))
                 (colnum (1+ (- lastcol firstcol)))
                 (rownum (1+ (- lastrow firstrow)))
                 (print-level nil)
                 (print-length nil))
            (funcall (if org
                         #'d-emacs-coords-draw-org-table
                       #'d-emacs-coords-draw-table)
                     colnum rownum firstrow lastrow firstcol lastcol placevals))
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

(defun d-emacs-coords-draw-placevals-in-temp-buffer (placevals &optional bounds runcoords org)
  "Execute `d-emacs-coords-draw-placevals' in a maximized temporary buffer.
All arguments are forwarded to `d-emacs-coords-draw-placevals'. See there for more
documentation."
  (d-execute-in-maximized-maybe-temp-buffer
   "*daselt-layout*" (lambda () (d-emacs-coords-draw-placevals placevals bounds runcoords org)
                             (org-mode))))

;;;;;; Drawing Commands

(defun d-emacs-coords-draw-coordwise-from-coordrxlst (coordrx1 coordrx2 coordrx3 &optional layout)
  "Draw layer matches in LAYOUT based on three coordinate regular expressions.

COORDRX1, COORDRX2, and COORDRX3 are regular expressions that specify
which coordinates can take which values. When called interactively,
the user is prompted to enter three regular expressions.

- If one of the COORDRXs is a fixed value (a number), the other two
  are used as row and column specifiers, in that order.
- If two COORDRXs are fixed, matches for the third are drawn in a row.
- If all three COORDRXs are variable, layers containing matches are drawn
  sequentially.

The default for LAYOUT is the symbol-value of `d-emacs-dfk-layout' or
`d-emacs-xkb-layout' if that is undefined.

The resulting matches are drawn either in the current buffer or a temporary
buffer, depending on the invocation context."
  (interactive (list (read-regexp "Number1 regexp: ")
                     (read-regexp "Number2 regexp: ")
                     (read-regexp "Number3 regexp: ")))

  (let* ((layout (or layout (symbol-value (d-emacs-coords--dfk-or-xkb-layout))))
         (coordrxlst (list coordrx1 coordrx2 coordrx3))
         (coordrx (mapconcat #'identity coordrxlst " "))
         (placevals (d-emacs-coords-placevals-matching-coordrx
                     (d-flatten-until
                      (d-emacs-coords-coordinatize-layout
                       layout)
                      (lambda (lst) (d--binding-p (car lst))))
                     coordrx)))

    (funcall (if (called-interactively-p 'any)
                 #'d-emacs-coords-draw-placevals-in-temp-buffer
               #'d-emacs-coords-draw-placevals)
             placevals)))

(defun d-emacs-coords-draw-keyboard-layer (laynum &optional org layout)
  "Draw the layer of LAYOUT with LAYNUM.

The default for LAYOUT is the symbol-value of `d-emacs-dfk-layout' or
`d-emacs-xkb-layout' if that is undefined.

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
  (let ((layout (or layout (symbol-value (d-emacs-coords--dfk-or-xkb-layout)))))
    (funcall (if (called-interactively-p 'any)
                 #'d-emacs-coords-draw-placevals-in-temp-buffer
               #'d-emacs-coords-draw-placevals)
             (apply #'append (d-emacs-coords-get-layer
                              (d-emacs-coords-coordinatize-layout layout)
                              laynum))
             nil
             nil
             org)))

(defun d-emacs-coords-draw-key-coordinates (&optional layer org)
  "Draw the coordinates of LAYER.

By default, LAYER is the first layer of `d-emacs-dfk-layout' or
`d-emacs-xkb-layout' if `d-emacs-dfk-layout' is undefined."
  (interactive)
  (let* ((layer (or layer (car (d-emacs-coords-coordinatize-layout
                                (symbol-value (d-emacs-coords--dfk-or-xkb-layout))))))
         (placevals (d-flatten-until (d-emacs-coords-run-through
                                      (d-emacs-coords-boundaries
                                       (mapcar #'car (apply #'append layer)))
                                      (lambda (coords) (cons coords (d-remove-surrounding-brackets
                                                                (format "%s" (cdr coords))))))
                                     (lambda (lst) (d-emacs-coords-p (caar lst))))))
    (d-emacs-coords-draw-placevals placevals nil nil org)))

;;;;; Layout Generation
(defun d-emacs-coords-layout-from-placevals (placevals &optional standardval)
  "Generate a layout from a family of placevals.

Add STANDARDVAL to empty strings for all places without values.

By default, STANDARDVAL is an empty string."
  (cl-flet ((fiber-by-layout-level (coordslist lev)
                                                  (d-emacs-fiber-by-property coordslist (lambda (coords) (nth lev coords)) t #'=)))
    (let* ((standardval (or standardval ""))
           (coords (mapcar #'car placevals))
           (abscoords (mapcar #'d-emacs-coords-rel-to-abs coords))
           (indices (d-cardinal (length (car coords))))
           (abscoordsbylayer (fiber-by-layout-level abscoords 0))
           (abscoordsbyrow (sort (mapcar (lambda (idxcoordslist)
                                                                               (cons (car idxcoordslist)
                                                 (fiber-by-layout-level (cdr idxcoordslist) 1)))
                                         abscoordsbylayer)
                                 :key #'car
                                 :lessp #'<))
           (abscoordsrowbounds (mapcar (lambda (idxlayer)
                                                                             (cons (car idxlayer)
                                               (sort
                                                (mapcar (lambda (idxrow)
                                                                                              (let* ((cols (mapcar (lambda (coords)
                                                                                                                     (car (last coords)))
                                                                               (cdr idxrow)))
                                                                 (firstcol (apply #'min cols))
                                                                 (lastcol (apply #'max cols)))
                                                            (cons (car idxrow)
                                                                  (cons (list firstcol)
                                                                        (list lastcol)))))
                                                        (cdr idxlayer))
                                                :key #'car
                                                :lessp #'<))) 
                                       abscoordsbyrow)))
      (d-emacs-coords-run-through abscoordsrowbounds
                                  (lambda (runcoords)
                                                                        (alist-get (d-emacs-coords-abs-to-rel runcoords) placevals standardval nil #'equal))))))

;;;; Provide
(provide 'd-emacs-coords)
;;; d-emacs-coords.el ends here
