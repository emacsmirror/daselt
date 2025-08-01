;;; daselt-coords.el --- Tools for the coordinatization and drawing of layouts  -*- lexical-binding: t; -*-

;; Copyright (C) 2024, 2025  Alexander Prähauser

;; Author: Alexander Prähauser <ahprae@protonmail.com>
;; Version: 1.0
;; Keywords: tools
;; URL: https://gitlab.com/nameiwillforget/d-emacs/-/blob/master/daselt-coords.el
;; Keywords: tools

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

;; `daselt-coords` is a comprehensive Emacs Lisp library designed to facilitate
;; the coordinatization and visualization of keyboard layouts. This package
;; provides a suite of tools for defining, transforming, and rendering
;; multi-layer keyboard configurations, making it ideal for users who seek to
;; customize their keyboard mappings or visualize complex key arrangements.

;; **Key Features:**

;; - **Coordinate Management:** Absolute and Relative Coordinates:** Convert
;; - **between absolute coordinates ;; - (specific key positions within layers)
;; - **and relative coordinates ;; - (positions relative to a defined midpoint),
;; - **allowing for flexible and intuitive layout definitions.

;; - **Layer Shifts:** Define and manage coordinate shifts for entire layers or
;; - individual rows, accommodating variations across different keyboard layers.

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
;; - `daselt-dfk-layout` or `daselt-xkb-layout` configurations, providing
;; - flexibility for users with predefined layouts.

;; **Defining a Layout:**
;; The customs of the group `daselt-coords' can be used to coordinatize layouts
;; according to your particular keyboard configuration. The defaults are written
;; as to coordinatize a standard ANSI or ISO keyboard like this:

;; +-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
;; |     |Q    |W    |E    |R    |T    |     |Y    |U    |I    |O    |P    |     |
;; +-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
;; |     |A    |S    |D    |F    |G    |     |H    |J    |K    |L    |'    |     |
;; +-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
;; |?    |Z    |X    |C    |V    |     |B    |     |N    |M    |,    |.    |?    |
;; +-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+

;; By modifying the provided customs you can re-set the number and length of
;; layers, rows and columns and insert `formal' places that don't correspond to
;; keys to make an irregular layout regular like the row in the middle and the
;; places next to the B-key here.

;; **Visualizing a Layer:** You can use `daselt-coords-draw-keyboard-layer`
;; to render a specific layer of your keyboard layout in an Emacs buffer, either
;; as a standard table or an Org-mode table. You can also use
;; `daselt-coords-draw-coordwise-from-coordrxlst', which allows you to draw
;; sections of a layout in a much more granular fashion. The drawing function
;; `daselt-coords-draw-placevals' can in principle be used to draw anything
;; that's coordinatized and is, for instance, used in `daselt-bind' to draw
;; bindlists by regular expressions.

;; **Other main functions:** Apart from this, the main functions in this package
;; are `daselt-coords-coordinatize-layout', which coordinatizes a given layout
;; by consing each of its values with its relative coordinates, and
;; `daselt-coords-binding', which returns the binding corresponding to a
;; particular (relative) coordinate list.

;; - **Use in Bindlists:** `daselt-coords' provides the base for
;; `daselt-bind', which provides utilities to define bindings by positions in
;; the layout (independently of the symbols they house).

;; - **Daselt Mode:** This package is part of the Daselt-project
;; (https://gitlab.com/nameiwillforget/d-emacs) and provides necessary
;; prerequisites for Daselt's Emacs implementation daselt.

;; `daselt-coords` empowers Emacs users to meticulously design and visualize
;; their keyboard layouts, offering granular control over key positioning and
;; layer management. Whether you're crafting a custom ergonomic setup or
;; experimenting with multi-layer configurations, this package provides the
;; necessary tools to achieve a tailored and efficient keyboard experience.


;;; Code:

;;;; Preamble
(require 'cl-lib)
(require 'table)
(require 'org-compat)
(require 'daselt-base)

(declare-function org-table-align "org-table" nil)
(declare-function daselt-bind-p "daselt-bind" (obj))
;; (declare-function daselt-base-with-max-buffer-maybe-return "daselt-base" (bufname fun))
;; (declare-function daselt-base-remove-list-index "daselt-base" (lst idx))
;; (declare-function daselt-base-filter-list "daselt-base" (lst pred))
;; (declare-function daselt-base-string-exists-and-nonempty "daselt-base" (str))
;; (declare-function daselt-base-forall-p "daselt-base" (list predicate))
;; (declare-function daselt-base-index "daselt-base" (list &optional fromone))
;; (declare-function daselt-base-cardinal "daselt-base" (n &optional fromone))


(defvar daselt-bind-translate-choices)

;;;; Customs
(defgroup daselt-coords
  nil
  "Customization group for daselt-coords."
  :group 'daselt
  :prefix "daselt-coords-")

(define-widget 'daselt-coords 'lazy
  "A list of coordinate numbers."
  :offset 4
  :tag "Coords"
  :type '(repeat number))

(define-widget 'daselt-coords-prefix-coords-pair 'lazy
  "A list of coordinate numbers."
  :offset 4
  :tag "Prefix-Coords"
  :type '(cons string coords))

(defcustom daselt-coords-abs-mid
  '(1 4.5)
  "Absolute coordinates of the midpoint of the d-xkb-layouts.
First coordinate is the number of the row counted downward. Second coordinate is
the number of the key counted rightward. If midpoint is between two rows or
keys, use a floating point number between the key numbers."
  :type 'daselt-coords
  :group 'daselt-coords)

(defcustom daselt-coords-layer-shifts-list
  '((0 1 1))
  "Shifts of individual layers in coordinates.
Each entry of the list should be a list consisting of three coordinates.

The first coordinate is the absolute coordinate of the layer, i.e. its position
in `daselt-coords-layer-list'.

The second coordinate is the row shift, meaning the number of rows that have to
be added or removed from the row coordinate of the midpoint in
`daselt-coords-abs-mid' in that particular layer. For instance, Daselt's
midpoint is generally in the second row, but in layer 0 a further row has to be
added on top, so 1 has to be added to the row coordinate of
`daselt-coords-abs-mid' for calculations in that layer.

The second coordinate is the column midpoint shift and works similarly to the
row midpoint shift. For instance, in Daselt's layer 0 the outer left row of keys
is used, while it is unused in other layers. Therefore 1 has to be added to the
column coordinate. Therefore, the layer shift coordinates for Daselt layer 0 are
\(0 1 1).

If no shift is necessary, it doesn't have to be specified."
  :type 'daselt-coords
  :group 'daselt-coords)

(defcustom daselt-coords-row-shifts-list
  '((2 0.5) (3 -1.5))
  "List of row shifts in absolute coordinates.

Each entry should consist of two numbers. The first is the absolute coordinate
of the row in an unshifted layer, meaning a layer without an entry in
`daselt-coords-layer-shifts-list'.

The second number is the column shift, meaning the number that has to be added
or subtracted from the column coordinate of `daselt-coords-abs-mid'.

For instance, Daselt's layout accounts for the shift of the lower key row on
standard keyboards by shifting that row by 0.5 and inserting formal places
around the middle key using `daselt-coords-formal-places-list', thus it has a
row shift of (2 0.5)."
  :type '(repeat coords)
  :group 'daselt-coords)

(defcustom daselt-coords-layer-row-shifts-list
  '((0 0 -0.5))
  "Shifts for rows in particular layers.
This constant works similar to `daselt-coords-row-shifts-list'.

Generally it is better to use the shifts in `daselt-coords-row-shifts-list' and
`daselt-coords-layer-shifts-list'.

Setting shifts using this constant is only necessary for shifts of rows that are
not in all layers.

For instance, in Daselt's layout, the number row is only in layer 0. Since row
shifts have to be given in absolute coordinates, shifts in this row cannot be
specified in `daselt-coords-row-shifts-list'. Instead they have to be specified
here.

Each element should be a three-coordinate list. The first coordinate is the
layer. The second coordinate is the row. The third is the row shift. These
should all be given in absolute coordinates."
  :type '(repeat coords)
  :group 'daselt-coords)

(defcustom daselt-coords-formal-places-list
  '((1 -1) (1 1) (-2 -1) (-2 1))
  "Relative coordinates of formal positions in the d-xkb-layouts.

These are positions that do not correspond to keys and are used to align rows
with each other when converting between absolute and relative coordinates.

If you have several formal places on the same side in the same row, please put
them in ascending order, otherwise the algorithm will get confused."
  :type '(repeat coords)
  :group 'daselt-coords)

(defcustom daselt-coords-layer-list
  '(0 1 2 3 4 5 6 7 8)
  "Layers in daselt-coords-layouts.
The index of a number is supposed to be its level in the xkb-layout, the number
value the relative layer coordinate. Layer 0 is not supposed to be included."
  :type '(repeat natnum)
  :group 'daselt-coords)

(defcustom daselt-coords-bad-combinations-list
  nil
  "A list of key combinations that do not work on your main keyboard(s).

Some key combinations may not be registered on specific keyboards. Which ones
varies by model.

When `daselt-bind-apply-binding' is executed, it checks each combination in
this list. If a combination is found, a variant binding is created in which

- the C-modifier is replaced with an A-modifier.

- the H-modifier is replaced by an s-M-modifier.

Key combinations should be specified using conses of prefixes and relative
Daselt coordinates."
  :type 'daselt-coords-prefix-coords-pair
  :group 'daselt-coords)

;;;; Constants
(defconst daselt-coords-layer-numbers-list
                    (daselt-base-cardinal (length daselt-coords-layer-list))
                    "Number of layers in daselt-coords-layouts.")


;;;; Functions
;;;;; Coordinates
;;;;;; Transformations
(defun daselt-coords-abs-to-rel (coords)
  "Transform absolute coordinates COORDS of a key into relative ones."
  (declare
   ;; (ftype (function ((list number)) (list number))) ; Compiler complains.
   (ftype (function (list) list))
   (side-effect-free t))
  (let* ((layer (nth 0 coords))
         (row (nth 1 coords))
         (col (nth 2 coords))
         (absmid daselt-coords-abs-mid)
         (absmidrow (nth 0 absmid))
         (absmidcol (nth 1 absmid))
         (layershift (alist-get layer daselt-coords-layer-shifts-list '(0 0)))
         (layerallrowsshift (nth 0 layershift))
         (layerallcolsshift (nth 1 layershift))
         (layermid (list (+ absmidrow layerallrowsshift)
                         (+ absmidcol layerallcolsshift)))
         (layermidrow (nth 0 layermid))
         (layermidcol (nth 1 layermid))
         (shiftedrowshifts (mapcar (lambda (rowshift)
                                     (list (+ layerallrowsshift (nth 0 rowshift))
                                           (nth 1 rowshift)))
                                   daselt-coords-row-shifts-list))
         (layerrowshift (or (nth 2 (car (daselt-base-filter-list
                                         daselt-coords-layer-row-shifts-list
                                         (lambda (shift) (and (= layer (nth 0 shift))
                                                         (= row (nth 1 shift)))))))
                            0))
         (rowshift (+ (car (alist-get row shiftedrowshifts '(0))) layerrowshift))
         (rowmid (+ layermidcol rowshift))
         (layer (nth layer daselt-coords-layer-list))
         (row (daselt-base-roundout (- row layermidrow)))
         (col (daselt-base-roundout (- col rowmid)))
         ;; We still have to account for formal places and midpoints between two keys.
         (colposp (natnump col))
         (formplacesinrow (daselt-base-filter-list daselt-coords-formal-places-list
                                                    (lambda (coords)
                                                      (let ((formrow (nth 0 coords)))
                                                        (= row formrow)))))
         (formplacesonside (daselt-base-filter-list formplacesinrow
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

(defun daselt-coords-rel-to-abs (coords)
  "Transform relative coordinates COORDS of a key into absolute ones."
  (declare (ftype (function (list) list))
           ;; (ftype (function ((list number)) (list number))); Compiler complains.
           (side-effect-free t))
  (let* ((layer (nth 0 coords))
         (row (nth 1 coords))
         (col (daselt-coords--remove-formal-places coords))
         (absmid daselt-coords-abs-mid)
         (absmidrow (nth 0 absmid))
         (absmidcol (nth 1 absmid))
         (layer (cl-position layer daselt-coords-layer-list))
         (layershift (alist-get layer daselt-coords-layer-shifts-list '(0 0)))
         (layerallrowsshift (nth 0 layershift))
         (layerallcolsshift (nth 1 layershift))
         (layermid (list (+ absmidrow layerallrowsshift)
                         (+ absmidcol layerallcolsshift)))
         (layermidrow (nth 0 layermid))
         (layermidcol (nth 1 layermid))
         (row (truncate (+ row layermidrow)))
         (layerrowshift (or (nth 2 (car (daselt-base-filter-list
                                         daselt-coords-layer-row-shifts-list
                                         (lambda (shift) (and (= layer (nth 0 shift))
                                                         (= row (nth 1 shift)))))))
                            0))
         (shiftedrowshifts (mapcar (lambda (rowshift)
                                     (list (+ layerallrowsshift (nth 0 rowshift))
                                           (nth 1 rowshift)))
                                   daselt-coords-row-shifts-list))
         (rowshift (+ (car (alist-get row shiftedrowshifts '(0))) layerrowshift))
         (rowmid (+ layermidcol rowshift))
         (col
          (funcall (if (natnump col) #'floor #'ceiling)
                   (+ col rowmid))))
    (list layer row col)))

(defun daselt-coords--remove-formal-places (coords)
  "Return position in row with formal places between COORDS and the row mid
removed.

COORDS should be in relative coordinates."
  (declare (ftype (function (list) number))
           ;; (ftype (function ((list number)) number)) ; Compiler complains.
           (side-effect-free t))
  (if (daselt-coords-p coords)
      (if coords
          (let* ((col (car (last coords)))
                 (row (nth (- (length coords) 2) coords))
                 (colposp (natnump col))
                 (formplacesinrow (daselt-base-filter-list daselt-coords-formal-places-list
                                                            (lambda (coords)
                                                              (let ((formrow (nth 0 coords)))
                                                                (= row formrow)))))
                 (formplacesonside (daselt-base-filter-list formplacesinrow
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
    (error "Wrong-type argument daselt-coords-p")))

;;;;;; Coordinatization
(defun daselt-coords-coordinatize-layout (layout)
  "Add relative coordinates to every element of LAYOUT.

Resulting layout places are pairs of coordinates and their corresponding key
symbol."
  (declare (ftype (function (list) list)
                  ;; (function ((list (list (list t)))) ; Compiler complains.
                  ;;           (list (list (list (cons (list number) t)))))
                  )
           (side-effect-free t))
  (mapcar (lambda (laynum)
            (mapcar (lambda (indrow)
                      (mapcar (lambda (indsymb)
                                (cons (daselt-coords-abs-to-rel
                                       (list laynum
                                             (car indrow)
                                             (car indsymb)))
                                      (cdr indsymb)))
                              (daselt-base-index (cdr indrow))))
                    (daselt-base-index (nth laynum layout))))
          daselt-coords-layer-numbers-list))

;;;;;; Layers
(defun daselt-coords-get-layer (coordlayout laynum)
  "Get the layer LAYNUM from a coordinatized layout COORDLAYOUT.

Remove the layer coordinate from each coordinate list."
  (declare (ftype
            ;; (function ((list (list (list (cons (list number) t)))) integer) ; Compiler complains.
            ;;           (list (list (cons (list number) t))))
            (function (list integer) list))
           (side-effect-free t))
  (mapcar (lambda (row)
            (mapcar (lambda (place)
                      (let ((coords (car place)))
                        (cons (cl-subseq coords 1 (length coords)) (cdr place))))
                    row))
          (alist-get laynum (mapcar (lambda (layer)
                                      (cons (caaaar layer) layer))
                                    coordlayout))))

;;;;;; Predicates
(defun daselt-coords-p (list)
  "Return t if LIST is a valid daselt-coords coordinate list.

This means it consists entirely of numbers."
  (declare (ftype (function (list) boolean))
           (pure t))
  (and (proper-list-p list)
       (daselt-base-forall-p list #'numberp)))

;;;;;; Boundaries
(defun daselt-coords-boundaries (coordslist)
  "Calculate the boundaries of a COORDSLIST.

Returns two coords:

- The first is made from the minima of all coords in COORDSLIST.

- The second is made out of all maxima.

Assumes all coords in COORDSLIST have the same length."
  (declare (ftype (function (list) cons)
                  ;; (function ((list (list number))) (cons (list number) (list number))) ; Compiler complains.
                  )
           (pure t))
  (daselt-base-list-to-cons
   (mapcar (lambda (extremum)
             (mapcar (lambda (idx)
                       (apply extremum (mapcar (lambda (coords)
                                                 (nth idx coords))
                                               coordslist)))
                     (daselt-base-cardinal (length (car coordslist)))))
           (list #'min #'max))))

;;;;;; Bindings
(defun daselt-coords-binding (coords &optional layout wholebinds)
  "Retrieve the value of LAYOUT at COORDS.

The default of LAYOUT is the `symbol-value' of `daselt-dfk-layout' or
`daselt-xkb-layout' if the first is undefined.

COORDS should contain three numbers:

- the layer number.

- the row number, where the middle row is numbered 0 and upper rows are
negative.

- the relative position of the key with respect to the central axis. The B-key
in a QWERTY keyboard is numbered 0, the keys next to it -2 and 2.

The 0-th layer is the value of the symbol bound to `daselt-xkb-layer-0', which
is not given by scanning the `daselt-xkb-file' but has to be provided. It is
intended as a layer containing double-bindings (i.e. using
`dual-function-keys'). It is assumed that double bindings in this layer are
separated by slashes `/'. This is why, by default, `daselt-coords-binding'
splits the returned string using `/'. To disable this behavior, set WHOLECOORDS
to t."
  (declare (ftype (function (list &optional list boolean)
                            ;; ((list number) &optional (list (list (list t))) boolean) ; Compiler complains.
                            t))
           (side-effect-free t))
  (let* ((abscoords (daselt-coords-rel-to-abs coords))
         (layout (if layout layout (symbol-value (daselt-coords--dfk-or-xkb-layout))))
         (unsplitbind (nth (nth 2 abscoords)
                           (nth (nth 1 abscoords)
                                (nth (nth 0 abscoords) layout))))
         (lastbindpart (if unsplitbind (car (last (split-string unsplitbind "/")))))
         (bind (if (and (not wholebinds)
                        (daselt-base-string-exists-and-nonempty lastbindpart))
                   lastbindpart
                 unsplitbind)))
    bind))

;;;;;; Function application
(defun daselt-coords-run-through (bounds fun &optional runcoords)
  "Iterate over each coordinate within specified BOUNDS, applying a function
FUN to each coordinate.

BOUNDS can be either:

 - A cons cell (MIN . MAX) where MIN and MAX are
coordinates (cons cells themselves) defining the rectangular region's opposite
corners.

 - A list of cons cells each containing a running coordinate and its
associated bounds for further iteration. This allows for recursive traversal
through nested coordinate spaces.

Each coordinate is represented as a cons cell (X . Y), where X and Y are
integers.

FUN is a function that will be applied to each coordinate tuple generated. The
tuple will be passed as a single argument to FUN. If RUNCOORDS is provided,
these coordinates will be prepended to each tuple before FUN is applied,
effectively allowing additional context to be passed through the recursive
iterations.

RUNCOORDS is an optional list of extra coordinates that should be included
with each coordinate tuple passed to FUN. This is useful for maintaining state
or additional information across the recursive traversal. Initially, this should
typically be left nil.

The traversal is depth-first, expanding across the X dimension before proceeding
along the Y dimension within the defined bounds. Inside the iteration, if BOUNDS
is defined as a list of cons cells (implying recursed traversal with additional
contextual coordinates), each element's bounds are used for further recursive
calls, with the running coordinate appended to RUNCOORDS.

This function is particularly useful for processing or analyzing grid-like
structures where an operation needs to be applied across a range of coordinates,
optionally utilizing additional context carried through RUNCOORDS."
  (declare (ftype (function (cons (function (list) t) &optional list
                                  ;; (cons (list number) (list (number))) ; Compiler complains.
                                  ;; (function ((list number)) t)
                                  ;; &optional (list (list number))
                                  )
                            t)))
  (if (consp bounds)
      (if (and (daselt-coords-p (car bounds))
               (daselt-coords-p (cdr bounds)))
          (let ((minima (car bounds))
                (maxima (cdr bounds)))
            (if (and minima maxima)
                (mapcar
                 (lambda (rightmost)
                   (daselt-coords-run-through (cons (cdr minima)
                                                     (cdr maxima))
                                               fun
                                               (append runcoords (list rightmost))))
                 (daselt-base-numbers-between (car minima) (car maxima)))
              (funcall fun runcoords)))
        (mapcar (lambda (bboundscons)
                  (let ((runcoord (car bboundscons))
                        (bbounds (cdr bboundscons)))
                    (daselt-coords-run-through bbounds fun (append runcoords
                                                                    (list runcoord)))))
                bounds))
    (error "Ill-formatted bounds %s" bounds)))

;;;;;; Other
(defun daselt-coords-concat (coords)
  "Concatenate a list of coordinates COORDS into a string."
  (declare (ftype (function (list) string))
           ;; (function ((list number)) string)) ; Compiler complains.
           (pure t))
  (mapconcat #'number-to-string (reverse coords) "-"))

(defun daselt-coords--dfk-or-xkb-layout ()
  "Return the value of the symbol in `daselt-dfk-layout'.

If it is undefined, use `daselt-xkb-layout' instead."
  (declare (ftype (function () symbol))
           (side-effect-free t))
  (if (and (symbolp 'daselt-dfk-layout)
           (boundp'daselt-dfk-layout))
      daselt-dfk-layout
    (if (and (symbolp 'daselt-xkb-layout)
             (boundp'daselt-xkb-layout))
        daselt-xkb-layout
      (error "No layout defined"))))

;;;;; Placevals
;;;;;; Regexp matching
(defun daselt-coords-placevals-matching-coordrx (placevals coordrx)
  "Retrieve all place values in PLACEVALS whose coordinates match COORDRX."
  (declare (ftype (function (list string) list)
                  ;; (function ((list (cons (list number) t)) string) (list (cons (list number) t))) ; Compiler complains.
                  )
           (pure t))
  (daselt-base-filter-list placevals (lambda (placeval)
                                        (let ((coords (car placeval)))
                                          (string-match-p coordrx (format "%s" coords))))))

(defun daselt-coords-placevals-matching-indexed-rx (placevals idx coordrx)
  "Retrieve place values among PLACEVALS whose IDX-th coordinate matches COORDRX."
  (declare (ftype (function (list number string)
                            list)
                  ;; (function ((list (cons (list number) t)) number string)
                  ;;           (list (cons (list number) t)))
                  )
           (pure t))
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
    (daselt-coords-placevals-matching-coordrx
     placevals
     finalrx)))

;;;;;; Index manipulation
(defun daselt-coords-remove-index-from-placevals (placevals idx)
  "Remove IDX from the coordinates of each placeval in PLACEVALS.

Return the modified list."
  (declare (ftype (function (list number) list)
                  ;; (function ((list (cons (list number) t)) number) (list (cons (list number) t))) ; Compiler complains.
                  )
           (pure t))
  (mapcar (lambda (placeval)
            (let ((coords (car placeval))
                  (val (cdr placeval)))
              (cons (daselt-base-remove-list-index coords idx)
                    val)))
          placevals))

(defun daselt-coords-find-coord-values-at-index (placevals idx)
  "Find all values the coordinates PLACEVALS have at IDX."
  (declare (ftype (function (list number) list)
                  ;; (function ((list (cons (list number) t)) number) (list number)) ; Compiler complains.
                  )
           (pure t))
  (mapcar (lambda (placeval)
            (let ((coords (car placeval)))
              (nth idx coords)))
          placevals))


;;;;;; Extraction
(defun daselt-coords-extract-value-string (val)
  "Extract the string corresponding to the value VAL of a placevalue."
  (declare (ftype (function (t) string))
           (pure t))
  (let* ((valname (if (symbolp val) (symbol-name val)))
         (valval (unless (or (stringp val) valname)
                   ;; Proceed if there's an error.
                   (condition-case nil (eval val)
                     (error (lambda ())))))
         (valvalname (if (symbolp valval)
                         (symbol-name valval)))
         (valcoordsval (if (daselt-coords-p val)
                           (daselt-coords-binding val)))
         (valstr (cond ((stringp val) val)
                       (valname valname)
                       ((stringp valval) valval)
                       (valvalname valvalname)
                       (valcoordsval valcoordsval)
                       (t ""))))
    valstr))


;;;;;; Drawing
(defun daselt-coords-draw-placevals (placevals &optional bounds runcoords org noupperrow)
  "Draw a keyboard layout from PLACEVALS.

PLACEVALS is a list of cons cells containing coordinates and a value.

Each PLACEVAL is a cons cell where the car is a pair of coordinates and the cdr
is a value that can be inserted or evaluated into an insertable form.

If BOUNDS is given, it should be be a cons of two conses, specifying the minimum
and maximum number of rows and columns in the layout. Otherwise, the boundaries
are calculated using the given placevals.

RUNCOORDS is used for recursive calls and typically should not be manually
specified.

If ORG is non-nil, output is formatted as an `org-mode' table.

NOUPPERROW is only of importance if ORG is t. Then it removes the upper row of
the org table, which indexes coordinates."
  (declare (ftype (function (list &optional cons list boolean boolean)
                            ;; ((list (cons (list number) t)) &optional ; Compiler complains.
                            ;;  (cons (list number) (list number))
                            ;;  (list (list number))
                            ;;  boolean
                            ;;  boolean)
                            ;; void ; Complains here too.
                            t)))
  (if placevals
      (let* (;; Test for the number of coordinates.
             (testcoords (caar placevals))
             (coordcard (length testcoords)))

        ;; Recurse for higher dimensions.
        (if (>= coordcard 3)
            (mapcar (lambda (firstcoord)
                      (let ((runcoords (append runcoords (list firstcoord)))) ; Redefine RUNCOORDS to include the coordinate that is recursed upon.
                        (insert (format "\n%s\n" (daselt-coords-concat runcoords)))
                        (progn (daselt-coords-draw-placevals
                                (daselt-coords-remove-index-from-placevals
                                 (daselt-coords-placevals-matching-indexed-rx
                                  placevals 0 (number-to-string firstcoord))
                                 0)
                                bounds runcoords org)
                               (insert "\n"))))
                    (cl-remove-duplicates (daselt-coords-find-coord-values-at-index placevals 0)))

          (let* ((coordslist (mapcar #'car placevals))
                 (bounds (or bounds (daselt-coords-boundaries coordslist)))
                 (firstrow (caar bounds))
                 (firstcol (nth 1 (car bounds)))
                 (lastrow (cadr bounds))
                 (lastcol (nth 1 (cdr bounds)))
                 (colnum (1+ (- lastcol firstcol)))
                 (rownum (1+ (- lastrow firstrow)))
                 (print-level nil)
                 (print-length nil))
            (funcall (if org
                         #'daselt-coords-draw-org-table
                       #'daselt-coords-draw-table)
                     colnum rownum firstrow lastrow firstcol lastcol placevals noupperrow))
          (daselt-base-goto-max)
          nil))))

;;;;;;;; Auxiliary
(defun daselt-coords-draw-table (colnum rownum firstrow lastrow firstcol lastcol placevals &optional _noupperrow)
  "Draw a daselt-table using table.el.

An auxiliary function to `daselt-coords-draw-placevals'. See there for more
documentation."
  (declare (ftype (function (integer integer integer integer integer integer
                                     ;; (list (cons (list number) t)) ; Compiler complains.
                                     list &optional boolean)
                            void)))
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
                                    (valstr (daselt-coords-extract-value-string val)))
                               (if (not (string-empty-p valstr))
                                   valstr
                                 (format "%s" val)))))
                       do (table-forward-cell 1))))

(defun daselt-coords-draw-org-table (colnum _rownum firstrow lastrow firstcol lastcol placevals &optional noupperrow)
  "Draw a daselt-table in `org-mode'.

An auxiliary function to `daselt-coords-draw-placevals'. See there for more
documentation."
  (declare (ftype (function (integer integer integer integer integer integer
                                     ;; (list (cons (list number) t)) ; Compiler complains.
                                     list &optional boolean)
                            void)))
  (unless noupperrow
    (insert "|-\n")
    (insert "|")
    (dotimes (i colnum)
      (insert (format " %d |" (+ firstcol i))))
    (insert "\n"))
  (insert "|-\n")

  ;; Insert rows for the Org table.
  (save-excursion
    (cl-loop for row from firstrow to lastrow
             do (progn
                  (insert "|")
                  (cl-loop for place from firstcol to lastcol
                           do (let* ((val (alist-get (list row place) placevals "" nil #'equal))
                                     (valstr (daselt-coords-extract-value-string val)))
                                (insert (if (not (string-empty-p valstr)) valstr " "))
                                (insert " |")) ; End of cell
                           )
                  (insert "\n|-\n")))
    (set-mark (point)))
  (org-table-align))

(defun daselt-coords-draw-placevals-in-temp-buffer (placevals &optional bounds runcoords org)
  "Execute `daselt-coords-draw-placevals' in a maximized temporary buffer.

All arguments are forwarded to `daselt-coords-draw-placevals'. See there for
more documentation."
  (declare (ftype (function (list &optional cons list boolean)
                            ;; ((list (cons (list number) t)) ; Compiler complains.
                            ;;  &optional (cons (list number) (list number))
                            ;;  (list (list number))
                            ;;  boolean)
                            t)))
  (daselt-base-with-max-buffer-maybe-return
   "*daselt-layout*" (lambda () (daselt-coords-draw-placevals placevals bounds runcoords org)
                       (org-mode))))

;;;;;;; Drawing Commands

(defun daselt-coords-draw-coordwise-from-coordrxlst (coordrx1 coordrx2 coordrx3 &optional layout)
  "Draw layer matches in LAYOUT based on three coordinate regular expressions.

COORDRX1, COORDRX2, and COORDRX3 are regular expressions that specify which
coordinates can take which values. When called interactively, the user is
prompted to enter three regular expressions.

- If one of the COORDRXs is a fixed value (a number), the other two are used as
  row and column specifiers, in that order.

- If two COORDRXs are fixed, matches for the third are drawn in a row.

- If all three COORDRXs are variable, layers containing matches are drawn
  sequentially.

The default for LAYOUT is the `symbol-value' of `daselt-dfk-layout' or
`daselt-xkb-layout' if that is undefined.

The resulting matches are drawn either in the current buffer or a temporary
buffer, depending on the invocation context."
  (declare (ftype (function (string string string &optional 
                                    ;; (list (list (list t))) ; Compiler complains.
                                    list)
                            t)))
  (interactive (list (read-regexp "Number1 regexp: ")
                     (read-regexp "Number2 regexp: ")
                     (read-regexp "Number3 regexp: ")))

  (let* ((layout (or layout (symbol-value (daselt-coords--dfk-or-xkb-layout))))
         (coordrxlst (list coordrx1 coordrx2 coordrx3))
         (coordrx (mapconcat #'identity coordrxlst " "))
         (placevals (daselt-coords-placevals-matching-coordrx
                     (daselt-base-flatten-until
                      (daselt-coords-coordinatize-layout
                       layout)
                      (lambda (lst) (daselt-bind-p (car lst))))
                     coordrx)))

    (funcall (if (called-interactively-p 'any)
                 #'daselt-coords-draw-placevals-in-temp-buffer
               #'daselt-coords-draw-placevals)
             placevals)))

(defun daselt-coords-draw-keyboard-layer (laynum &optional org layout)
  "Draw the layer of LAYOUT with LAYNUM.

The default for LAYOUT is the `symbol-value' of `daselt-dfk-layout' or
`daselt-xkb-layout' if that is undefined.

If ORG is t, draw an org-table."
  (declare (ftype (function (integer &optional boolean (list (list (list t)))) void)))
  (interactive (list (string-to-number
                      (nth 1 (eval ; This macro is a hack to get around the different arity of `daselt--translate-read-multiple-choice' and `read-multiple-choice'.
                              `(read-multiple-choice
                                "Layer number: "
                                (append  (list (list (string-to-char (daselt-coords-binding '(1 0 -2)))
                                                     "0")
                                               (list (string-to-char (daselt-coords-binding '(1 0 2)))
                                                     "4"))
                                         (mapcar (lambda (num)
                                                   (list (string-to-char (format "%d" num))
                                                         (number-to-string num)))
                                                 daselt-coords-layer-numbers-list))
                                nil nil nil ,(if (boundp daselt-bind-translate-choices)
                                                 daselt-bind-translate-choices
                                               nil)))))))
  (let ((layout (or layout (symbol-value (daselt-coords--dfk-or-xkb-layout)))))
    (funcall (if (called-interactively-p 'any)
                 #'daselt-coords-draw-placevals-in-temp-buffer
               #'daselt-coords-draw-placevals)
             (apply #'append (daselt-coords-get-layer
                              (daselt-coords-coordinatize-layout layout)
                              laynum))
             nil
             nil
             org)))

(defun daselt-coords-draw-key-coordinates (&optional layer org)
  "Draw the coordinates of LAYER.

By default, LAYER is the first layer of `daselt-dfk-layout' or
`daselt-xkb-layout' if `daselt-dfk-layout' is undefined.

Use an org table if ORG is t."
  (declare (ftype (function (&optional (list (list t)) boolean)
                            void)))
  (interactive)
  (let* ((layer (or layer (daselt-coords-get-layer
                           (daselt-coords-coordinatize-layout
                            (symbol-value (daselt-coords--dfk-or-xkb-layout)))
                           1)))
         (placevals (daselt-base-flatten-until (daselt-coords-run-through
                                                (daselt-coords-boundaries
                                                 (mapcar #'car (apply #'append layer)))
                                                (lambda (coords) (cons coords (concat " "(daselt-base-remove-surrounding-brackets (format "%s" coords))))))
                                               (lambda (lst) (daselt-coords-p (caar lst))))))
    (daselt-coords-draw-placevals placevals nil nil org t)))

;;;;;; Layout Generation
(defun daselt-coords-layout-from-placevals (placevals &optional standardval)
  "Generate a layout from a family of PLACEVALS.

Add STANDARDVAL to empty strings for all places without values.

By default, STANDARDVAL is an empty string."
  (declare (ftype (function (list &optional string) list)
                  ;; (function ((list (cons (list number) t)) ; Compiler complains.
                  ;;            &optional string)
                  ;;           (list (list (list t))))
                  )
           (side-effect-free t))
  (cl-flet ((fiber-by-layout-level (coordslist lev)
              (daselt-base-fiber-by-property coordslist (lambda (coords) (nth lev coords)) t #'=)))
    (let* ((standardval (or standardval ""))
           (coords (mapcar #'car placevals))
           (abscoords (mapcar #'daselt-coords-rel-to-abs coords))
           (abscoordsbylayer (fiber-by-layout-level abscoords 0))
           (abscoordsbyrow (sort (mapcar (lambda (idxcoordslist)
                                           (cons (car idxcoordslist)
                                                 (fiber-by-layout-level (cdr idxcoordslist) 1)))
                                         abscoordsbylayer)
                                 (lambda (cns1 cns2) (< (car cns1) (car cns2)))))
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
                                                (lambda (cns1 cns2) (< (car cns1) (car cns2))))))
                                       abscoordsbyrow)))
      (daselt-coords-run-through abscoordsrowbounds
                                 (lambda (runcoords)
                                   (alist-get (daselt-coords-abs-to-rel runcoords) placevals standardval nil #'equal))))))

;;;;; Generation functions
(defun daselt-coords-for-layouts-in (fun &optional layoutsyms)
  "Execute FUN for each layout symbol in LAYOUTSYMS.

At each iteration LAYOUTSYM, `daselt-coords--dfk-or-xkb' returns LAYOUTSYM
and calls FUN with LAYOUTSYM as an argument."
  (declare (ftype (function (function &optional list) list)
                  ;; (function (function &optional (list (list (list (list t))))) list) ; Compiler complains.
                  ))
  (mapcar (lambda (layoutsym)
            (cl-letf (((symbol-function 'daselt-coords--dfk-or-xkb-layout)
                       (lambda () layoutsym)))
              (funcall fun layoutsym)))
          layoutsyms))

;;;; Provide
(provide 'daselt-coords)
;;; daselt-coords.el ends here

