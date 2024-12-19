;;;  d-xkb-functions.el --- Functions for Daselt's xkb module              -*- lexical-binding: t; -*-

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

;;  d-xkb functions.

;;; Code:

;;;; Preamble
(require 'cl-macs)
(require 'table)

(declare-function org-table-align "org-table" nil)
(declare-function d-execute-in-maximized-maybe-temp-buffer "d-functions" (bufname fun))
(declare-function d-remove-list-index "d-functions" (lst idx))
(declare-function d-filter-by-predicate "d-functions" (lst pred))
(declare-function d-string-exists-and-nonempty "d-functions" (str))
(declare-function d-forall-p "d-functions" (list predicate))
(declare-function org-sublist "org-compat" (list start end))
(declare-function d-add-list-indices "d-functions" (list &optional fromone))
(declare-function d-mark-line "d-functions" (&optional arg))
(declare-function d-cardinal "d-functions" (n &optional fromone))
(declare-function d-uppercase-p "d-functions" (str))

(defvar d-xkb-layer-boundaries)
(defvar d-xkb-layer-0-boundaries)
(defvar d-filter-by-predicate)
(defvar d-xkb-extended-layout)
(defvar d-xkb-special-key-names)
(defvar d-xkb-remaining-char-mappings)
(defvar d-xkb-layer-numbers-list)
(defvar d-xkb-file)
(defvar d-debug)
(defvar d-keep-read-buffers)

;;;; Reading d-xkb-file
(defun d-xkb--get-key-binding (beg end num keyname)
  "Extract the key binding on layer NUM for the key with name KEYNAME.
The region searched is that from BEG to END.
Returns nil if no binding is found."
  (goto-char beg)
  (if (search-forward keyname end t)
      (progn (beginning-of-line)
             (search-forward "\{" end t)
             (mark-sexp)
             (nth num (split-string
                       (buffer-substring (region-beginning)
                                         (region-end))
                       " " t)))
    nil))

(defun d-xkb--format-special-key (str)
  "Format a string STR describing a control character or function key.
Return a form suitable for Emacs."
  (concat "<" (replace-regexp-in-string "_" "-" (downcase str)) ">"))

(defun d-xkb--format-xkb-signal-name (rawsigname)
  "Format the signal name RAWSIGNAME read by d-xkb into a form usable by Emacs.
Removes spaces and NoSymbol's, converts Unicode characters, converts keypad
signal names, and replaces other signal names with their Emacs equivalents."
  (let ((signame rawsigname))
    (if rawsigname
        (progn
          ;; Remove tabs, spaces and commas if they exist.
          (setq signame (replace-regexp-in-string "\t" "" signame))
          (setq signame (remove ?\  signame))
          (setq signame (remove ?, signame))

          ;; Remove NoSymbol's.
          (cond
           ((string= signame "NoSymbol")
            (setq signame nil))

           ;; If it's a keypad-signal or a control character, downcase, put <> around it and replace _ with -.
           ((or (string-match-p "KP" signame)
                (member signame d-xkb-special-key-names))
            (setq signame (d-xkb--format-special-key signame)))

           ;; Try inserting the signal as a character.
           ((char-from-name signame t)
            (setq signame (char-to-string (char-from-name signame t))))

           ;; If the string starts with a U, is longer than one symbol and all letters are uppercase it's safe to assume it's the code of a unicode symbol and convert it to that symbol.
           ((and (d-uppercase-p signame) (string-match-p "U" (substring signame 0 1))
                 (> (length signame) 1))
            (setq signame (char-to-string (string-to-number (substring signame 1) 16))))

           ;; If it's a Greek letter, create the corresponding name and use it for insertion. Uppercase first.
           ((string-match-p "Greek" signame)
            (setq signame (let ((letter (nth 1 (split-string signame "_"))))
                            (if (d-uppercase-p letter)
                                (char-to-string
                                 (char-from-name (concat "greek capital letter " letter) t))
                              (char-to-string
                               (char-from-name (concat "greek small letter " letter) t))))))

           ;; Replace other symbols
           (t (cl-loop for cand in d-xkb-remaining-char-mappings
                       do (if (equal (car cand) signame)
                              (setq signame (cdr cand)))))
           )
          signame)
      nil)))

(defun d-xkb--inherit-from-parent-map (beg end laynum rownum keynum)
  "Lookup the key binding of a key in the parent map of the current map.
The current map is that within the range defined by BEG to END.
The key position is given by LAYNUM, ROWNUM and KEYNUM.
This works only if the parent map appears earlier in the file."
  (goto-char beg)
  (if (search-forward "include" end t)
      (progn (d-mark-line)
             (let* ((includelinelist (remove "" (split-string (remove ?\" (buffer-substring
                                                                           (region-beginning)
                                                                           (region-end)))
                                                              "\\(\(\\|\)\\|[ ]+\\|\_\\)" t)))
                    (parent (nth 2 includelinelist)))
               (nth (if (= 2 rownum) keynum (1- keynum))
                    (nth rownum (nth (1- laynum)
                                     (symbol-value
                                      (intern (concat "d-xkb-" parent "-layout"))))))
               ))
    nil))

;;;; Generate layouts
(defun d-xkb--generate-layer (beg end laynum)
  "Generate a list from a d-xkb layer.
The layer is defined by the region from BEG to END in `d-xkb-file'
and the layer number LAYNUM."
  (mapcar (lambda (rowletter)
            (let ((rowprefix (concat "A" rowletter)))
              (append (if (equal rowletter "B")
                          (list
                           (let ((binding (d-xkb--format-xkb-signal-name
                                           (d-xkb--get-key-binding beg end laynum "LSGT"))))
                             (if binding binding (d-xkb--inherit-from-parent-map beg end laynum 3
                                                                                 0))))
                        nil)
                      (mapcar (lambda (keynum)
                                (let* ((keynumstr (if (eq keynum 10)
                                                      (number-to-string keynum)
                                                    (concat "0" (number-to-string keynum))))
                                       (keyname (concat rowprefix keynumstr))
                                       (binding (d-xkb--format-xkb-signal-name
                                                 (d-xkb--get-key-binding beg end laynum keyname))))
                                  (if binding binding
                                    (d-xkb--inherit-from-parent-map
                                     beg end laynum (cond  ((string= rowletter "D") 1)
                                                           ((string= rowletter "C") 2)
                                                           ((string= rowletter "B") 3))
                                     keynum))))
                              (d-cardinal 10 t)))))
          '("D" "C" "B")))

(defun d-xkb--generate-layout-from-layers (string)
  "Generate a layout constant called d-xkb-STRING-layout.
The layout is a list containing values from layers defined as
d-xkb-STRING-layer-ELT for elements ELT in d-xkb-layer-numbers-list. If a
specific layer is not defined, it defaults to d-xkb-layer-ELT."
  (let ((layout (concat "d-xkb-" string "-layout")))
    (unless (boundp (intern layout))
      (set (intern layout)
           (mapcar (lambda (elt)
                     (if (boundp (intern (concat "d-xkb-" string "-layer-" (number-to-string elt))))
                         (symbol-value (intern (concat "d-xkb-" string "-layer-" (number-to-string elt))))
                       (symbol-value (intern (concat "d-xkb-layer-" (number-to-string elt))))))
                   d-xkb-layer-numbers-list)))))
(defun d-xkb--generate-layouts ()
  "Generate lists from layouts defined in the d-xkb file.
This function searches for `xkb_symbols', marks the line, extracts layout names,
and processes them with `d-xkb--generate-layer' to define the layout."
  (let ((dxkbbuf (find-file-noselect d-xkb-file)))
    (set-buffer dxkbbuf)
    (goto-char (point-min))
    (while (search-forward "xkb_symbols" nil t)
      (let ((linebeg (prog2 (beginning-of-line) (point))))
        (progn (d-mark-line)
               (let ((laynameful
                      (nth 1 (split-string
                              (buffer-substring (region-beginning) (region-end)) " "))))
                 (if (string-match-p "base" laynameful)
                     (let ((layname (remove ?\" (car (split-string laynameful "_")))))
                       (progn (goto-char linebeg)
                              (search-forward "\{" nil t)
                              (backward-char)
                              (mark-sexp)
                              (let ((laybeg (region-beginning)) (layend (region-end)))
                                (set (intern (concat "d-xkb-" layname "-layout"))
                                     (mapcar (lambda (laynum)
                                               (d-xkb--generate-layer laybeg layend laynum))
                                             d-xkb-layer-numbers-list))))))))))
    (unless (or d-debug d-keep-read-buffers) (kill-buffer dxkbbuf))))

;;;; Coordinates
;;;;; Transformations
(defun d-xkb--rel-from-abs-coords (coords &optional extt)
  "Transform absolute coordinate COORDS of a key into relative ones.
If EXTT is non-nil, assume coordinates are relative to an extended layout with a
0-th layer."
  (let ((layer (nth 0 coords))
        (row (nth 1 coords))
        (place (nth 2 coords)))
    (cl-flet* ((iextt (a b) (if extt a b))
               (ix1+ (a) (iextt (1+ a) a)))

      (list (setq layer (cond ((and extt (= layer 0)) 0)
                              ((= layer (ix1+ 0)) 1)
                              ((= layer (ix1+ 1)) 2)
                              ((= layer (ix1+ 2)) 5)
                              ((= layer (ix1+ 3)) 6)
                              ((= layer (ix1+ 4)) 3)
                              ((= layer (ix1+ 5)) 7)
                              ((= layer (ix1+ 6)) 4)
                              ((= layer (ix1+ 7)) 8)))

            (prog2 (unless (= layer 0) (setq extt nil)) ; We assume here the upper layers of an extended layout are not extended.
                (setq row (- row (ix1+ 1))))

            (setq place
                  (cond ((= row -2)
                         (cond ((< place 5)
                                (- place 6))
                               ((= place 5)
                                (- place 5))
                               ((> place 5)
                                (- place 4))))
                        ((<= row 0)
                         (if (> place (ix1+ 4))
                             (- place (ix1+ 4))
                           (- place (ix1+ 5))))
                        ((= row 1)
                         (cond ((< place (ix1+ 5)) (- place (ix1+ 6)))
                               ((= place (ix1+ 5)) 0)
                               ((> place (ix1+ 5)) (- place (ix1+ 4)))))
                        ((= row 2)
                         (1- place))))))))

(defun d-xkb--abs-from-rel-coords (coords &optional extt)
  "Transform relative coordinates COORDS to absolute ones.
If EXTT is t, assume the layout is extended."
  (let ((layer (nth 0 coords))
        (row (nth 1 coords))
        (place (nth 2 coords)))
    (cl-flet* ((iextt (a b) (if extt a b))
               (ix1+ (a) (iextt (1+ a) a)))

      (list (cond ((= layer 0) 0)
                  ((= layer 1) (ix1+ 0))
                  ((= layer 2) (ix1+ 1))
                  ((= layer 5) (ix1+ 2))
                  ((= layer 6) (ix1+ 3))
                  ((= layer 3) (ix1+ 4))
                  ((= layer 7) (ix1+ 5))
                  ((= layer 4) (ix1+ 6))
                  ((= layer 8) (ix1+ 7)))

            (prog2 (unless (= layer 0) (setq extt nil)) ; We assume here the upper layers of an extended layout are not extended.
                (+ row (ix1+ 1)))

            (cond ((= row -2)
                   (cond ((< place 0)
                          (+ place 6))
                         ((= place 0)
                          (+ place 5))
                         ((> place 0)
                          (+ place 4))))
                  ((<= row 0)
                   (if (< place 0)
                       (+ place (ix1+ 5))
                     (+ place (ix1+ 4))))
                  ((= row 1)
                   (cond ((< place 0)
                          (+ place (ix1+ 6)))
                         ((= place 0)
                          (+ place (ix1+ 5)))
                         ((> place 0)
                          (+ place (ix1+ 4)))))
                  ((= row 2)
                   (1+ place)))))))

;;;;; Coordinatization
(defun d-xkb-coordinatize-layout (layout &optional extt)
  "Add relative coordinates to every element of the d-xkb-layout LAYOUT.
Resulting layout places are pairs of coordinates and their corresponding key
symbol. If EXTT is non-nil, assume the layout is extended."
  (cl-flet* ((iextt (a &optional b) (if extt a b)))
    (mapcar (lambda (laynum)
              (mapcar (lambda (indrow)
                        (mapcar (lambda (indsymb)
                                  (cons (d-xkb--rel-from-abs-coords
                                         (list laynum
                                               (car indrow)
                                               (car indsymb))
                                         extt)
                                        (cdr indsymb)))
                                (d-add-list-indices (cdr indrow))))
                      (d-add-list-indices (nth (- laynum (iextt 0 1)) layout))))
            (append (iextt `(0)) d-xkb-layer-numbers-list))))

;;;;; Layers
(defun d-xkb-get-layer (coordlayout laynum)
  "Get the layer LAYNUM from a coordinatized layout COORDLAYOUT.
Remove the layer coordinate from each coordinate list."
  (mapcar (lambda (row)
            (mapcar (lambda (place)
                      (let ((coords (car place)))
                        (cons (cl-subseq coords 1 (1- (length coords))) (cdr place))))
                    row))
          (alist-get laynum (mapcar (lambda (layer)
                                      (cons (caaaar layer) layer))
                                    coordlayout))))

;;;;; Predicates
(defun d-xkb--coordinates-p (list)
  "Return t if LIST is a valid d-xkb coordinate list.
This means it consists entirely of numbers."
  (and (proper-list-p list)
       (d-forall-p list #'numberp)))

;;;;; Strings and Coords
(defun d-xkb--binding-from-coords (coords &optional extlayout)
  "Retrieve the xkb key binding name as a string from COORDS.
Use EXTENDED LAYOUT or `d-xkb-extended-layout' by default. COORDS should contain
three numbers:

- the layer number (0 for the modifier layer).

- the row number, where the middle row is numbered 0 and upper rows are
negative.

- the relative position of the key with respect to the central axis. The B-key
in a QWERTY keyboard is numbered 0, the keys next to it -2 and 2."
  (let* ((abscoords (d-xkb--abs-from-rel-coords coords t))
         (extlayout (if extlayout extlayout (symbol-value d-xkb-extended-layout)))
         (unsplitbind (nth (nth 2 abscoords)
                           (nth (nth 1 abscoords)
                                (nth (nth 0 abscoords) extlayout))))
         (lastbindpart (car (last (split-string unsplitbind "/"))))
         (bind (if (d-string-exists-and-nonempty lastbindpart)
                   lastbindpart
                 unsplitbind)))
    bind))

(defun d-xkb-string-together-coords (coords)
  "Concatenate a list of coordinates COORDS into a string."
  (if (= (length coords) 1)
      (number-to-string (car coords))
    (let ((runstr ""))
      (cl-loop for coord in (reverse (cdr coords)) ; We shouldn't have a - after the last coord
               do (setq runstr (concat runstr (number-to-string coord) "-"))
               finally (setq runstr (concat runstr (number-to-string (car coords))))
               finally return runstr))))

;;;; Placevals
;;;;;; Regexp matching
(defun d-xkb-placevals-matching-coordrx (placevals coordrx)
  "Retrieve all place values in PLACEVALS whose coordinates match COORDRX."
  (d-filter-by-predicate placevals (lambda (placeval)
                                     (let ((coords (car placeval)))
                                       (string-match-p coordrx (format "%s" coords))))))

(defun d-xkb-placevals-matching-indexed-rx (placevals idx coordrx)
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
    (d-xkb-placevals-matching-coordrx
     placevals
     finalrx)))

;;;;;; Index manipulation
(defun d-xkb-remove-index-from-placevals (placevals idx)
  "Remove IDX from the coordinates of each placeval in PLACEVALS.
Return the modified list."
  (mapcar (lambda (placeval)
            (let ((coords (car placeval))
                  (val (cdr placeval)))
              (cons (d-remove-list-index coords idx)
                    val)))
          placevals))

(defun d-xkb-find-coord-values-at-index (placevals idx)
  "Find all values the coordinates PLACEVALS have at IDX."
  (mapcar (lambda (placeval)
            (let ((coords (car placeval)))
              (nth idx coords)))
          placevals))


;;;;;; Extraction
(defun d-xkb-extract-value-string (val)
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

;;;;;; Drawing
(defun d-xkb-draw-placevals (placevals &optional drawfull runcoords org)
  "Draw a keyboard layout from PLACEVALS.
PLACEVALS is a list of cons cells containing coordinates and a value.

Each PLACEVAL is a cons cell where the car is a pair of coordinates and the cdr
is a value that can be inserted or evaluated into an insertable form.

If DRAWFULL is non-nil, specified tables are always sized according to the
corresponding layer in `d-xkb-extended-layout`. It can also be a cons of two
conses, specifying the minimum and maximum number of rows and columns in the
layout.

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
                        (insert (format "\n%s\n" (d-xkb-string-together-coords runcoords)))

                        (progn (d-xkb-draw-placevals (d-xkb-remove-index-from-placevals
                                                      (d-xkb-placevals-matching-indexed-rx
                                                       placevals 0 (number-to-string firstcoord))
                                                      0)

                                                     ;; If DRAWFULL is t and doesn't provide coordinates we have to provide the layer rows and columns here since the layer coordinate gets deleted when the recursion begins.
                                                     (if (and drawfull
                                                              (atom drawfull)
                                                              (= coordcard 3))
                                                         (if (= 0 firstcoord)
                                                             d-xkb-layer-0-boundaries
                                                           d-xkb-layer-boundaries))
                                                     runcoords
                                                     org)
                               (insert "\n"))))

                    (cl-remove-duplicates (d-xkb-find-coord-values-at-index placevals 0)))

          (let* ((coordlist (mapcar (lambda (placeval)
                                      (car placeval))
                                    placevals))

                 ;; Use the last coordinate to calculate columns.
                 (colcoords (unless drawfull ; Unneccessary if DRAWFULL.
                              (mapcar (lambda (coords)
                                        (car (last coords)))
                                      coordlist)))
                 (firstcol (if drawfull
                               (cadr drawfull)
                             (apply #'min colcoords)))
                 (lastcol (if drawfull
                              (cddr drawfull)
                            (apply #'max colcoords)))
                 (colnum (1+ (- lastcol firstcol)))

                 ;; Similar for rows.
                 (rowcoords (unless drawfull
                              (if (= 2 coordcard)
                                  (mapcar (lambda (coords)
                                            (car coords))
                                          coordlist)
                                1)))
                 (firstrow (if drawfull
                               (caar drawfull)
                             (apply #'min rowcoords)))
                 (lastrow (if drawfull
                              (cdar drawfull)
                            (apply #'max rowcoords)))
                 (rownum (1+ (- lastrow firstrow))))

            (let ((print-level nil)
                  (print-length nil))

              (if org
                  (d-xkb-draw-daselt-org-table colnum rownum firstrow lastrow firstcol lastcol placevals)
                (d-xkb-draw-daselt-table colnum rownum firstrow lastrow firstcol lastcol placevals))))

          (goto-char (point-max))))))

;;;;;;; Auxiliary
(defun d-xkb-draw-daselt-table (colnum rownum firstrow lastrow firstcol lastcol placevals)
  "Draw a daselt-table using table.el.
An auxiliary function to `d-xkb-draw-placevals'. See there for more
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
                                    (valstr (d-xkb-extract-value-string val)))
                               (if (not (string-empty-p valstr))
                                   valstr
                                 (format "%s" val)))))
                       do (table-forward-cell 1))))

(defun d-xkb-draw-daselt-org-table (colnum _rownum firstrow lastrow firstcol lastcol placevals)
  "Draw a daselt-table in org-mode.
An auxiliary function to `d-xkb-draw-placevals'. See there for more
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
                                     (valstr (d-xkb-extract-value-string val)))
                                (insert (if (not (string-empty-p valstr)) valstr " "))
                                (insert " |")) ; End of cell
                           )
                  (insert "\n|-\n")))
    (set-mark (point))
    (goto-char pos)
    (org-table-align)))


(defun d-xkb-draw-placevals-in-temp-buffer (placevals &optional drawfull runcoords org)
  "Execute `d-xkb-draw-placevals' in a maximized temporary buffer.
All arguments are forwarded to `d-xkb-draw-placevals'.
See there for more documentation."
  (d-execute-in-maximized-maybe-temp-buffer
   "*daselt-layout*" (lambda () (d-xkb-draw-placevals placevals drawfull runcoords org)
                       (org-mode))))


;;;; Provide
(provide 'd-xkb-functions)
;;; d-xkb-functions.el ends here
