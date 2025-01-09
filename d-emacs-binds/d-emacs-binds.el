;;; d-emacs-binds.el --- Tools for the definition, manipulation and application of bindlists  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Alexander Prähauser

;; Author: Alexander Prähauser <ahprae@protonmail.com>
;; Keywords: "tools

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

;; 

;;; Code:

;;;; Preamble
(require 'd-emacs-base)
(require 'd-emacs-coords)

;;;; Bindlist formatting
;;;;; General
(defun d-head-if-exists (list)
  "Check if LIST has a head.
An element counts as a head if it isn't identified as a binding."
  (if (and (proper-list-p list) (not (d--binding-p list)))
      (if (d--binding-p (car list))
          nil
        (car list))))

;;;;; Modifiers
(defun d-index-prefix-modifiers (prefix &optional modlist)
  "Return a list of indexed modifiers in PREFIX.
The indexing is done according to the position of the modifier in MODLIST. If
MODLIST is not specified, `d-modifiers-list' is used."
  (unless (not prefix)
    (let ((modlist (if modlist modlist d-modifiers-list))
          (case-fold-search nil))
      (remq nil (mapcar (lambda (indmodifier)
                          (if (string-match-p (concat (char-to-string (cdr indmodifier))
                                                      "-")
                                              prefix)
                              indmodifier))
                        (d-add-list-indices modlist))))))

(defun d-index-and-sort-modifiers (mods &optional indexed modlist)
  "Index the modifiers in MODS based on their position in MODLIST and sort them.
The default MODLIST is `d-emacs-xkb-modifiers-list'.
If INDEXED is t, assume the MODS are already indexed and don't index them again."
  (let* ((modlist (or modlist d-modifiers-list))
         (indmods (if indexed
                      mods
                    (d-filter-by-predicate (d-add-list-indices modlist)
                                           (lambda (indmod) (member (cdr indmod) mods))))))
    (sort indmods
          :lessp (lambda (indmod1 indmod2)
                   (< (car indmod1)
                      (car indmod2))))))

(defun d-sort-modifiers (mods &optional retainidx modlist)
  "Sort modifiers in MODS; returns the modifiers without indices.
If RETAINIDX is true, retain the indices in the output.
MODLIST is the list of modifiers used for sorting, by default it is
`d-modifiers-list'."
  (let ((indmods (d-index-and-sort-modifiers mods retainidx modlist)))
    (mapcar (lambda (indmod)
              (nth 1 indmod))
            indmods)))

(defun d-modifiers-of-prefix (prefix &optional modlist keepindices)
  "Sort modifiers of PREFIX.
If MODLIST isprovided, it sorts against that instead of `d-modifiers-list'. If
KEEPINDICES is true, keep modifier indices."
  (let ((sorted (d-index-and-sort-modifiers (d-index-prefix-modifiers prefix modlist) t)))
    (if keepindices
        (if sorted sorted "") ; Let's return the empty string if there aren't any modifiers.
      (mapcar (lambda (indmod)
                (cdr indmod))
              (if sorted sorted "")))))

(defun d-string-together-modifiers (mods)
  "Concatenate the given list of MODS into a prefix."
  (cl-loop for mod in (reverse mods)
           concat (concat (char-to-string mod) "-")))

;;;;; Coordinates
(defun d--coords-from-binding (binding)
  "Retrieve coordinates associated with a BINDING if available.
Otherwise, return nil."
  (cond ((stringp (car binding))
         nil)
        ((and (consp (car binding)) (d-emacs-coords-p (cdar binding)))
         (cdar binding))
        ((d-emacs-coords-p (car binding)) (car binding))))

;;;;; Elaborate forms
(defun d--get-layout-matches-for-binding-string (str)
  "Match (indexed) layout entries against the last part of the string STR.
Return a cons of STR and the list of matching conses."
  (d-recursive-get-cons
   str
   (d-emacs-coords-coordinatize-layout
    (symbol-value (d-emacs-coords--dfk-or-xkb-layout)))
   (lambda (str compstr)
     (let ((case-fold-search nil))
       (string-match-p
        (rx-to-string

         ;; We have to put in a check to avoid partial matches of characters that don't actually match.
         `(: (or string-start
                 "-")
             ,(if (= 1 (length compstr))
                  compstr
                (car (last (split-string compstr "/"))))
             string-end))

        ;; If the tested string in the layout is longer than one character, split it along /'s. This is mostly for elements of d-emacs-xkb-layer-0. Let's just hope nobody ever defines a signal name containing /'s. But that seems unlikely.
        str)))
   t))

(defun d--get-unique-layout-match (str)
  "Obtain the correct match for STR from a list of potential layout matches.
Typically returns the longest match, excluding matches from layer 0 if others
are available."
  (let* ((matches (d--get-layout-matches-for-binding-string str))

         ;; Throw away 0-layer matches if another one exists.
         (redmatches (d-filter-by-predicate matches
                                            (lambda (match)
                                              (let* ((coords (car match))
                                                     (laycoord (car coords)))
                                                (not (= 0 laycoord))))))
         (matches (if redmatches redmatches matches)))

    (cond ((and (proper-list-p (cdr matches)) (cdr matches))
           (car (sort (cdr matches) (lambda (cons1 cons2)
                                      (if (> (length (cdr cons2))
                                             (length (cdr cons1)))
                                          cons2
                                        cons1)))))
          ((not (cdr matches))
           (prog2 (if d-mention-unmatched
                      (message "%s in %s is not matched by any signal in %s."
                               (car matches)
                               (current-buffer)
                               d-emacs-xkb-layout))
               (car matches)))
          (t (cdr matches)))))

(defun d--elaborate-on-bindstr (bindstr)
  "Transform a binding string BINDSTR into its elaborate form.
The binding is created by the position of the best match in the layout. If no
match is found, the suffix is converted into an elaborate binding."
  (let ((match (d--get-unique-layout-match bindstr)))
    (if match
        (let* ((matchstr (cdr match))
               (propermatchstr (car (last (string-split matchstr "/"))))
               (matchcoords (car match)))
          (cons (cons (d-modifiers-of-prefix
                       (string-remove-suffix propermatchstr bindstr)
                       nil t)
                      propermatchstr)
                matchcoords))
      (cons (cons (d-modifiers-of-prefix bindstr nil t) bindstr) nil))))

(defun d--elaborate-on-binding (binding)
  "Transform a d-emacs-xkb BINDING into its elaborate form.

If the binding is given by a binding string, it extracts the prefix, the suffix
and its corresponding coordinates from the string by matching the end of the
string against the symbols in the layout. If no matching suffix in the layout
given by d-emacs-xkb-layout is found, it tries to extract modifiers from the string
and returns the string along with the extracted modifiers and nil in place of
coordinates.

If the binding is given by a prefix and suffix, it adds coordinates
corresponding to the suffix.

Otherwise it adds empty strings so that the returned binding is always either of
the form

  (((PREFIX . SUFFIX) . COORDS) . VALUE)

or the original binding if its binding string could not be matched against any
symbol in the given layout."
  (let* ((value (cdr binding))

         (head (cond ((d--binding-suffix-form-p binding)
                      (let ((bindstr (car binding)))
                        (d--elaborate-on-bindstr bindstr)))

                     ;; Add coordinates corresponding to suffix if COORDSONLY is on.
                     ((d--binding-prefix-suffix-form-p binding)
                      (let* ((prefix (caar binding))
                             (prefixmods (d-modifiers-of-prefix prefix nil t))
                             (suffix (cdar binding))
                             (match (d--get-unique-layout-match suffix))
                             (coords (car match)))
                        (cons (cons prefixmods suffix) coords)))
                     ((d--binding-coords-form-p binding)
                      (cons (cons "" "") (car binding)))
                     ((d--binding-prefix-coords-form-p binding)
                      (cons (cons (d-modifiers-of-prefix (caar binding) nil t) "")
                            (cdar binding)))
                     ((d--binding-prefix-suffix-coords-form-p binding)
                      (cons (cons (d-modifiers-of-prefix (caaar binding) nil t)
                                  (cdaar binding))
                            (cdar binding)))
                     ((d--binding-elaborate-form-p binding) (car binding))
                     (t (error "%s in %s is an ill-formatted binding" binding (current-buffer)))))
         (elaborate-binding (cons head value)))
    elaborate-binding))

(defun d--reduce-binding (elbind &optional coordsonly)
  "Transform an elaborate binding ELBIND into its reduced form.
If COORDSONLY is given, use coordinates instead of suffixes whenever possible."
  (let* ((indmods (caaar elbind))
         (prefix (unless (not (caaar elbind))
                   (d-string-together-modifiers
                    (d-remove-indices indmods))))
         (suffix (cdaar elbind))
         (haspfx (and prefix (stringp prefix)
                      (not (string-empty-p prefix))))
         (hassfx (and suffix (stringp suffix)
                      (not (string-empty-p suffix))))
         (coords (cdar elbind))
         (value (cdr elbind))

         ;; Let's redefine COORDSONLY since we now know whether we have coordinates or not.
         (coordsonly (and coords coordsonly)))
    (cl-flet ((add-prefix-if-exists (arg)
                (if haspfx
                    (cons prefix arg)
                  arg)))
      (cons (if coordsonly (add-prefix-if-exists coords)
              (if hassfx (add-prefix-if-exists suffix)
                (if coords (add-prefix-if-exists coords)
                  (prog2 (message "Binding signal of %s in %s is empty." elbind
                                  (current-buffer))
                      nil))))
            value))))

;;;;; Comparison
(defun d--compare-standardized-modifier-lists (indmods1 indmods2)
  "Compare INDMODS1 and INDMODS2, two lists of standardized key modifiers.
 
Each list should be sorted and indexed by prefix as per
`d-sort-modifiers-by-prefix'. Return `(t)', `(nil)', or the string nil to
reflect how INDMODS1 compares to INDMODS2:

Return values and their meanings: - `(t)': INDMODS1 precedes INDMODS2 in the
sorted order. - `(nil)': INDMODS1 follows INDMODS2 in the sorted order. - nil:
INDMODS1 and INDMODS2 are equivalent.

The comparison is performed as follows: 1. Starting from the end, compare
corresponding pairs of modifiers in both lists. If the first differing pair is
found, return `(t)' if the modifier in INDMODS1 is less than that in INDMODS2,
or `(nil)' otherwise.

2. If both lists of modifiers are identical at this point, return nil.

3. If all modifiers in the shorter list match the corresponding elements in the
   longer list, but the lists are not equal in length, return `(t)' if INDMODS1
   is the shorter list (preceding by definition), or `(nil)' if INDMODS2 is
   shorter.

This function ensures consistent sorting of key modifiers lists by their
specificity and lexicographical order."
  ;; First compare modifiers over the length of the smaller.
  (let ((lengthcomp (cl-loop for indmod1 in (reverse indmods1)
                             for indmod2 in (reverse indmods2)
                             do (cond ((< (car indmod1) (car indmod2))
                                       (cl-return `(t)))
                                      ((> (car indmod1) (car indmod2))
                                       (cl-return `(nil)))))))
    (if lengthcomp
        lengthcomp

      ;; If the modifiers of the smaller are contained in those of the larger, test if they are equal.
      (unless (equal indmods1 indmods2)

        ;; If that's not the case, then the one with more modifiers should come last.
        (if (< (length indmods1) (length indmods2))
            `(t)
          `(nil))))))

(defun d--compare-coords (coords1 coords2)
  "This function compares two d-emacs-xkb-coordinates COORDS1 and COORDS2 lexically.
First it checks layer, then row, then place.
If it finds no difference between the coordinates it sends the string
    \"isequal\"."
  (cl-loop for coord1 in coords1
           and coord2 in coords2
           do (cond ((< coord1 coord2)
                     (cl-return `(t)))
                    ((> coord1 coord2)
                     (cl-return `(nil))))))

(defun d--compare-suffixes (suffix1 suffix2)
  "Compare SUFFIX1 to SUFFIX2.
If SUFFIX1 islonger than SUFFIX2, it signals `(nil)'. If SUFFIX2 is longer than
SUFFIX1, it signals `(t)'. If they have the same length, it comparison uses the
following rules: If one of them is capitalized and the other isn't, the one that
is capitalized comes last. Otherwise, it compares them according to their
constituent character codes."

  (d-compare-by-sequential-predicates
   suffix1 suffix2
   #'d-string-shorter-or-samep
   (lambda (sfx)
     (string= sfx (upcase sfx)))
   #'string<))

(defun d--compare-elaborate-bindings (elbind1 elbind2 &optional coordsonly)
  "Compare elaborate bindings ELBIND1 and ELBIND2.

If COORDSONLY is t, then this function doesn't consider suffixes in sorting.
Note that, since in this case the function has three arguments, it can't be
directly used as a :lessp function by `sort', but has to be surrounded by a
lambda to be used in a two-argument function.

The main use of this function is in `d--sort-and-format-bindlist', see there for
the sorting order."
  (let* ((srtmods1 (caaar elbind1))
         (srtmods2 (caaar elbind2))
         (suffix1 (cdaar elbind1))
         (suffix2 (cdaar elbind2))
         (hassfx1 (and suffix1 (stringp suffix1) (not (string-empty-p suffix1))))
         (hassfx2 (and suffix2 (stringp suffix2) (not (string-empty-p suffix2))))
         (coords1 (cdar elbind1))
         (coords2 (cdar elbind2))

         ;; Compare by modifiers.
         (compmods (d--compare-standardized-modifier-lists srtmods1
                                                           srtmods2)))

    ;; Look if suffixes exist in one case but not the other.
    (cl-flet ((true-and-not (val1 val2)
                (d-compare-if-decidable (lambda (vval1 vval2)
                                          (and vval1 (not vval2)))
                                        val1 val2)))

      (let* ((distinct-suffix-existence (true-and-not hassfx1 hassfx2))
             (distinct-coords-existence (true-and-not coords1 coords2)))

        (cond  ;; Unmatched bindings should come first.
         (distinct-coords-existence (not (car distinct-coords-existence)))

         (compmods (car compmods))

         ;; If they have the same prefixes and COORDSONLY isn't on, look if one of them has been given as a key combination string by comparing suffixes. A non-empty suffix indicates that that binding features a key combination string and should come before one than does not have a suffix.
         ((unless coordsonly distinct-suffix-existence)
          (car distinct-suffix-existence))

         ((and hassfx1 hassfx2 (not coordsonly))
          (let ((compsfx (d--compare-suffixes suffix1 suffix2)))
            (if compsfx (car compsfx)
              (message  "%s and %s have the same pre- and suffixes in %S."
                        elbind1 elbind2 (current-buffer)))))
         
         ((and coords1 coords2)
          (let ((compcoords (d--compare-coords coords1 coords2)))
            (if compcoords (car compcoords)
              (message "%s and %s have the same prefixes and coordinates in %S."
                       elbind1 elbind2 (current-buffer))))))))))

;;;;; Formatting
;;;;;; Lists
(defun d--sort-and-format-bindlist (blist &optional coordsonly prefun modlist)
  "Sort a d-emacs-xkb bindlist BLIST and format the result.

  Key combinations that are not matched by the layout in d-emacs-xkb-layout are put at
  the very top (because they are most likely errors or depreciated).

  Modifiers are ordered according to their order in MODLIST (d-modifiers-list by
  default). Sets of modifiers are ordered according to the modifier in them the
  furthest back in MODLIST and so are added below the modifier the furthest
  back.

  Key combinations given by a full combination string are listed before others
  and are ordered alphabetically (since they are supposed to be recalled
  phonetically or lexically, not positionally). Key combination strings with
  capital characters appear after those with downcased characters, and Greek
  letters appear after Latin ones. Combinations with symbols that are neither
  appear after either and are not otherwise sorted.

  Keys are ordered according to their layer, row and place in the row.

  With optional argument COORDSONLY, the function translates bindings that are
  given by a binding string into ones given by coordinates if that is possible,
  i.e. if the end of the binding string corresponds to a signal in the
  d-emacs-xkb-layout.

  PREFUN is a function that is applied to the bindlist after it is transformed
  into an elaborate bindlist. This is useful to apply functions that should act
  on an elaborate bindlist and whose results should be sorted, like coordinate
  transformations.

  To allow for using this function in `d--recursively-act-on-bindlist', it
  checks if the input is an atom or nil and, if so, it returns the input."
  (if (or (atom blist) (not blist))
      blist
    (let* ((modlist (if modlist modlist d-modifiers-list))
           (case-fold-search nil)

           ;; Bring bindings in elaborate form and sort contained bindlists.
           (elaborate-list (mapcar (lambda (elt)
                                     (cond ((d--binding-p elt)
                                            (d--elaborate-on-binding
                                             elt))
                                           ((atom elt) elt)
                                           (t (d--sort-and-format-bindlist
                                               elt coordsonly prefun modlist))))
                                   blist))

           ;; Do any function that should be applied before the sorting.
           (prefun-elaborate-list (if prefun (funcall prefun elaborate-list) elaborate-list))

           (sorted-list (sort prefun-elaborate-list :lessp
                              (lambda (elt1 elt2)
                                (cond ((atom elt1) t) ; Atoms should be at the beginning.
                                      ((atom elt2) nil)
                                      ((not (d--binding-p elt1)) t) ; Then contained lists.
                                      ((not (d--binding-p elt2)) nil)
                                      (t (d--compare-elaborate-bindings
                                          elt1 elt2 coordsonly))))))

           (formatted-list (d--format-sorted-bindlist sorted-list coordsonly))
           
           ;; We have to remove the prefixes of sorted elements because they are already in the suffix string.
           (formatted-sans-unmatched-prefixes-list
            (mapcar
             (lambda (potunmatch)
               (if (d--elaborate-unmatched-binding-p potunmatch)
                   (cons (cons (cons nil (cdaar potunmatch)) (cdar potunmatch))
                         (cdr potunmatch))
                 potunmatch))
             formatted-list))

           (final-list (mapcar (lambda (elt)
                                 (if (d--binding-p elt)
                                     (d--reduce-binding elt coordsonly)
                                   elt))
                               formatted-sans-unmatched-prefixes-list)))
      final-list)))

(defun d--format-sorted-bindlist (sblist &optional coordsonly)
  "Take a sorted bindlist SBLIST and format it.
That means inserting headings for unmatched elements, modifier combinations,
layers and rows.

If COORDSONLY is t, assume coordinates are prefered to suffixes when elaborate
bindings are reduced."
  (let (runlst)
    (cl-flet (;; Function to add paragraphs
              (hasstr (arg) (and arg (not (string-empty-p arg)))))

      (remq nil ; Remove if-clauses that are evaluated negatively.
            (cl-loop for n from 0 to (1- (length sblist))
                     do (let ((potbind (nth n sblist)))
                          (if (not (d--binding-p potbind))
                              (setq runlst (append runlst (list potbind)))

                            (if (or (= n 0)
                                    (not (d--binding-p (nth (1- n) sblist))))

                                (let* ((binding potbind)

                                       (indmods (caaar binding))
                                       (prefix (d-string-together-modifiers
                                                (d-remove-indices indmods)))
                                       (haspfx (hasstr prefix))
                                       
                                       (suffix (cdaar binding))
                                       (hassfx (hasstr suffix))
                                       
                                       (coords (cdar binding))
                                       (layer (car coords))
                                       (row (nth 1 coords)))
                                  
                                  (setq runlst
                                        (append runlst
                                                (list
                                                 (if (and coords (or coordsonly
                                                                     (not hassfx)))
                                                     "\n;;;;; Coordinates\n"
                                                   "\n;;;;; Strings\n")
                                                 
                                                 (if haspfx
                                                     (format ";;;;;; %s\n" prefix))

                                                 (if (and layer (or (not hassfx)
                                                                    coordsonly))
                                                     (format ";;;;;;; %s%s\n" prefix layer))
                                                 
                                                 (if (and row (or (not hassfx)
                                                                  coordsonly))
                                                     (format ";;;;;;;; %s%s-%s\n"
                                                             prefix
                                                             layer
                                                             row))

                                                 binding))))

                              (let* ((binding1 (nth (1- n) sblist))
                                     (binding2 (nth n sblist))
                                     (indmods1 (caaar binding1))
                                     (indmods2 (caaar binding2))
                                     ;; (prefix1 (d-string-together-modifiers
                                     ;;           (mapcar (lambda (indmod) (nth 1 indmod)) indmods2)))
                                     (prefix2 (d-string-together-modifiers
                                               (d-remove-indices indmods2)))
                                     (suffix1 (cdaar binding1))
                                     (suffix2 (cdaar binding2))
                                     (hassfx1 (hasstr suffix1))
                                     (hassfx2 (hasstr suffix2))
                                     (coords1 (cdar binding1))
                                     (coords2 (cdar binding2))
                                     (layer1 (nth 0 coords1))
                                     (layer2 (nth 0 coords2))
                                     (row1 (nth 1 coords1))
                                     (row2 (nth 1 coords2))

                                     (eqmatch (not (and (not coords1)
                                                        coords2)))
                                     (eqhssfx (or (and hassfx1 hassfx2)
                                                  (and (not hassfx1) (not hassfx2))))
                                     (eqpfx (or (and (not indmods1) (not indmods2))
                                                (and indmods1 indmods2
                                                     (not ; Nil means they are the same.
                                                      (d--compare-standardized-modifier-lists
                                                       indmods1
                                                       indmods2)))))
                                     (eqlay (or (and (not layer1) (not layer2))
                                                (and layer1 layer2 (= layer1 layer2))))
                                     (eqrow (or (and (not row1) (not row2))
                                                (and row1 row2 (= row1 row2)))))
                                
                                ;; The transition between strings and coordinates has to be placed differently depending on whether the suffixes are replaced by coordinates.
                                (if (or (and (not eqmatch) coordsonly)
                                        (and (not eqhssfx) (not coordsonly)))
                                    (setq runlst (append runlst
                                                         (list (d-generate-newlines 2)
                                                               (format
                                                                ";;;;; Coordinates")))))

                                (if (not eqpfx)
                                    (setq runlst (append
                                                  runlst
                                                  (if (d-string-exists-and-nonempty prefix2)
                                                      (list (d-generate-newlines
                                                             (if (or (and (not eqmatch)
                                                                          coordsonly)
                                                                     (and (not eqhssfx)
                                                                          (not coordsonly)))
                                                                 1
                                                               2))
                                                            (format
                                                             ";;;;;; %s"
                                                             prefix2))))))

                                (if (and (or (not eqpfx) (not eqlay))
                                         (or coordsonly
                                             (and (not hassfx1) (not hassfx2))
                                             (and (not coordsonly)
                                                  (not eqmatch))))
                                    
                                    (setq runlst (append runlst
                                                         (if layer2
                                                             (list (d-generate-newlines (if (and eqpfx
                                                                                                 eqmatch)
                                                                                            2
                                                                                          1))
                                                                   (format
                                                                    ";;;;;;; %s%s"
                                                                    prefix2
                                                                    layer2))))))

                                (if (and (not (and eqpfx eqlay eqrow))
                                         (or coordsonly
                                             (and (not hassfx1) (not hassfx2))))
                                    (setq runlst (append runlst
                                                         (if row2
                                                             (list (d-generate-newlines (if (and eqpfx
                                                                                                 eqlay)
                                                                                            2
                                                                                          1))
                                                                   (format
                                                                    ";;;;;;;; %s%s-%s"
                                                                    prefix2
                                                                    layer2
                                                                    row2))))))

                                (setq runlst (append runlst
                                                     (list (d-generate-newlines 1)
                                                           binding2)))))
                            ))
                     finally return runlst)))))

(defun d--sort-and-format-bindlists (&optional coordsonly prefun modlist directory)
  "Recurse through `d-emacs/pkg-configs' and format all bindlists within.
If PREFUN is specified, it denotes a function to run on each bindlist once its
bidings are in an elaborate form.

If COORDSONLY is t, prefer coordinates to suffixes when reducing elaborate
bindings.

 If MODLIST is given, use it to order modifiers instead of `d-modifiers-list'.

If `DIRECTORY' is given, it should be a subdirectory of `pkg-configs'. In that
case, recurse through `DIRECTORY'."
  (interactive)
  (d--act-on-pkg-files-by-type
   `(((lambda (filename) (d--sort-and-format-bindlists-in-file
                          filename ,coordsonly ,prefun ,modlist))
      .
      "bindlists"))
   (if directory directory)))

;;;;;; Strings
(defun d--sort-and-format-marked-bindlist-string (&optional coordsonly prefun modlist)
  "Sort and format a marked bindlist-string.
The function will read the contents of the selected region and process them
using `d--sort-and-format-bindlist' and
`d--format-bindlist-into-string-before-insertion', then replace the marked
region with the result.

COORDSONLY, PREFUN and MODLIST are passed forward to
`d--sort-and-format-bindlist'."
  (let* ((blist (d--extract-bindlist))
         (formattedblist
          (d--sort-and-format-bindlist blist coordsonly prefun modlist))
         (formattedstring (d--format-bindlist-into-string-before-insertion formattedblist)))
    (d-replace-region-with-arg formattedstring)
    (unless (eobp) (forward-char))))

(defun d--format-bindlist-into-string-before-insertion (blist &optional headname)
  "Convert BLIST into a formatted string for reinsertion.
If HEADNAME is provided, use that as the head for the converted structure.
Otherwise the headname of the list or the name of the containing folder is
used."
  (let* ((print-level nil)
         (print-length nil)
         (initialstring (format "%S" blist))

         ;; Apply some finishing touches through string operations.
         (str-with-unquoted-newlines
          (replace-regexp-in-string
           (rx (: "\"" (group (one-or-more "\n")) "\""))
           "\\1"
           initialstring))

         (str-with-unquoted-comments
          (replace-regexp-in-string
           (rx (: "\"" (group (zero-or-more "\n") ";;;;" (one-or-more ";")
                              (one-or-more not-newline)
                              (zero-or-one "\n"))
                  "\""))
           "\\1"
           str-with-unquoted-newlines))

         (str-with-comments-at-line-beginnings
          (replace-regexp-in-string
           (rx (minimal-match line-start blank (group ";;;;" (one-or-more (not "\"")) line-end)))
           "\\1"
           str-with-unquoted-comments))

         (str-with-points-for-functions
          (replace-regexp-in-string " function " " . #'"
                                    str-with-comments-at-line-beginnings))

         (str-with-points-for-ifs
          (replace-regexp-in-string " \\(if .*\)\\)" " . \(\\1\)"
                                    str-with-points-for-functions))

         (str-with-points-for-lambdas
          (replace-regexp-in-string " \\(lambda .*\)\\)" " . \(\\1\)"
                                    str-with-points-for-ifs))

         (str-with-points-for-dynamic-rebinds
          (replace-regexp-in-string " \\(d-emacs-dynamic-binding .*\)\\)" " . \(\\1\)"
                                    str-with-points-for-lambdas))

         (str-with-points-and-brackets-around-coords
          (replace-regexp-in-string
           (rx blank (group (zero-or-one "-") num blank
                            (zero-or-one "-") num blank
                            (zero-or-one "-") num)
               "\)")
           " \. \(\\1\)\)"
           str-with-points-for-dynamic-rebinds))

         (head (if (string-match (rx-to-string '(: string-start "("
                                                   (minimal-match
                                                    (group (or (one-or-more (or "-" letter))
                                                               (: "\"" (one-or-more not-newline) "\"")))
                                                    (one-or-more space))
                                                   (group (or "\(" ";" "\n"))))
                                 str-with-points-and-brackets-around-coords)
                   (substring str-with-points-and-brackets-around-coords
                              (match-beginning 1) (match-end 1))))

         (str-with-line-breaks-after-head
          (if head (replace-regexp-in-string
                    (rx-to-string '(: string-start "("
                                      (minimal-match
                                       (group (or (one-or-more (or "-" letter))
                                                  (: "\"" (one-or-more not-newline) "\"")))
                                       (one-or-more space))
                                      (group (or "\(" ";"))))
                    "\(\\1\n\\2"
                    str-with-points-and-brackets-around-coords)
            str-with-points-and-brackets-around-coords))

         (finalstring (concat (format "\n;;;; %s\n`"
                                      (if headname headname
                                        (if head
                                            head
                                          (let ((filename (buffer-file-name)))
                                            (if filename
                                                (concat
                                                 (format "%s-mode-map"
                                                         (d-containing-directory-base-name
                                                          filename))))))))
                              str-with-line-breaks-after-head)))
    finalstring))

;;;;;; Buffers
(defun d--delete-duplicate-comment-lines ()
  "Delete duplicate comment lines separated by blank lines in current buffer."
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (let* ((curline (thing-at-point 'line t))
             (curpos (line-end-position))
             (curtrimline (string-trim curline))
             (nextline "")
             (nexttrimline ""))
        (if (string-match-p (rx string-start ";") curtrimline)
            (cl-loop while (and (not (eobp))
                                (not (string-match-p (rx string-start (not ";"))
                                                     nexttrimline)))
                     do (progn (forward-line)
                               (setq nextline (thing-at-point 'line t))
                               (setq nexttrimline (string-trim nextline))
                               (if (string= curtrimline nexttrimline)
                                   (delete-region curpos (line-end-position))))))
        (goto-char curpos)
        (unless (eobp)
          (forward-line))))))

;;;;; Extraction
(defun d--extract-binding-string (binding &optional translate csectoshft doublebind)
  "Return a binding string or list of strings from Daselt-binding.

BINDING is expected in a specific form compatible with Daselt. Unless DOUBLEBIND
is t, the return value is a string representing the binding, potentially
adjusted based on the optional parameters TRANSLATE, CSECTOSHFT.

- If TRANSLATE is t, translate the binding using translation alists
  `d-emacs-key-translations-alist' and `d-stump-emacs-key-translations-alist'.
  
- If CSECTOSHFT is t, and the binding corresponds to the second layer with
  either no modifiers or one including `C-', replace the binding suffix with its
  downcased form and add an \"S-\" modifier.

- If DOUBLEBIND is t, check if the suffix of the binding or the key from
  coordinates matches the car of a cons cell in `d-emacs-double-symbs-alist'. If
  matched, form a second binding using the corresponding cdr to form the
  returned string. If `d-stump' is t, also check if a string in
  `d-stump-emacs-key-translations-alist' matches the current binding string.
  This is necessary to be able to apply discrete modifiers to translated
  bindings. In either case, return a list of all binding strings.

Signal an error if the binding is invalid (neither a suffix nor has matching
coordinates)."
  (let* ((elbind (d--elaborate-on-binding binding))
         (coords (cdar elbind))
         (sfx (cdaar elbind))
         (mods (d-remove-indices (caaar elbind)))
         (pfx (if coords ; If the binding is unmatched, then it has already its modifiers in its suffix.
                  (d-string-together-modifiers mods)
                ""))
         (coordval (if coords (d-emacs-coords-binding coords)))
         (newsfx ;; Let's put an error check here.
          (let ((newsfx (if (d-string-exists-and-nonempty sfx)
                            sfx
                          (if coords coordval))))
            (if (d-string-exists-and-nonempty newsfx)
                newsfx
              (error (if coords (format "Coordinates %s in binding %s have no match in %s."
                                        coords binding (d-emacs-coords--dfk-or-xkb-layout))
                       (format "%s has neither coordinates nor a suffix." binding))))))
         (non-translated-string (concat pfx newsfx))
         shifted) ; To check later whether it was shifted.

    ;; If on the second layer and csectoshft is t, and if either C is a modifier or there either are no mods and the length of the retained sfx is 1, replace the obtained sfx with its downcased variant and add S-modifier.
    (if (and csectoshft
             coordval
             (not (cl-member coordval
                             d-emacs-no-shift-list
                             :test #'string=))
             (= 2 (nth 0 coords))
             (or (and (not mods) (> (length coordval) 1))
                 (member ?C mods)))

        (setq coordval (downcase coordval)
              pfx (concat "S-" pfx)
              non-translated-string (concat pfx coordval)
              shifted t))
    
    (let* ((doubleval (if (and doublebind
                               (= 1 (length newsfx)))
                          (alist-get (string-to-char newsfx)
                                     d-emacs-double-symbs-alist)))
           (doublestr (if doubleval (concat pfx (char-to-string doubleval))))

           ;; The key combinations translated by Stump need to be double-bound when discrete mods are applied.
           (stumpdoublebind
            (if (and doublebind
                     d-stump
                     (not shifted)) ; Don't doublebind shifted things.
                (let ((discmods (cl-intersection mods d-discrete-modifiers-list)))
                  (if discmods
                      (let ((match (alist-get non-translated-string
                                              d-stump-emacs-key-translations-alist
                                              nil
                                              nil
                                              (lambda (ntstr carstr)
                                                (string-match-p ntstr carstr)))))
                        (if match
                            (concat (d-string-together-modifiers discmods) match)))))))

           (transstr (if translate
                         (let* ((st-trans
                                 (if d-stump (alist-get non-translated-string
                                                        d-stump-emacs-key-translations-alist
                                                        non-translated-string
                                                        nil
                                                        #'string=)
                                   non-translated-string))
                                (em-trans (if d-emacs-translate-keys
                                              (alist-get st-trans
                                                         d-emacs-key-translations-alist
                                                         st-trans nil #'string=)
                                            st-trans)))
                           em-trans))))

      (if doublebind
          (remq nil (list (if translate
                              transstr
                            non-translated-string)

                          (if doubleval doublestr)

                          (if (d-string-exists-and-nonempty stumpdoublebind)
                              stumpdoublebind)))
        (if translate transstr non-translated-string)))))



;; Work in progress.
;; (defun d--extract-binding-vector (binding &optional translate csectoshft doublebind)
;;   "This function takes a binding in Daselt-compatible form and returns the corresponding binding This.

;; TRANSLATE and DOUBLEBIND work as for #'d--extract-binding-string."
;;   (let* ((pfx (caar binding))
;;          (modifiers (d-modifiers-of-prefix pfx))
;;          (suffix (cdar binding))
;;          (coords (cdar binding))
;;          (bindingfromcoords (if coords (d-emacs-coords-binding coords) nil))
;;          (value (cdr binding))
;;          (base-vector (vconcat modifiers (list (or suffix bindingfromcoords))))
;;          (vector-after-doublebind (if (and doublebind (or suffix
;;                                                           (and coords (length=1 bindingfromcoords))))
;;                                       (let ((doubleval (alist-get (string-to-char (or suffix bindingfromcoords))
;;                                                                   d-emacs-double-symbs-alist)))
;;                                         (if doubleval
;;                                             (vconcat modifiers (list doubleval))
;;                                           base-vector))
;;                                     base-vector))
;;          (final-vector (if (and csectoshft bindingfromcoords
;;                                 (not (member bindingfromcoords d-emacs-no-shift-list))
;;                                 (= 2 (nth 0 coords))
;;                                 (or (not modifiers) (member 'control modifiers)))
;;                            (vconcat (vector 'shift) (downcase vector-after-doublebind))
;;                          vector-after-doublebind)))

;;     (if translate
;;         (let ((translated (translate-vector final-vector))) ; assuming translate-vector function
;;           translated)
;;       final-vector)))
(defun d--extract-bindlist (&optional noconstruct)
  "Extract the bindlist of a marked region.
If NOCONSTRUCT is t, extract only bindlists that are not constructed.
Constructed bindlists are distinguished by the fact that it is necessary to
evaluate them twice."
  (let* ((evalregion (eval (d-read-region))))
    (if (d--bindlist-p evalregion)
        evalregion
      (unless noconstruct
        (eval evalregion)))))

(defun d--extract-constant-cons ()
  "Extract a constant in a daselt-constants-file by evaluating the region."
  (eval (d-read-region)))

(defalias 'd--extract-advicelist 'd--extract-constant-cons
  "Extract an advicelist in a daselt-advicelist-file by evaluating the region.")

;;;;; Generation
(defun d--generate-define-key-strings-from-marked-bindlist ()
  "Create a `define-key' string for each binding in the currently marked bindlist."
  (let* ((blist (eval (d-read-region)))
         (map (car blist))
         (body (cdr blist)))
    (mapcar (lambda (binding)
              (concat "(define-key " map " (kbd \""
                      (d--escape-chars-in-str (d--extract-binding-string binding))
                      "\"\) "
                      (let ((bindval (cdr binding)))
                        (if (stringp bindval)
                            (concat "\"" bindval "\"")
                          (if (symbolp (eval bindval))
                              (concat "'" (symbol-name (eval bindval))))))
                      "\)\n"))
            body)))



;;;;; Saving bindlists
(defun d--save-bindlist-as-variable (blist)
  "Save BLIST as a variable.
Works similarly to `d-emacs--with-eval-backup-and-apply-bindlist' but does not
include a call to `d-emacs--apply-binding'."
  (let* ((pkgname (d-containing-directory-base-name (buffer-file-name)))
         ;; (pkgsymb (intern pkgname))
         (mapsymbdefaultname (concat pkgname "-mode-map"))
         
         (pkgdirname (file-name-directory (buffer-file-name)))
         (pkgdirnameparts (split-string pkgdirname "/"))

         (pfx (cl-loop for n from 0 to (1- (length pkgdirnameparts))
                       do (if (string= "pkg-configs" (nth n pkgdirnameparts))
                              (cl-return (nth (1+ n) pkgdirnameparts)))))
         (pfx (if (string-empty-p pfx)
                  "d"
                pfx))
         (prefix (concat pfx "-")))

    (if (not (d-head-if-exists blist))
        (let* ((filepath (buffer-file-name))
               (filename (file-name-nondirectory filepath))
               (filenamebase (file-name-base filepath))
               (symbname (if (d--special-file-p filename)
                             (substring filenamebase 0 -1)
                           (concat prefix
                                   (if (d--user-defined-file-p filename)
                                       "user-defined-"
                                     "")
                                   mapsymbdefaultname
                                   "-bindlist"))))
          (set (intern symbname)
               blist))

      (cl-flet* ((head-over-body (bblist)
                   (and (d-head-if-exists bblist)
                        (or (stringp (car bblist))  ; Let's ensure the head is a symbol
                            (symbolp (car bblist))) ; or string.
                        (not (d-head-if-exists (cdr bblist)))))

                 (name-if-symbol (elt)
                   (if (symbolp elt)
                       (symbol-name elt)
                     elt)))

        (if (head-over-body blist)
            (let ((namecore (name-if-symbol (car blist))))
              (set (intern (concat prefix namecore "-bindlist"))
                   blist))
          
          (d-funcalls-recursively
           blist
           `(((lambda (bblist &optional heads)
                (let* ((namecore (name-if-symbol bblist)))
                  (set (intern (concat prefix namecore "-bindlist"))
                       bblist)))
              .
              (lambda (bblist &optional heads)
                (and (d-head-if-exists bblist)
                     (or (stringp (car bblist))  ; Let's ensure the head is a symbol
                         (symbolp (car bblist))) ; or string.
                     (not (d-head-if-exists (cdr bblist)))))))
           (lambda (idx lst &optional _heads)
             (let ((elt (nth idx lst)))
               (and (not (atom elt))
                    (not (d--binding-p elt)))))))))))

;;;; Coordinate changes
(defun d--exchange-coordinates (coordlistlist &optional modlist coordsonly directory)
  "Exchange coordinates in all bindlists in `d-emacs/pkg-configs' or DIRECTORY.

COORDLISTLIST is a list of lists where the kth entry in the nth contained list
specifies the value that is to be used to replace the values matching the k-1th
entry. Entries matching the k-th entry are to be replaced by the k+1-th entry.
The last entry in the list is not replaced by any other entry but just remains
how it is if it is matched.

- For instance, to permute the first coordinate, specify the first list of
  COORDSLISTLIST as a list starting and ending with the same entry.

If you don't want to change a coordinate, you can feed the function nil instead
of a COORDLIST. The optional arguments are directly forwarded to
d--sort-and-format-bindlists, see the documentation there for their function.

MODLIST, COORDSONLY and DIRECTORY are forwarded to
`d-sort-and-format-bindlists'."
  (d--sort-and-format-bindlists
   coordsonly
   (lambda (blist) (d--change-coords-in-bindlist-during-sorting blist coordlistlist))
   modlist
   directory))

(defun d--change-coords-in-bindlist (blist coordlistlist)
  "Change coordinates in BLIST according to COORDLISTLIST.
Return the modified bindlist."
  (mapcar (lambda (bind) (if (d--binding-p bind)
                             (d--change-coords-in-binding bind coordlistlist)
                           (if (consp bind)
                               (d--change-coords-in-bindlist bind coordlistlist)
                             bind)))
          blist))

(defun d--change-coords-in-bindlist-during-sorting (blist coordlistlist)
  "Change coordinates in BLIST according to COORDLISTLIST.
Return the modified bindlist.

Note that unlike `d--change-coords-in-bindlist' this function does not recurse
into sub-lists of a bindlist. This is because it should be used as a prefun for
`d--sort-and-format-bindlist'. `d--sort-and-format-bindlist' already passes on
prefuns in its recursive calls, so if this function would recurse as well, the
coordinate change would be applied twice."
  (mapcar (lambda (bind) (if (d--binding-p bind)
                             (d--change-coords-in-binding bind coordlistlist)
                           bind))
          blist))

(defun d--change-coords-in-binding (bind coordlistlist)
  "Change coordinates in BIND according to COORDLISTLIST.
Return the modified binding."
  (if (stringp (car bind))
      bind
    (let* ((carcoordsp (d-emacs-coords-p (car bind)))
           (cdarcoordsp (unless carcoordsp (d-emacs-coords-p (cdar bind))))
           (origcoords (cond (carcoordsp (car bind))
                             (cdarcoordsp (cdar bind))))
           (newcoords (unless (not origcoords)
                        (d--change-coordlist origcoords coordlistlist)))
           (carrest (cond (carcoordsp nil)
                          (cdarcoordsp (caar bind))
                          (t (car bind))))
           (val (cdr bind)))
      (cond (carcoordsp (cons newcoords val))
            (cdarcoordsp (cons (cons carrest newcoords) val))
            (t (cons carrest val))))))

(defun d--change-coordlist (origcoords coordlistlist)
  "Change the coordinates in ORIGCOORDS based on the COORDLISTLIST.
ORIGCOORDS is a list of coordinates. COORDLISTLIST is a list of lists, each
inner list COORDLIST representing a set of coordinates.

Each coordinate in ORIGCOORDS is compared to the values in the COORDLIST in
COORDLISTLIST that has the same index. For the first matching coordinate in
COORDLIST, the function returns the next coordinate value from COORDLIST. If
no matching coordinate is found or the matching coordinate is the last entry in
COORDLIST, the function returns the original coordinate value from ORIGCOORDS."
  (mapcar (lambda (indcoord)
            (let ((coordlist (nth (car indcoord) coordlistlist)))
              (if coordlist
                  (cl-loop for n
                           from 0
                           to (- (length coordlist) 2)
                           do (if (= (nth n coordlist) (cdr indcoord))
                                  (cl-return (nth (1+ n)
                                                  coordlist)))
                           finally return (cdr indcoord))
                (cdr indcoord))))
          (d-add-list-indices origcoords)))

;;;; Drawing
(defun d-execute-in-maximized-maybe-temp-buffer (bufname fun)
  "Execute FUN in the buffer BUFNAME.
Maximize the created buffer window and ask whether to restore the previous
window configuration."
  (let ((display-buffer-alist '((".*" display-buffer-full-frame)))
        (windconf (current-window-configuration)))
    (with-current-buffer-window
        bufname
        nil
        (lambda (_a _b) (if (yes-or-no-p "Restore previous window configuration? ")
                            (set-window-configuration windconf)))
      (funcall fun))))



(defun d--placeval-from-elaborate-binding (elbind)
  "Return PLACEVAL whose car is coords of ELBIND and cdr is its cdr.
If ELBIND has no coordinates, return nil."
  (let ((coords (cdar elbind))
        (val (cdr elbind)))
    (if coords (cons coords val))))

(defun d--elbinds-matching-modifier-regexps (blist modrxs)
  "Return elaborate forms of bindings in BLIST matching MODS.
Filter bindings by modifier regexps MODRXS. A modifier regexp is a string
matched against all modifiers in a binding. If the regexp string starts with
`^', the binding is matched by the regexp if and only if no modifier in the
binding matches the string."
  (let* ((case-fold-search nil)
         (pblist (d-filter-by-predicate blist #'d--binding-p))
         (elblist (mapcar (lambda (bind)
                            (d--elaborate-on-binding bind))
                          pblist))
         (purelblist (d-filter-by-predicate elblist (lambda (bind)
                                                      (not (d--string-binding-p bind))))))

    (d-filter-by-predicate purelblist
                           (lambda (elbind)
                             (cl-flet* ((ispositive (modrx)
                                          (not (string-match-p (rx string-start "^")
                                                               modrx))))
                               (let* ((elbindmods (d-remove-indices
                                                   (caaar elbind)))
                                      (modstrs (mapcar #'char-to-string elbindmods)))
                                 (if (equal modrxs '(nil))
                                     t
                                   (cl-every
                                    (lambda (modrx)
                                      (if (ispositive modrx)
                                          (cl-member modrx modstrs :test #'string-match-p)
                                        (not (cl-member modrx modstrs
                                                        :test #'string-match-p))))
                                    modrxs))))))))

(provide 'd-emacs-binds)
;;; d-emacs-binds.el ends here

"
