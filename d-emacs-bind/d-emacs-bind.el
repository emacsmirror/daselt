;;; d-emacs-bind.el -- Tools for the definition, manipulation and application of bindlists  -*- lexical-binding: t; -*-

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

;; d-emacs-bind.el is a comprehensive utility library designed to facilitate the
;; creation, management, and application of bindlists within Emacs. Bindlists
;; are structured lists of key bindings that allow for complex and organized
;; keybinding configurations using key coordinates or keystrings.

;; Key Features: - **Definition and Validation**: Provides predicates to verify
;; the structure of bindlists, ensuring they adhere to the expected format for
;; reliable manipulation.

;; - **Manipulation Tools**: Includes functions to sort and format bindlists
;;   based on modifier order, key coordinates, and user-defined criteria. This
;;   ensures that key bindings are organized systematically, enhancing
;;   readability and maintainability.

;; - **Key Translation**: Offers mechanisms to translate key bindings to
;;   circumvent terminal conflicts and integrate seamlessly with window managers
;;   like StumpWM. Users can define custom key translation mappings to preserve
;;   intended key behaviors across different environments.

;; - **Coordinate-Based Mappings**: Supports keybindings defined by coordinates,
;;   allowing for precise control over key actions based on their position
;;   within the keyboard layout.

;; - **Integration with Daselt**: As part of the Daselt suite, d-emacs-bind.el
;;   leverages shared utilities and follows standardized conventions, ensuring
;;   compatibility and ease of use within the Daselt ecosystem.

;; - **Customization Options**: Offers a range of customizable variables to
;;   tailor modifier orders, translation behaviors, and binding formats to meet
;;   individual user preferences and workflow requirements.

;; This library is ideal for Emacs users seeking advanced keybinding management,
;; enabling the creation of sophisticated and conflict-free keybinding setups.
;; Whether you're customizing your workflow, integrating with specialized window
;; managers, or managing complex modifier combinations, d-emacs-bind.el provides
;; the necessary tools to streamline and enhance your Emacs keybinding
;; experience.

;;; Code:

;;;; Preamble
(require 'd-emacs-base)
(require 'd-emacs-coords)
(require 'cl-macs)
(require 'subr-x)

(declare-function d-stump-translated-emacs-keys "d-stump-functions" nil)
(declare-function d-emacs-read-region "d-emacs-base" (&optional properties))
(declare-function d-emacs-funcalls-recursively "d-emacs-base" (obj funtests &optional recursetest formatfun eltcolfun lstcolfun restargs restargfun contt debug))
(declare-function d-emacs--escape-chars-in-str "d-emacs-base" (str))
(declare-function d-emacs-string-exists-and-nonempty "d-emacs-base" (str))
(declare-function d-emacs-generate-newlines "d-emacs-base" (k))
(declare-function d-emacs-compare-if-decidable "d-emacs-base" (test arg1 arg2))
(declare-function d-emacs-coords-binding "d-emacs-coords" (coords &optional layout wholebinds))
(declare-function d-emacs-containing-directory-base-name "d-emacs-base" (filepath))
(declare-function d-emacs-replace-region "d-emacs-base" (arg))
(declare-function d-emacs-compare-by-sequential-predicates "d-emacs-base" (arg1 arg2 &rest predicates))
(declare-function d-emacs-remove-indices "d-emacs-base" (indlst))
(declare-function d-emacs-coords--dfk-or-xkb-layout "d-emacs-coords" nil)
(declare-function d-emacs-coords-coordinatize-layout "d-emacs-coords" (layout))
(declare-function d-recursive-get-cons "d-emacs-base" (obj allist &optional testfn reverse))
(declare-function d-emacs-filter-list "d-emacs-base" (lst pred))
(declare-function d-emacs-index "d-emacs-base" (list &optional fromone))
(declare-function d-emacs-coords-p "d-emacs-coords" (list))
(declare-function d-emacs-exists-p "d-emacs-base" (list predicate))
(declare-function d-emacs-leq-p "d-emacs-base" (seq1 seq2))

;;;; Constants
(defconst d-emacs-bind-modifiers-list
  (list ?C ?H ?M ?S ?s ?A)
  "List of modifiers in their standard order in Daselt.

Note that this is different from the standard order of modifiers in Emacs.")

(defconst d-emacs-bind-discrete-modifiers-list
  (list ?M ?s ?A)
  "List of discrete modifiers in their standard order in Daselt.")

(defconst d-emacs-bind-escape-kbd-regexps-list
  `(,(rx (group ",")) ,(rx (group ".")) ,(rx (group (syntax string-quote))))
  "List of character strings that should be escaped.
Used by functions like `d--generate-key-strings-from-marked-bindlist'")

(defconst d-emacs-bind-layers-to-shift-list
  '(2 8)
  "Layers to which a shift modifier should be added when `d-emacs-bind-string'
is called with CSECTOSHFT set.")

(defconst d-emacs-bind-no-shift-list
  '("'")
  "List of strings on layers in `d-emacs-bind-layers-to-shift-list'
 that should not be replaced by their downcased version with a
shift modifier when `d-emacs-bind-string' is called with `csectoshft' set to
t.")

;;;; Customs
(defgroup d-emacs-bind
    nil
    "Containing group for d-emacs-bind."
    :group 'd-emacs)

(define-widget 'bindlist 'lazy
  "A d-emacs-bindlist."
  :offset 4
  :tag "Bindlist"
  :type '(restricted-sexp :match-alternatives (#'d-emacs-bind-bindlist-p)))

(defcustom d-emacs-bind-translate-keys
  (if (> (string-to-number (substring emacs-version 0 (string-match-p "\\." emacs-version))) 29) t nil)
  "Enable translation of keys defined in `d-emacs-bind-key-translations-alist'.
This translation is intended for Emacs versions 30 or higher (29 may
also work) to address terminal translations that conflict with key
bindings. When active, use the translated key combinations in bindings."
  :type 'boolean
  :group 'd-emacs)

(defcustom d-emacs-bind-translate-choices
  t
  "Replace `y' and `n' in multiple-choice queries with alternative values.
If a query uses symbols at coordinates (1 0 2) or (1 0 -2), replace them with
the values at coordinates (5 0 2) or (5 0 -2), typically unused Greek letters."
  :type 'boolean
  :group 'd-emacs)

(defcustom d-emacs-bind-translate-C-1-1--2-C-g
  nil
  "If non-nil, translate `C-g' to C-(1 1 -2) and vice versa.
Note that the `C-g' function to stop running processes cannot be translated, so
the option is disabled by default."
  :type 'boolean
  :group 'd-emacs-bind)

(defcustom d-emacs-bind-key-translations-alist
  `(("C-m" . "C-á") ("C-i" . "C-ĥ") ("C-[" . "C-é"))
  "List of key translations to circumvent terminal interference.
Each element is a cons cell where the car is a key combination
to be translated and the cdr is the desired translation. For
example, on terminals like xterm, `C-i' may be translated to TAB.
Setting `d-emacs-bind-translate-keys' to t will use these translations
to preserve intended key bindings."
  :type '(repeat (cons string string))
  :group 'd-emacs-bind)

(defcustom d-emacs-bind-double-symbs-alist
  '((?Ͳ . ?ͳ) (?Ϙ . ?ϙ))
  "Alist of symbol translations for elaborate binding suffixes.
If the first symbol in a cons cell is the suffix of the elaborate
form of a binding in a bindlist, the same binding should apply to the
second symbol as well."
  :type '(repeat (cons character character))
  :group 'd-emacs-bind)

(defcustom d-emacs-bind-overwrite-translate-keys
  nil
  "Determine if keys in `d-emacs-bind-key-translations-alist' should be
overwritten.
When non-nil, bindings with the original keys will be overwitten. When nil,
bindings will use an A-Modifier instead of a C-modifier."
  :type 'boolean
  :group 'd-emacs-bind)

(defcustom d-emacs-bind-mention-unmatched
  nil
  "Notify when a suffix in a keybind is not in `d-emacs-xkb-layout'.

Useful for users who import their keybinds, as it highlights unmatched suffixes."
  :type 'boolean
  :group 'd-emacs-bind)

(defcustom d-emacs-bind-outside-translations-alist
                                            nil
                                            "Alist of key combinations that are translated from outside to Emacs.

Automatically generated from the contents of the remapped-keys-file.

If you have d-emacs-stump, you can use `d-stump-translated-emacs-keys'
to set this.

Automatically set when starting `d-emacs-mode' if `d-stump' is t."
                                            :type '(alist :key-type string :value-type string)
                                            :group 'd-emacs-bind)

(defcustom d-emacs-bind-replace-untranslated-keys
        (not (bound-and-true-p d-emacs-stump))
        "Set to t if you are not using key translation but want to have the
would-be-translated key combinations to be replaced by ones in which the `C-'
modifier is replaced by an `A-'modifier."
        :type 'boolean
        :group 'd-emacs-bind)

;;;; Functions
;;;;; Predicates
(defun d-emacs-bind-bindlist-p (cand)
  "Return t if CAND is a bindlist.
The way used to test this is by recursing through CAND until a binding is
found."
  (if (atom cand)
      nil
    (cl-loop for elt in cand
             do (if (d-emacs-bind-p elt)
                    (cl-return t)
                  (unless (atom elt)
                    (if (d-emacs-bind-bindlist-p elt)
                        (cl-return t)))))))

(defun d-emacs-bind--bindlist-symb-p (symb)
  "Return t if SYMB is a bindlist symbol.
This is tested by looking at whether the name of SYMB ends in `-bindlist', SYMB
is a bound variable and the value of SYMB returns t when tested with
`d-emacs-bind-bindlist-p'."
  (and (string-match-p (rx "-bindlist" string-end)
                       (symbol-name symb))
       (boundp symb)
       (d-emacs-bind-bindlist-p (symbol-value symb))))

(defun d-emacs-bind--string-binding-p (cns)
  "Return t if CNS is a binding given by a binding string."
  (and (d-emacs-bind-p cns)
       (not (d-emacs-coords-p (car cns)))
       (not (d-emacs-coords-p (cdar cns)))))

(defun d-emacs-bind--recursively-check-if-binding-cons-p (cns)
  "Check if CNS looks like the car of a binding.
If not, look at whether CAR is a cns, and, if so, apply yourself to it.
Moreover, if the CNS has more than one element, apply yourself to the second
element. This is necessary for a binding predicate that still allows the cdr of
the binding to be arbitrary, here with the restriction that it cannot contain
another binding form."
  (and (consp cns)
       (or (d-emacs-bind--binding-location-p cns)
           (d-emacs-bind--recursively-check-if-binding-cons-p (car cns))
           (if (proper-list-p (cdr cns))
               (d-emacs-exists-p (cdr cns) #'d-emacs-bind--recursively-check-if-binding-cons-p)))))

(defun d-emacs-bind--suffix-form-p (cns)
  "Return t if CNS looks like a binding in suffix form.
This means its car is a string, and it is either not a proper list or its second
element is not a binding."
  (condition-case nil
      (and (consp cns)
           (atom (car cns))
           (stringp (car cns)))
    (error nil)))

(defun d-emacs-bind--prefix-suffix-form-p (cns)
  "Return t if CNS looks like a binding in prefix-suffix-form.
This means its car is a cons of two strings, and it is either not a proper list
or its second element is not a binding."
  (condition-case nil
      (and (consp cns)
           (consp (car cns))
           (stringp (caar cns))
           (stringp (cdar cns)))
    (error nil)))

(defun d-emacs-bind--coords-form-p (cns)
  "Return t if CNS looks like a binding in coords-form.
This means its car is a cns for which `d-emacs-coords-p' is t, and it is
either not a proper list or its second element is not a binding."
  (condition-case nil
      (and (consp cns)
           (d-emacs-coords-p (car cns)))
    (error nil)))

(defun d-emacs-bind--prefix-coords-form-p (cns)
  "Return t if CNS is a binding in prefix-coords form.
This means that the car must be a cons of a string (the prefix) and a
d-emacs-xkb coordinate list."
  (and (consp cns)
       (consp (car cns))
       (stringp (caar cns))
       (d-emacs-coords-p (cdar cns))))

(defun d-emacs-bind--prefix-suffix-coords-form-p (cns)
  "Return t if CNS looks like a binding in prefix-suffix-coords-form.
This means its car is cons whose car is a cons of two strings and whose cdr is
either nil or a cns for which d-emacs-coords-p is t, and it is either not a
proper list or its second element is not a binding."
  (condition-case nil (and (consp (car cns)) (consp (caar cns))
                           (stringp (caaar cns)) (stringp (cdaar cns))
                           (or (not (cdar cns))
                               (d-emacs-coords-p (cdar cns))))
    (error nil)))

(defun d-emacs-bind--elaborate-form-p (cns)
  "Return t if CNS looks like a binding in elaborate form.
This means its car is cons whose car is a cons of a list and a string and whose
cdr is either nil or a cns for which d-emacs-coords-p is t, and it is either
not a proper list or its second element is not a binding."
  (condition-case nil (and (consp (car cns)) (consp (caar cns))
                           (listp (caaar cns)) (stringp (cdaar cns))
                           (or (not (cdar cns))
                               (d-emacs-coords-p (cdar cns))))
    (error nil)))

(defun d-emacs-bind--elaborate-unmatched-binding-p (cns)
  "Return t if CNS is an elaborate unmatched binding.
This means `d-emacs-bind--elaborate-form-p' is t and it has no
coordinates."
  (and (d-emacs-bind--elaborate-form-p cns)
       (not (cdar cns))))

(defun d-emacs-bind--binding-location-p (cns)
  "Return t if the car of CNS is a binding location.

A binding location consists of either

- a string, like a normal string fed to `kbd',

- a d-emacs-xkb coordinate list (see `d-emacs-coords-p'),

a cons whose car is a string of prefixes like `M-C-' and a suffix which is the
name of the signal that is sent from the keyboard without any applied modifiers
\(so a letter name or a name like `<kp-add>'),

- a cons whose car is a prefix and whose cdr is a d-emacs-xkb-coordinate-list,

- a cons

  - whose car is a cons consisting of a prefix and a suffix

  - and whose cdr is a d-emacs-xkb-coordinate-list,

- a cons whose car is

  - a cons whose car is a list of modifiers, given as characters, like `C', `M'
    etc. and whose cdr is a suffix,

- and whose cdr is is a d-emacs-xkb-coordinate-list.

The last two forms are redundant and so usually not needed, although the last
form (the so-called elaborate form) is used by some daselt-functions, such as
`d-emacs-bind--compare-elaborate-bindings'."
  (and (consp cns)
       (or (d-emacs-bind--suffix-form-p cns)
           (d-emacs-bind--prefix-suffix-form-p cns)
           (d-emacs-bind--coords-form-p cns)
           (d-emacs-bind--prefix-coords-form-p cns)
           (d-emacs-bind--prefix-suffix-coords-form-p cns)
           (d-emacs-bind--elaborate-form-p cns))))

(defun d-emacs-bind-p (cns)
  "This function returns t if CNS has the form of a Daselt-binding.
This means it is a cons whose car is a binding car and if CNS is not a list that
contains any other binding forms."
  (and (consp cns)
       (d-emacs-bind--binding-location-p cns)
       (not (if (proper-list-p (cdr cns))
                (d-emacs-exists-p (cdr cns) #'d-emacs-bind--recursively-check-if-binding-cons-p)))))

;;;;; Bindlist formatting
;;;;;; General
(defun d-emacs-bind-head (list)
  "Check if LIST has a head.
An element counts as a head if it isn't identified as a binding."
  (if (and (proper-list-p list) (not (d-emacs-bind-p list)))
      (if (d-emacs-bind-p (car list))
          nil
        (car list))))

;;;;;; Modifiers
(defun d-emacs-bind--prefix-modifiers-index (prefix &optional modlist)
  "Return a list of indexed modifiers in PREFIX.
The indexing is done according to the position of the modifier in MODLIST. If
MODLIST is not specified, `d-emacs-bind-modifiers-list' is used."
  (unless (not prefix)
    (let ((modlist (if modlist modlist d-emacs-bind-modifiers-list))
          (case-fold-search nil))
      (remq nil (mapcar (lambda (indmodifier)
                          (if (string-match-p (concat (char-to-string (cdr indmodifier))
                                                      "-")
                                              prefix)
                              indmodifier))
                        (d-emacs-index modlist))))))

(defun d-emacs-bind-index-and-emacs-bind-modifiers-sort (mods &optional indexed modlist)
  "Index the modifiers in MODS based on their position in MODLIST and sort them.
The default MODLIST is `d-emacs-xkb-modifiers-list'.
If INDEXED is t, assume the MODS are already indexed and don't index them again."
  (let* ((modlist (or modlist d-emacs-bind-modifiers-list))
         (indmods (if indexed
                      mods
                    (d-emacs-filter-list (d-emacs-index modlist)
                                         (lambda (indmod) (member (cdr indmod) mods))))))
    (sort indmods
          :lessp (lambda (indmod1 indmod2)
                   (< (car indmod1)
                      (car indmod2))))))

(defun d-emacs-bind-modifiers-sort (mods &optional retainidx modlist)
  "Sort modifiers in MODS; returns the modifiers without indices.
If RETAINIDX is true, retain the indices in the output.
MODLIST is the list of modifiers used for sorting, by default it is
`d-emacs-bind-modifiers-list'."
  (let ((indmods (d-emacs-bind-index-and-emacs-bind-modifiers-sort mods retainidx modlist)))
    (mapcar (lambda (indmod)
              (nth 1 indmod))
            indmods)))

(defun d-emacs-bind--prefix-modifiers (prefix &optional modlist keepindices)
  "Sort modifiers of PREFIX.
If MODLIST isprovided, it sorts against that instead of
`d-emacs-bind-modifiers-list'. If KEEPINDICES is true, keep modifier indices."
  (let ((sorted (d-emacs-bind-index-and-emacs-bind-modifiers-sort (d-emacs-bind--prefix-modifiers-index prefix modlist) t)))
    (if keepindices
        (if sorted sorted "") ; Let's return the empty string if there aren't any modifiers.
      (mapcar (lambda (indmod)
                (cdr indmod))
              (if sorted sorted "")))))

(defun d-emacs-bind-modifiers-to-string (mods)
  "Concatenate the given list of MODS into a prefix."
  (cl-loop for mod in (reverse mods)
           concat (concat (char-to-string mod) "-")))

;;;;;; Coordinates
(defun d-emacs-bind-coords-from-binding (binding)
  "Retrieve coordinates associated with a BINDING if available.
Otherwise, return nil."
  (cond ((stringp (car binding))
         nil)
        ((and (consp (car binding)) (d-emacs-coords-p (cdar binding)))
         (cdar binding))
        ((d-emacs-coords-p (car binding)) (car binding))))

;;;;;; Elaborate forms
(defun d-emacs-bind--get-layout-matches-for-binding-string (str)
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

(defun d-emacs-bind--get-unique-layout-match (str)
  "Obtain the correct match for STR from a list of potential layout matches.
Typically returns the longest match, excluding matches from layer 0 if others
are available."
  (let* ((matches (d-emacs-bind--get-layout-matches-for-binding-string str))

         ;; Throw away 0-layer matches if another one exists.
         (redmatches (d-emacs-filter-list matches
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
           (prog2 (if d-emacs-bind-mention-unmatched
                      (message "%s in %s is not matched by any signal in %s."
                               (car matches)
                               (current-buffer)
                               (d-emacs-coords--dfk-or-xkb-layout)))
               (car matches)))
          (t (cdr matches)))))

(defun d-emacs-bind--elaborate-on-bindstr (bindstr)
  "Transform a binding string BINDSTR into its elaborate form.
The binding is created by the position of the best match in the layout. If no
match is found, the suffix is converted into an elaborate binding."
  (let ((match (d-emacs-bind--get-unique-layout-match bindstr)))
    (if match
        (let* ((matchstr (cdr match))
               (propermatchstr (car (last (string-split matchstr "/"))))
               (matchcoords (car match)))
          (cons (cons (d-emacs-bind--prefix-modifiers
                       (string-remove-suffix propermatchstr bindstr)
                       nil t)
                      propermatchstr)
                matchcoords))
      (cons (cons (d-emacs-bind--prefix-modifiers bindstr nil t) bindstr) nil))))

(defun d-emacs-bind--elaborate-on-binding (binding)
  "Transform a d-emacs-xkb BINDING into its elaborate form.

If the binding is given by a binding string, it extracts the prefix, the suffix
and its corresponding coordinates from the string by matching the end of the
string against the symbols in the layout. If no matching suffix in the layout
given by d-emacs-xkb-layout is found, it tries to extract modifiers from the
string and returns the string along with the extracted modifiers and nil in
place of coordinates.

If the binding is given by a prefix and suffix, it adds coordinates
corresponding to the suffix.

Otherwise it adds empty strings so that the returned binding is always either of
the form

  (((PREFIX . SUFFIX) . COORDS) . VALUE)

or the original binding if its binding string could not be matched against any
symbol in the given layout."
  (let* ((value (cdr binding))

         (head (cond ((d-emacs-bind--suffix-form-p binding)
                      (let ((bindstr (car binding)))
                        (d-emacs-bind--elaborate-on-bindstr bindstr)))

                     ;; Add coordinates corresponding to suffix if COORDSONLY is on.
                     ((d-emacs-bind--prefix-suffix-form-p binding)
                      (let* ((prefix (caar binding))
                             (prefixmods (d-emacs-bind--prefix-modifiers prefix nil t))
                             (suffix (cdar binding))
                             (match (d-emacs-bind--get-unique-layout-match suffix))
                             (coords (car match)))
                        (cons (cons prefixmods suffix) coords)))
                     ((d-emacs-bind--coords-form-p binding)
                      (cons (cons "" "") (car binding)))
                     ((d-emacs-bind--prefix-coords-form-p binding)
                      (cons (cons (d-emacs-bind--prefix-modifiers (caar binding) nil t) "")
                            (cdar binding)))
                     ((d-emacs-bind--prefix-suffix-coords-form-p binding)
                      (cons (cons (d-emacs-bind--prefix-modifiers (caaar binding) nil t)
                                  (cdaar binding))
                            (cdar binding)))
                     ((d-emacs-bind--elaborate-form-p binding) (car binding))
                     (t (error "%s in %s is an ill-formatted binding" binding (current-buffer)))))
         (elaborate-binding (cons head value)))
    elaborate-binding))

(defun d-emacs-bind--reduce-binding (elbind &optional coordsonly)
  "Transform an elaborate binding ELBIND into its reduced form.
If COORDSONLY is given, use coordinates instead of suffixes whenever possible."
  (let* ((indmods (caaar elbind))
         (prefix (unless (not (caaar elbind))
                   (d-emacs-bind-modifiers-to-string
                    (d-emacs-remove-indices indmods))))
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

;;;;;; Comparison
(defun d-emacs-bind--compare-standardized-modifier-lists (indmods1 indmods2)
  "Compare INDMODS1 and INDMODS2, two lists of standardized key modifiers.
 
Each list should be sorted and indexed by prefix as per
`d-emacs-bind-modifiers-sort-by-prefix'. Return `(t)', `(nil)', or the string
nil to reflect how INDMODS1 compares to INDMODS2:

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

(defun d-emacs-bind--compare-coords (coords1 coords2)
  "This function compares two d-emacs-xkb-coordinates COORDS1 and COORDS2.

First it checks layer, then row, then place. If it finds no difference between
the coordinates it sends the string \"isequal\"."
  (cl-loop for coord1 in coords1
           and coord2 in coords2
           do (cond ((< coord1 coord2)
                     (cl-return `(t)))
                    ((> coord1 coord2)
                     (cl-return `(nil))))))

(defun d-emacs-bind--compare-suffixes (suffix1 suffix2)
  "Compare SUFFIX1 to SUFFIX2.
If SUFFIX1 islonger than SUFFIX2, it signals `(nil)'. If SUFFIX2 is longer than
SUFFIX1, it signals `(t)'. If they have the same length, it comparison uses the
following rules: If one of them is capitalized and the other isn't, the one that
is capitalized comes last. Otherwise, it compares them according to their
constituent character codes."

  (d-emacs-compare-by-sequential-predicates
   suffix1 suffix2
   #'d-emacs-leq-p
   (lambda (sfx)
     (string= sfx (upcase sfx)))
   #'string<))

(defun d-emacs-bind--compare-elaborate-bindings (elbind1 elbind2 &optional coordsonly)
  "Compare elaborate bindings ELBIND1 and ELBIND2.

If COORDSONLY is t, then this function doesn't consider suffixes in sorting.
Note that, since in this case the function has three arguments, it can't be
directly used as a :lessp function by `sort', but has to be surrounded by a
lambda to be used in a two-argument function.

The main use of this function is in `d-emacs-bind--sort-and-format-bindlist',
see there for the sorting order."
  (let* ((srtmods1 (caaar elbind1))
         (srtmods2 (caaar elbind2))
         (suffix1 (cdaar elbind1))
         (suffix2 (cdaar elbind2))
         (hassfx1 (and suffix1 (stringp suffix1) (not (string-empty-p suffix1))))
         (hassfx2 (and suffix2 (stringp suffix2) (not (string-empty-p suffix2))))
         (coords1 (cdar elbind1))
         (coords2 (cdar elbind2))

         ;; Compare by modifiers.
         (compmods (d-emacs-bind--compare-standardized-modifier-lists srtmods1
                                                                       srtmods2)))

    ;; Look if suffixes exist in one case but not the other.
    (cl-flet ((true-and-not (val1 val2)
                (d-emacs-compare-if-decidable (lambda (vval1 vval2)
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
          (let ((compsfx (d-emacs-bind--compare-suffixes suffix1 suffix2)))
            (if compsfx (car compsfx)
              (message  "%s and %s have the same pre- and suffixes in %S."
                        elbind1 elbind2 (current-buffer)))))
         
         ((and coords1 coords2)
          (let ((compcoords (d-emacs-bind--compare-coords coords1 coords2)))
            (if compcoords (car compcoords)
              (message "%s and %s have the same prefixes and coordinates in %S."
                       elbind1 elbind2 (current-buffer))))))))))

;;;;;; Formatting
;;;;;; Lists
(defun d-emacs-bind--sort-and-format-bindlist (blist &optional coordsonly prefun modlist)
  "Sort a d-emacs-xkb bindlist BLIST and format the result.

  Key combinations that are not matched by the layout in d-emacs-xkb-layout are
  put at the very top (because they are most likely errors or depreciated).

  Modifiers are ordered according to their order in MODLIST
  (d-emacs-bind-modifiers-list by default). Sets of modifiers are ordered
  according to the modifier in them the furthest back in MODLIST and so are
  added below the modifier the furthest back.

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
    (let* ((modlist (if modlist modlist d-emacs-bind-modifiers-list))
           (case-fold-search nil)

           ;; Bring bindings in elaborate form and sort contained bindlists.
           (elaborate-list (mapcar (lambda (elt)
                                     (cond ((d-emacs-bind-p elt)
                                            (d-emacs-bind--elaborate-on-binding
                                             elt))
                                           ((atom elt) elt)
                                           (t (d-emacs-bind--sort-and-format-bindlist
                                               elt coordsonly prefun modlist))))
                                   blist))

           ;; Do any function that should be applied before the sorting.
           (prefun-elaborate-list (if prefun (funcall prefun elaborate-list) elaborate-list))

           (sorted-list (sort prefun-elaborate-list :lessp
                              (lambda (elt1 elt2)
                                (cond ((atom elt1) t) ; Atoms should be at the beginning.
                                      ((atom elt2) nil)
                                      ((not (d-emacs-bind-p elt1)) t) ; Then contained lists.
                                      ((not (d-emacs-bind-p elt2)) nil)
                                      (t (d-emacs-bind--compare-elaborate-bindings
                                          elt1 elt2 coordsonly))))))

           (formatted-list (d-emacs-bind--format-sorted-bindlist sorted-list coordsonly))
           
           ;; We have to remove the prefixes of sorted elements because they are already in the suffix string.
           (formatted-sans-unmatched-prefixes-list
            (mapcar
             (lambda (potunmatch)
               (if (d-emacs-bind--elaborate-unmatched-binding-p potunmatch)
                   (cons (cons (cons nil (cdaar potunmatch)) (cdar potunmatch))
                         (cdr potunmatch))
                 potunmatch))
             formatted-list))

           (final-list (mapcar (lambda (elt)
                                 (if (d-emacs-bind-p elt)
                                     (d-emacs-bind--reduce-binding elt coordsonly)
                                   elt))
                               formatted-sans-unmatched-prefixes-list)))
      final-list)))

(defun d-emacs-bind--format-sorted-bindlist (sblist &optional coordsonly)
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
                          (if (not (d-emacs-bind-p potbind))
                              (setq runlst (append runlst (list potbind)))

                            (if (or (= n 0)
                                    (not (d-emacs-bind-p (nth (1- n) sblist))))

                                (let* ((binding potbind)

                                       (indmods (caaar binding))
                                       (prefix (d-emacs-bind-modifiers-to-string
                                                (d-emacs-remove-indices indmods)))
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
                                     ;; (prefix1 (d-emacs-bind-modifiers-to-string
                                     ;;           (mapcar (lambda (indmod) (nth 1 indmod)) indmods2)))
                                     (prefix2 (d-emacs-bind-modifiers-to-string
                                               (d-emacs-remove-indices indmods2)))
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
                                                      (d-emacs-bind--compare-standardized-modifier-lists
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
                                                         (list (d-emacs-generate-newlines 2)
                                                               (format
                                                                ";;;;; Coordinates")))))

                                (if (not eqpfx)
                                    (setq runlst (append
                                                  runlst
                                                  (if (d-emacs-string-exists-and-nonempty prefix2)
                                                      (list (d-emacs-generate-newlines
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
                                                             (list (d-emacs-generate-newlines (if (and eqpfx
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
                                                             (list (d-emacs-generate-newlines (if (and eqpfx
                                                                                                       eqlay)
                                                                                                  2
                                                                                                1))
                                                                   (format
                                                                    ";;;;;;;; %s%s-%s"
                                                                    prefix2
                                                                    layer2
                                                                    row2))))))

                                (setq runlst (append runlst
                                                     (list (d-emacs-generate-newlines 1)
                                                           binding2)))))
                            ))
                     finally return runlst)))))

;;;;;; Strings
(defun d-emacs-bind--sort-and-format-marked-bindlist-string (&optional coordsonly prefun modlist)
  "Sort and format a marked bindlist-string.
The function will read the contents of the selected region and process them
using `d-emacs-bind--sort-and-format-bindlist' and
`d-emacs-bind--format-bindlist-into-string-before-insertion', then replace the
marked region with the result.

COORDSONLY, PREFUN and MODLIST are passed forward to
`d-emacs-bind--sort-and-format-bindlist'."
  (let* ((blist (d-emacs-read-region))
         (formattedblist
          (d-emacs-bind--sort-and-format-bindlist blist coordsonly prefun modlist))
         (formattedstring (d-emacs-bind--format-bindlist-into-string-before-insertion formattedblist)))
    (d-emacs-replace-region formattedstring)
    (unless (eobp) (forward-char))))

(defun d-emacs-bind--format-bindlist-into-string-before-insertion (blist &optional headname)
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

         (finalstring (concat (format "\n;;;; %s\n"
                                      (if headname headname
                                        (if head
                                            head
                                          (let ((filename (buffer-file-name)))
                                            (if filename
                                                (concat
                                                 (format "%s-mode-map"
                                                         (d-emacs-containing-directory-base-name
                                                          filename))))))))
                              str-with-line-breaks-after-head)))
    finalstring))

;;;;;; Extraction
(defun d-emacs-bind-string (binding &optional translate csectoshft doublebind)
                      "Return a binding string or list of strings from Daselt-binding.

BINDING is expected in a specific form compatible with Daselt. Unless DOUBLEBIND
is t, the return value is a string representing the binding, potentially
adjusted based on the optional parameters TRANSLATE, CSECTOSHFT.

- If TRANSLATE is t, translate the binding using translation alists
  `d-emacs-bind-key-translations-alist' and
  `d-emacs-bind-outside-translations-alist'.
  
- If CSECTOSHFT is t, and the binding corresponds to the second layer with
  either no modifiers or one including `C-', replace the binding suffix with its
  downcased form and add an \"S-\" modifier.

- If DOUBLEBIND is t, check if the suffix of the binding or the key from
  coordinates matches the car of a cons cell in
  `d-emacs-bind-double-symbs-alist'. If matched, form a second binding using the
  corresponding cdr to form the returned string. Also check if a string in `d-emacs-bind-outside-translations-alist' matches the
  current binding string. This is necessary to be able to apply discrete
  modifiers to translated bindings. In either case, return a list of all binding
  strings.

Signal an error if the binding is invalid (neither a suffix nor has matching
coordinates)."
                      (let* ((elbind (d-emacs-bind--elaborate-on-binding binding))
         (coords (cdar elbind))
         (sfx (cdaar elbind))
         (mods (d-emacs-remove-indices (caaar elbind)))
         (pfx (if coords ; If the binding is unmatched, then it has already its modifiers in its suffix.
                                                          (d-emacs-bind-modifiers-to-string mods)
                                    ""))
         (coordval (if coords (d-emacs-coords-binding coords)))
         (newsfx ;; Let's put an error check here.
          (let ((newsfx (if (d-emacs-string-exists-and-nonempty sfx)
                                                                    sfx
                                              (if coords coordval))))
            (if (d-emacs-string-exists-and-nonempty newsfx)
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
                             d-emacs-bind-no-shift-list
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
                                     d-emacs-bind-double-symbs-alist)))
           (doublestr (if doubleval (concat pfx (char-to-string doubleval))))

           ;; The key combinations translated by Stump need to be double-bound when discrete mods are applied.
           (stumpdoublebind
            (if (and doublebind
                     (not shifted)) ; Don't doublebind shifted things.
                                                        (let ((discmods (cl-intersection mods d-emacs-bind-discrete-modifiers-list)))
                  (if discmods
                                                              (let ((match (alist-get non-translated-string
                                              d-emacs-bind-outside-translations-alist
                                              nil
                                              nil
                                              (lambda (ntstr carstr)
                                                                    (string-match-p ntstr carstr)))))
                        (if match
                                                                    (concat (d-emacs-bind-modifiers-to-string discmods) match)))))))

           (transstr (if translate
                                                                 (let* ((st-trans
                                 (alist-get non-translated-string
                                            d-emacs-bind-outside-translations-alist
                                            non-translated-string
                                            nil
                                            #'string=))
                                (em-trans (if d-emacs-bind-translate-keys
                                                                                      (alist-get st-trans
                                                         d-emacs-bind-key-translations-alist
                                                         st-trans nil #'string=)
                                            st-trans)))
                           em-trans))))

      (if doublebind
                                                  (remq nil (list (if translate
                                                                      transstr
                                                non-translated-string)

                          (if doubleval doublestr)

                          (if (d-emacs-string-exists-and-nonempty stumpdoublebind)
                                                                      stumpdoublebind)))
        (if translate transstr non-translated-string)))))

;;;;;; Generation
(defun d-emacs-bind--generate-define-key-strings-from-bindlist (blist)
        "Create a `define-key' string for each binding in the currently marked bindlist."
        (let* ((map (car blist))
         (body (cdr blist)))
    (mapcar (lambda (binding)
                    (concat "(define-key " map " (kbd \""
                      (d-emacs--escape-chars-in-str (d-emacs-bind-string binding))
                      "\"\) "
                      (let ((bindval (cdr binding)))
                        (if (stringp bindval)
                                        (concat "\"" bindval "\"")
                                (if (symbolp (eval bindval))
                                          (concat "'" (symbol-name (eval bindval))))))
                      "\)\n"))
            body)))

;;;;; Saving
(defun d-emacs-bind-save-bindlist-as-variable (blist &optional pfx)
  "Save BLIST as a variable.
Works similarly to `d-emacs-bind-with-eval-backup-and-apply-bindlist' but does not
include a call to `d-emacs-bind-apply-binding'.

PFX is the prefix given to the saved bindlists. It is `d-emacs-' by default."
  (let* ((pkgname (d-emacs-containing-directory-base-name (buffer-file-name)))
         ;; (pkgsymb (intern pkgname))
         (mapsymbdefaultname (concat pkgname "-mode-map"))
         (pkgdirname (file-name-directory (buffer-file-name)))
         (pkgdirnameparts (split-string pkgdirname "/"))
         (pfx (or pfx "d-emacs-")))
    (if (not (d-emacs-bind-head blist))
        (let* ((filepath (buffer-file-name))
               (filename (file-name-nondirectory filepath))
               (filenamebase (file-name-base filepath))
               (symbname (if (string-match-p "special" filename)
                             (substring filenamebase 0 -1)
                           (concat pfx
                                   (if (string-match-p "user-defined" filename)
                                       "user-defined-"
                                     "")
                                   mapsymbdefaultname
                                   "-bindlist"))))
          (set (intern symbname)
               blist))

      (cl-flet* ((head-over-body (bblist)
                   (and (d-emacs-bind-head bblist)
                        (or (stringp (car bblist))  ; Let's ensure the head is a symbol
                            (symbolp (car bblist))) ; or string.
                        (not (d-emacs-bind-head (cdr bblist)))))

                 (name-if-symbol (elt)
                   (if (symbolp elt)
                       (symbol-name elt)
                     elt)))

        (if (head-over-body blist)
            (let ((namecore (name-if-symbol (car blist))))
              (set (intern (concat pfx namecore "-bindlist"))
                   blist))
          
          (d-emacs-funcalls-recursively
           blist
           `(((lambda (bblist &optional heads)
                (let* ((namecore (name-if-symbol bblist)))
                  (set (intern (concat pfx namecore "-bindlist"))
                       bblist)))
              .
              (lambda (bblist &optional heads)
                (and (d-emacs-bind-head bblist)
                     (or (stringp (car bblist))  ; Let's ensure the head is a symbol
                         (symbolp (car bblist))) ; or string.
                     (not (d-emacs-bind-head (cdr bblist)))))))
           (lambda (idx lst &optional _heads)
             (let ((elt (nth idx lst)))
               (and (not (atom elt))
                    (not (d-emacs-bind-p elt)))))))))))

;;;; More customs
(defcustom d-emacs-bind-replace-binding-strings-alist
  (remq nil (append (unless (or (bound-and-true-p d-emacs-stump)
                                d-emacs-bind-translate-C-1-1--2-C-g)
                      `(("C-g" . ,(d-emacs-bind-string `(("C-" . (1 1 -2)))))))
                    (unless d-emacs-bind-translate-keys
                      (mapcar (lambda (cns)
                                (let ((str (car cns)))
                                  (cons str (string-replace "C-" "A-" str))))
                              d-emacs-bind-key-translations-alist))))
  "Association list of binding strings and their replacements.
This allows certain key bindings to be replaced, particularly those that would
be translated on Emacs 30+ and `C-g', whose interrupting action can't be
translated.")
;;;; More functions
;;;;; Application
(defun d-emacs-bind-act-on-bindings (blist fun &optional nooutput)
      "Recursively apply FUN to all bindings in BLIST.

This function traverses BLIST, which is expected to be a structure containing
bindings, and applies the function FUN to each binding it encounters. It
determines elements that qualify as bindings using `d-emacs-bind-p'.

Parameters: - BLIST: The list or structure containing potential bindings. - FUN:
The function to apply to each binding. - NOOUTPUT: If non-nil, do not collect
the output in a list.

The function uses `d-funcall-recursively' to manage traversal: - It checks if
each element is a binding using `d-emacs-bind-p'. - Elements that are not atoms
and do not qualify as bindings are further recursed into as lists. - If NOOUTPUT
is nil, collected results are combined using `cons'.

The results are conditionally collected based on whether NOOUTPUT is set. Head
elements of lists are determined using `d-emacs-bind-head' and added to RESTARGS
so they can be used by FUN."
      (d-emacs-funcall-recursively blist
                               fun
                               (lambda (idx lst &optional _heads)
                                     (let ((elt (nth idx lst)))
                                   (d-emacs-bind-p elt)))
                               (lambda (idx lst &optional _heads)
                                     (let ((elt (nth idx lst)))
                                   (and (not (atom elt))
                                        (not (d-emacs-bind-p elt)))))
                               nil
                               (if nooutput nil #'cons)
                               (if nooutput nil #'cons)
                               nil
                               (lambda (lst heads)
                                     (append heads (let ((newhead (d-emacs-bind-head lst)))
                                                 (if newhead (list newhead)))))
                               nil))



(defun d-emacs-bind-with-eval-backup-and-apply-bindlist (blist &optional backuppfx)
      "Rebind keys in a given keymap after evaluating an associated condition.
The rebinding is specified by the bindlist BLIST, which has structurally two
forms:

1. A single keymap with EVAL: ([MAP] BIND1 BIND2 ...)

2. Multiple keymaps with respective bindings: (EVAL (MAP1 BIND11 BIND12 ...)
  (MAP2 BIND21 BIND22 ...))

In both forms: - EVAL is an expression to be evaluated within
`with-eval-after-load'. If the EVAL entry is ommitted, it defaults to the
feature whose name is the same as directory name containing the current buffer's
file. - MAP is a symbol referring to the keymap to modify. If the MAP entry is
omitted, it will default to the mode map corresponding to the containing
directory name.

For each MAP, the current keymap is backed up as `BACKUPPFX-MAP-backup' before
rebindings are applied. If `BACKUPPFX-MAP-backup' is already bound to a keymap,
no backup is made, indicating that a prior backup exists. BACKUPPFX is
`d-emacs-' by default.

The keymap's symbol (MAP) can only be evaluated within `with-eval-after-load',
as bindings should apply after the relevant features are loaded."
      (let* ((pkgname (d-emacs-containing-directory-base-name (buffer-file-name)))
         (pkgsymb (intern pkgname))
         (mapsymbdefault (intern (concat pkgname "-mode-map"))))

    (d-emacs-bind-act-on-bindings
     blist
     (lambda (bind &optional heads)
           (let* ((headpairt (= (length heads) 2))
              (evalcnd (if headpairt
                                   (car heads)
                             pkgsymb))
              (mapsymb (if headpairt
                                   (car (last heads))
                             (if heads
                                     (car heads)
                               mapsymbdefault)))
              (backuppfx (or backuppfx "d-emacs-"))
              (backupsymb (intern (concat backuppfx (symbol-name mapsymb) "-backup"))))

         (with-eval-after-load evalcnd
           (let ((map (symbol-value mapsymb))) ; Mapsymb has to be evaluated only within the with-eval-after-load expression.

             (progn (unless (and (boundp backupsymb) ; Don't overwrite an already existing backup.
                                 (keymapp (symbol-value backupsymb)))
                      (set backupsymb map))

                    (d-emacs-bind-apply-binding bind map))))))
     t)))

(defun d-emacs-bind-apply-binding (binding map)
    "Apply the key BINDING in MAP.

The binding value is evaluated and assigned to the corresponding keys.

Bindings are translated if `d-emacs-bind-translate-keys' is set to t."
    (let* ((orig-binding-string (d-emacs-bind-string binding d-emacs-bind-translate-keys t t))
         (binding-strings (mapcar (lambda (bstr)
                                      (if d-emacs-bind-replace-untranslated-keys
                                            (alist-get bstr
                                                   d-emacs-bind-replace-binding-strings-alist
                                                   bstr)))
                                  orig-binding-string))
         (value (cdr binding)))
    (mapcar (lambda (bstr)
                (define-key map (kbd bstr) (eval value))
                (if (d-emacs-exists-p d-emacs-coords-bad-combinations-list
                                    (lambda (combination)
                                        (string= (d-emacs-bind-string (cons combination nil))
                                               bstr)))
                      (define-key map
                              (kbd (string-replace
                                    "H-" "s-M-"
                                    (string-replace "C-" "A-" bstr)))
                              (eval value))))
            binding-strings)))


;;;;; Coordinate changes


(defun d-emacs-bind-change-coords-in-bindlist (blist coordlistlist)
  "Change coordinates in BLIST according to COORDLISTLIST.
Return the modified bindlist."
  (mapcar (lambda (bind) (if (d-emacs-bind-p bind)
                                                    (d-emacs-bind-change-coords-in-binding bind coordlistlist)
                                    (if (consp bind)
                                                      (d-emacs-bind-change-coords-in-bindlist bind coordlistlist)
                                      bind)))
          blist))

(defun d-emacs-bind-change-coords-in-bindlist-during-sorting (blist coordlistlist)
  "Change coordinates in BLIST according to COORDLISTLIST.
Return the modified bindlist.

Note that unlike `d-emacs-bind-change-coords-in-bindlist' this function does
not recurse into sub-lists of a bindlist. This is because it should be used as a
prefun for `d-emacs-bind--sort-and-format-bindlist'.
`d-emacs-bind--sort-and-format-bindlist' already passes on prefuns in its
recursive calls, so if this function would recurse as well, the coordinate
change would be applied twice."
  (mapcar (lambda (bind) (if (d-emacs-bind-p bind)
                            (d-emacs-bind-change-coords-in-binding bind coordlistlist)
                        bind))
          blist))

(defun d-emacs-bind-change-coords-in-binding (bind coordlistlist)
  "Change coordinates in BIND according to COORDLISTLIST.
Return the modified binding."
  (if (stringp (car bind))
      bind
    (let* ((carcoordsp (d-emacs-coords-p (car bind)))
           (cdarcoordsp (unless carcoordsp (d-emacs-coords-p (cdar bind))))
           (origcoords (cond (carcoordsp (car bind))
                             (cdarcoordsp (cdar bind))))
           (newcoords (unless (not origcoords)
                        (d-emacs-bind-change-coords origcoords coordlistlist)))
           (carrest (cond (carcoordsp nil)
                          (cdarcoordsp (caar bind))
                          (t (car bind))))
           (val (cdr bind)))
      (cond (carcoordsp (cons newcoords val))
            (cdarcoordsp (cons (cons carrest newcoords) val))
            (t (cons carrest val))))))

(defun d-emacs-bind-change-coords (origcoords coordlistlist)
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
          (d-emacs-index origcoords)))

;;;;; Drawing
(defun d-emacs-bind-with-max-buffer-maybe-return (bufname fun)
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



(defun d-emacs-bind--placeval-from-elaborate-binding (elbind)
  "Return PLACEVAL whose car is coords of ELBIND and cdr is its cdr.
If ELBIND has no coordinates, return nil."
  (let ((coords (cdar elbind))
        (val (cdr elbind)))
    (if coords (cons coords val))))

(defun d-emacs-bind--elbinds-matching-modifier-regexps (blist modrxs)
    "Return elaborate forms of bindings in BLIST matching MODS.
Filter bindings by modifier regexps MODRXS. A modifier regexp is a string
matched against all modifiers in a binding. If the regexp string starts with
`^', the binding is matched by the regexp if and only if no modifier in the
binding matches the string."
    (let* ((case-fold-search nil)
         (pblist (d-emacs-filter-list blist #'d-emacs-bind-p))
         (elblist (mapcar (lambda (bind)
                              (d-emacs-bind--elaborate-on-binding bind))
                          pblist))
         (purelblist (d-emacs-filter-list elblist (lambda (bind)
                                                      (not (d-emacs-bind--string-binding-p bind))))))

    (d-emacs-filter-list purelblist
                         (lambda (elbind)
                             (cl-flet* ((ispositive (modrx)
                                          (not (string-match-p (rx string-start "^")
                                                             modrx))))
                             (let* ((elbindmods (d-emacs-remove-indices
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

;;;;; Import
(defun d-emacs-bind-parse-for-keybindings (rx &optional mappos keypos valpos consespos mapdefaultfun)
    "Parse REGION for keybindings using RX.
Return lists of maps and bindlistpieces.

REGION is the active region, or the current buffer if no region is active.

MAPPOS is the position of the keymapname in the RX as a group number.

KEYPOS is the position of the key combination in the RX. VALPOS is the position
of the key combination in the RX.

CONSESPOS is the position of conses in bindings in `use-package' `:bind'
sections.

MAPDEFAULTFUN is a function that describes how to obtain a map symbol if none is
found in the RX match."
    (save-excursion
      (d-emacs-goto-min)
      (let (; (start (if (use-region-p)
                                        ; (region-beginning)
                                        ; (point-min)))
          (end (if (use-region-p)
                       (region-end)
                   (point-max)))
          maps
          blistpieces)
      (while (re-search-forward rx end t)
        (let ((map (let ((maprxstr (if mappos (match-string mappos))))
                     (if (d-emacs-string-exists-and-nonempty maprxstr)
                             (read maprxstr)
                         (if mapdefaultfun (funcall mapdefaultfun)))))
              (key (if keypos (d-emacs-remove-text-properties-from-string
                               (match-string keypos))))
              (val (if valpos (read (match-string valpos))))
              (conses (if consespos (read (concat "(" (match-string consespos) ")")))))

          (push map maps)
          (push (if (and keypos valpos)
                        (list (cons key val))
                    (if consespos
                          conses))
                blistpieces)))

      (cons maps blistpieces))))

(defun d-emacs-bind--do-parse-for-define-key-bindings ()
  "Parse all `define-key'-bindings in REGION.
Return lists of maps and bindlistpieces. REGION is the active region, or the
current buffer if no region is active."
  (let ((drx (rx
              line-start
              (* (not (or ";" "\n")))
              (or "(define-key"
                  "(keymap-set")
              (+ blank)
              (group (+ (not space))) ;; MAP
              (+ space)
              "("
              "kbd"
              (+ space)
              "\""
              (group (+ (not (any "\"")))) ;; KEY
              "\""
              ")"
              (+ space)
              (group (+? anything)) ;; VAL
              ")")))
    (d--parse-for-keybindings drx 1 2 3)))

(defun d-emacs-bind--do-parse-for-global-key-set-bindings ()
  "Parse all `global-set-key'-bindings in REGION.
Return lists of maps and bindlistpieces. REGION is the active region, or the
current buffer if no region is active."
  (let ((grx (rx (* (not ";"))
                 (or "(global-set-key" "(global-key-set")
                 (* blank)
                 "\""
                 "("
                 "kbd"
                 (+ space)
                 "\""
                 (group (+ (not (any "\"")))) ;; KEY
                 "\""
                 ")"
                 (group (+ (not blank)))          ;; VAL
                 ")")))
    (d--parse-for-keybindings grx nil 1 2)))

(defun d-emacs-bind--do-parse-for-bind-key-bindings ()
  "Parse all `bind-key'-bindings in REGION.
Return lists of maps and bindlistpieces. REGION is the active region, or the
current buffer if no region is active."
  (let ((brx (rx line-start
                 (* (not ";"))
                 "(bind-key"
                 (zero-or-one "*")
                 (* blank)
                 "\""
                 (group (+ (not (any "\"")))) ;; KEY
                 "\""
                 (* blank)
                 (group (+ (not blank)))          ;; VAL
                 (optional (* blank) (group (+? (not blank))))  ;; MAP
                 ")")))
    (d--parse-for-keybindings brx 3 1 2)))

(defun d-emacs-bind--do-parse-for-use-package-bindings ()
  "Parse all `:bind'-sections of `use-package' configurations in REGION.
Return lists of maps and bindlistpieces. REGION is the active region, or the
current buffer if no region is active."
  (let ((urx (rx line-start
                 (* (not (or ";" "\n")))
                 ":bind"
                 (group (zero-or-one "*")) ; If it's of the form `:bind*'.
                 (zero-or-one (* (or space "\n"))
                              "(:map"
                              (zero-or-more (or space "\n"))
                              (group (* (not (any "(" ")" space "\n"))))) ; MAP
                 (* (or (: line-start (* blank) ";" not-newline "\n")
                        (not "("))) ; Skip as many commented lines or lines without a bracket as you can.
                 (group (* (or "\n"
                               (: line-start (* blank) line-end)
                               (: line-start (* blank) ";" (* not-newline) "\n") ; CONSES
                               (* (* space)
                                  "("
                                  (* (or (not (any "(" ")"))
                                         "\(\""
                                         "\)\""))
                                  ")"
                                  (* space))))))))

    (d--parse-for-keybindings urx 2 nil nil 3 (lambda () 'global-map))))

;;;; Provide
(provide 'd-emacs-bind)
;;; d-emacs-bind.el ends here
