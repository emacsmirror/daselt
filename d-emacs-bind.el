;;; d-emacs-bind.el --- Tools for coordinatized bindings -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Alexander Prähauser

;; Author: Alexander Prähauser <ahprae@protonmail.com>
;; Version: 1.0
;; Keywords: tools
;; URL: https://gitlab.com/nameiwillforget/d-emacs/-/blob/master/d-emacs-bind.el

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

;; Usage: The most important structure type in d-emacs-bind is that of a
;; bindlist, which houses bindings that can be applied to a keymap or used for
;; generating variables or configurations of other programs. See
;; `d-emacs-bind-user-defined-example-bindlist.dbl' for a documented example of
;; a bindlist. Bindlists can be applied with `d-emacs-bind-apply-bindlist',
;; saved to a variable with `d-emacs-bind-save-bindlist-as-variable', sorted
;; with `d-emacs-bind-sort-and-format-bindlist' and drawn with
;; `d-emacs-bind-draw-bindlist-layer' and
;; `d-emacs-bind-draw-bindings-from-regexps'. Existing bindings can be imported
;; using `d-emacs-bind-convert-bindings-to-bindlist'. The action of these
;; functions, in particular the application of bindlists, can be customized
;; using the options in the group `d-emacs-bind'.

;;; Code:

;;;; Preamble
(require 'd-emacs-base)
(require 'd-emacs-coords)
(require 'cl-lib)
(require 'subr-x)

;; (declare-function d-stump-translated-emacs-keys "d-stump-functions" nil)
;; (declare-function d-emacs-base-read-region "d-emacs-base" (&optional properties))
;; (declare-function d-emacs-base-funcalls-recursively "d-emacs-base" (obj funtests &optional recursetest formatfun eltcolfun lstcolfun restargs restargfun contt debug))
;; (declare-function d-emacs-base--escape-chars-in-str "d-emacs-base" (str))
;; (declare-function d-emacs-base-string-exists-and-nonempty "d-emacs-base" (str))
;; (declare-function d-emacs-base-generate-newlines "d-emacs-base" (k))
;; (declare-function d-emacs-base-compare-if-decidable "d-emacs-base" (test arg1 arg2))
;; (declare-function d-emacs-coords-binding "d-emacs-coords" (coords &optional layout wholebinds))
;; (declare-function d-emacs-base-containing-directory-base-name "d-emacs-base" (filepath))
;; (declare-function d-emacs-base-replace-region "d-emacs-base" (arg))
;; (declare-function d-emacs-base-compare-by-sequential-predicates "d-emacs-base" (arg1 arg2 &rest predicates))
;; (declare-function d-emacs-base-remove-indices "d-emacs-base" (indlst))
;; (declare-function d-emacs-coords--dfk-or-xkb-layout "d-emacs-coords" nil)
;; (declare-function d-emacs-coords-coordinatize-layout "d-emacs-coords" (layout))
;; (declare-function d-emacs-base-recursive-get-cons "d-emacs-base" (obj allist &optional testfn reverse))
;; (declare-function d-emacs-base-filter-list "d-emacs-base" (lst pred))
;; (declare-function d-emacs-base-index "d-emacs-base" (list &optional fromone))
;; (declare-function d-emacs-coords-p "d-emacs-coords" (list))
;; (declare-function d-emacs-base-exists-p "d-emacs-base" (list predicate))
;; (declare-function d-emacs-base-leq-p "d-emacs-base" (seq1 seq2))

;;;; Variables
(defvar d-emacs-bind-boundaries
  nil
  "List of boundaries of layers of different lengths.

This is used purely to increase performance of commands like
`d-emacs-bind-draw-bindings-from-regexps'. Generally you shouldn't set this by
hand but use `d-emacs-coords-boundaries' to calculate these based on the layers
in your `d-emacs-xkb-layout' and `d-emacs-dfk-layout'.")

;;;; Constants
(defconst d-emacs-bind-modifiers-list
  (list ?C ?H ?M ?S ?s ?A)
  "List of modifiers in their standard order in Daselt.

Note that this is different from the standard order of modifiers in Emacs.")

(defconst d-emacs-bind-discrete-modifiers-list
  (list ?M ?s ?A)
  "List of discrete modifiers in their standard order in Daselt.")

(defconst d-emacs-bind-layers-to-shift-list
  '(2 8)
  "Layers to which a shift modifier should be added when `d-emacs-bind-string'
is called with CSECTOSHFT set.")

(defconst d-emacs-bind-no-shift-list
  '("'")
  "List of strings on layers in `d-emacs-bind-layers-to-shift-list' that should
not be replaced by their downcased version with a shift modifier when
`d-emacs-bind-string' is called with `csectoshft' set to t.")


;;;; Customs
(defgroup d-emacs-bind
  nil
  "Containing group for d-emacs-bind."
  :group 'd-emacs)

(define-widget 'd-emacs-bind-bindlist 'lazy
  "A d-emacs-bindlist."
  :offset 4
  :tag "Bindlist"
  :type '(restricted-sexp :match-alternatives (#'d-emacs-bind-bindlist-p)))

(defcustom d-emacs-bind-translate-keys
  (if (>= (string-to-number (substring emacs-version 0 (string-match-p "\\." emacs-version))) 29) t nil)
  "Enable translation of keys in `d-emacs-bind-key-translations-alist'.

This translation is intended for Emacs versions 29 or higher. to address
terminal translations that conflict with key bindings. When active, use the
translated key combinations in bindings."
  :type 'boolean
  :group 'd-emacs-bind)

(defcustom d-emacs-bind-translate-choices
  t
  "Replace `y' and `n' in multiple-choice queries with alternative values.

If a query uses symbols at coordinates (1 0 2) or (1 0 -2), replace them with
the values at coordinates (5 0 2) or (5 0 -2), typically unused Greek letters."
  :type 'boolean
  :group 'd-emacs-bind)

(defcustom d-emacs-bind-translate-C-1-1--2-C-g
  nil
  "If non-nil, translate `C-g` to C-(1 1 -2) and vice versa.

Note that the `C-g` function to stop running processes cannot be translated, so
the option is disabled by default."
  :type 'boolean
  :group 'd-emacs-bind)

(defcustom d-emacs-bind-key-translations-alist
  `(("C-m" . "C-á") ("C-i" . "C-ĥ") ("C-[" . "C-é"))
  "List of key translations to circumvent terminal interference.

Each element is a cons cell where the car is a key combination to be translated
and the cdr is the desired translation. For example, on terminals like xterm,
`C-i' may be translated to TAB. Setting `d-emacs-bind-translate-keys' to t will
use these translations to preserve intended key bindings."
  :type '(repeat (cons string string))
  :group 'd-emacs-bind)

(defcustom d-emacs-bind-double-symbs-alist
  '((?Ͳ . ?ͳ) (?Ϙ . ?ϙ))
  "Alist of symbol translations for elaborate binding suffixes.

If the first symbol in a cons cell is the suffix of the elaborate form of a
binding in a bindlist, the same binding should apply to the second symbol as
well."
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

Useful for users who import their keybinds, as it highlights unmatched
suffixes."
  :type 'boolean
  :group 'd-emacs-bind)

(defcustom d-emacs-bind-outside-translations-alist
  nil
  "Alist of key combinations that are translated from outside to Emacs.

Automatically generated from the contents of the remapped-keys-file.

If you have d-emacs-stump, you can use `d-stump-translated-emacs-keys' to set
this.

Automatically set when starting `d-emacs-mode' if `d-stump' is t."
  :type '(alist :key-type string :value-type string)
  :group 'd-emacs-bind)


(defcustom d-emacs-bind-no-shift-if-string-list
                '("<backspace>" "<return>")
                "List of signals that are on a shift layer but should not be treated as such
if they are given as strings.

An implicit shift-modifier is applied to bindings on the second layer when they
are given by coordinates with a C-modifier (because of reasons), but when they
are given by strings, any modifiers are either given explicitly or implicitly by
using uppercase letters. So when function keys that exist on the second layer
are given by strings, it has to be specified that they should not be treated as
being on the second layer when they are formatted."
                :type '(repeat string)
                :group 'd-emacs-bind)

(defcustom d-emacs-bind-replace-binding-strings-alist
  '(nil)
  "Association list of binding strings and their replacements.

This allows certain key bindings to be replaced, particularly those that would
be translated on Emacs 29+ and `C-g', whose interrupting action can't be
translated.

If this option is set to (nil), the list is generated by
`d-emacs-bind-generate-replace-binding-strings-alist'."
  :type '(alist :key-type string :value-type string)
  :group 'd-emacs-bind)

;;;; Functions
;;;;; Predicates
(defun d-emacs-bind-bindlist-p (cand)
  "Return t if CAND is a bindlist.

The way used to test this is by recursing through CAND until a binding is
found."
  (declare (ftype (function (t) boolean))
           (pure t))
  (if (atom cand)
                          nil
              (cl-loop for elt in cand
             do (if (d-emacs-bind-p elt)
                                        (cl-return t)
                            (unless (atom elt)
                    (if (d-emacs-bind-bindlist-p elt)
                                            (cl-return t)))))))

(defun d-emacs-bind-bindlist-symb-p (sym)
    "Return t if SYM is a bindlist symbol.

This is tested by looking at whether the name of SYM ends in `-bindlist', SYM is
a bound variable and the value of SYM returns t when tested with
`d-emacs-bind-bindlist-p'."
    (declare (ftype (function (symbol) boolean))
           (side-effect-free t))
    (cl-check-type sym symbol)
    (and (string-match-p (rx "-bindlist" string-end)
                       (symbol-name sym))
       (boundp sym)
       (d-emacs-bind-bindlist-p (symbol-value sym))))

(defun d-emacs-bind--string-binding-p (cns)
  "Return t if CNS is a binding given by a binding string."
  (declare (ftype (function (cons) boolean))
           (pure t))
  (cl-check-type cns cons)
  (and (d-emacs-bind-p cns)
       (not (d-emacs-coords-p (car cns)))
       (not (d-emacs-coords-p (cdar cns)))))

(defun d-emacs-bind--recursively-check-if-binding-cons-p (obj)
  "Check if OBJ looks like a binding.

If not, then if OBJ is an atom, return nil. Otherwise apply yourself to the car
and return t if the application does. Moreover, if the OBJ has more than one
element, apply yourself to each element of the cdr until an application retuns
t. Then return t."
  (declare (ftype (function (cons) boolean))
           (pure t))
  (and (consp obj)
       (or (d-emacs-bind--binding-location-p obj)
           (d-emacs-bind--recursively-check-if-binding-cons-p (car obj))
           (if (proper-list-p (cdr obj))
               (d-emacs-base-exists-p (cdr obj) #'d-emacs-bind--recursively-check-if-binding-cons-p)))))

(defun d-emacs-bind--suffix-form-p (cns)
  "Return t if CNS looks like a binding in suffix form.

This means its car is a string, and it is either not a proper list or its second
element is not a binding."
  (declare (ftype (function (cons) boolean))
           (pure t))
  (cl-check-type cns cons)
  (and (atom (car cns))
       (stringp (car cns))))

(defun d-emacs-bind--prefix-suffix-form-p (cns)
  "Return t if CNS looks like a binding in prefix-suffix-form.

This means its car is a cons of two strings, and it is either not a proper list
or its second element is not a binding."
  (declare (ftype (function (cons) boolean))
           (pure t))
  (cl-check-type cns cons)
  (and (consp (car cns))
       (stringp (caar cns))
       (stringp (cdar cns))))

(defun d-emacs-bind--coords-form-p (cns)
  "Return t if CNS looks like a binding in coords-form.

This means its car is a cns for which `d-emacs-coords-p' is t, and it is either
not a proper list or its second element is not a binding."
  (declare (ftype (function (cons) boolean))
           (pure t))
  (cl-check-type cns cons)
  (and (consp cns)
       (d-emacs-coords-p (car cns))))

(defun d-emacs-bind--prefix-coords-form-p (cns)
  "Return t if CNS is a binding in prefix-coords form.

This means that the car must be a cons of a string (the prefix) and a
d-emacs-xkb coordinate list."
  (declare (ftype (function (cons) boolean))
           (pure t))
  (cl-check-type cns cons)
  (and (consp (car cns))
       (stringp (caar cns))
       (d-emacs-coords-p (cdar cns))))

(defun d-emacs-bind--prefix-suffix-coords-form-p (cns)
  "Return t if CNS looks like a binding in prefix-suffix-coords-form.

This means its car is cons whose car is a cons of two strings and whose cdr is
either nil or a cns for which `d-emacs-coords-p' is t, and it is either not a
proper list or its second element is not a binding."
  (declare (ftype (function (cons) boolean))
           (pure t))
  (cl-check-type cns cons)
  (and (consp (car cns)) (consp (caar cns))
       (stringp (caaar cns)) (stringp (cdaar cns))
       (or (not (cdar cns))
           (d-emacs-coords-p (cdar cns)))))

(defun d-emacs-bind-elaborate-form-p (cns)
  "Return t if CNS looks like a binding in elaborate form.

This means its car is cons whose car is a cons of a list and a string and whose
cdr is either nil or a cns for which `d-emacs-coords-p' is t, and it is either
not a proper list or its second element is not a binding."
  (declare (ftype (function (cons) boolean))
           (pure t))
  (cl-check-type cns cons)
  (and (consp (car cns)) (consp (caar cns))
       (listp (caaar cns)) (stringp (cdaar cns))
       (or (not (cdar cns))
           (d-emacs-coords-p (cdar cns)))))

(defun d-emacs-bind-elaborate-unmatched-binding-p (cns)
  "Return t if CNS is an elaborate unmatched binding.

This means `d-emacs-bind-elaborate-form-p' is t and it has no coordinates."
  (declare (ftype (function (cons) boolean))
           (pure t))
  (cl-check-type cns cons)
  (and (d-emacs-bind-elaborate-form-p cns)
       (not (cdar cns))))

(defun d-emacs-bind--binding-location-p (cns)
    "Return t if the car of CNS is a binding location.

A binding location consists of either

- a string, like a normal string fed to `kbd`,

- a d-emacs-xkb coordinate list (see `d-emacs-coords-p'),

a cons whose car is a string of prefixes like `M-C-` and a suffix which is the
name of the signal that is sent from the keyboard without any applied modifiers
\(so a letter name or a name like `<kp-add>`),

- a cons whose car is a prefix and whose cdr is a d-emacs-xkb-coordinate-list,

- a cons

  - whose car is a cons consisting of a prefix and a suffix

  - and whose cdr is a d-emacs-xkb-coordinate-list,

- a cons whose car is

  - a cons whose car is a list of modifiers, given as characters, like `C`, `M`
    etc. and whose cdr is a suffix,

- and whose cdr is is a d-emacs-xkb-coordinate-list.

The last two forms are redundant and so usually not needed, although the last
form (the so-called elaborate form) is used by some daselt-functions, such as
`d-emacs-bind-compare-elaborate-bindings'."
    (declare (ftype (function (cons) boolean))
           (pure t))
    (cl-check-type cns cons)
    (or (d-emacs-bind--suffix-form-p cns)
      (d-emacs-bind--prefix-suffix-form-p cns)
      (d-emacs-bind--coords-form-p cns)
      (d-emacs-bind--prefix-coords-form-p cns)
      (d-emacs-bind--prefix-suffix-coords-form-p cns)
      (d-emacs-bind-elaborate-form-p cns)))

(defun d-emacs-bind-p (obj)
  "This function returns t if OBJ has the form of a Daselt-binding.

This means it is a cons whose car is a binding car and if OBJ is not a list that
contains any other binding forms."
  (declare (ftype (function (t) boolean))
           (pure t))
  (and (consp obj)
       (d-emacs-bind--binding-location-p obj)
       (not (if (proper-list-p (cdr obj))
                (d-emacs-base-exists-p (cdr obj) #'d-emacs-bind--recursively-check-if-binding-cons-p)))))

;;;;; Bindlists
;;;;;; General
(defun d-emacs-bind-head (obj)
  "Check if OBJ is a bindlist with a head and return it if it does.

An element counts as a head if it isn't identified as a binding."
  (declare (ftype (function (obj) t))
           (pure t))
  (if (and (proper-list-p obj)
           (not (d-emacs-bind-p obj)))
      (let ((head (car obj)))
        (unless (d-emacs-bind-p head)
          head))))

;;;;;; Modifiers
(defun d-emacs-bind-index-prefix-modifiers (prefix &optional modlist)
  "Return a list of indexed modifiers in PREFIX.

The indexing is done according to the position of the modifier in MODLIST. If
MODLIST is not specified, `d-emacs-bind-modifiers-list' is used."
  (declare (ftype (function (string &optional list) list)
                  ;; (function (string &optional (list integer)) (list (list integer))) ; Compiler complains.
                  )
           (side-effect-free t))
  (when prefix
    (let ((modlist (if modlist modlist d-emacs-bind-modifiers-list))
          (case-fold-search nil))
      (remq nil (mapcar (lambda (indmodifier)
                          (if (string-match-p (concat (char-to-string (cdr indmodifier))
                                                      "-")
                                              prefix)
                              indmodifier))
                        (d-emacs-base-index modlist))))))

(defun d-emacs-bind-index-and-sort-modifiers (mods &optional indexed modlist)
  "Index the modifiers in MODS based on their position in MODLIST and sort them.

The default MODLIST is `d-emacs-xkb-modifiers-list'. If INDEXED is t, assume the
MODS are already indexed and don't index them again."
  (declare (ftype (function (cons &optional boolean list) list)
                  ;; (function ((or (list integer) (list (cons integer integer))) ; Compiler complains.
                  ;;            &optional boolean (list integer))
                  ;;           (list (cons integer integer)))
                  )
           (side-effect-free t))
  (let* ((modlist (or modlist d-emacs-bind-modifiers-list))
         (indmods (if indexed
                      mods
                    (d-emacs-base-filter-list (d-emacs-base-index modlist)
                                              (lambda (indmod) (member (cdr indmod) mods))))))

    (sort indmods
          (lambda (indmod1 indmod2)
            (< (car indmod1)
               (car indmod2))))))

(defun d-emacs-bind-prefix-modifiers (prefix &optional modlist keepindices)
  "Sort modifiers of PREFIX.

If MODLIST is provided, it sorts against that instead of
`d-emacs-bind-modifiers-list'. If KEEPINDICES is true, keep modifier indices."
  (declare (ftype (function (string &optional list boolean) list)
                  ;; (function (string &optional (list integer) boolean)
                  ;;           (or (list integer)
                  ;;               (list (cons integer integer))))
                  )
           (side-effect-free t))
  (cl-check-type prefix string)
  (let ((sorted (d-emacs-bind-index-and-sort-modifiers (d-emacs-bind-index-prefix-modifiers prefix modlist) t)))
    (if keepindices
        (if sorted sorted) ; Let's return nothing if there aren't any modifiers.
      (d-emacs-base-remove-indices sorted))))

(defun d-emacs-bind-modifiers-to-string (mods)
  "Concatenate the given list of MODS into a prefix."
  (declare (ftype (function (list
                             ;; (list integer) ; Compiler complains.
                             )
                            string))
           (pure t))
  (let ((modchain (mapconcat #'char-to-string (reverse mods) "-")))
    (if (d-emacs-base-string-exists-and-nonempty modchain)
        (concat modchain "-")
      "")))

;;;;;; Coordinates
(defun d-emacs-bind-coords-from-binding (binding)
  "Retrieve coordinates associated with a BINDING if available.

Otherwise, return nil."
  (declare (ftype (function (t) list
                            ;; (list number) ; Compiler complains.
                            ))
           (pure t))
  (cond ((stringp (car binding))
         nil)
        ((and (consp (car binding)) (d-emacs-coords-p (cdar binding)))
         (cdar binding))
        ((d-emacs-coords-p (car binding)) (car binding))))

;;;;;; Elaborate forms
(defun d-emacs-bind--get-layout-matches-for-binding-string (str &optional laysym)
  "Match (indexed) layout entries against the last part of the string STR.

Return a cons of STR and the list of matching conses.

LAYSYM should be the symbol of the layout that is used to match coordinates.
By default it is the symbol returned by `d-emacs-coords--dfk-or-xkb-layout'."
  (declare (ftype (function (string &optional symbol) cons
                            ;; (cons string (list (cons (list number) string))) ; Compiler complains.
                            ))
           (side-effect-free t))
  (cl-check-type str string)
  (d-emacs-base-recursive-get-cons
   str
   (d-emacs-coords-coordinatize-layout
    (symbol-value (or laysym (d-emacs-coords--dfk-or-xkb-layout))))
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

(defun d-emacs-bind--get-unique-layout-match (str &optional laysym)
  "Obtain the correct match for STR from a list of potential layout matches.

Typically returns the longest match, excluding matches from layer 0 if others
are available.

LAYSYM should be the symbol of the layout that is used to match coordinates.
By default it is the symbol returned by `d-emacs-coords--dfk-or-xkb-layout'."
  (declare (ftype (function (string &optional symbol) cons
                            ;; (cons (list number) string) ; Compiler complains.
                            ))
           (side-effect-free t))
  (let* ((laysym (or laysym (d-emacs-coords--dfk-or-xkb-layout)))
         (matches (d-emacs-bind--get-layout-matches-for-binding-string str))

         ;; Throw away 0-layer matches if another one exists.
         (redmatches (d-emacs-base-filter-list matches
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
                               laysym))
               (car matches)))
          (t (cdr matches)))))

(defun d-emacs-bind--elaborate-on-bindstr (bindstr &optional laysym)
  "Transform a binding string BINDSTR into its elaborate form.

The binding is created by the position of the best match in the layout. If no
match is found, the suffix is converted into an elaborate binding.

LAYSYM should be the symbol of the layout that is used to match coordinates.
By default it is the symbol returned by `d-emacs-coords--dfk-or-xkb-layout'."
  (declare (ftype (function (string &optional symbol) cons
                            ;; (or (cons (cons (list integer) string) (list number)) ; Compiler complains.
                            ;;     (cons ((cons (list integer) string)) void))
                            ))
           (side-effect-free t))
  (let ((match (d-emacs-bind--get-unique-layout-match bindstr laysym)))
    (if match
        (let* ((matchstr (cdr match))
               (propermatchstr (car (last (string-split matchstr "/"))))
               (matchcoords (car match)))
          (cons (cons (d-emacs-bind-prefix-modifiers
                       (string-remove-suffix propermatchstr bindstr)
                       nil t)
                      propermatchstr)
                matchcoords))
      (cons (cons (d-emacs-bind-prefix-modifiers bindstr nil t) bindstr) nil))))

(defun d-emacs-bind-elaborate-on-binding (binding &optional laysym)
  "Transform a d-emacs-xkb BINDING into its elaborate form.

If the binding is given by a binding string, it extracts the prefix, the suffix
and its corresponding coordinates from the string by matching the end of the
string against the symbols in the layout. If no matching suffix in the layout
given by `d-emacs-xkb-layout' is found, it tries to extract modifiers from the
string and returns the string along with the extracted modifiers and nil in
place of coordinates.

If the binding is given by a prefix and suffix, it adds coordinates
corresponding to the suffix.

Otherwise it adds empty strings so that the returned binding is always either of
the form

  (((PREFIX . SUFFIX) . COORDS) . VALUE)

or the original binding if its binding string could not be matched against any
symbol in the given layout.

LAYSYM should be the symbol of the layout that is used to match strings.
By default it is the symbol returned by `d-emacs-coords--dfk-or-xkb-layout'."
  (declare (ftype (function (t &optional symbol) (or cons cons)
                            ;; (or (cons (cons (cons (list integer) string) (list number)) t) ; Compiler complains.
                            ;;     (cons (cons ((cons (list integer) string)) void) t))
                            ))
           (side-effect-free t))
  (unless (d-emacs-bind-p binding)
    (error "Wrong-type argument, d-emacs-binding, %s" binding))
  (let* ((value (cdr binding))
         (head (cond ((d-emacs-bind--suffix-form-p binding)
                      (let ((bindstr (car binding)))
                        (d-emacs-bind--elaborate-on-bindstr bindstr laysym)))

                     ;; Add coordinates corresponding to suffix if COORDSONLY is on.
                     ((d-emacs-bind--prefix-suffix-form-p binding)
                      (let* ((prefix (caar binding))
                             (prefixmods (d-emacs-bind-prefix-modifiers prefix nil t))
                             (suffix (cdar binding))
                             (match (d-emacs-bind--get-unique-layout-match suffix))
                             (coords (car match)))
                        (cons (cons prefixmods suffix) coords)))
                     ((d-emacs-bind--coords-form-p binding)
                      (cons (cons nil "") (car binding)))
                     ((d-emacs-bind--prefix-coords-form-p binding)
                      (cons (cons (d-emacs-bind-prefix-modifiers (caar binding) nil t) "")
                            (cdar binding)))
                     ((d-emacs-bind--prefix-suffix-coords-form-p binding)
                      (cons (cons (d-emacs-bind-prefix-modifiers (caaar binding) nil t)
                                  (cdaar binding))
                            (cdar binding)))
                     ((d-emacs-bind-elaborate-form-p binding) (car binding))
                     (t (error "%s in %s is an ill-formatted binding" binding (current-buffer)))))
         (elaborate-binding (cons head value)))
    elaborate-binding))

(defun d-emacs-bind-reduce-binding (elbind &optional coordsonly)
  "Transform an elaborate binding ELBIND into its reduced form.

If COORDSONLY is given, use coordinates instead of suffixes whenever possible."
  (declare (ftype (function (cons
                             ;; (or (cons (cons (cons (list integer) string) (list number)) t) ; Compiler complains.
                             ;;     (cons (cons ((cons (list integer) string)) void) t))
                             &optional boolean)
                            t))
           (side-effect-free t))
  (unless (d-emacs-bind-elaborate-form-p elbind)
    (error "Wrong-type argument, elaborate binding, %s" elbind))
  (let* ((indmods (caaar elbind))
         (prefix (when (caaar elbind)
                   (d-emacs-bind-modifiers-to-string
                    (d-emacs-base-remove-indices indmods))))
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
(defun d-emacs-bind-compare-standardized-modifier-lists (indmods1 indmods2)
  "Compare INDMODS1 and INDMODS2, two lists of standardized key modifiers.

Each list should be sorted and indexed by prefix as per
`d-emacs-bind-index-and-sort-modifiers'. Return `(t)', `(nil)', or the string
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
longer list, but the lists are not equal in length, return `(t)' if INDMODS1 is
the shorter list (preceding by definition), or `(nil)' if INDMODS2 is shorter.

This function ensures consistent sorting of key modifiers lists by their
specificity and lexicographical order."
  (declare (ftype
            (function (list list) list)
            ;; (function ((list (cons integer integer)) (list (cons integer integer))) ; Compiler complains.
            ;;           (or (list boolean) void))
            )
           (pure t))
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

(defun d-emacs-bind-compare-coords (coords1 coords2)
  "This function compares two d-emacs-xkb-coordinates COORDS1 and COORDS2.

First it checks layer, then row, then place. If it finds no difference between
the coordinates, it returns nil."
  (declare (ftype (function (list list) list)
                  ;; (function ((list number) (list number)) ; Compiler complains.
                  ;;           (or (list boolean) void))
                  )
           (pure t))
  (cl-loop for coord1 in coords1
           and coord2 in coords2
           do (cond ((< coord1 coord2)
                     (cl-return `(t)))
                    ((> coord1 coord2)
                     (cl-return `(nil))))))

(defun d-emacs-bind-compare-suffixes (suffix1 suffix2)
  "Compare SUFFIX1 to SUFFIX2.

If SUFFIX1 islonger than SUFFIX2, it signals `(nil)'. If SUFFIX2 is longer than
SUFFIX1, it signals `(t)'. If they have the same length, it comparison uses the
following rules: If one of them is capitalized and the other isn't, the one that
is capitalized comes last. Otherwise, it compares them according to their
constituent character codes."
  (declare (ftype (function (string string) list
                            ;; (or (list boolean) void) ; Compiler complains.
                            ))
           (pure t))
  (d-emacs-base-compare-by-sequential-predicates
   suffix1 suffix2
   #'d-emacs-base-leq-p
   (lambda (sfx)
     (string= sfx (upcase sfx)))
   #'string<))

(defun d-emacs-bind-compare-elaborate-bindings (elbind1 elbind2 &optional coordsonly)
  "Compare elaborate bindings ELBIND1 and ELBIND2.

If COORDSONLY is t, then this function doesn't consider suffixes in sorting.
Note that, since in this case the function has three arguments, it can't be
directly used as a function by `sort', but has to be surrounded by a lambda to
be used in a two-argument function.

The main use of this function is in `d-emacs-bind-sort-and-format-bindlist', see
there for the sorting order."
  (declare (ftype (function (cons cons &optional boolean) list
                            ;; ((or (cons (cons (cons (list integer) string) (list number)) t)
                            ;;      (cons (cons ((cons (list integer) string)) void) t))
                            ;;  (or (cons (cons (cons (list integer) string) (list number)) t)
                            ;;      (cons (cons ((cons (list integer) string)) void) t))
                            ;;  &optional boolean)
                            ;; (or (list boolean) void)
                            )
                  ;; (function ; Compiler complains.
                  ;;  ((or (cons (cons (cons (list integer) string) (list number)) t)
                  ;;       (cons (cons ((cons (list integer) string)) void) t))
                  ;;   (or (cons (cons (cons (list integer) string) (list number)) t)
                  ;;       (cons (cons ((cons (list integer) string)) void) t))
                  ;;   &optional boolean)
                  ;;  (or (list boolean) void))
                  )
           (pure t))
  (let* ((srtmods1 (caaar elbind1))
         (srtmods2 (caaar elbind2))
         (suffix1 (cdaar elbind1))
         (suffix2 (cdaar elbind2))
         (hassfx1 (d-emacs-base-string-exists-and-nonempty suffix1))
         (hassfx2 (d-emacs-base-string-exists-and-nonempty suffix2))
         (coords1 (cdar elbind1))
         (coords2 (cdar elbind2))

         ;; Compare by modifiers.
         (compmods (d-emacs-bind-compare-standardized-modifier-lists srtmods1
                                                                     srtmods2)))

    ;; Look if suffixes exist in one case but not the other.
    (cl-flet ((true-and-not (val1 val2)
                (d-emacs-base-compare-if-decidable (lambda (vval1 vval2)
                                                     (and vval1 (not vval2)))
                                                   val1 val2)))

      (let* ((distinct-suffix-existence (true-and-not hassfx1 hassfx2))
             (distinct-coords-existence (true-and-not coords1 coords2)))

        (cond  ;; Unmatched bindings should come first.
         (distinct-coords-existence (not (car distinct-coords-existence)))

         ;; If COORDSONLY isn't on, look if one of them has been given as a key combination string by comparing suffixes. A non-empty suffix indicates that that binding features a key combination string and should come before one than does not have a suffix.
         ((unless coordsonly distinct-suffix-existence)
          (car distinct-suffix-existence))

         (compmods (car compmods))
         
         ((and hassfx1 hassfx2 (not coordsonly))
          (let ((compsfx (d-emacs-bind-compare-suffixes suffix1 suffix2)))
            (if compsfx (car compsfx)
              (message  "%s and %s have the same pre- and suffixes in %S."
                        elbind1 elbind2 (current-buffer)))))

         ((and coords1 coords2)
          (let ((compcoords (d-emacs-bind-compare-coords coords1 coords2)))
            (if compcoords (car compcoords)
              (message "%s and %s have the same prefixes and coordinates in %S."
                       elbind1 elbind2 (current-buffer))))))))))

;;;;;; Formatting
;;;;;;; Lists
(defun d-emacs-bind-sort-and-format-bindlist (blist &optional coordsonly prefun modlist)
  "Sort a d-emacs-xkb bindlist BLIST and format the result.

Key combinations that are not matched by the layout in `d-emacs-xkb-layout' are
put at the very top (because they are most likely errors or depreciated).

Modifiers are ordered according to their order in MODLIST
\(d-emacs-bind-modifiers-list by default). Sets of modifiers are ordered
according to the modifier in them the furthest back in MODLIST and so are added
below the modifier the furthest back.

Key combinations given by a full combination string are listed before others and
are ordered alphabetically (since they are supposed to be recalled phonetically
or lexically, not positionally). Key combination strings with capital characters
appear after those with downcased characters, and Greek letters appear after
Latin ones. Combinations with symbols that are neither appear after either and
are not otherwise sorted.

Keys are ordered according to their layer, row and place in the row.

With optional argument COORDSONLY, the function translates bindings that are
given by a binding string into ones given by coordinates if that is possible,
i.e. if the end of the binding string corresponds to a signal in the
`d-emacs-xkb-layout'.

PREFUN is a function that is applied to the bindlist after it is transformed
into an elaborate bindlist. This is useful to apply functions that should act on
an elaborate bindlist and whose results should be sorted, like coordinate
transformations.

To allow for using this function in `d--recursively-act-on-bindlist', it checks
if the input is an atom or nil and, if so, it returns the input.

This function is declared as side-effect-free, so please don't use PREFUNS with
side effects."
  (declare (ftype (function (t &optional boolean function list
                               ;; (function (list) list) (list integer) ; Compiler complains.
                               )
                            list))
           (side-effect-free t))
  (if (or (atom blist) (not blist))
      blist
    (let* ((modlist (if modlist modlist d-emacs-bind-modifiers-list))
           (case-fold-search nil)

           ;; Bring bindings in elaborate form and sort contained bindlists.
           (elaborate-list (mapcar (lambda (elt)
                                     (cond ((d-emacs-bind-p elt)
                                            (d-emacs-bind-elaborate-on-binding
                                             elt))
                                           ((atom elt) elt)
                                           (t (d-emacs-bind-sort-and-format-bindlist
                                               elt coordsonly prefun modlist))))
                                   blist))

           ;; Do any function that should be applied before the sorting.
           (prefun-elaborate-list (if prefun (funcall prefun elaborate-list) elaborate-list))

           (sorted-list (sort prefun-elaborate-list
                              (lambda (elt1 elt2)
                                (cond ((atom elt1) t) ; Atoms should be at the beginning.
                                      ((atom elt2) nil)
                                      ((not (d-emacs-bind-p elt1)) t) ; Then contained lists.
                                      ((not (d-emacs-bind-p elt2)) nil)
                                      (t (d-emacs-bind-compare-elaborate-bindings
                                          elt1 elt2 coordsonly))))))

           (formatted-list (d-emacs-bind--format-sorted-bindlist sorted-list coordsonly))

           ;; We have to remove the prefixes of sorted elements because they are already in the suffix string.
           (formatted-sans-unmatched-prefixes-list
            (mapcar
             (lambda (potunmatch)
               (if (and (consp potunmatch)
                        (d-emacs-bind-elaborate-unmatched-binding-p potunmatch))
                   (cons (cons (cons nil (cdaar potunmatch)) (cdar potunmatch))
                         (cdr potunmatch))
                 potunmatch))
             formatted-list))

           (final-list (mapcar (lambda (elt)
                                 (if (d-emacs-bind-p elt)
                                     (d-emacs-bind-reduce-binding elt coordsonly)
                                   elt))
                               formatted-sans-unmatched-prefixes-list)))
      final-list)))

(defun d-emacs-bind--format-sorted-bindlist (sblist &optional coordsonly)
  "Take a sorted bindlist SBLIST and format it.
That means inserting headings for unmatched elements, modifier combinations,
layers and rows.

If COORDSONLY is t, assume coordinates are prefered to suffixes when elaborate
bindings are reduced."
  (declare (ftype (function (list &optional boolean) list))
           (side-effect-free t))
  (let (runlst)
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
                                              (d-emacs-base-remove-indices indmods)))
                                     (haspfx (d-emacs-base-string-exists-and-nonempty prefix))

                                     (suffix (cdaar binding))
                                     (hassfx (d-emacs-base-string-exists-and-nonempty suffix))

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
                                             (d-emacs-base-remove-indices indmods2)))
                                   (suffix1 (cdaar binding1))
                                   (suffix2 (cdaar binding2))
                                   (hassfx1 (d-emacs-base-string-exists-and-nonempty suffix1))
                                   (hassfx2 (d-emacs-base-string-exists-and-nonempty suffix2))
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
                                                    (d-emacs-bind-compare-standardized-modifier-lists
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
                                                       (list (d-emacs-base-generate-newlines 2)
                                                             (format
                                                              ";;;;; Coordinates")))))

                              (if (not eqpfx)
                                  (setq runlst (append
                                                runlst
                                                (if (d-emacs-base-string-exists-and-nonempty prefix2)
                                                    (list (d-emacs-base-generate-newlines
                                                           (if (or (and (not eqmatch)
                                                                        coordsonly)
                                                                   (and (not eqhssfx)
                                                                        (not coordsonly)))
                                                               1
                                                             2))
                                                          (format
                                                           ";;;;;; %s"
                                                           prefix2))))))

                              (if (and (or (not eqpfx)
                                           (and (not eqlay)
                                                ;; This clause is for the specific case that function keys are on a shift-layer.
                                                (or coordsonly
                                                    (not (and hassfx2 (cl-member suffix2 d-emacs-bind-no-shift-if-string-list :test #'string=))))))

                                       (or coordsonly
                                           (and (not hassfx1) (not hassfx2))
                                           (and (not coordsonly)
                                                (not eqmatch))))

                                  (setq runlst (append runlst
                                                       (if layer2
                                                           (list
                                                            ;; If both have different match status or prefixes, a headline above the layer number will be inserted, so only one newline is needed. If both are given by strings, one is matched and the other is not, the match status is irrelevant and two newlines are needed.
                                                            (d-emacs-base-generate-newlines
                                                             (if (and eqpfx
                                                                      (or eqmatch
                                                                          (and (not coordsonly)
                                                                               hassfx1 hassfx2)))
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
                                                           (list (d-emacs-base-generate-newlines (if (and eqpfx
                                                                                                          eqlay)
                                                                                                     2
                                                                                                   1))
                                                                 (format
                                                                  ";;;;;;;; %s%s-%s"
                                                                  prefix2
                                                                  layer2
                                                                  row2))))))

                              (setq runlst (append runlst
                                                   (list (d-emacs-base-generate-newlines 1)
                                                         binding2)))))))
                   finally return runlst))))

;;;;;;; Strings
(defun d-emacs-bind--sort-and-format-marked-bindlist-string (&optional coordsonly prefun modlist)
  "Sort and format a marked bindlist-string.

The function will read the contents of the selected region and process them
using `d-emacs-bind-sort-and-format-bindlist' and
`d-emacs-bind--format-bindlist-into-string-before-insertion', then replace the
marked region with the result.

COORDSONLY, PREFUN and MODLIST are passed forward to
`d-emacs-bind-sort-and-format-bindlist'."
  (declare (ftype (function (&optional boolean (function (list) list) list
                                       ;; (list integer) ; Compiler complains.
                                       )
                            ;; void  ; Compiler complains.
                            t)))
  (save-excursion
    (let* ((blist (d-emacs-base-read-region))
           (formattedblist
            (d-emacs-bind-sort-and-format-bindlist blist coordsonly prefun modlist))
           (formattedstring (d-emacs-bind--format-bindlist-into-string-before-insertion formattedblist)))
      (d-emacs-base-replace-region formattedstring)
      
      nil)))

(defun d-emacs-bind--format-bindlist-into-string-before-insertion (blist &optional headname)
  "Convert BLIST into a formatted string for reinsertion.

If HEADNAME is provided, use that as the head for the converted structure.
Otherwise the headname of the list or the name of the containing folder is
used."
  (declare (ftype (function (list &optional string) string))
           (side-effect-free t))
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
          (replace-regexp-in-string " \\(d-emacs-mode-dynamic-binding .*\)\\)" " . \(\\1\)"
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
                                                         (d-emacs-base-containing-directory-base-name
                                                          filename))))))))
                              str-with-line-breaks-after-head)))
    finalstring))

;;;;;; Extraction
(defun d-emacs-bind-string (binding &optional translate csectoshft doublebind laysym)
  "Return a binding string or list of strings from Daselt-binding.

BINDING is expected to satisfy `d-emacs-bind-p'. Unless DOUBLEBIND is t, the
return value is a string representing the binding, potentially adjusted based on
the optional parameters TRANSLATE, CSECTOSHFT.

- If TRANSLATE is t, translate the binding using translation alists
  `d-emacs-bind-key-translations-alist' and
  `d-emacs-bind-outside-translations-alist'.

- If CSECTOSHFT is t, and the binding corresponds to the second layer with
  either no modifiers or one including `C-', replace the binding suffix with its
  downcased form and add an \"S-\" modifier.

- If DOUBLEBIND is t, check if the suffix of the binding or the key from
  coordinates matches the car of a cons cell in
  `d-emacs-bind-double-symbs-alist'. If matched, form a second binding using the
  corresponding cdr to form the returned string. Also check if a string in
  `d-emacs-bind-outside-translations-alist' matches the current binding string.
  This is necessary to be able to apply discrete modifiers to translated
  bindings. In either case, return a list of all binding strings.

- LAYSYM should be the symbol of the layout that is used. By default it is the
  symbol returned by `d-emacs-coords--dfk-or-xkb-layout'.

Signal an error if the binding is invalid (neither a suffix nor has matching
coordinates)."
  (declare (ftype (function (t &optional boolean boolean boolean symbol)
                            (or string list
                                ;; (list string)
                                )))
           (side-effect-free t))
  (let* ((laysym (or laysym (d-emacs-coords--dfk-or-xkb-layout)))
         (elbind (d-emacs-bind-elaborate-on-binding binding))
         (coords (cdar elbind))
         (sfx (cdaar elbind))
         (mods (d-emacs-base-remove-indices (caaar elbind)))
         (pfx (if coords ; If the binding is unmatched, then it has already its modifiers in its suffix.
                  (d-emacs-bind-modifiers-to-string mods)
                ""))
         (coordval (if coords (d-emacs-coords-binding coords)))
         (newsfx ;; Let's put an error check here.
          (let ((newsfx (if (d-emacs-base-string-exists-and-nonempty sfx)
                            sfx
                          (if coords coordval))))
            (if (d-emacs-base-string-exists-and-nonempty newsfx)
                newsfx
              (error (if coords (format "Coordinates %s in binding %s have no match in %s."
                                        coords binding laysym)
                       (format "%s has neither coordinates nor a suffix." binding))))))
         (non-translated-string (concat pfx newsfx))
         shifted) ; To check later whether it was shifted.

    ;; If on the second layer and csectoshft is t, and if either C is a modifier or there either are no mods and the length of the retained sfx is 1, replace the obtained sfx with its downcased variant and add S-modifier.
    (if (and csectoshft
             coordval
             (not (d-emacs-base-string-exists-and-nonempty sfx)) ; If the binding is given by a binding string, we don't want to shift it.
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

                          (if (d-emacs-base-string-exists-and-nonempty stumpdoublebind)
                              stumpdoublebind)))
        (if translate transstr non-translated-string)))))

;;;;;; Generation
(defun d-emacs-bind--generate-define-key-strings-from-bindlist (blist)
  "Create a `define-key' string for each binding in BLIST."
  (declare (ftype (function (list) (list string)))
           (side-effect-free t))
  (let* ((map (car blist))
         (body (cdr blist)))
    (mapcar (lambda (binding)
                    (concat "(define-key " map " (kbd \""
                      (d-emacs-base--escape-chars-in-str (d-emacs-bind-string binding))
                      "\"\) "
                      (let ((bindval (cdr binding)))
                        (if (stringp bindval)
                                        (concat "\"" bindval "\"")
                                (if (symbolp (eval bindval))
                                          (concat "'" (symbol-name (eval bindval))))))
                      "\)\n"))
            body)))

;;;;; Saving
(defun d-emacs-bind--set-bindlist-symbol (sym blist filename)
  "Set SYM to BLIST and mention its setting place FILENAME in documentation."
  (declare (ftype (function (symbol list string) symbol)))
  (set sym blist)
  (put sym 'variable-documentation (format "This bindlist was read in from %s." filename))
  sym)

(defun d-emacs-bind-save-bindlist-as-variable  (blist &optional pfx)
  "Save BLIST as a variable.

PFX is the prefix given to the saved bindlists. It is `d-emacs-' by default."
  (declare (ftype (function (list &optional string) (or symbol
                                                        ;; (list symbol) ; Compiler complains.
                                                        list))))
  (let* ((pkgname (d-emacs-base-containing-directory-base-name (buffer-file-name)))
         ;; (pkgsymb (intern pkgname))
         (mapsymbdefaultname (concat pkgname "-mode-map"))
         (pfx (or pfx "d-emacs"))
         (filepath (buffer-file-name))
         (head (d-emacs-bind-head blist)))

    (if (or (not head)
            (not (or (symbolp head)
                     (stringp head))))
                                                    (let* ((filename (file-name-nondirectory filepath))
               (filenamebase (file-name-base filepath))
               (symbol (if (string-match-p "special" filename)
                                                                       (intern (substring filenamebase 0 -1))
                                               (d-emacs-base-intern-from-parts (concat ; Mapconcat would insert two -'s for the empty string.
                                                          pfx
                                                          (if (string-match-p "-user-defined" filename)
                                                                                                          "-user-defined"
                                                                                  ""))
                                                         mapsymbdefaultname
                                                         "bindlist"))))
          (d-emacs-bind--set-bindlist-symbol symbol blist filepath))

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
              (d-emacs-bind--set-bindlist-symbol (d-emacs-base-intern-from-parts pfx namecore "bindlist")
                                                 blist
                                                 filepath))

          ;; This should produce a list of bindlist symbols.
          (d-emacs-base-funcalls-recursively
           blist
           `(((lambda (bblist &optional heads)
                                      (let* ((pfx ,pfx)
                       (filepath ,filepath)
                       (head (d-emacs-bind-head bblist))
                       (namecore (if (symbolp head)
                                                                                 (symbol-name head)
                                                         (if (stringp head)
                                                                                   head
                                                           (error "Expected a symbol or string a head of headed bindlist")))))
                  (d-emacs-bind--set-bindlist-symbol (d-emacs-base-intern-from-parts pfx namecore "bindlist")
                                                     bblist
                                                     filepath)))
              .
              (lambda (idx lst &optional heads) ; Test
                                      (let ((bblist (nth idx lst)))
                  (and (d-emacs-bind-bindlist-p bblist)
                       (not (d-emacs-bind-head (cdr bblist)))))
                                      )))
           (lambda (idx lst &optional _heads)
                                   (let ((elt (nth idx lst)))
               (d-emacs-bind-bindlist-p elt)))
           nil nil #'append))))))

;;;;; Custom generation functions
(defun d-emacs-bind-generate-replace-binding-strings-alist ()
  "Function to generate `d-emacs-bind-replace-binding-strings-alist'.

If `d-emacs-bind-replace-binding-strings-alist' is set manually, this function
is set to nil."
  (declare (ftype (function () list)))
  (remq nil (append (unless (or (bound-and-true-p d-emacs-stump)
                                d-emacs-bind-translate-C-1-1--2-C-g)
                      `(("C-g" . ,(d-emacs-bind-string `(("C-" . (1 1 -2)))))))
                    (unless d-emacs-bind-translate-keys
                      (mapcar (lambda (cns)
                                (let ((str (car cns)))
                                  (cons str (string-replace "C-" "A-" str))))
                              d-emacs-bind-key-translations-alist)))))

;;;;; Application
(defun d-emacs-bind-act-on-bindings (blist fun &optional nooutput)
  "Recursively apply FUN to all bindings in BLIST.

This function traverses BLIST, which is expected to be a structure containing
bindings, and applies the function FUN to each binding it encounters. It
determines elements that qualify as bindings using `d-emacs-bind-p'.

Parameters:

- BLIST: The list or structure containing potential bindings.

- FUN: The function to apply to each binding.

- NOOUTPUT: If non-nil, do not collect the output in a list.

The function uses `d-funcall-recursively' to manage traversal:

- It checks if each element is a binding using `d-emacs-bind-p'.

- Elements that are not atoms and do not qualify as bindings are further
  recursed into as lists.

- If NOOUTPUT is nil, collected results are combined using `cons'.

The results are conditionally collected based on whether NOOUTPUT is set. Head
elements of lists are determined using `d-emacs-bind-head' and added to RESTARGS
so they can be used by FUN."
  (declare (ftype (function (list (function (t) t) &optional boolean) t
                            ;; (or list void) ; Compiler complains.
                            )))
  (d-emacs-base-funcall-recursively blist
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

(defun d-emacs-bind-with-eval-unless-init (filepath fun &optional initcond)
  "If FILEPATH contains `-init-' in its base, execute FUN.

Otherwise, execute FUN once INITCOND has been evaluated.

INITCOND defaults to the feature whose name is that of the directory containig
FILEPATH."
  (declare (ftype (function (string (function (string) t) &optional (or string symbol)) t)))
  (let* ((filename (d-emacs-base-containing-directory-base-name filepath))
         (pkgsym (intern filename)))
    (if (string-match-p "-init-" filename)
        (funcall fun filepath)
      (with-eval-after-load (or initcond pkgsym)
        (funcall fun filepath)))))

(defun d-emacs-bind-apply-bindlist (blist &optional backuppfx witheval)
  "Rebind keys in a given keymap after evaluating an associated condition.

The rebinding is specified by the bindlist BLIST, which has structurally two
forms:

1. A single keymap: ([MAP] BIND1 BIND2 ...)

2. Multiple keymaps with respective bindings: (EVAL (MAP1 BIND11 BIND12 ...)
  (MAP2 BIND21 BIND22 ...))

EVAL is an expression to be evaluated within
`with-eval-after-load' if WITHEVAL is t. If the EVAL entry is ommitted, it
defaults to the feature whose name is the same as directory name containing the
current buffer's file.

- MAP is a symbol referring to the keymap to modify. If the MAP entry is
omitted, it will default to the mode map corresponding to the containing
directory name.

For each MAP, the current keymap is backed up as `BACKUPPFX-MAP-backup' before
rebindings are applied. If `BACKUPPFX-MAP-backup' is already bound to a keymap,
no backup is made, indicating that a prior backup exists. BACKUPPFX is
`d-emacs-' by default.

If WITHEVAL is t, the bindlist will still be applied without evaluation if it is
in a file with `-init-' in its base name, if no evaluation condition can be set
or if the buffer is not currently visiting a file. Be careful, if the map the
bindlist is applied to is not loaded, application will throw an error."
  (declare (ftype (function (list &optional string boolean)
                            ;; void  ; Compiler complains.
                            t)))
  (let* ((bufname (buffer-file-name))
         (pkgname (if bufname (d-emacs-base-containing-directory-base-name bufname)))
         (mapsymbdefault (if pkgname (intern (concat pkgname "-mode-map")))))

    (d-emacs-bind-act-on-bindings
     blist
     (lambda (bind &optional heads)
       (let* ((headpairt (= (length heads) 2))
              (evalcnd (if headpairt
                           (car heads)
                         (intern pkgname)))
              (mapsymb (if headpairt
                           (car (last heads))
                         (if heads
                             (car heads)
                           (or mapsymbdefault
                               (error "No map symbol specified and buffer not visiting a file")))))
              (backuppfx (or backuppfx "d-emacs-"))
              (backupsymb (intern (concat backuppfx (symbol-name mapsymb) "-backup"))))
         (cl-flet ((backup-and-apply-binding (&optional _dummy1 _dummy2)
                     (let ((map (symbol-value mapsymb))) ; Mapsymb has to be evaluated only within the with-eval-after-load expression.

                       (unless (and (boundp backupsymb) ; Don't overwrite an already existing backup.
                                    (keymapp (symbol-value backupsymb)))
                         (set backupsymb map)
                         nil)
                       (d-emacs-bind-apply-binding bind map))))
           (if (and witheval bufname evalcnd)
               (d-emacs-bind-with-eval-unless-init
                (buffer-file-name) #'backup-and-apply-binding evalcnd)
             (backup-and-apply-binding)))))
     t)
    nil))

(defun d-emacs-bind-apply-binding (binding map)
  "Apply the key BINDING in MAP.

The binding value is evaluated and assigned to the corresponding keys.

Bindings are translated if `d-emacs-bind-translate-keys' is set to t."
  (declare (ftype (function (cons keymap)
                            ;; void ; Compiler complains.
                            t)))
  (let* ((orig-binding-strings (d-emacs-bind-string binding d-emacs-bind-translate-keys t t))
         (binding-strings
          (mapcar (lambda (bstr)
                    (alist-get bstr
                               (if (equal '(nil) d-emacs-bind-replace-binding-strings-alist)
                                   (d-emacs-bind-generate-replace-binding-strings-alist)
                                 d-emacs-bind-replace-binding-strings-alist)
                               bstr
                               nil
                               #'string=))
                  orig-binding-strings))
         (value (cdr binding)))
    (mapc (lambda (bstr)
            (define-key map (kbd bstr) (eval value))
            (if (d-emacs-base-exists-p d-emacs-coords-bad-combinations-list
                                       (lambda (combination)
                                         (string= (d-emacs-bind-string (cons combination nil))
                                                  bstr)))
                (define-key map
                            (kbd (string-replace
                                  "H-" "s-M-"
                                  (string-replace "C-" "A-" bstr)))
                            (eval value))))
          binding-strings)
    nil))


;;;;; Coordinate changes


(defun d-emacs-bind-change-coords-in-bindlist (blist coordlistlist)
  "Change coordinates in BLIST according to COORDLISTLIST.

Return the modified bindlist."
  (declare (ftype (function (list list
                                  ;; (list (list number)) ; Compiler complains.
                                  )
                            list))
           (pure t))
  (mapcar (lambda (bind) (if (d-emacs-bind-p bind)
                             (d-emacs-bind-change-coords-in-binding bind coordlistlist)
                           (if (consp bind)
                               (d-emacs-bind-change-coords-in-bindlist bind coordlistlist)
                             bind)))
          blist))

(defun d-emacs-bind-change-coords-in-bindlist-during-sorting (blist coordlistlist)
  "Change coordinates in BLIST according to COORDLISTLIST.

Return the modified bindlist.

Note that unlike `d-emacs-bind-change-coords-in-bindlist' this function does not
recurse into sub-lists of a bindlist. This is because it should be used as a
prefun for `d-emacs-bind-sort-and-format-bindlist'.
`d-emacs-bind-sort-and-format-bindlist' already passes on prefuns in its
recursive calls, so if this function would recurse as well, the coordinate
change would be applied twice."
  (declare (ftype (function (list (list (list number))) list))
           (pure t))
  (mapcar (lambda (bind) (if (d-emacs-bind-p bind)
                             (d-emacs-bind-change-coords-in-binding bind coordlistlist)
                           bind))
          blist))

(defun d-emacs-bind-change-coords-in-binding (bind coordlistlist)
  "Change coordinates in BIND according to COORDLISTLIST.

Return the modified binding."
  (declare (ftype (function (cons list
                                  ;; (list (list number)) ; Compiler complains.
                                  )
                            cons))
           (pure t))
  (if (stringp (car bind))
      bind
    (let* ((carcoordsp (d-emacs-coords-p (car bind)))
           (cdarcoordsp (unless carcoordsp (d-emacs-coords-p (cdar bind))))
           (origcoords (cond (carcoordsp (car bind))
                             (cdarcoordsp (cdar bind))))
           (newcoords (when origcoords
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
COORDLIST, the function returns the next coordinate value from COORDLIST. If no
matching coordinate is found or the matching coordinate is the last entry in
COORDLIST, the function returns the original coordinate value from ORIGCOORDS."
  (declare (ftype (function (list list) list)
                  ;; (function ((list number) (list (list number))) (list number))  ; Compiler complains.
                  )
           (pure t))
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
          (d-emacs-base-index origcoords)))

;;;;; Drawing
(defun d-emacs-bind--elbind-to-placeval (elbind)
  "Return PLACEVAL whose car is coords of ELBIND and cdr is its cdr.

If ELBIND has no coordinates, return nil."
  (declare (ftype (function (cons) cons)
                  ;; (function ((or (cons (cons (cons (list integer) string) (list number)) t) ; Compiler complains.
                  ;;                (cons (cons ((cons (list integer) string)) void) t)))
                  ;;           (cons (list number) t))
                  ))
  (let ((coords (cdar elbind))
        (val (cdr elbind)))
    (if coords (cons coords val))))

(defun d-emacs-bind-draw-bindlist-layer (blistsymb laycoord &rest mods)
  "Draw a layer of the bindlist identified by BLISTSYMB.

Use a maximized window. LAYCOORD specifies the layer to draw, and MODS the
modifiers of the layer."
  (declare (ftype (function (symbol number &rest list
                                    ;; (list integer) ; Compiler complains.
                                    )
                            void)))
  (interactive (append (list (intern (completing-read "Bindlist: " obarray
                                                      (lambda (symb)
                                                        (and (boundp symb)
                                                             (d-emacs-bind-bindlist-p
                                                              (symbol-value symb))))
                                                      t nil
                                                      'variable-name-history
                                                      "d-emacs-d-emacs-mode-map-bindlist"))
                             (completing-read "Layer: "
                                              (mapcar
                                               (lambda (number) (number-to-string number))
                                               d-emacs-coords-layer-numbers-list)
                                              t nil nil nil
                                              "1"))
                       (cl-loop for repl = (completing-read "Modifier (empty to exit): "
                                                            (mapcar (lambda (mod)
                                                                      (char-to-string mod))
                                                                    d-emacs-bind-modifiers-list))
                                while (not (string-empty-p repl))
                                collect repl)))
  (let ((placevals (d-emacs-coords-placevals-matching-indexed-rx
                    (remq nil ; Filters out bindings without coordinate matches.
                          (mapcar #'d-emacs-bind--elbind-to-placeval
                                  (d-emacs-bind--elbinds-matching-modifier-regexps
                                   (symbol-value blistsymb) mods)))
                    0
                    laycoord)))
    (funcall
     (if (called-interactively-p 'any)
         #'d-emacs-coords-draw-placevals-in-temp-buffer
       #'d-emacs-coords-draw-placevals)
     placevals
     nil
     nil
     current-prefix-arg)))

(defun d-emacs-bind--elbinds-matching-modifier-regexps (blist modrxs)
  "Return elaborate forms of bindings in BLIST matching MODS.

Filter bindings by modifier regexps MODRXS. A modifier regexp is a string
matched against all modifiers in a binding. If the regexp string starts with
`^', the binding is matched by the regexp if and only if no modifier in the
binding matches the string."
  (declare (ftype (function (t list) list)
                  ;; (function (t (list string)) (list (list (cons (list number) t)))) ; Compiler complains.
                  )
           (pure t))
  (let* ((case-fold-search nil)
         (pblist (d-emacs-base-filter-list blist #'d-emacs-bind-p))
         (elblist (mapcar (lambda (bind)
                            (d-emacs-bind-elaborate-on-binding bind))
                          pblist))
         (purelblist (d-emacs-base-filter-list elblist (lambda (bind)
                                                         (not (d-emacs-bind--string-binding-p bind))))))

    (d-emacs-base-filter-list purelblist
                              (lambda (elbind)
                                (cl-flet* ((ispositive (modrx)
                                             (not (string-match-p (rx string-start "^")
                                                                  modrx))))
                                  (let* ((elbindmods (d-emacs-base-remove-indices
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

(defun d-emacs-bind-draw-bindings-from-regexps (blistrx valrx coordrx modrxs &optional boundaries)
  "Draw the bindings matching BLISTRX, VALRX, COORDRX and MODRXS.
This is the most powerful of the Daselt-helper functions.

- BLISTRX matches against all bindlist-names. For example, if you want to draw
  all layers of all bindlists for StumpWM, provide `stumpwm` for BLISTRX and
  have the other regexps be empty strings.

- VALRX matches against the values of all bindings in matched bindlists. For
  example, if you want to draw all bindings to `projectile`-functions in all
  bindlists, provide `projectile' for VALRX and leave the other regexps empty.

- COORDRX matches against all coordinates. So if you want to draw the first row
  of all layers of all bindlists with any modifiers, provide `. -1 [-]?.` for
  COORDRX and leave the other regexps empty.

- MODRXS is a list of regexps that match against the modifiers of bindings. The
  syntax is adapted to make the matching intuitive: if the regexp starts with
  `^`, a binding is matched if and only if the regexp does not match any
  modifiers in it. So for instance, if you want to draw all bindings with a
  C-modifier and no s-modifier, with or without any other modifiers, provide two
  regexps for MODRXS, `C` `^s`.

- BOUNDARIES are the boundaries the drawn layouts should have. If left nil then

  - if `d-emacs-bind-boundaries' is non-nil then

    - the first element of it is used if any coordinates that should be drawn
      are on layer 0.

    - otherwise, the second element of `d-emacs-bind-boundaries' is used.

  - otherwise, the boundaries are calculated automatically from the supplied
    placevals. Note that this can leave it visually unclear where a particular
    value is in the layout unless the entire layout is spanned by the
    coordinates of the placevals that should be drawn.

If you are using `d-emacs-mode', then `d-emacs-bind-boundaries' is set
automatically and you don't have to worry about it."
  (declare (ftype (function (string string string string &optional cons
                                    ;; (cons (list number) (list number)) ; Compiler complains.
                                    )
                            void)))
  (interactive (list (read-string "Bindlist regexp (leave empty to match all): ")
                     (read-string "Value regexp (leave empty to match all): ")
                     (read-string "Coordinate regexp (leave empty to match all): ")
                     (cl-loop for repl = (completing-read "Modifier (empty to exit): "
                                                          (mapcar (lambda (mod)
                                                                    (char-to-string mod))
                                                                  d-emacs-bind-modifiers-list))
                              while (not (string-empty-p repl))
                              collect repl)))

  (let* ((blistsymbs (d-emacs-base-filter-obarray #'d-emacs-bind-bindlist-symb-p))
         (matchedblsymbs (if (string-empty-p blistrx)
                             blistsymbs
                           (d-emacs-base-filter-list blistsymbs (lambda (blistsymb)
                                                                  (string-match-p
                                                                   blistrx
                                                                   (symbol-name blistsymb)))))))

    (d-emacs-base-with-max-buffer-maybe-return
     "*daselt-layout*"
     (lambda ()
       (cl-loop for blistsymb in matchedblsymbs
                for blist = (symbol-value blistsymb)
                do (insert "\n" (symbol-name blistsymb) "\n")
                do (let* ((modmatchedbinds
                           (if modrxs
                               (d-emacs-bind--elbinds-matching-modifier-regexps
                                blist modrxs)
                             blist)))

                     ;; Isolate the matched bindings for each modifier combination.
                     (cl-loop for mods in (d-emacs-base-powerlist d-emacs-bind-modifiers-list)
                              do (let* ((specificmodrxs
                                         (append (mapcar #'char-to-string mods)
                                                 (mapcar (lambda (mod)
                                                           (concat
                                                            "^" (char-to-string mod)))
                                                         (d-emacs-base-complement
                                                          d-emacs-bind-modifiers-list
                                                          mods))))

                                        (specificmodmatchedbinds (d-emacs-bind--elbinds-matching-modifier-regexps modmatchedbinds specificmodrxs))

                                        (modmatchedplacevals
                                         (remq nil (mapcar #'d-emacs-bind--elbind-to-placeval
                                                           specificmodmatchedbinds)))

                                        ;; If C-g is not translated by `d-stump' or `d-emacs-bind-translate-C-1-1--2-C-g' and the modifier is `C', check all placevals if they are bound to "g", and, if so, put the value of that placeval on `C-1-1--2'.
                                        (modmatchedplacevals-C-g-remapped
                                         (if (and (equal mods '(C))
                                                  (not (or (bound-and-true-p d-emacs-stump)
                                                           d-emacs-bind-translate-C-1-1--2-C-g)))
                                             (mapcar
                                              (lambda (placeval)
                                                (let* ((coords (car placeval))
                                                       (val (cdr placeval)))
                                                  (if (string= "g" (d-emacs-coords-binding coords))
                                                      (cons '(1 1 -2)
                                                            val)
                                                    placeval)))
                                              modmatchedplacevals)
                                           modmatchedplacevals))
                                        
                                        (coordmatchedplacevals
                                         (if (d-emacs-base-string-exists-and-nonempty coordrx)
                                             (d-emacs-coords-placevals-matching-coordrx
                                              modmatchedplacevals coordrx)
                                           modmatchedplacevals-C-g-remapped))

                                        (valmatchedplacevals
                                         (if (d-emacs-base-string-exists-and-nonempty valrx)
                                             (d-emacs-base-filter-list
                                              coordmatchedplacevals
                                              (lambda (placeval)
                                                (string-match-p
                                                 valrx (d-emacs-coords-extract-value-string
                                                        (cdr placeval)))))
                                           coordmatchedplacevals)))

                                   (when valmatchedplacevals
                                     (insert (format "\n%s\n"
                                                     (d-emacs-bind-modifiers-to-string
                                                      mods)))
                                     (d-emacs-coords-draw-placevals
                                      valmatchedplacevals

                                      ;; If there's something on layer 0, use extended boundaries.
                                      (or boundaries
                                          (if (d-emacs-base-exists-p
                                               valmatchedplacevals
                                               (lambda (placeval)
                                                 (= (caar placeval) 0)))
                                              (nth 0 d-emacs-bind-boundaries)
                                            (nth 1 d-emacs-bind-boundaries))))))))
                
                do (insert "\n"))))))

;; (defun d-draw-free-places-from-regexps (blistrx coordrx &rest modrxs)
;;   "Draw free bindings that match BLISTRX and COORDRX.
;; The arguments work as for `d-emacs-bind-draw-bindings-from-regexps', see the documentation
;; there."
;;   (interactive (append (list (read-string "Bindlist regexp (leave empty to match all): ")
;;                              (read-string "Coordinate regexp (leave empty to match all): "))
;;                        (cl-loop for repl = (completing-read "Modifier (empty to exit): "
;;                                                             (mapcar (lambda (mod)
;;                                                                       (char-to-string mod))
;;                                                                     d-emacs-bind-modifiers-list))
;;                                 while (not (string-empty-p repl))
;;                                 collect repl)))

;;   (let* ((blistsymbs (d-emacs-base-filter-obarray (λ (sym)
;;                                                      (d-emacs-bind-bindlist-p (symbol-value sym)))))
;;          (matchedblsymbs (if (string-empty-p blistrx)
;;                              blistsymbs
;;                            (d-emacs-base-filter-list blistsymbs (lambda (blistsymb)
;;                                                                   (string-match-p
;;                                                                    blistrx
;;                                                                    (symbol-name blistsymb))))))

;;          (allcoords (d-emacs-base-flatten-until  d-xkb-coordinates
;;                                                  (lambda (lst)
;;                                                    (d-emacs-coords-p
;;                                                     (car lst)))))

;;          (allmodifiercoordscombinations (apply #'append (mapcar (lambda (coords)
;;                                                                   (mapcar
;;                                                                    (lambda (mods)
;;                                                                      (cons (d-emacs-bind-modifiers-to-string mods)
;;                                                                            coords))
;;                                                                    (d-emacs-base-powerlist d-emacs-bind-modifiers-list)))
;;                                                                 allcoords)))

;;          (allbinds (mapcar (lambda (modcoords)
;;                              (cons modcoords "free"))
;;                            allmodifiercoordscombinations))

;;          (allmodmatchedbinds (d-emacs-bind--elbinds-matching-modifier-regexps
;;                               allbinds modrxs))

;;          (usedmodmatchedbinds (cl-loop for blistsymb in matchedblsymbs
;;                                        for blist = (symbol-value blistsymb)
;;                                        append
;;                                        (if modrxs
;;                                            (d-emacs-bind--elbinds-matching-modifier-regexps
;;                                             blist modrxs)
;;                                          blist)))

;;          (freemodmatchedbinds (d-emacs-base-complement allmodmatchedbinds usedmodmatchedbinds
;;                                                        (lambda (allbind usedbind)
;;                                                          (let ((allcoords (cdar allbind))
;;                                                                (usedcoords (cdar usedbind))
;;                                                                (allindpfxs (caaar allbind))
;;                                                                (usedindpfxs (caaar usedbind)))
;;                                                            (and (equal allcoords usedcoords)
;;                                                                 (equal allindpfxs usedindpfxs)))))))

;;     (d-emacs-base-with-max-buffer-maybe-return
;;      "*daselt-layout*"
;;      ;; Isolate the matched bindings for each modifier combination.
;;      (lambda ()
;;        (insert (format "Matched layouts: %s\n" matchedblsymbs))
;;        (cl-loop for mods in (d-emacs-base-powerlist d-emacs-bind-modifiers-list)
;;                 do (let* ((specificmodrxs
;;                            (append (mapcar #'char-to-string mods)
;;                                    (mapcar (lambda (mod)
;;                                              (concat
;;                                               "^" (char-to-string mod)))
;;                                            (d-emacs-base-complement
;;                                             d-emacs-bind-modifiers-list mods))))

;;                           (specificmodmatchedfreebinds (d-emacs-bind--elbinds-matching-modifier-regexps freemodmatchedbinds specificmodrxs))

;;                           (modmatchedfreeplacevals
;;                            (mapcar #'d-emacs-bind--elbind-to-placeval
;;                                    specificmodmatchedfreebinds))

;;                           (coordmatchedfreeplacevals
;;                            (if (string-empty-p coordrx)
;;                                modmatchedfreeplacevals
;;                              (d-xkb-placevals-matching-coordrx
;;                               modmatchedfreeplacevals coordrx))))

;;                      (if coordmatchedfreeplacevals
;;                          (progn (insert (format "\n%s\n"
;;                                                 (d-emacs-bind-modifiers-to-string
;;                                                  mods)))
;;                                 (d-emacs-coords-draw-placevals coordmatchedfreeplacevals t))))
;;                 do (insert "\n"))))))

;;;;; Import
(defun d-emacs-bind-convert-bindings-to-bindlist (&optional coordsonly)
  "Convert the marked key bindings into a Daselt-bindlist.
If COORDSONLY is t, replace suffixes by coordinates whenever possible.

Four formats are accepted:

- Bindings of the form `(define-key MAP (kbd KEY) VAL)' or `(keymap-set MAP
\(kbd KEY) VAL)'.

- Bindings of the form `(bind-key KEY VAL &optional MAP)'.

- Bindings of the form `(global-set-key (kbd KEY) VAL)' or `(keymap-global-set
\(kbd KEY) VAL)'.

- Sections of `use-package' configurations of the form `:bind (CONSES)' or
`:bind (:map MAP CONSES)'."
  (declare (ftype (function (&optional boolean) void)))
  (interactive (list (yes-or-no-p  "Convert prefixes to coordinates? ")))
  (let* ((parsefuns (d-emacs-base-filter-obarray
                     (lambda (symb)
                       (and (fboundp symb)
                            (string-match-p (rx string-start
                                                "d-emacs-bind--parse-for-")
                                            (symbol-name symb))))))
         (mapsblistpieces (mapcar #'funcall parsefuns))
         (maps (apply #'append (mapcar #'car mapsblistpieces)))
         (blistpieces (apply #'append (mapcar #'cdr mapsblistpieces)))
         (redmaps (cl-remove-duplicates maps)))

    (pop-to-buffer "*daselt-imported-bindlists*")
    (cl-loop for redmap in redmaps
             do (let* ((blist redmap))
                  (cl-loop for idx from 0 to (1- (length blistpieces))
                           do (let* ((map (nth idx maps))
                                     (blistpiece (nth idx blistpieces)))
                                (if (eq map redmap)
                                    (setq blist (cons blist blistpiece)))))
                  (d-emacs-base-goto-max)
                  (if (d-emacs-bind-bindlist-p blist)
                      (insert
                       (d-emacs-bind--format-bindlist-into-string-before-insertion
                        (d-emacs-bind-sort-and-format-bindlist blist coordsonly)
                        coordsonly)
                       "\n"))))))

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
  (declare (ftype (function (string &optional integer integer integer  integer (function () symbol))
                            cons
                            ;; (cons (list symbol) (list list)) ; Compiler complains.
                            ))
           (side-effect-free t))
  (save-excursion
    (d-emacs-base-goto-min)
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
                     (if (d-emacs-base-string-exists-and-nonempty maprxstr)
                         (read maprxstr)
                       (if mapdefaultfun (funcall mapdefaultfun)))))
              (key (if keypos (d-emacs-base-remove-text-properties-from-string
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

(defun d-emacs-bind--parse-for-define-key-bindings ()
  "Parse all `define-key'-bindings in REGION.

Return lists of maps and bindlistpieces. REGION is the active region, or the
current buffer if no region is active."
  (declare (ftype (function () cons
                            ;; (cons (list symbol) (list list)) ; Compiler complains.
                            ))
           (side-effect-free t))
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
    (d-emacs-bind-parse-for-keybindings drx 1 2 3)))

(defun d-emacs-bind--parse-for-global-key-set-bindings ()
  "Parse all `global-set-key'-bindings in REGION.

Return lists of maps and bindlistpieces. REGION is the active region, or the
current buffer if no region is active."
  (declare (ftype (function () (cons (list symbol) (list list))))
           (side-effect-free t))
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
    (d-emacs-bind-parse-for-keybindings grx nil 1 2)))

(defun d-emacs-bind--parse-for-bind-key-bindings ()
  "Parse all `bind-key'-bindings in REGION.

Return lists of maps and bindlistpieces. REGION is the active region, or the
current buffer if no region is active."
  (declare (ftype (function () cons
                            ;; (cons (list symbol) (list list)) ; Compiler complains.
                            ))
           (side-effect-free t))
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
    (d-emacs-bind-parse-for-keybindings brx 3 1 2)))

(defun d-emacs-bind--parse-for-use-package-bindings ()
  "Parse all `:bind'-sections of `use-package' configurations in REGION.

Return lists of maps and bindlistpieces. REGION is the active region, or the
current buffer if no region is active."
  (declare (ftype (function () cons
                            ;; (cons (list symbol) (list list)) ; Compiler complains.
                            ))
           (side-effect-free t))
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

    (d-emacs-bind-parse-for-keybindings urx 2 nil nil 3 (lambda () 'global-map))))

;;;; Provide
(provide 'd-emacs-bind)
;;; d-emacs-bind.el ends here
