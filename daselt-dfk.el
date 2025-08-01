;;; daselt-dfk.el --- Creation of dual-function-keys configurations -*- lexical-binding: t; -*-

;; Copyright (C) 2024, 2025  Alexander Prähauser

;; Author: Alexander Prähauser <ahprae@protonmail.com>
;; Version: 1.0
;; Keywords: tools, external
;; URL: https://gitlab.com/nameiwillforget/d-emacs/-/blob/master/daselt-dfk.el

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

;; daselt-dfk.el facilitates the creation and management of dual-function key
;; (DFK) configurations within Emacs. Leveraging the `daselt-coords` layer, this
;; package allows users to define keys that perform different actions based on
;; whether they are tapped or held, enhancing keyboard functionality and
;; efficiency.

;; Key Features:

;; - **Dual-Function Key Configuration**: Defines keys that can act as modifiers
;; (e.g., Ctrl, Meta, Hyper, Super) when held and perform standard key actions
;; when tapped.

;; - **Customizable Layout Support**: Supports both ANSI and ISO keyboard layouts,
;; ensuring flexibility across different keyboard types.

;; - **Layer Integration**: Integrates dual-function keys into multiple layers,
;; allowing complex keybinding schemes that can adapt to various user
;; workflows.

;; - **Extensive Customization Options**: Offers a wide range of customizable
;; settings, including modifier key coordinates, locking behaviors, tap and
;; hold timings, and synthetic key delays.

;; - **Automatic Configuration Generation**: Provides functions to automatically
;; generate YAML configuration files based on defined keybindings and layout
;; settings, streamlining the setup process.

;; - **Integration with Existing Layouts**: Includes macros to import and extend
;; existing `daselt-xkb` layouts, enabling seamless integration with
;; pre-existing keyboard configurations.

;; - **Discrete Modifier Handling**: Supports discrete modifiers, allowing certain
;; keys to act as standalone modifiers without affecting other keybindings.

;; Overall, daselt-dfk.el provides a robust framework for enhancing keyboard
;; interactions in Emacs through dual-function keys, offering both flexibility and
;; ease of integration for users looking to customize their editing environment.

;; Usage:

;; The main two functions of this package are `daselt-dfk-generate-config' and
;; `daselt-dfk-generate-layer-0-placevals'. The first generates a config based
;; on the bindform in the symbol value of the symbol stored in
;; `daselt-dfk-bindform-symbol'. The default is
;; `daselt-dfk-default-bindlist-form'. The second generates a list of placevals
;; from the same bindform that can be used to form a daselt-coords-layer. This
;; is done through `daselt-dfk-import-layout' and
;; `daselt-dfk-import-current-layout'. Both `daselt-dfk-generate-config' and
;; `daselt-dfk-generate-layer-0-placevals' can be tailored through the customs
;; in the group `daselt-dfk'.

;;; Code:
;;;; Preamble
(defvar daselt-xkb-layout)
(require 'daselt-coords)


;;;; Initial Constants
(defconst daselt-dfk-default-bindlist-form
  '`(;; Ctrl
     ((2 0) . ((2 0) . ,(nth 2 daselt-dfk-C-coords)))
     ("kp3" . ("kp3" . ,(nth 2 daselt-dfk-C-coords)))

     ;; Hyper
     ((-2 -6) . (,(nth 0 daselt-dfk-H-coords) . ,(nth 0 daselt-dfk-H-coords)))
     ((1 0) . ((1 0) . ,(nth 0 daselt-dfk-H-coords)))

     ;; Super
     ((-2 -5) . (,(nth 0 daselt-dfk-s-coords)
                 . ,(nth 0 daselt-dfk-s-coords)))

     ;; Meta
     ((2 -4) . (,(car daselt-dfk-M-coords) . ,(car daselt-dfk-M-coords)))

     ;; Alt
     ,(unless (string= daselt-dfk-keyboard-layout-type "ansi")
        `((1 -7) . ("f8" . ,(nth 0 daselt-dfk-H-coords))))
     ((1 7) . ("f8" . ,(nth 0 daselt-dfk-H-coords)))

     ;; Shift and discrete Meta
     ((2 -1) . ("f15" . ,(nth 0 daselt-dfk-locking-2-coords)))
     ((2 1) . ("f15" . ,(nth 0 daselt-dfk-locking-2-coords)))

     ;; 3
     ((0 -6) . ("f14" . ,(nth 0 daselt-dfk-locking-3-coords)))
     ((0 6) . ("f14" . ,(nth 1 daselt-dfk-locking-3-coords)))

     ;; 4
     (,(if daselt-dfk-outside-mods '(-2 -3) '(-1 -3))
      . (,(if daselt-dfk-outside-mods "scrolllock" '(-1 -3))
         . (,(nth 1 daselt-dfk-non-locking-2-coords)
            ,(nth 0 daselt-dfk-non-locking-3-coords))))
     (,(if daselt-dfk-outside-mods '(-2 3) '(-1 3))
      . (,(if daselt-dfk-outside-mods "scrolllock" '(-1 3))
         . (,(nth 0 daselt-dfk-non-locking-2-coords)
            ,(nth 1 daselt-dfk-non-locking-3-coords))))

     ;; 5
     ((-2 -4) . ((-2 -4) . ,(nth 0 daselt-dfk-locking-5-coords)))
     ((-2 4) . ((-2 4) . ,(nth 0 daselt-dfk-locking-5-coords)))

     ;; 6
     (,(if daselt-dfk-outside-mods '(-2 -2) '(-2 -3))
      . (,(if daselt-dfk-outside-mods
              '(-2 -2)
            "scrolllock")
         . (,(nth 1 daselt-dfk-non-locking-2-coords)
            ,(nth 0 daselt-dfk-locking-5-coords))))
     (,(if daselt-dfk-outside-mods '(-2 2) '(-2 3))
      . (,(if daselt-dfk-outside-mods
              '(-2 2)
            "scrolllock")
         . (,(nth 0 daselt-dfk-non-locking-2-coords)
            ,(nth 0 daselt-dfk-locking-5-coords))))

     ;; 7
     (,(if (string= daselt-dfk-keyboard-layout-type "ansi") '(1 -7) '(1 -6))
      . ((1 -6) . (,(nth 0 daselt-dfk-non-locking-3-coords)
                   ,(nth 0 daselt-dfk-non-locking-5-coords))))
     ((1 6)
      . ((1 6) . (,(nth 1 daselt-dfk-non-locking-3-coords)
                  ,(nth 1 daselt-dfk-non-locking-5-coords))))

     ;; 8
     ((-1 -6) . ("f11" . (,(nth 1 daselt-dfk-non-locking-2-coords)
                          ,(nth 0 daselt-dfk-locking-3-coords)
                          ,(nth 0 daselt-dfk-non-locking-5-coords))))
     ((-1 6) . ("f11" . (,(nth 0 daselt-dfk-non-locking-2-coords)
                         ,(nth 1 daselt-dfk-locking-3-coords)
                         ,(nth 1 daselt-dfk-non-locking-5-coords)))))
  "Form to generate the d-dfk layouts.
When evaluated, returns a bindlist that can be used in
`daselt-dfk-generate-config'. The exact composition of the bindlist depends on
the constants `daselt-dfk-outside-mods' and `daselt-dfk-keyboard-layout-type'.

The numbers 2, 3 and 5 in variable names here refer to
ISO-level-shift-modifiers. These are used by xkb to access higher levels through
combining them. In Daselt's layer numbering, these are the modifier combinations
needed to access layers:

3 = 3
4 = 3 + 5 (meaning a layer 4 accessed by combining a level-3-shift and
a level-5-shift,)
5 = 5
6 = 2 + 5
7 = 3 + 5
8 = 2 + 3 + 5

These layer-level combinations are saved in `daselt-dfk-layer-level-shifts' to
allow the level combinations to be translated into layers in
`daselt-dfk-coords-layer-0'.

Keys are chosen to solve the following problems:

- When the combinations sent when KEY1 and KEY2 are held share signals SIGS,
  then these are not received when both keys are pressed together (tapped or
  held). Therefore, if held KEY1 is a modifier/layer-shift that has to be
  applicable when KEY2 is tapped, it cannot share any signals with KEY2 held.
  This is important for KEY_E, KEY_I, KEY_102ND and KEY_SLASH, which are
  layer-shifts that also house symbols on all layers. Here, it is assumed
  modifiers are only applied from across the keyboard so keys on the same side
  can share modifiers when held.

- (1 -6) ((1 -7) in ANSI-layouts) and (1 7) to access layer 8 and are
  supposed to lock layers 2-4 when applied to their layer-shift keys. For this,
  level-shift KEYs to layers 2-4 are configured to lock when KEY is held while
  the layer-8-shift across the keyboard is held.

- However, when a KEY1 is tapped that contains a level-shift LEV that is
  configured in dxkb to lock LEV on layer 8, while another key KEY2 is held that
  such that the union of the level-shifts on KEY1 and KEY2 contains all three
  level-shifts, then LEV is locked even though it shouldn't because KEY1 is only
  tapped and its held bindings should not factor in.")

(defconst daselt-dfk-supported-layout-types
  '("ansi" "iso")
  "List of supported keyboard types.")

;;;; Customs
(defgroup daselt-dfk
  nil
  "Custom group for daselt-dfk."
  :group 'daselt)

(defcustom daselt-dfk-outside-mods
  nil
  "Toggle to have generation of the d-dfk-layout put modifiers outside key letters."
  :type 'boolean
  :group 'daselt-dfk)

(defcustom daselt-dfk-keyboard-layout-type
  "iso"
  "Type of the keyboard layout.

This is how the keys on your keyboard are a arrranged.
Currently supported options are ansi and iso."
  :type 'string
  :group 'daselt-dfk
  :options '("ansi" "iso"))

(defcustom daselt-dfk-bindform-symbol
  'daselt-dfk-default-bindlist-form
  "Symbol of the bindlist form used to generate the daselt-dfk-config.

The value of this symbol should be a form that evaluates to a bindlist with the
following format:

- it should be headless.

- each car of a binding should either

  - consist of KEY coordinates describing a key.

  - be a string that becomes a key-definition according to `input-event-codes'
once it is upcased and `KEY_' is added at the beginning of the string. You can
look up key definitions at
https://github.com/torvalds/linux/blob/master/include/uapi/linux/input-event-codes.h.

- each cdr should be a cons consisting of

  - a car that is either a coordinate or description or a list of them. This is
    what KEY sends when it is tapped.

  - a cdr with the same specifications as the car. This is what KEY sends when
    it is held."
  :type 'symbol
  :group 'daselt-dfk)

(defcustom daselt-dfk-C-coords
              '((-2 -2) (-2 2) (2 3))
              "Coordinates for keys that are used for Ctrl-signals.

These are not the keys that are actually used as Ctrl-keys but the
keys whose signals are used. In other words, if a key in the
`daselt-dfk-bindform-symbol' is used as a Ctrl key, it is
endowed with one of the signals of these keys when held."
              :type '(repeat coords)
              :group 'daselt-dfk)

(defcustom daselt-dfk-M-coords
          '((-2 0))
          "Coordinates for keys that are used for Meta-signals.

These are not the keys that are actually used as Meta-keys but the
keys whose signals are used. In other words, if a key in the
`daselt-dfk-bindform-symbol' is used as a Meta key, it is
endowed with one of the signals of these keys when held."
          :type '(repeat coords)
          :group 'daselt-dfk)

(defcustom daselt-dfk-H-coords
      '((-2 6))
      "Coordinates for keys that are used for Hyper-signals.

These are not the keys that are actually used as Hyper-keys but the
keys whose signals are used. In other words, if a key in the
`daselt-dfk-bindform-symbol' is used as a Hyper key, it is
endowed with one of the signals of these keys when held."
      :type '(repeat coords)
      :group 'daselt-dfk)

(defcustom daselt-dfk-s-coords
    '((-2 5))
    "Coordinates for keys that are used for Super-signals.

These are not the keys that are actually used as Super-keys but the
keys whose signals are used. In other words, if a key in the
`daselt-dfk-bindform-symbol' is used as a Super key, it is
endowed with one of the signals of these keys when held."
    :type '(repeat coords)
    :group 'daselt-dfk)

(defcustom daselt-dfk-locking-2-coords
          '((0 7))
          "Coordinates for keys that are used for Shift-signals that lock.

By default these are added to the `daselt-dfk-2-coords'. See there for more
information."
          :type '(repeat coords)
          :group 'daselt-dfk)

(defcustom daselt-dfk-non-locking-2-coords
  '((1 -7) (1 7))
  "Coordinates for keys that are used for Shift-signals that don't lock.

By default these are added to the `daselt-dfk-2-coords'.
See there for more information."
  :type '(repeat coords)
  :group 'daselt-dfk)

(defcustom daselt-dfk-locking-3-coords
  '((-2 3) (2 1))
  "Coordinates for keys that are used for ISO-Level-3-Shift-signals that lock.

By default these are added to the `daselt-dfk-2-coords'.
See there for more information."
  :type '(repeat coords)
  :group 'daselt-dfk)

(defcustom daselt-dfk-non-locking-3-coords
  '((0 -6) (-2 -5))
  "Coordinates for keys used for ISO-Level-3-Shift-signals that don't lock.

By default these are added to the `daselt-dfk-2-coords'. See there for more
information."
  :type '(repeat coords)
  :group 'daselt-dfk)

(defcustom daselt-dfk-locking-5-coords
  '((-1 6))
  "Coordinates for keys that are used for ISO-Level-5-Shift-signals that lock.

By default these are added to the `daselt-dfk-2-coords'.
See there for more information."
  :type '(repeat coords)
  :group 'daselt-dfk)

(defcustom daselt-dfk-non-locking-5-coords
  '((0 6) (-2 -6))
  "Coordinates for keys that are used for ISO-Level-5-Shift-signals
that don't lock.

By default these are added to the `daselt-dfk-2-coords'. See there
for more information."
  :type '(repeat coords)
  :group 'daselt-dfk)

(defcustom daselt-dfk-row-mid-codes-list
  '((-2 . 7) (-1 . 20.5) (0 . 34.5) (1 . 48) (2 . 57))
  "The keycodes for the midpoints of rows in the coordinate system.
If a midpoint is between two keys with adjecent codes, use KEY1CODE.5
for the mid.

`daselt-dfk-generate-config' calculates the keycodes for the row
simply by adding/ subtracting the distance to the mid from the
mid-keycode, rounding appropriately if necessary. Note that
`daselt-dfk-generate-config' accounts for formal places in
`daselt-coords-formal-places-list'.

If keys at positions cannot be calculated in this way, they have to be
specified in `daselt-dfk-special-codes-list'.

If the midpoint is between two keys without adjecent codes, the entire
row has to be specified in `daselt-dfk-special-codes-list'."
  :type '(repeat (cons number number))
  :group 'daselt-dfk)

(defcustom daselt-dfk-special-codes-list
  '(((0 -6) . 58) ((1 -7) . 42) ((1 -6) . 86) ((2 1) . 100) ((2 -4) . 29) ((2 2) . 126)  ((2 3) . 97))
  "List of positions with special codes in the coordinate system.

Add a place to this list if its code can't be calculated by adding or
subtracting from the midpoint of the row."
  :type '(repeat (cons coords natnum))
  :group 'daselt-dfk)

(defcustom daselt-dfk-tap-time
  nil
  "Time until a tap becomes a hold.

If nil or t, use the default of `dfk'.

Otherwise, the time should be given in Milliseconds."
  :type '(choice boolean number)
  :group 'daselt-dfk)

(defcustom daselt-dfk-double-tap-time
  nil
  "Time that can pass for two taps to be considered a double tap.

If nil or t, use the default of `dfk'.

Otherwise, the time should be given in Milliseconds."
  :type '(choice boolean number)
  :group 'daselt-dfk)

(defcustom daselt-dfk-synthetic-keys-time
  nil
  "Time between emulated keys in combos.

If nil or t, use the default of `dfk'.

Otherwise, the time should be given in Milliseconds."
  :type '(choice boolean number)
  :group 'daselt-dfk)

(defcustom daselt-dfk-directory
  (if (boundp 'd-directory)
      (concat d-directory "d-dfk/"))
  "Directory the generated configs should be put in."
  :type 'directory
  :group 'daselt-dfk)

(defcustom daselt-dfk-insert-names
  nil
  "Replace keycodes with keynames.

If you set to t, `daselt-dfk' expects a copy of
`https://raw.githubusercontent.com/torvalds/linux/refs/heads/master/include/uapi/linux/input-event-codes.h'
called `input-event-codes.h' in your `daselt-dfk-directory'."
  :type 'boolean
  :group 'daselt-dfk)

;;;;; Used for generating layer 0
(defcustom daselt-dfk-layer-level-shifts
  '((1 . nil)
    (2 . (2))
    (3 . (3))
    (4 . (2 3))
    (5 . (5))
    (6 . (2 5))
    (7 . (3 5))
    (8 . (2 3 5)))
  "Alist of layers and corresponding shift numbers.

The cars of the conses in this list are layers. The cdr is the
list of numbers of the shifts that have to be combined to access that
layer."
  :type '(alist :key-type natnum :value-type (repeat natnum))
  :group 'daselt-dfk)

(defcustom daselt-dfk-special-tap-coord-strings
  '(((-2 -4) . "<backtab>") ((-2 4) . "<tab>") ((2 0) . "SPC"))
  "Alist of coordinates for which the signs they house have special names.

This is used to generate `daselt-dfk-coords-layer-0'.

A name counts as special if it cannot be found out by looking up the coords
in one of the daselt-dfk-coords-lists or in the layout specified by
`daselt-xkb-layout'."
  :type '(alist :key-type coords :value-type string)
  :group 'daselt-dfk)

(defcustom daselt-dfk-special-signal-translations-list
  '(("f14" . "<XF86Launch5>") ("f15" . "<XF86Launch6>") ("scrolllock" . "Compose"))
  "List of key signals that have to be translated to be used by Emacs.

This arises because the key names used by `input-event-names' are different from
those used by Emacs."
  :type '(alist :key-type string :value-type string)
  :group 'daselt-dfk)

(defcustom daselt-dfk-discrete-modifiers-list
  '((("<XF86Launch6>") . "dM") (("<XF86Launch6>" ?C) . "ds") (("f8") . "dA"))
  "List of keys that act as discrete modifiers.

This is used to generate layer 0 for `daselt-coords'.

Each element of this list is supposed to be a CONS.

- The car of CONS should be a cons

  - whose car is the string that is the key's name in Emacs.

  - whose cdr is a list of modifiers, given by their corresponding
    characters.

- The cdr of CONS should be a string describing the discrete modifier
  as `dX`, where `X` is the corresponding continuous modifier."
  :type '(alist :key-type (alist :key-type string :value-type (repeat character))
                :value-type string)
  :group 'daselt-dfk)

(defcustom daselt-dfk-special-layer-0-placevals-alist
  (append (mapcar (lambda (coords)
                    (cons coords "C"))
                  daselt-dfk-C-coords)
          (mapcar (lambda (coords)
                    (cons coords "M"))
                  daselt-dfk-M-coords)
          (mapcar (lambda (coords)
                    (cons coords "H"))
                  daselt-dfk-H-coords)
          (mapcar (lambda (coords)
                    (cons coords "s"))
                  daselt-dfk-s-coords))
  "List of placevals that have to be specified manually to add to layer 0."
  :type '(alist :key-type coords :value-type string)
  :group 'daselt-dfk)

;;;; Constants
(defconst daselt-dfk-shift-levels
  '(2 3 5)
  "Levels for which shifts exist that shift to that level.
These are inherited from xkb.")

(defconst daselt-dfk-layers-by-length
  (sort (daselt-base-fiber-by-property daselt-dfk-layer-level-shifts
                                        (lambda (lst) (length (cdr lst))) t)
        (lambda (cns1 cns2) (< (car cns1) (car cns2)))))

(defconst daselt-dfk-modifiers
  '('C 'M 'H 's)
  "The modifiers used by daselt-dfk.

Note that S(hift) is not among these modifiers because it counts as a level
shift.")

(defconst daselt-dfk-2-coords
      (append daselt-dfk-locking-2-coords daselt-dfk-non-locking-2-coords)
      "Coordinates for keys that are used for Shift-signals.

These are not the keys that are actually used as Shift-keys but the
keys whose signals are used. In other words, if a key in the
`daselt-dfk-bindform-symbol' is used as a Shift key, it is
endowed with one of the signals of these keys when held.")

(defconst daselt-dfk-3-coords
    (append daselt-dfk-locking-3-coords daselt-dfk-non-locking-3-coords)
    "Coordinates for keys that are used for ISO-Level-3-Shift-signals.

These are not the keys that are actually used as
ISO-Level-3-Shift-keys but the keys whose signals are used. In other
words, if a key in the `daselt-dfk-bindform-symbol' is used as
an ISO-Level-3-Shift key, it is endowed with one of the signals of
these keys when held.")

(defconst daselt-dfk-5-coords
    (append daselt-dfk-locking-5-coords daselt-dfk-non-locking-5-coords)
    "Coordinates for keys that are used for ISO-Level-5-Shift-signals.

These are not the keys that are actually used as
ISO-Level-5-Shift-keys but the keys whose signals are used. In other
words, if a key in the `daselt-dfk-bindform-symbol' is used as
an ISO-Level-5-Shift key, it is endowed with one of the signals of
these keys when held.")

(defconst daselt-dfk-modifier-coords-alist
  (let ((symbols (daselt-base-filter-obarray
                  (lambda (sym)
                    (string-match-p (rx "daselt-dfk-" (group not-newline) "-coords")
                                    (symbol-name sym))))))
    (mapcar (lambda (sym)
              (let* ((symname (symbol-name sym))
                     (match (progn (string-match (rx "daselt-dfk-"
                                                     (group not-newline)
                                                     "-coords")
                                                 symname)
                                   (read
                                    (match-string 1 symname)))))
                (cons match sym)))
            symbols))
  "Alist of modifier coordinate lists with their corresponding modifiers.")

;;;; Functions
(defun daselt-dfk--calculate-coords-code (coords)
  "Calculate the keycode corresponding to COORDS."
  (declare (ftype
            ;; (function ((list number)) integer) ; Compiler complains.
            (function (list) integer))
           (side-effect-free t))
  (alist-get coords daselt-dfk-special-codes-list
             (let* ((row (car coords))
                    (col (daselt-coords--remove-formal-places coords)))
               (+ col (funcall (if (<= 0 col) #'floor #'ceiling)
                               (alist-get row daselt-dfk-row-mid-codes-list))))
             nil #'equal))

(defun daselt-dfk--convert-datum (datum)
  "Extract the `dual-function-keys'-info from an info DATUM.

DATUM can be either a string or `daselt-coords'-coordinates."
  (declare (ftype (function ((or string list
                                 ;; (list number) ; Compiler complains.
                                 ))
                            string))
           (side-effect-free t))
  (cond ((stringp datum)
         (upcase (concat "key_" datum)))
        ((daselt-coords-p datum)
         (let* ((keycode (number-to-string (daselt-dfk--calculate-coords-code datum)))
                (eventcodes (concat daselt-dfk-directory "input-event-codes.h"))
                (keyname (if daselt-dfk-insert-names
                             (let ((buf (current-buffer)))
                               (progn (set-buffer (find-file-noselect eventcodes))
                                      (daselt-base-goto-min)
                                      (search-forward "* Keys and buttons")
                                      (re-search-forward (eval `(rx (+ space) ,keycode)))
                                      (let* ((line (thing-at-point 'line))
                                             (linesplit (split-string line (rx (+ space)))))
                                        (prog1 (nth 1 linesplit)
                                          (set-buffer buf))))))))
           (or keyname (number-to-string keycode))))))

(defun daselt-dfk--datum-p (obj)
  "Check if OBJ is a datum.

A datum here is either a string, a list of coordinates or a list of lists of
coordinates."
  (declare (ftype (function (t) boolean))
           (pure t))
  (and obj
       (or (stringp obj)
           (daselt-coords-p obj)
           (cl-every #'daselt-coords-p obj))))

(defun daselt-dfk-generate-config (&optional blist filename)
  "Generate a `dual-function-keys' configuration from BLIST.

Write the config to FILENAME in `daselt-dfk-directory'. By default, FILENAME is
generated from `daselt-dfk-outside-mods' and
`daselt-dfk-keyboard-layout-type'. It is not necessary to include the `.yaml'
extension in FILENAME."
  (declare (ftype (function (&optional list string) string)))
  (interactive)
  (let ((blist (or blist (remq nil (eval (symbol-value daselt-dfk-bindform-symbol)))))
        (filename (or filename (concat daselt-dfk-directory
                                       "d-dfk"
                                       (if daselt-dfk-outside-mods "-out")
                                       (concat "-" daselt-dfk-keyboard-layout-type)
                                       ".yaml"))))
    (cl-flet ((convertdatumlist (data)
                (concat "\[" (mapconcat (lambda (datum)
                                          (daselt-dfk--convert-datum
                                           datum))
                                        data
                                        ", ")
                        "\]")))

      (with-temp-buffer
        (when (or daselt-dfk-tap-time
                  daselt-dfk-double-tap-time
                  daselt-dfk-synthetic-keys-time)
          (insert "Timing:\n")
          (if daselt-dfk-tap-time
              (insert (format "  TAP_MILLISEC: %s\n"
                              daselt-dfk-tap-time)))
          (if daselt-dfk-double-tap-time
              (insert (format "  DOUBLE_TAP_MILLISEC: %s\n"
                              daselt-dfk-double-tap-time)))
          (if daselt-dfk-synthetic-keys-time
              (insert (format "  SYNTHETIC_KEYS_PAUSE_MILLISEC: %s\n"
                              daselt-dfk-synthetic-keys-time)))
          (insert "\n"))

        (insert "MAPPINGS:\n")
        (dolist (bind blist)
          (let* ((ini (car bind))
                 (trans (cdr bind))
                 (inistr (daselt-dfk--convert-datum ini))
                 (tapdata (condition-case nil (car trans)
                            (error "Ill-formatted translation %s" trans)))
                 (holddata (condition-case nil (cdr trans)
                             (error "Ill-formatted translation %s" trans)))
                 (tapstr (if (daselt-dfk--datum-p tapdata)
                             (daselt-dfk--convert-datum tapdata)
                           (convertdatumlist tapdata)))
                 (holdstr (if (daselt-dfk--datum-p holddata)
                              (daselt-dfk--convert-datum holddata)
                            (convertdatumlist holddata))))
            (insert (format "  - KEY: %s
    TAP: %s
    HOLD: %s

" inistr tapstr holdstr))))

        (write-file filename)))))

(defun daselt-dfk-generate-standard-configs ()
  "Generate all supported configs for `d-dfk'."
  (declare (ftype (function () string)))
  (interactive)
  (dolist (tval '(nil t))
    (dolist (lay daselt-dfk-supported-layout-types)
      (let ((daselt-dfk-outside-mods tval)
            (daselt-dfk-keyboard-layout-type lay))
        (daselt-dfk-generate-config)))))

(defun daselt-dfk-coords-modifier (coords)
  "Return the modifier that a pair of key COORDS represents if there is one."
  (declare (ftype (function (list
                             ;; (list number) ; Compiler complains.
                             )
                            (or void integer)))
           (side-effect-free t))
  (daselt-base-reverse-alist-get
   coords
   daselt-dfk-modifier-coords-alist
   nil
   (lambda (modlistsym coords)
     (cl-member coords
                (symbol-value modlistsym)
                :test #'equal))))

(defun daselt-dfk-levels-to-layer (levs)
  "Return the layer that is reached through LEVS."
  (declare (ftype (function ((list integer)) integer))
           (side-effect-free t))
  (let ((levnum (length levs)))
    (daselt-base-reverse-alist-get
     levs
     (alist-get levnum daselt-dfk-layers-by-length #'equal)
     nil
     #'daselt-base-setequal)))

(cl-defun daselt-dfk-datum-to-string (dtm)
  "Convert a daselt-dfk-datum DTM into a string."
  (declare (ftype (function ((or string list
                                 ;; (list number) ; Compiler complains.
                                 ))
                            string)))
  (unless (daselt-dfk--datum-p dtm)
    (error "Expected a daselt-dfk-datum"))
  (if (stringp dtm)
      (alist-get dtm
                 daselt-dfk-special-signal-translations-list
                 dtm
                 nil
                 #'string=)
    (let (levs)
      (if (daselt-coords-p dtm)
          (let ((match (daselt-dfk-coords-modifier dtm)))
            (if match
                (if (cl-member match daselt-dfk-shift-levels :test #'equal)
                    (setq levs (list match))

                  ;; Exit if it's just a non-level modifier.
                  (cl-return-from daselt-dfk-datum-to-string (symbol-name match)))))
        (setq levs (mapcar #'daselt-dfk-coords-modifier dtm)))
      
      (if levs (let ((lay (daselt-dfk-levels-to-layer levs)))
                 (if lay (number-to-string lay)))))))

(defun daselt-dfk-generate-layer-0-placevals ()
  "Return the placevals for layer 0 for `daselt-coords'.

Based on the customs in `daselt-dfk'."
  (declare (ftype (function () list
                            ;; (list (cons (list number) t)) ; Compiler complains.
                            )))
  (let ((origblist (remq nil (eval (symbol-value daselt-dfk-bindform-symbol)))))
    
    (cl-flet*
        ((coords-of-level (num)
           (intern (concat "daselt-dfk-" (number-to-string num) "-coords")))

         ;; Find out if there are discrete modifiers on the key
         (disc-mod-strs (str)
           (let ((dmods (daselt-base-filter-list
                         daselt-dfk-discrete-modifiers-list
                         (lambda (cns1) (string= str (caar cns1))))))
             (mapcar (lambda (dmodcns)
                       (let ((mods (cdar dmodcns))
                             (dmodstr (cdr dmodcns)))
                         (concat (if mods "\⟨")
                                 (mapconcat #'char-to-string
                                            mods
                                            "-")
                                 (if mods "\⟩")
                                 dmodstr)))
                     dmods))))

      ;; Remove all places that were given using strings
      (append (remq nil
                    (mapcar (lambda (bind)
                              (let* ((bplace (append '(0) (car bind)))
                                     (bval (cdr bind))
                                     (bvaltap (car bval))
                                     (bvalhold (cdr bval))
                                     (holdstr (daselt-dfk-datum-to-string bvalhold))
                                     (tapstr (or (daselt-dfk-datum-to-string bvaltap)
                                                 (alist-get
                                                  bvaltap
                                                  daselt-dfk-special-tap-coord-strings
                                                  nil
                                                  nil
                                                  #'equal)))
                                     (discmodstrs (disc-mod-strs tapstr))
                                     (stringslist (cl-remove-duplicates
                                                   (remq nil (append (list holdstr)
                                                                     discmodstrs
                                                                     (list tapstr)))
                                                   :test #'string=)))
                                (if (daselt-coords-p bplace)
                                    (cons bplace
                                          (mapconcat #'identity stringslist "/")))))
                            origblist))

              ;; We add special values after adding 0 as a layer coordinate.
              (mapcar (lambda (placeval)
                        (cons (append '(0) (car placeval))
                              (cdr placeval)))
                      daselt-dfk-special-layer-0-placevals-alist)))))

;;;; Add layer 0 to other layouts
(defmacro daselt-dfk-import-layout (laysym &optional pfx)
  "Generate a `daselt-dfk'-layout from the layout named LAYSYM.

The layout is bound to the variable `daselt-dfk-CORE-layout', where
the name of LAYSYM is of the form `PFX-CORE-layout'.

PFX is `daselt-xkb-' by default."
  (let* ((pfx (or pfx "daselt-xkb-"))
         (namecore (daselt-base-namecore laysym pfx "-layout"))
         (dfkname (intern (concat "daselt-dfk-" namecore "-layout"))))
    `(let ((layer0layout (daselt-coords-layout-from-placevals
                          (daselt-dfk-generate-layer-0-placevals))))
       (defconst ,dfkname
         (append layer0layout (symbol-value ',laysym))))))

(defun daselt-dfk-import-current-layout ()
  "Import the layout that is currently specified in `daselt-xkb-layout'.

Set the result as the value of `daselt-dfk-layout'."
  (if (boundp daselt-xkb-layout)
      (eval `(defconst daselt-dfk-layout (daselt-dfk-import-layout ,daselt-xkb-layout)))
    (error "No daselt-xkb-layout defined")))

;;;; Provide
(provide 'daselt-dfk)
;;; daselt-dfk.el ends here
