;;; d-emacs-dfk.el --- Creation of dual-function-keys configurations from a d-emacs-coords-layer and integration into layouts  -*- lexical-binding: t; -*-

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
(require 'd-emacs-coords)

;;;; Constants
(defconst d-emacs-dfk-default-bindlist-form
  '`(;; Ctrl
     ((2 0) . ((2 0) . ,(nth 3 d-emacs-dfk-C-coords)))

     ;; Hyper
     ((1 0) . ,(nth 0 d-emacs-dfk-H))

     ;; Meta
     ((2 3) . ((car d-emacs-dfk-M-coords)))

     ;; Alt
     ,(unless (string= d-emacs-dfk-keyboard-type "ansi")
        '((1 -7) . (("f8" . "leftshift"))))
     ((1 7) . ("f8" . "rightshift"))
     
     ;; Shift and discrete Meta
     ((2 -1) . ("f15" . ,(nth 0 d-emacs-dfk-locking-S-coords)))
     ((2 1) . ("f15" . ,(nth 0 d-emacs-dfk-locking-S-coords)))

     ;; 3
     (,(if d-emacs-dfk-outside-mods '(-2 -3) '(-1 -3))
      . ,(if d-emacs-dfk-outside-mods
             `("scrolllock" . ,(nth 0 d-emacs-dfk-locking-3-coords))
           `((-1 -3) . ,(nth 0 d-emacs-dfk-locking-3-coords))))
     (,(if d-emacs-dfk-outside-mods '(-2 3) '(-1 3))
      . ,(if d-emacs-dfk-outside-mods
             `("scrolllock" . ,(nth 1 d-emacs-dfk-locking-3-coords))
           `((-1 3) . ,(nth 1 d-emacs-dfk-locking-3-coords))))

     ;; 4
     ((0 -6) . ("f14" . (,(nth 0 d-emacs-dfk-locking-3-coords)
                         . ,(nth 0 d-emacs-dfk-locking-5-coords))))
     ((0 6) . ("f14" . (,(nth 1 d-emacs-dfk-locking-3-coords)
                        . ,(nth 0 d-emacs-dfk-locking-5-coords))))

     ;; 5
     ((-2 -4) . ((-2 -4) . ,(nth 0 d-emacs-dfk-locking-5-coords)))
     ((-2 4) . ((-2 4) . ,(nth 0 d-emacs-dfk-locking-5-coords)))

     ;; 6
     ((-1 -6) . ("f11" . (,(nth 0 d-emacs-dfk-locking-S-coords)
                          ,(nth 0 d-emacs-dfk-non-locking-5-coords))))
     ((-1 6) . ("f11" . (,(nth 0 d-emacs-dfk-locking-S-coords)
                         (nth 1 d-emacs-dfk-non-locking-5-coords))))

     ;; 7
     (,(if d-emacs-dfk-outside-mods '(-2 -2) '(-2 -3))
      . (,(if d-emacs-dfk-outside-mods
              '(-2 -2)
            "scrolllock")
         . (,(nth 0 d-emacs-dfk-locking-S-coords)
            ,(nth 0 d-emacs-dfk-locking-3-coords))))
     (,(if d-emacs-dfk-outside-mods '(-2 2) '(-2 3))
      . (,(if d-emacs-dfk-outside-mods
              '(-2 2)
            "scrolllock")
         . (,(nth 1 d-emacs-dfk-locking-3-coords)
            ,(nth 0 d-emacs-dfk-locking-S-coords))))

     ;; 8
     (,(if (string= d-emacs-dfk-keyboard-type "ansi")
           '(1 -7)
         '(1 -6))
      . ((1 -6) . (,(nth 0 d-emacs-dfk-non-locking-2-coords)
                   ,(nth 1 d-emacs-dfk-locking-3-coords)
                   ,(nth 0 d-emacs-dfk-non-locking-5-coords))))
     ((1 6) . (,(nth 1 d-emacs-dfk-non-locking-2-coords)
               ,(nth 0 d-emacs-dfk-locking-3-coords)
               ,(nth 1 d-emacs-dfk-non-locking-5-coords))))
  "Form to generate the d-dfk layouts.
When evaluated, returns a bindlist that can be used in
`d-emacs-dfk-generate-config'.
The exact composition of the bindlist depends on the constants
`d-emacs-dfk-outside-mods' and `d-emacs-dfk-keyboard-type'.")

;;;; Customs
(defgroup d-emacs-dfk
                nil
                "Custom group for d-emacs-dfk."
                :group 'Daselt)

(defcustom d-emacs-dfk-outside-mods
  nil
  "Toggle to have generation of the d-dfk-layout put modifiers outside key letters."
  :type 'boolean
  :group 'd-emacs-dfk)

(defcustom d-emacs-dfk-keyboard-layout-type
  "iso"
  "Type of the keyboard layout.
This is how the keys on your keyboard are arranged.
Currently supported options are ansi and iso."
  :type 'string
  :group 'd-emacs-dfk
  :options '("ansi" "iso"))

(defcustom d-emacs-dfk-config-directory
  (if (boundp d-directory)
      (concat d-directory "d-emacs-dfk/"))
  "Directory the generated dual-function-keys-configs should be put into."
  :type 'directory
  :group 'd-emacs-dfk)

(defcustom d-emacs-dfk-special-bindlist
  (eval d-emacs-dfk-default-keylist-form)
  "Bindlist used to generate the `d-emacs-dfk-special-bindlist'.
This bindlist should have the following format:

- it should be headless.

- each car of a binding should consist of KEY coordinates.

- each cdr should be either

  - a KEY2 coordinate. In this case, KEY sends the KEY signal when tapped and
    the KEY2 signal when held.

  - a list consisting of KEY2X coordinates. In this case, KEY sends KEY when
    tapped and the combination KEY2X when held.

  - a cons consisting of

     - a car that is either a key coordinate or a list of coordinates.
       This is what KEY sends when it is tapped.

     - a cdr with the same specifications as the car.
       This is what KEY sends when it is held."
  :type 'list
  :group 'd-emacs-dfk)

(defcustom d-emacs-dfk-C-coords
  '((-2 -2) (-2 2) (2 -4) (2 3))
  "Coordinates for keys that are used for Ctrl-signals.

These are not the keys that are actually used as Ctrl-keys but the keys whose
signals are used. In other words, if a key in the `d-emacs-dfk-special-bindlist'
is used as a Ctrl key, it is endowed with one of the signals of these keys when
held."
  :type '(repeat coords)
  :group 'd-emacs-dfk)

(defcustom d-emacs-dfk-M-coords
  '((-2 0))
  "Coordinates for keys that are used for Meta-signals.

These are not the keys that are actually used as Meta-keys but the keys whose
signals are used. In other words, if a key in the `d-emacs-dfk-special-bindlist'
is used as a Meta key, it is endowed with one of the signals of these keys when
held."
  :type '(repeat coords)
  :group 'd-emacs-dfk)

(defcustom d-emacs-dfk-H-coords
  '((-2 6))
  "Coordinates for keys that are used for Hyper-signals.

These are not the keys that are actually used as Hyper-keys but the keys whose
signals are used. In other words, if a key in the `d-emacs-dfk-special-bindlist'
is used as a Hyper key, it is endowed with one of the signals of these keys when
held."
  :type '(repeat coords)
  :group 'd-emacs-dfk)

(defcustom d-emacs-dfk-locking-S-coords
  '((0 7))
  "Coordinates for keys that are used for Shift-signals that lock.

By default these are added to the `d-emacs-dfk-S-coords'.
See there for more information."
  :type '(repeat coords)
  :group 'd-emacs-dfk)

(defcustom d-emacs-dfk-non-locking-2-coords
  '((1 -7) (1 7))
  "Coordinates for keys that are used for Shift-signals that don't lock.

By default these are added to the `d-emacs-dfk-S-coords'.
See there for more information."
  :type '(repeat coords)
  :group 'd-emacs-dfk)

(defcustom d-emacs-dfk-locking-3-coords
  '((-2 3) (2 1))
  "Coordinates for keys that are used for ISO-Level-3-Shift-signals that lock.

By default these are added to the `d-emacs-dfk-S-coords'.
See there for more information."
  :type '(repeat coords)
  :group 'd-emacs-dfk)

(defcustom d-emacs-dfk-non-locking-3-coords
  nil
  "Coordinates for keys that are used for ISO-Level-3-Shift-signals that don't lock.

By default these are added to the `d-emacs-dfk-S-coords'.
See there for more information."
  :type '(repeat coords)
  :group 'd-emacs-dfk)

(defcustom d-emacs-dfk-locking-5-coords
  '((-1 6))
  "Coordinates for keys that are used for ISO-Level-5-Shift-signals that lock.

By default these are added to the `d-emacs-dfk-S-coords'.
See there for more information."
  :type '(repeat coords)
  :group 'd-emacs-dfk)

(defcustom d-emacs-dfk-non-locking-5-coords
  '((0 -6) (0 6))
  "Coordinates for keys that are used for ISO-Level-5-Shift-signals that don't lock.

By default these are added to the `d-emacs-dfk-S-coords'.
See there for more information."
  :type '(repeat coords)
  :group 'd-emacs-dfk)

(defcustom d-emacs-dfk-5-coords
  '(append d-emacs-dfk-locking-5-coords)
  "Coordinates for keys that are used for ISO-Level-5-Shift-signals.

These are not the keys that are actually used as ISO-Level-5-Shift-keys but the
keys whose signals are used. In other words, if a key in the
`d-emacs-dfk-special-bindlist' is used as an ISO-Level-5-Shift key, it is endowed
with one of the signals of these keys when held."
  :type '(repeat coords)
  :group 'd-emacs-dfk)

;;;; Constants
(defconst d-emacs-dfk-S-coords
  (append d-emacs-dfk-locking-S-coords d-emacs-dfk-non-locking-2-coords)
  "Coordinates for keys that are used for Shift-signals.

These are not the keys that are actually used as Shift-keys but the keys whose
signals are used. In other words, if a key in the `d-emacs-dfk-special-bindlist'
is used as a Shift key, it is endowed with one of the signals of these keys when
held.")

(defconst d-emacs-dfk-3-coords
  '(append d-emacs-dfk-locking-3-coords d-emacs-dfk-non-locking-3-coords)
  "Coordinates for keys that are used for ISO-Level-3-Shift-signals.

These are not the keys that are actually used as ISO-Level-3-Shift-keys but the
keys whose signals are used. In other words, if a key in the
`d-emacs-dfk-special-bindlist' is used as an ISO-Level-3-Shift key, it is endowed
with one of the signals of these keys when held.")

(defconst d-emacs-dfk-5-coords
  '(append d-emacs-dfk-locking-5-coords d-emacs-dfk-non-locking-5-coords)
  "Coordinates for keys that are used for ISO-Level-5-Shift-signals.

These are not the keys that are actually used as ISO-Level-5-Shift-keys but the
keys whose signals are used. In other words, if a key in the
`d-emacs-dfk-special-bindlist' is used as an ISO-Level-5-Shift key, it is endowed
with one of the signals of these keys when held.")

(defconst d-emacs-dfk-mid
  (let ((absmid d-emacs-coords-abs-mid)
        (l0shift (car (alist-get '0 d-emacs-coords-layer-shifts-list '((0 0)))))) ; Car bc the layer shifts are lists.
    (list (+ (nth 0 absmid) (nth 0 l0shift))
          (* (nth 1 absmid) (nth 1 l0shift))))
  "The midpoint used by `d-emacs-dfk' to calculate key values.

It's assumed that this is the same as the midpoint of layer 0 in
`d-emacs-coords' and can thus be calculated from the absolute midpoint of
`d-emacs-coords' and the layer-0-shift in `d-emacs-coords-layer-shifts-list'
(with no shift being the default).

It's also assumed that the absolute coordinates of the `d-emacs-dfk-mid' start
counting from the very left of the keyboard but do not count the uppermost
keyboard row (fn-keys).")

(defconst d-emacs-dfk-row-shifts
  
  "")

;;;; Functions
(defun d-emacs-dfk-generate-config ()
  "Generate a `dual-function-keys' configuration from `d-emacs-dfk-special-bindlist'."
  (interactive)
  (with-temp-buffer
    (dolist (bind d-emacs-dfk-special-bindlist)
      (let ((inicoords (car bind))
            ())
        (insert (format "  - KEY: %s
   TAP: %s
   HOLD: %s

"))))))

(provide 'd-emacs-dfk)
;;; d-emacs-dfk.el ends here
