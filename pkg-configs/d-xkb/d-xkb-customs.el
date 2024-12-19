;;; d-xkb-customs.el --- Daselt's Emacs module customization options        -*- lexical-binding: t; -*-

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

;; This file houses the d-xkb-customization options for Daselt.

;;; Code:

(defgroup d-xkb
  nil
  "Customization group for d-xkb."
  :group 'Daselt
  :prefix "d-xkb-")

(defcustom d-xkb-file
  "/usr/share/X11/xkb/symbols/dxkb"
  "Location of the file housing Daselt's xkb layout."
  :type 'directory
  :group 'd-xkb)

(define-widget 'coords 'lazy
  "A list of coordinate numbers."
  :offset 4
  :tag "Coords"
  :type '(repeat integer))

(define-widget 'prefix-coords-pair 'lazy
  "A list of coordinate numbers."
  :offset 4
  :tag "Coords"
  :type '(cons string coords))

(require 'd-xkb-constants)

(defcustom d-xkb-layout
    'd-xkb-main-layout
    "The keyboard-layout you're using.
Should be one of the variants of Daselt in the `d-xkb-file'."
    :group 'Daselt
    :type 'symbol
    :options d-xkb-layouts)

(defcustom d-xkb-bad-key-combinations-list
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
  :type 'prefix-coords-pair)

(provide 'd-xkb-customs)
;;; d-xkb-customs.el ends here
