;;; d-emacs-image-bindlists.el --- d-emacs-bindlists for image  -*- lexical-binding: t; -*-

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

;; This file contains the keylists for maps of image. If d-image is t, it is parsed automatically when d-emacs-mode is started. Each element in this file should be either

;; 1. A LIST consisting of conses of whose car is a binding-location (see d--binding-location-p) binding-cars and values. This map list corresponds to  and should generally be at be at the top if it exists, though sometimes it makes sense to put it lower down if it references maps that are previously defined. When d-emacs-mode is activated and  is evaluated,  is backed up at d-emacs--mode-backup and the key combinations in this list are rebound to their corresponding values.

;; 2. A cons CONS1 whose car is a symbol SYMB whose cdr is a list as in 1. If SYMB is already bound when  is evaluated and d-emacs-mode is activated, then its value should be a keymap. This map is backed at d-emacs-SYMB-backup and then rebound as above. If SYMB is bound at that point, a new keymap is defined and bound to SYMB.

;; 3. A cons CONS2 whose car is a symbol or string and whose cdr is a list of conses like CONS1 as in 2. The first element of CONS2 denotes a file or feature that has to be evaluated before the keys in the bindlists in the cdr of CONS2 can be rebound.

;; Each of these options should be given using a backquote.

;;; Code:

;;;; image-mode-map
`(
;;;;; Coordinates
;;;;;;; 1
;;;;;;;; 1--1
  ((1 -1 -3) . #'image-mode-unmark-file) 
  ((1 -1 3) . #'image-mode-mark-file) 

;;;;;;;; 1-0 
  ((1 0 -3) . #'image-scroll-up) 
  ((1 0 -2) . #'image-scroll-left) 
  ((1 0 2) . #'image-scroll-right) 
  ((1 0 3) . #'image-scroll-down) 

;;;;;;; 2 
;;;;;;;; 2--1 
  ((2 -1 -3) . #'image-crop) 
  ((2 -1 2) . #'image-revert-speed) 
  ((2 -1 3) . #'image-cut) 

;;;;;;;; 2-0 
  ((2 0 -2) . #'image-decrease-speed) 
  ((2 0 -2) . #'image-previous-file) 
  ((2 0 -1) . #'image-decrease-size) 
  ((2 0 1) . #'image-increase-size) 
  ((2 0 2) . #'image-increase-speed) 
  ((2 0 2) . #'image-next-file) 
  ((2 0 5) . #'image-save) 

;;;;;;;; 2-1 
  ((2 1 -3) . #'image-reset-speed) 

;;;;;;; 4 
;;;;;;;; 4-0 
  ((4 0 -2) . #'image-previous-line) 
  ((4 0 2) . #'image-next-line) 

;;;;;;; 5 
;;;;;;;; 5--1 
  ((5 -1 -4) . #'image-transform-fit-to-width) 
  ((5 -1 -2) . #'image-transform-fit-to-window) 
  ((5 -1 -1) . #'image-transform-set-percent) 
  ((5 -1 1) . #'image-transform-set-smoothing) 
  ((5 -1 2) . #'image-mode-fit-frame) 
  ((5 -1 4) . #'image-transform-fit-to-height) 

;;;;;;;; 5-0 
  ((5 0 -1) . #'image-transform-set-scale) 
  ((5 0 1) . #'image-transform-set-rotation) 

;;;;;;;; 5-1 
  ((5 1 0) . #'image-transform-reset-to-original) 
  ((5 1 2) . #'image-transform-fit-both) 

;;;;;;; 7 
;;;;;;;; 7--1 
  ((7 -1 4) . #'image-goto-frame) 

;;;;;;;; 7-0 
  ((7 0 -2) . #'image-previous-frame) 
  ((7 0 2) . #'image-next-frame))

;;; d-emacs-image-bindlists.el ends here
