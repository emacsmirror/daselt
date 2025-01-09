;;; d-stump-remapped-keys-special-bindlists.el --- lists of keys that are remapped in d-stump. for d-stump  -*- lexical-binding: t; -*-

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

;; This file contains the keylists for StumpWM maps of d-stump. If daselt-d-stump is t, it is parsed automatically when daselt-mode is started. Each element in this file should be either

;; 1. A LIST consisting of conses of whose car is a binding-location (see d--binding-location-p) binding-cars and values. This map list corresponds to the shortcuts that are sent to all programs that are not otherwise specified and should generally be at be at the top if it exists.

;; 2. A cons CONS1 whose car is a program name and whose cdr is a LIST as in 1.

;; Each of these options should be given using a backquote.

;;; Code:

;;;; "Firefox"
`("Firefox" 
;;;;; Coordinates
;;;;;; C-
;;;;;;; C-1
;;;;;;;; C-1--1
  (("C-" . (1 -1 -4)) . "C-r") 
  (("C-" . (1 -1 -3)) . "C-t") 
  (("C-" . (1 -1 -2)) . "C-z") 
  (("C-" . (1 -1 -1)) . "Prior") 
  (("C-" . (1 -1 1)) . "Next") 
  (("C-" . (1 -1 2)) . "C-y") 
  (("C-" . (1 -1 3)) . "C-w") 

;;;;;;;; C-1-0 
  (("C-" . (1 0 -5)) . "C-o") 
  (("C-" . (1 0 -4)) . "C-S-g") 
  (("C-" . (1 0 -3)) . "Up") 
  (("C-" . (1 0 -2)) . "Left") 
  (("C-" . (1 0 -1)) . "Home") 
  (("C-" . (1 0 1)) . "End") 
  (("C-" . (1 0 2)) . "Right") 
  (("C-" . (1 0 3)) . "Down") 
  (("C-" . (1 0 4)) . "C-g") 

;;;;;;;; C-1-1 
  (("C-" . (1 1 -3)) . "C-c") 
  (("C-" . (1 1 -2)) . "Escape") 
  (("C-" . (1 1 3)) . "C-v") 

;;;;;;; C-2 
;;;;;;;; C-2-0 
  (("C-" . (2 0 1)) . "C-q") 

;;;;;;;; C-2-1 
  (("C-" . (2 1 -3)) . "C-x"))

;;;; "emacs"
`("emacs" 
;;;;; Strings
;;;;;; C-
  (("C-" . "g") . "C-x") 
  (("C-" . "x") . "C-g") 

;;;;; Coordinates 
  (("C-" . (1 0 -3)) . "Up") 
  (("C-" . (1 0 -2)) . "Left") 
  (("C-" . (1 0 -1)) . "Home") 
  (("C-" . (1 0 1)) . "End") 
  (("C-" . (1 0 2)) . "Right") 
  (("C-" . (1 0 3)) . "Down"))

;;;; stumpwm-mode-map
`(
;;;;; Coordinates
;;;;;; C-
;;;;;;; C-1
;;;;;;;; C-1--1
  (("C-" . (1 -1 -2)) . "Prior") 
  (("C-" . (1 -1 1)) . "Next") 
  (("C-" . (1 -1 2)) . "C-c") 

;;;;;;;; C-1-0 
  (("C-" . (1 0 -5)) . "C-o") 
  (("C-" . (1 0 -3)) . "Up") 
  (("C-" . (1 0 -2)) . "Left") 
  (("C-" . (1 0 -1)) . "Home") 
  (("C-" . (1 0 1)) . "End") 
  (("C-" . (1 0 2)) . "Right") 
  (("C-" . (1 0 3)) . "Down") 
  (("C-" . (1 0 4)) . "C-f") 

;;;;;;;; C-1-1 
  (("C-" . (1 1 -2)) . "C-v") 
  (("C-" . (1 1 0)) . "C-z") 
  (("C-" . (1 1 2)) . "Escape") 
  (("C-" . (1 1 3)) . "C-y") 

;;;;;;; C-2 
;;;;;;;; C-2-0 
  (("C-" . (2 0 1)) . "C-q") 

;;;;;;;; C-2-1 
  (("C-" . (2 1 -3)) . "C-x"))

;;; d-stump-remapped-keys-special-bindlists.el ends here
