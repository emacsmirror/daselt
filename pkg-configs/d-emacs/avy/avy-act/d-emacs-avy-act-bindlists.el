  ;;; d-emacs-avy-act-bindlists.el --- d-emacs-bindlists for emacs  -*- lexical-binding: t; -*-

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

;; This file contains the keylists for maps of emacs. If daselt-emacs is t, it is parsed automatically when daselt-mode is started. Each element in this file should be either

;; 1. A LIST consisting of conses of whose car is a binding-location (see d--binding-location-p) binding-cars and values. This map list corresponds to  and should generally be at be at the top if it exists, though sometimes it makes sense to put it lower down if it references maps that are previously defined. When d-emacs-mode is activated and  is evaluated,  is backed up at d-emacs--mode-backup and the key combinations in this list are rebound to their corresponding values.

;; 2. A cons CONS1 whose car is a symbol SYMB whose cdr is a list as in 1. If SYMB is already bound when  is evaluated and d-emacs-mode is activated, then its value should be a keymap. This map is backed at d-emacs-SYMB-backup and then rebound as above. If SYMB is bound at that point, a new keymap is defined and bound to SYMB.

;; 3. A cons CONS2 whose car is a symbol or string and whose cdr is a list of conses like CONS1 as in 2. The first element of CONS2 denotes a file or feature that has to be evaluated before the keys in the bindlists in the cdr of CONS2 can be rebound.

;; Each of these options should be given using a backquote.

;;; Code:

;;;; avy-act-function-map
`(avy-act-function-map 
;;;;; Coordinates
;;;;;;; 1
;;;;;;;; 1-0
  ((1 0 -3) . #'avy-goto-char-in-line) 
  ((1 0 -2) . #'avy-goto-char-timer) 
  ((1 0 -1) . #'avy-goto-line) 
  ((1 0 1) . #'avy-goto-end-of-line) 
  ((1 0 2) . #'avy-goto-word-1) 

;;;;;;;; 1-1 
  ((1 1 -6) . #'avy-act-functions-help))

;;;; avy-act-selection-command-map
`(avy-act-selection-command-map 
;;;;; Coordinates
;;;;;;; 1
;;;;;;;; 1--1
  ((1 -1 -3) . #'delete-region) 

;;;;;;;; 1-0 
  ((1 0 -4) . #'avy-act-overwrite) 

;;;;;;;; 1-1 
  ((1 1 -3) . #'kill-ring-save) 
  ((1 1 3) . #'yank) 

;;;;;;; 2 
;;;;;;;; 2-1 
  ((2 1 -3) . #'kill-region))

;;;; avy-act-position-selection-map
`(avy-act-position-selection-map 
;;;;; Coordinates
;;;;;;; 1
;;;;;;;; 1-0
  ((1 0 -4) . #'mark-paragraph) 
  ((1 0 -3) . #'er/mark-sentence) 
  ((1 0 -2) . #'er/mark-word) 
  ((1 0 2) . #'mark-sexp) 
  ((1 0 3) . #'mark-line) 
  ((1 0 4) . #'mark-defun) 

;;;;;;;; 1-1 
  ((1 1 -2) . #'avy-act-mark-nothing) 
  ((1 1 2) . #'avy-act-mark-character))

;;;; avy-act-post-action-map
`(avy-act-post-action-map 
;;;;; Coordinates
;;;;;;; 4
;;;;;;;; 4-0
  ((4 0 -2) . #'avy-act-delete-char-from-distance) 
  ((4 0 2) . #'avy-act-insert-space-from-distance))

;;; d-emacs-avy-act-bindlists.el ends here
