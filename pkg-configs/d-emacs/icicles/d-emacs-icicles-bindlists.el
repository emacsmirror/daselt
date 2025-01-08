 ;;; d-emacs-icicles-bindlists.el --- d-emacs-bindlists for icicles  -*- lexical-binding: t; -*-

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

;; This file contains the keylists for maps of icicles. If daselt-icicles is t, it is parsed automatically when daselt-mode is started. Each element in this file should be either

;; 1. A LIST consisting of conses of whose car is a binding-location (see d--binding-location-p) binding-cars and values. This map list corresponds to  and should generally be at be at the top if it exists, though sometimes it makes sense to put it lower down if it references maps that are previously defined. When d-emacs-mode is activated and  is evaluated,  is backed up at d-emacs--mode-backup and the key combinations in this list are rebound to their corresponding values.

;; 2. A cons CONS1 whose car is a symbol SYMB whose cdr is a list as in 1. If SYMB is already bound when  is evaluated and d-emacs-mode is activated, then its value should be a keymap. This map is backed at d-emacs-SYMB-backup and then rebound as above. If SYMB is bound at that point, a new keymap is defined and bound to SYMB.

;; 3. A cons CONS2 whose car is a symbol or string and whose cdr is a list of conses like CONS1 as in 2. The first element of CONS2 denotes a file or feature that has to be evaluated before the keys in the bindlists in the cdr of CONS2 can be rebound.

;; Each of these options should be given using a backquote.

;;; Code:

;; ;;;; minibuffer-local-must-match-map
;; `(minibuffer-local-must-match-map 
;; ;;;;; Coordinates
;; ;;;;;; C-
;; ;;;;;;; C-3
;; ;;;;;;;; C-3--1
;;    (("C-" . (3 -1 -3)) . #'icicle-previous-prefix-candidate) 
;;    (("C-" . (3 -1 -2)) . #'icicle-prefix-complete) 
;;    (("C-" . (3 -1 2)) . #'icicle-candidate-action) 
;;    (("C-" . (3 -1 3)) . #'icicle-next-prefix-candidate) 
;;    (("C-" . (3 -1 4)) . #'icicle-switch-to-Completions-buf) 

;; ;;;;;;;; C-3-0 
;;    (("C-" . (3 0 -5)) . #'icicle-help-on-candidate) 
;;    (("C-" . (3 0 -3)) . #'icicle-previous-apropos-candidate) 
;;    (("C-" . (3 0 -2)) . #'icicle-apropos-complete) 
;;    (("C-" . (3 0 3)) . #'icicle-next-apropos-candidate) 

;; ;;;;;;;; C-3-1 
;;    (("C-" . (3 1 -2)) . #'icicle-narrow-candidates-with-predicate) 
;;    (("C-" . (3 1 0)) . #'icicle-toggle-option) 
;;    (("C-" . (3 1 2)) . #'icicle-narrow-candidates) 

;; ;;;;;;; C-8 
;; ;;;;;;;; C-8-0 
;;    (("C-" . (8 0 -1)) . #'icicle-candidate-set-complement) 
;;    (("C-" . (8 0 1)) . #'icicle-apropos-complete-and-widen) 

;; ;;;;;; M- 
;; ;;;;;;; M-8 
;; ;;;;;;;; M-8--1 

;; ;;;;;; s- 
;; ;;;;;;; s-8 
;; ;;;;;;;; s-8-0 
;;    (("s-" . (8 0 1)) . #'icicle-widen-candidates) 

;; ;;;;;; s-M- 
;; ;;;;;;; s-M-3 
;; ;;;;;;;; s-M-3--1 
;;    (("s-M-" . (3 -1 2)) . #'d-emacs-icicle-set-sort-to-match-regexp-hist-lex-p)))

(provide 'd-emacs-icicles-bindlists)
;;; d-emacs-icicles-bindlists.el ends here
