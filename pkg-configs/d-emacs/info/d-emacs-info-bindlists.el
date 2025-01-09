;;; d-emacs-info-bindlists.el --- d-emacs-bindlists for Info  -*- lexical-binding: t; -*-

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

;; This file contains the keylists for maps of Info. If d-Info is t, it is parsed automatically when d-emacs-mode is started. Each element in this file should be either

;; 1. A LIST consisting of conses of whose car is a binding-location (see d--binding-location-p) binding-cars and values. This map list corresponds to  and should generally be at be at the top if it exists, though sometimes it makes sense to put it lower down if it references maps that are previously defined. When d-emacs-mode is activated and  is evaluated,  is backed up at d-emacs--mode-backup and the key combinations in this list are rebound to their corresponding values.

;; 2. A cons CONS1 whose car is a symbol SYMB whose cdr is a list as in 1. If SYMB is already bound when  is evaluated and d-emacs-mode is activated, then its value should be a keymap. This map is backed at d-emacs-SYMB-backup and then rebound as above. If SYMB is bound at that point, a new keymap is defined and bound to SYMB.

;; 3. A cons CONS2 whose car is a symbol or string and whose cdr is a list of conses like CONS1 as in 2. The first element of CONS2 denotes a file or feature that has to be evaluated before the keys in the bindlists in the cdr of CONS2 can be rebound.

;; Each of these options should be given using a backquote.

;;; Code:

;;;; Info-mode-map
`(Info-mode-map 
;;;;; Coordinates
;;;;;;; 1
;;;;;;;; 1--1
  ((1 -1 -4) . #'avy-act-follow-in-new-tab) 
  ((1 -1 -3) . #'Info-up) 
  ((1 -1 -2) . #'Info-prev) 
  ((1 -1 -1) . #'Info-top-node) 
  ((1 -1 1) . #'Info-final-node) 
  ((1 -1 2) . #'Info-next) 
  ((1 -1 4) . #'avy-act-follow) 

;;;;;;;; 1-0 
  ((1 0 -5) . #'Info-follow-nearest-node) 
  ((1 0 -3) . #'d-emacs-scroll-chunk-down) 
  ((1 0 -3) . #'Info-scroll-down) 
  ((1 0 -3) . #'Info-scroll-up) 
  ((1 0 -2) . #'Info-prev-reference) 
  ((1 0 2) . #'Info-next-reference) 
  ((1 0 3) . #'d-emacs-scroll-chunk-up) 
  ((1 0 5) . #'Info-save-current-node) 

;;;;;;;; 1-1 
  ((1 1 -2) . #'Info-last-menu-item) 
  ((1 1 2) . #'Info-next-menu-item) 

;;;;;;; 2 
;;;;;;;; 2--1 
  ((2 -1 -4) . #'Info-toc) 
  ((2 -1 4) . #'Info-toc-outline) 

;;;;;;;; 2-0 
  ((2 0 -2) . #'Info-history-back) 
  ((2 0 2) . #'Info-history-forward) 

;;;;;;; 3 
;;;;;;;; 3-0 
  ((3 0 1) . #'quit-window) 
  ((3 0 2) . #'d-emacs-C-3-0-2) 

;;;;;;; 8 
;;;;;;;; 8-0 
  ((8 0 -2) . #'d-emacs-in-new-buffer-Info-prev) 
  ((8 0 2) . #'d-emacs-in-new-buffer-Info-next))

;;; d-emacs-info-bindlists.el ends here
