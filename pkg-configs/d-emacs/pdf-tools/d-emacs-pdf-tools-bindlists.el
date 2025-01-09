;;; d-emacs-pdf-tools-bindlists.el --- d-emacs-bindlists for pdf-tools  -*- lexical-binding: t; -*-

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

;; This file contains the keylists for maps of pdf-tools. If d-pdf-tools is t, it is parsed automatically when d-emacs-mode is started. Each element in this file should be either

;; 1. A LIST consisting of conses of whose car is a binding-location (see d--binding-location-p) binding-cars and values. This map list corresponds to  and should generally be at be at the top if it exists, though sometimes it makes sense to put it lower down if it references maps that are previously defined. When d-emacs-mode is activated and  is evaluated,  is backed up at d-emacs--mode-backup and the key combinations in this list are rebound to their corresponding values.

;; 2. A cons CONS1 whose car is a symbol SYMB whose cdr is a list as in 1. If SYMB is already bound when  is evaluated and d-emacs-mode is activated, then its value should be a keymap. This map is backed at d-emacs-SYMB-backup and then rebound as above. If SYMB is bound at that point, a new keymap is defined and bound to SYMB.

;; 3. A cons CONS2 whose car is a symbol or string and whose cdr is a list of conses like CONS1 as in 2. The first element of CONS2 denotes a file or feature that has to be evaluated before the keys in the bindlists in the cdr of CONS2 can be rebound.

;; Each of these options should be given using a backquote.

;;; Code:

;;;; pdf-view-mode-map

;;;; pdf-view-mode-map
`(pdf-view-mode-map 
;;;;; Coordinates
;;;;;;; 1
;;;;;;;; 1--1
  ((1 -1 -5) . #'pdf-view-midnight-minor-mode) 
  ((1 -1 -4) . #'pdf-view-position-to-register) 

;;;;;;;; 1-0 
  ((1 0 -3) . #'d-emacs-pdf-view-scroll-chunk-up) 
  ((1 0 -2) . #'image-backward-hscroll) 
  ((1 0 -1) . #'pdf-view-scroll-down-or-previous-page) 
  ((1 0 1) . #'pdf-view-scroll-up-or-next-page) 
  ((1 0 2) . #'image-forward-hscroll) 
  ((1 0 3) . #'d-emacs-pdf-view-scroll-chunk-down) 

;;;;;;;; 1-1 
  ((1 1 -2) . #'pdf-view-fit-height-to-window) 
  ((1 1 2) . #'pdf-view-fit-width-to-window) 

;;;;;;; 4 
;;;;;;;; 4--1 
  ((4 -1 4) . #'pdf-view-goto-page) 

;;;;;;;; 4-0 
  ((4 0 -4) . #'org-noter) 
  ((4 0 4) . #'pdf-view-rotate))

;;;; pdf-outline
`(pdf-outline
  (pdf-outline-minor-mode-map 
;;;;; Coordinates
;;;;;;; 1
;;;;;;;; 1-0
   ((1 0 -1)) 

;;;;;;; 2 
;;;;;;;; 2--1 
   ((2 -1 4) . #'pdf-outline))

  
;;;; pdf-outline-buffer-mode-map
  (pdf-outline-buffer-mode-map 
;;;;; Coordinates
;;;;;;; 1
;;;;;;;; 1--1
   ((1 -1 -1) . #'outline-backward-same-level) 
   ((1 -1 1) . #'outline-forward-same-level) 
   ((1 -1 4) . #'pdf-outline-follow-link) 

;;;;;;;; 1-0 
   ((1 0 -3) . #'previous-line) 
   ((1 0 -2) . #'hide-subtree) 
   ((1 0 2) . #'show-subtree) 
   ((1 0 3) . #'next-line) 

;;;;;;; 4 
;;;;;;;; 4--1 
   ((4 -1 -4) . #'outline-cycle-buffer) 
   ((4 -1 4) . #'outline-cycle) 

;;;;;;;; 4-0 
   ((4 0 -3) . #'pdf-outline-up-heading) 
   ((4 0 -2) . #'pdf-outline-display-link) 
   ((4 0 1) . #'quit-window) 
   ((4 0 1) . #'pdf-outline-quit) 
   ((4 0 2) . #'pdf-outline-follow-link-and-quit)))

;;;; pdf-history
`(pdf-history
  (pdf-history-minor-mode-map 
;;;;; Coordinates
;;;;;;; 1
;;;;;;;; 1-0
   ((1 0 -2) . #'pdf-history-backward) 
   ((1 0 2) . #'pdf-history-forward) 
   ((1 0 3))))

;;; d-emacs-pdf-tools-bindlists.el ends here
