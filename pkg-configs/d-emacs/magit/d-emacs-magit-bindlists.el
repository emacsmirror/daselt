;;; d-emacs-magit-bindlists.el --- d-emacs-bindlists for magit  -*- lexical-binding: t; -*-

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

;; This file contains the keylists for maps of magit. If d-magit is t, it is parsed automatically when d-emacs-mode is started. Each element in this file should be either

;; 1. A LIST consisting of conses of whose car is a binding-location (see d--binding-location-p) binding-cars and values. This map list corresponds to  and should generally be at be at the top if it exists, though sometimes it makes sense to put it lower down if it references maps that are previously defined. When d-emacs-mode is activated and  is evaluated,  is backed up at d-emacs--mode-backup and the key combinations in this list are rebound to their corresponding values.

;; 2. A cons CONS1 whose car is a symbol SYMB whose cdr is a list as in 1. If SYMB is already bound when  is evaluated and d-emacs-mode is activated, then its value should be a keymap. This map is backed at d-emacs-SYMB-backup and then rebound as above. If SYMB is bound at that point, a new keymap is defined and bound to SYMB.

;; 3. A cons CONS2 whose car is a symbol or string and whose cdr is a list of conses like CONS1 as in 2. The first element of CONS2 denotes a file or feature that has to be evaluated before the keys in the bindlists in the cdr of CONS2 can be rebound.

;; Each of these options should be given using a backquote.

;;; Code:

;;;; magit-mode-map
`(
;;;;; Coordinates
;;;;;;; 5
;;;;;;;; 5--1
  ((5 -1 -4) . #'magit-edit-thing) 
  ((5 -1 -3) . #'magit-pull) 
  ((5 -1 3) . #'magit-push) 
  ((5 -1 4) . #'magit-visit-thing) 

;;;;;;;; 5-0 
  ((5 0 -3) . #'magit-diff-show-or-scroll-up) 
  ((5 0 -2) . #'magit-unstage) 
  ((5 0 2) . #'magit-stage) 
  ((5 0 3) . #'magit-diff-show-or-scroll-down) 
  ((5 0 5) . #'magit-browse-thing) 

;;;;;;;; 5-1 
  ((5 1 -3) . #'magit-copy-thing) 
  ((5 1 -2) . #'magit-refresh) 
  ((5 1 2) . #'magit-run) 

;;;;;;; 6 
;;;;;;;; 6--1 
  ((6 -1 -3) . #'magit-delete-thing) 
  ((6 -1 4) . #'magit-dired-jump) 

;;;;;;;; 6-0 
  ((6 0 -3) . #'magit-section-up) 
  ((6 0 -2) . #'magit-unstage-all) 
  ((6 0 2) . #'magit-stage-all) 

;;;;;;;; 6-1 
  ((6 1 -2) . #'magit-refresh-all) 
  ((6 1 2) . #'magit-git-command) 

;;;;;;; 7 
;;;;;;;; 7--1 
  ((7 -1 4) . #'magit-section-cycle) 

;;;;;;;; 7-0 
  ((7 0 -2) . #'magit-section-backward-sibling) 
  ((7 0 2) . #'magit-section-forward-sibling) 

;;;;;;;; 7-1 
  ((7 1 -3) . #'magit-copy-buffer-revision) 
  ((7 1 -2) . #'magit-reset) 

;;;;;;; 8 
;;;;;;;; 8-1 
  ((8 1 -3) . #'magit-clone) 
  ((8 1 -2) . #'magit-reset-quickly))

;;; d-emacs-magit-bindlists.el ends here
