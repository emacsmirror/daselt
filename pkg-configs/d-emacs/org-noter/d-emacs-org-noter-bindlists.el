;;; d-emacs-org-noter-bindlists.el --- d-emacs-bindlists for org-noter  -*- lexical-binding: t; -*-

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

;; This file contains the keylists for maps of org-noter. If d-org-noter is t, it is parsed automatically when d-emacs-mode is started. Each element in this file should be either

;; 1. A LIST consisting of conses of whose car is a binding-location (see d--binding-location-p) binding-cars and values. This map list corresponds to  and should generally be at be at the top if it exists, though sometimes it makes sense to put it lower down if it references maps that are previously defined. When d-emacs-mode is activated and  is evaluated,  is backed up at d-emacs--mode-backup and the key combinations in this list are rebound to their corresponding values.

;; 2. A cons CONS1 whose car is a symbol SYMB whose cdr is a list as in 1. If SYMB is already bound when  is evaluated and d-emacs-mode is activated, then its value should be a keymap. This map is backed at d-emacs-SYMB-backup and then rebound as above. If SYMB is bound at that point, a new keymap is defined and bound to SYMB.

;; 3. A cons CONS2 whose car is a symbol or string and whose cdr is a list of conses like CONS1 as in 2. The first element of CONS2 denotes a file or feature that has to be evaluated before the keys in the bindlists in the cdr of CONS2 can be rebound.

;; Each of these options should be given using a backquote.

;;; Code:

;;;; org-noter-doc-mode-map
`(org-noter-doc-mode-map 
;;;;; Coordinates
;;;;;;; 1
;;;;;;;; 1--1
  ((1 -1 -4) . #'org-noter-insert-note-toggle-no-questions) 
  ((1 -1 4) . #'org-noter-insert-precise-note-toggle-no-questions) 

;;;;;;;; 1-0 
  ((1 0 -4) . #'org-noter-insert-note) 
  ((1 0 -3) . #'org-noter-sync-prev-page-or-chapter) 
  ((1 0 -2) . #'org-noter-sync-prev-note) 
  ((1 0 2) . #'org-noter-sync-next-note) 
  ((1 0 3) . #'org-noter-sync-next-page-or-chapter) 
  ((1 0 4) . #'org-noter-insert-precise-note) 

;;;;;;;; 1-1 
  ((1 1 0) . #'org-noter-sync-current-note) 

;;;;;;; 3 
;;;;;;;; 3-0 
  ((3 0 1) . #'org-noter-kill-session) 

;;;;;; M- 
;;;;;;; M-3 
;;;;;;;; M-3-1 
  (("M-" . (3 1 0)) . #'org-noter-sync-current-page-or-chapter))

;;;; org-noter-notes-mode-map
`(org-noter-notes-mode-map 
;;;;; Coordinates
;;;;;; s-
;;;;;;; s-3
;;;;;;;; s-3-0
  (("s-" . (3 0 -3)) . #'org-noter-sync-prev-page-or-chapter) 
  (("s-" . (3 0 -2)) . #'org-noter-prev-note) 
  (("s-" . (3 0 2)) . #'org-noter-sync-next-note) 
  (("s-" . (3 0 3)) . #'org-noter-sync-next-page-or-chapter) 

;;;;;;;; s-3-1 
  (("s-" . (3 1 0)) . #'org-noter-sync-current-note) 

;;;;;; s-C- 
;;;;;;; s-C-3 
;;;;;;;; s-C-3-1 
  (("s-C-" . (3 1 0)) . #'org-noter-sync-current-page-or-chapter))

;;; d-emacs-org-noter-bindlists.el ends here
