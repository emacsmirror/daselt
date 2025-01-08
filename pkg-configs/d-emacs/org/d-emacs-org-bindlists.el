;;; d-emacs-org-bindlists.el --- d-emacs-bindlists for org  -*- lexical-binding: t; -*-

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

;; This file contains the keylists for maps of org. If daselt-org is t, it is parsed automatically when daselt-mode is started. Each element in this file should be either

;; 1. A LIST consisting of conses of whose car is a binding-location (see d--binding-location-p) binding-cars and values. This map list corresponds to  and should generally be at be at the top if it exists, though sometimes it makes sense to put it lower down if it references maps that are previously defined. When d-emacs-mode is activated and  is evaluated,  is backed up at d-emacs--mode-backup and the key combinations in this list are rebound to their corresponding values.

;; 2. A cons CONS1 whose car is a symbol SYMB whose cdr is a list as in 1. If SYMB is already bound when  is evaluated and d-emacs-mode is activated, then its value should be a keymap. This map is backed at d-emacs-SYMB-backup and then rebound as above. If SYMB is bound at that point, a new keymap is defined and bound to SYMB.

;; 3. A cons CONS2 whose car is a symbol or string and whose cdr is a list of conses like CONS1 as in 2. The first element of CONS2 denotes a file or feature that has to be evaluated before the keys in the bindlists in the cdr of CONS2 can be rebound.

;; Each of these options should be given using a backquote.

;;; Code:

;;;; org-mode-map
`(
;;;;; Coordinates
;;;;;;; 0
;;;;;;;; 0-0
  ((0 0 -6) . #'cdlatex-math-symbol) 

;;;;;;; 2 
;;;;;;;; 2--1 
  ((2 -1 3)) 

;;;;;; C- 
;;;;;;; C-4 
;;;;;;;; C-4-0 
  (("C-" . (4 0 -3)) . #'org-previous-block) 
  (("C-" . (4 0 -2)) . #'org-backward-element) 
  (("C-" . (4 0 -1)) . #'org-previous-item) 
  (("C-" . (4 0 1)) . #'org-previous-item) 
  (("C-" . (4 0 2)) . #'org-forward-element) 
  (("C-" . (4 0 3)) . #'org-next-block) 

;;;;;;; C-7 
;;;;;;;; C-7--1 
  (("C-" . (7 -1 -4)) . #'org-global-cycle) 
  (("C-" . (7 -1 -2)) . #'org-todo) 
  (("C-" . (7 -1 4)) . #'org-cycle) 

;;;;;;;; C-7-0 
  (("C-" . (7 0 -4)) . #'org-backward-heading-same-level) 
  (("C-" . (7 0 -2)) . #'org-previous-visible-heading) 
  (("C-" . (7 0 -1)) . #'org-do-demote) 
  (("C-" . (7 0 1)) . #'org-do-demote) 
  (("C-" . (7 0 2)) . #'org-next-visible-heading) 
  (("C-" . (7 0 4)) . #'org-forward-heading-same-level) 

;;;;;;;; C-7-1 
  (("C-" . (7 1 -3)) . #'org-store-link) 
  (("C-" . (7 1 3)) . #'org-insert-link) 

;;;;;; M- 
;;;;;;; M-4 
;;;;;;;; M-4--1 
  (("M-" . (4 -1 -5)) . #'org-roam-buffer-dedicated-p) 
  (("M-" . (4 -1 -3)) . #'org-footnote-action) 
  (("M-" . (4 -1 -2)) . #'org-insert-drawer) 
  (("M-" . (4 -1 2)) . #'org-insert-item) 
  (("M-" . (4 -1 3)) . #'org-insert-structure-template) 
  (("M-" . (4 -1 5)) . #'org-roam-buffer-toggle) 

;;;;;;;; M-4-0 
  (("M-" . (4 0 -3)) . #'d-emacs-org-insert-superheading) 
  (("M-" . (4 0 -2)) . #'org-insert-heading) 
  (("M-" . (4 0 -1)) . #'org-beginning-of-item) 
  (("M-" . (4 0 1)) . #'org-end-of-item) 
  (("M-" . (4 0 2)) . #'org-insert-heading-after-current) 
  (("M-" . (4 0 3)) . #'org-insert-subheading) 
  (("M-" . (4 0 5)) . #'org-goto) 

;;;;;;;; M-4-1 
  (("M-" . (4 1 -3)) . #'org-insert-comment) 
  (("M-" . (4 1 -2)) . #'org-insert-heading-respect-content) 
  (("M-" . (4 1 2)) . #'org-toggle-inline-images) 

;;;;;;; M-7 
;;;;;;;; M-7--8 
  (("M-" . (7 -8 -2)) . #'org-fold-hide-subtree) 
  (("M-" . (7 -8 1)) . #'org-fold-show-subtree) 

;;;;;;;; M-7-0 
  (("M-" . (7 0 -2)) . #'org-fold-hide-entry) 
  (("M-" . (7 0 -1)) . #'org-fold-hide-sublevels) 
  (("M-" . (7 0 1)) . #'org-fold-show-all) 
  (("M-" . (7 0 2)) . #'org-fold-show-entry) 
  (("M-" . (7 0 3)) . #'org-fold-show-children) 

;;;;;; M-C- 
;;;;;;; M-C-4 
;;;;;;;; M-C-4--1 
  (("M-C-" . (4 -1 -3)) . #'d-emacs-org-convert-quote) 
  (("M-C-" . (4 -1 3)) . #'org-insert-todo-heading-respect-content) 

;;;;;;;; M-C-4-0 
  (("M-C-" . (4 0 -3)) . #'d-emacs-org-insert-todo-superheading) 
  (("M-C-" . (4 0 -2)) . #'org-insert-todo-heading) 
  (("M-C-" . (4 0 -1)) . #'org-beginning-of-item-list) 
  (("M-C-" . (4 0 1)) . #'org-end-of-item-list) 
  (("M-C-" . (4 0 2)) . #'d-emacs-org-insert-todo-heading-after-current) 
  (("M-C-" . (4 0 3)) . #'org-insert-todo-subheading) 

;;;;;; s- 
;;;;;;; s-7 
;;;;;;;; s-7-0 
  (("s-" . (7 0 -1)) . #'org-promote-subtree) 
  (("s-" . (7 0 1)) . #'org-demote-subtree) 

;;;;;; s-C- 
;;;;;;; s-C-4 
;;;;;;;; s-C-4-0 
  (("s-C-" . (4 0 -2)) . #'org-outdent-item-tree) 
  (("s-C-" . (4 0 -1)) . #'org-move-item-up) 
  (("s-C-" . (4 0 1)) . #'org-move-item-down) 
  (("s-C-" . (4 0 2)) . #'org-indent-item-tree))

(provide 'd-emacs-org-bindlists)
;;; d-emacs-org-bindlists.el ends here
