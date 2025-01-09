  ;;; d-emacs-org-brain-bindlists.el --- d-emacs-bindlists for org-brain  -*- lexical-binding: t; -*-

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

;; This file contains the keylists for maps of org-brain. If d-org-brain is t, it is parsed automatically when d-emacs-mode is started. Each element in this file should be either

;; 1. A LIST consisting of conses of whose car is a binding-location (see d--binding-location-p) binding-cars and values. This map list corresponds to  and should generally be at be at the top if it exists, though sometimes it makes sense to put it lower down if it references maps that are previously defined. When d-emacs-mode is activated and  is evaluated,  is backed up at d-emacs--mode-backup and the key combinations in this list are rebound to their corresponding values.

;; 2. A cons CONS1 whose car is a symbol SYMB whose cdr is a list as in 1. If SYMB is already bound when  is evaluated and d-emacs-mode is activated, then its value should be a keymap. This map is backed at d-emacs-SYMB-backup and then rebound as above. If SYMB is bound at that point, a new keymap is defined and bound to SYMB.

;; 3. A cons CONS2 whose car is a symbol or string and whose cdr is a list of conses like CONS1 as in 2. The first element of CONS2 denotes a file or feature that has to be evaluated before the keys in the bindlists in the cdr of CONS2 can be rebound.

;; Each of these options should be given using a backquote.

;;; Code:

;;;; org-brain-select-map
`(org-brain-select-map 
;;;;; Coordinates
;;;;;;; 8
;;;;;;;; 8-0
  ((8 0 -3) . #'org-brain-add-selected-parents) 
  ((8 0 -2) . #'org-brain-clear-selected) 
  ((8 0 2) . #'org-brain-add-selected-friendships) 
  ((8 0 3) . #'org-brain-add-selected-children) 

;;;;;;;; 8-1 
  ((8 1 -3) . #'org-brain-remove-selected-parents) 
  ((8 1 -2) . #'org-brain-delete-selected-entries) 
  ((8 1 2) . #'org-brain-remove-selected-friendships) 
  ((8 1 5) . #'org-brain-remove-selected-children))

;;;; org-brain-visualize-mode-map

;;;; org-brain-visualize-mode-map
`(org-brain-visualize-mode-map 
;;;;; Coordinates
;;;;;;; 1
;;;;;;;; 1--1
  ((1 -1 -4) . #'org-brain-hide-ancestor-level) 
  ((1 -1 -2) . #'org-brain-pin) 
  ((1 -1 2) . #'org-brain-select-dwim) 
  ((1 -1 3) . #'org-brain-visualize-follow) 
  ((1 -1 4) . #'org-brain-goto) 
  ((1 -1 4) . #'org-brain-hide-descendant-level) 

;;;;;;;; 1-0 
  ((1 0 -3) . #'scroll-up-command) 
  ((1 0 -2) . #'backward-button) 
  ((1 0 -1) . #'beginning-of-buffer) 
  ((1 0 1) . #'end-of-buffer) 
  ((1 0 2) . #'forward-button) 
  ((1 0 3) . #'scroll-down-command) 

;;;;;;;; 1-1 
  ((1 1 -3) . #'revert-buffer) 
  ((1 1 4) . #'org-brain-goto-current) 

;;;;;;; 2 
;;;;;;;; 2-0 
  ((2 0 -4) . #'org-brain-set-title) 
  ((2 0 -3) . #'org-brain-add-resource) 
  ((2 0 -2) . #'org-brain-annotate-edge) 
  ((2 0 2) . #'org-brain-add-nickname) 
  ((2 0 3) . #'org-brain-add-child-headline) 

;;;;;;; 3 
;;;;;;;; 3--1 
  ((3 -1 -3) . #'org-brain-delete-entry) 

;;;;;;; 4 
;;;;;;;; 4-0 
  ((4 0 -3) . #'org-brain-change-local-parent) 
  ((4 0 -2) . #'org-brain-visualize-attach) 
  ((4 0 3) . #'org-brain-refile) 
  ((4 0 4) . #'org-brain-visualize-wander) 

;;;;;;; 5 
;;;;;;;; 5-0 
  ((5 0 -1) . #'org-brain-visualize-random) 
  ((5 0 1) . #'org-brain-visualize-quit) 
  ((5 0 2) . #'org-brain-open-resource) 

;;;;;;; 6 
;;;;;;;; 6--1 
  ((6 -1 -3) . #'org-brain-visualize-parent) 
  ((6 -1 3) . #'org-brain-visualize-mind-map) 

;;;;;;;; 6-0 
  ((6 0 -3) . #'org-brain-add-parent) 
  ((6 0 -2) . #'org-brain-add-relationship) 
  ((6 0 2) . #'org-brain-add-friendship) 
  ((6 0 3) . #'org-brain-add-child) 

;;;;;;;; 6-1 
  ((6 1 -3) . #'org-brain-remove-parent) 
  ((6 1 -2) . #'org-brain-visualize-back) 
  ((6 1 2) . #'org-brain-pin) 
  ((6 1 5) . #'org-brain-remove-child))

;;; d-emacs-org-brain-bindlists.el ends here
