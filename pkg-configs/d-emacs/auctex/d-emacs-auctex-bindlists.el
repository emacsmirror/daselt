;;; d-emacs-auctex-bindlists.el --- d-emacs-bindlists for auctex  -*- lexical-binding: t; -*-

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

;; This file contains the keylists for maps of auctex. If d-auctex is t, it is parsed automatically when d-emacs-mode is started. Each element in this file should be either

;; 1. A LIST consisting of conses of whose car is a binding-location (see d--binding-location-p) binding-cars and values. This map list corresponds to  and should generally be at be at the top if it exists, though sometimes it makes sense to put it lower down if it references maps that are previously defined. When d-emacs-mode is activated and  is evaluated,  is backed up at d-emacs--mode-backup and the key combinations in this list are rebound to their corresponding values.

;; 2. A cons CONS1 whose car is a symbol SYMB whose cdr is a list as in 1. If SYMB is already bound when  is evaluated and d-emacs-mode is activated, then its value should be a keymap. This map is backed at d-emacs-SYMB-backup and then rebound as above. If SYMB is bound at that point, a new keymap is defined and bound to SYMB.

;; 3. A cons CONS2 whose car is a symbol or string and whose cdr is a list of conses like CONS1 as in 2. The first element of CONS2 denotes a file or feature that has to be evaluated before the keys in the bindlists in the cdr of CONS2 can be rebound.

;; Each of these options should be given using a backquote.

;;; Code:

;;;; LaTeX-mode-map
`(LaTeX-mode-map 
;;;;; Coordinates
;;;;;;; 8
;;;;;;;; 8-0
  ((8 0 -5) . (if d-lua-insert-normpairs #'d-insert-normpair-yas-snippet)) 
  ((8 0 5) . (if d-lua-insert-normpairs #'d-insert-othernormpair-yas-snippet)) 

;;;;;; C- 
;;;;;;; C-3 
;;;;;;;; C-3--1 
  (("C-" . (3 -1 -3)) . #'LaTeX-mark-environment) 
  (("C-" . (3 -1 3)) . #'LaTeX-mark-section) 
  (("C-" . (3 -1 4)) . #'TeX-fold-dwim) 

;;;;;;;; C-3-0 
  (("C-" . (3 0 -2)) . #'latex/backward-environment) 
  (("C-" . (3 0 -1)) . #'latex/beginning-of-environment) 
  (("C-" . (3 0 1)) . #'latex/end-of-environment) 
  (("C-" . (3 0 2)) . #'latex/forward-environment) 

;;;;;;;; C-3-1 
  (("C-" . (3 1 -4)) . #'latex/hide-show-all) 
  (("C-" . (3 1 2)) . #'LaTeX-command-section) 
  (("C-" . (3 1 3)) . #'reftex-reference) 

;;;;;;; C-6 
;;;;;;;; C-6--1 
  (("C-" . (6 -1 4)) . #'reftex-toc) 

;;;;;;;; C-6-0 
  (("C-" . (6 0 -3)) . #'latex/up-section) 
  (("C-" . (6 0 -2)) . #'latex/previous-section) 
  (("C-" . (6 0 -1)) . #'latex/previous-section-same-level) 
  (("C-" . (6 0 1)) . #'latex/next-section-same-level) 
  (("C-" . (6 0 2)) . #'latex/next-section) 

;;;;;; M- 
;;;;;;; M-3 
;;;;;;;; M-3--1 
  (("M-" . (3 -1 -4)) . #'TeX-fold-region) 
  (("M-" . (3 -1 -3)) . #'LaTeX-mark-environment) 
  (("M-" . (3 -1 -2)) . #'LaTeX-close-environment) 
  (("M-" . (3 -1 3)) . #'LaTeX-mark-section) 
  (("M-" . (3 -1 4)) . #'TeX-fold-clearout-region) 

;;;;;;;; M-3-0 
  (("M-" . (3 0 -4)) . #'d-lua-change-mathvar) 
  (("M-" . (3 0 -3)) . #'TeX-fold-paragraph) 
  (("M-" . (3 0 -3)) . #'TeX-fold-macro) 
  (("M-" . (3 0 -1)) . #'TeX-fold-buffer) 
  (("M-" . (3 0 1)) . #'TeX-fold-clearout-buffer) 
  (("M-" . (3 0 3)) . #'TeX-fold-clearout-paragraph) 
  (("M-" . (3 0 4)) . #'d-lua-change-mathvar-withinstring) 
  (("M-" . (3 0 5)) . #'arxiv-lookup) 

;;;;;;; M-6 
;;;;;;;; M-6--1 
  (("M-" . (6 -1 -4)) . #'TeX-fold-section) 
  (("M-" . (6 -1 4)) . #'TeX-fold-clearout-section) 

;;;;;; M-C- 
;;;;;;; M-C-3 
;;;;;;;; M-C-3--1 
  (("M-C-" . (3 -1 -4)) . #'reftex-label) 
  (("M-C-" . (3 -1 -3)) . #'er/mark-LaTeX-inside-environment) 
  (("M-C-" . (3 -1 4)) . #'reftex-goto-label) 

;;;;;;;; M-C-3-0 
  (("M-C-" . (3 0 -2)) . #'LaTeX-environment) 
  (("M-C-" . (3 0 2)) . #'LaTeX-section) 

;;;;;; s- 
;;;;;;; s-3 
;;;;;;;; s-3--1 
  (("s-" . (3 -1 -3)) . #'d-lua-LaTeX-delete-environment-outside) 
  (("s-" . (3 -1 3)) . #'d-lua-remove-modifier))

;;;; reftex-toc-mode
;;;; reftex-toc
`(reftex-toc
  (reftex-toc-mode-map 
;;;;; Coordinates
;;;;;;; 1
;;;;;;;; 1-0
   ((1 0 -3) . #'reftex-toc-previous) 
   ((1 0 -2) . #'reftex-toc-goto-line) 
   ((1 0 -1) . #'reftex-toc-previous-heading) 
   ((1 0 1) . #'reftex-toc-next-heading) 
   ((1 0 2) . #'reftex-toc-goto-line-and-hide) 
   ((1 0 3) . #'reftex-toc-next) 

;;;;;;;; 1-1 
   ((1 1 2) . #'revert-buffer) 

;;;;;;; 2 
;;;;;;;; 2--1 
   ((2 -1 -4) . #'reftex-toc-external) 
   ((2 -1 4) . #'reftex-toc-jump) 

;;;;;;;; 2-0 
   ((2 0 -2) . #'reftex-toc-show-calling-point) 
   ((2 0 -1) . #'reftex-toc-promote) 
   ((2 0 1) . #'reftex-toc-demote) 
   ((2 0 2) . #'reftex-toc-view-line) 

;;;;;;;; 2-1 
   ((2 1 2) . #'reftex-toc-revert) 

;;;;;;; 4 
;;;;;;;; 4-0 
   ((3 0 -4) . #'reftex-toc-rename-label) 
   ((3 0 1) . #'reftex-toc-quit) 

;;;;;; H- 
;;;;;;; H-4 
;;;;;;;; H-4-0 
   (("H-" . (3 0 1)) . #'reftex-toc-quit-and-kill) 

;;;;;; M- 
;;;;;;; M-4 
;;;;;;;; M-4--1 
   (("M-" . (3 -1 -2)) . #'reftex-toggle-auto-toc-recenter) 
   (("M-" . (3 -1 2)) . #'reftex-toggle-auto-view-crossref) 
   (("M-" . (3 -1 4)) . #'reftex-toc-max-level) 

;;;;;;;; M-4-0 
   (("M-" . (3 0 -4)) . #'reftex-toc-toggle-labels) 
   (("M-" . (3 0 -3)) . #'reftex-toc-toggle-index) 
   (("M-" . (3 0 -2)) . #'reftex-toc-toggle-file-boundary) 
   (("M-" . (3 0 2)) . #'reftex-toc-toggle-context) 
   (("M-" . (3 0 3)) . #'reftex-toc-toggle-follow) 
   (("M-" . (3 0 4)) . #'reftex-toc-toggle-dedicated-frame) 

;;;;;; M-C- 
;;;;;;; M-C-4 
;;;;;;;; M-C-4-0 
   (("M-C-" . (3 0 4)) . #'reftex-toc-display-index)))

;;;; reftex-sel
`(reftex-sel
  (reftex-select-label-mode-map 
;;;;; Coordinates
;;;;;;; 1
;;;;;;;; 1--1
   ((1 -1 -4) . #'reftex-select-jump-to-previous) 
   ((1 -1 -3) . #'reftex-select-unmark) 
   ((1 -1 3) . #'reftex-select-mark) 
   ((1 -1 3) . #'reftex-select-mark-to) 
   ((1 -1 4) . #'reftex-select-jump) 

;;;;;;;; 1-0 
   ((1 0 -3) . #'reftex-select-previous) 
   ((1 0 -2) . #'reftex-select-callback) 
   ((1 0 -1) . #'reftex-select-previous-heading) 
   ((1 0 1) . #'reftex-select-next-heading) 
   ((1 0 2) . #'reftex-select-accept) 
   ((1 0 3) . #'reftex-select-next) 

;;;;;;;; 1-1 
   ((1 1 2) . #'reftex-select-read-label) 
   ((1 1 2) . #'reftex-select-show-insertion-point) 

;;;;;;; 2 
;;;;;;;; 2--1 
   ((2 -1 -3) . #'reftex-select-mark-and) 
   ((2 -1 3) . #'reftex-select-mark-comma) 

;;;;;;;; 2-0 
   ((2 0 -3) . #'reftex-select-cycle-ref-style-backward) 
   ((2 0 3) . #'reftex-select-cycle-ref-style-forward) 

;;;;;;; 4 
;;;;;;;; 4-0 
   ((3 0 1) . #'reftex-select-quit)))
;;; d-emacs-auctex-bindlists.el ends here
