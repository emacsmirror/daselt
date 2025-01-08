;;; d-emacs-emacs-bindlists.el --- d-emacs-bindlists for emacs  -*- lexical-binding: t; -*-

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

;;;; global-map
`(global-map 
;;;;; Coordinates
;;;;;; C-
;;;;;;; C-1
;;;;;;;; C-1-0
 (("C-" . (1 0 -1)) . #'scroll-down-command) 
 (("C-" . (1 0 1)) . #'scroll-up-command)) 

;;;; function-key-map
`(function-key-map 
;;;;; Strings
 ("<f7>" . #'event-apply-hyper-modifier) 

;;;;; Coordinates 
;;;;;;; 0 
 ((0 1 -7) . #'event-apply-alt-modifier) 

;;;;;;;; 0-2 
 ((0 2 -1) . #'event-apply-meta-modifier) 

;;;;;; C- 
;;;;;;; C-0 
;;;;;;;; C-0-2 
 (("C-" . (0 2 -1)) . #'event-apply-super-modifier)) 

;;;; button-buffer-map
`(button-buffer-map 
;;;;; Coordinates
;;;;;;; 2
;;;;;;;; 2-0
  ((2 0 -2) . #'backward-button) 
  ((2 0 2) . #'forward-button))

;;;; special-mode-map
`(special-mode-map 
;;;;; Coordinates
;;;;;;; 1
;;;;;;;; 1--1
  ((1 -1 -4) . #'avy-act-follow-in-new-tab) 
  ((1 -1 4) . #'avy-act-follow) 

;;;;;;;; 1-0 
  ((1 0 -3) . #'d-emacs-scroll-chunk-down) 
  ((1 0 -1) . #'scroll-down-command) 
  ((1 0 1) . #'scroll-up-command) 
  ((1 0 3) . #'d-emacs-scroll-chunk-up) 

;;;;;;;; 1-1 
  ((1 1 -2) . #'revert-buffer) 

;;;;;;; 2 
;;;;;;;; 2-0 
  ((2 0 -1) . #'beginning-of-buffer) 
  ((2 0 1) . #'end-of-buffer) 

;;;;;;; 3 
;;;;;;;; 3--1 
  ((3 -1 -4) . #'avy-act-follow-in-new-horizontal-window) 
  ((3 -1 4) . #'avy-act-follow-in-new-vertical-window) 

;;;;;;;; 3-0 
  ((3 0 1) . #'quit-window) 
  ((3 0 2) . #'d-follow-or-open) 

;;;;;;; 6 
;;;;;;;; 6--1 
  ((6 -1 -4) . #'d-emacs-new-horizontal-window-and-buffer) 
  ((6 -1 4) . #'d-emacs-new-vertical-window-and-buffer))

;;;; help-map
`(help-map 
;;;;; Coordinates
;;;;;;; 3
;;;;;;;; 3--1
  ((3 -1 3) . #'d-free-places-from-regexps) 

;;;;;;;; 3-0 
((3 0 -3) . #'d-emacs-coords-draw-placevals-from-coordrx) 
((3 0 -2) . #'d-emacs-coords-draw-keyboard-layer) 
((3 0 2) . #'d-draw-bindlist-layer) 
((3 0 3) . #'d-draw-bindings-from-regexps))

;;;; isearch-mode-map
`(isearch-mode-map 
;;;;; Coordinates
;;;;;; C-
;;;;;;; C-1
;;;;;;;; C-1-0
  (("C-" . (1 0 -4)) . #'isearch-repeat-backward) 
  (("C-" . (1 0 4)) . #'isearch-repeat-forward))

;;;; minibuffer-local-map
`(minibuffer-local-map 
;;;;; Coordinates
;;;;;;; 0
;;;;;;;; 0-0
  ((0 0 -6) . #'cdlatex-math-symbol) 

;;;;;;; 7 
;;;;;;;; 7-0 
  ((7 0 -5) . #'d-insert-normpair-yas-snippet) 
  ((7 0 5) . #'d-insert-othernormpair-yas-snippet) 

;;;;;; C- 
;;;;;;; C-0 
;;;;;;;; C-0--2 
  (("C-" . (0 -2 -4)) . #'cdlatex-math-modify) 

;;;;;;; C-1 
;;;;;;;; C-1-0 
  (("C-" . (1 0 2)) . #'d-emacs-move-right-or-exit) 

;;;;;;; C-8 
;;;;;;;; C-8-0 
  (("C-" . (8 0 -4)) . #'previous-matching-history-element) 
  (("C-" . (8 0 -3)) . #'previous-history-element) 
  (("C-" . (8 0 3)) . #'next-history-element) 
  (("C-" . (8 0 4)) . #'next-matching-history-element) 

;;;;;;;; C-8-1 
  (("C-" . (8 1 2)) . #'minibuffer-complete-and-exit) 

;;;;;; s- 
;;;;;;; s-3 
;;;;;;;; s-3-0 
  (("s-" . (3 0 -4)) . #'minibuffer-complete-history) 
  (("s-" . (3 0 4)) . #'minibuffer-complete-defaults))

;;;; minibuffer-local-isearch-map
`(minibuffer-local-isearch-map 
;;;;; Coordinates
;;;;;; C-
;;;;;;; C-3
;;;;;;;; C-3-0
  (("C-" . (3 0 2)) . #'isearch-complete-edit) 

;;;;;;;; C-3-1 
  (("C-" . (3 1 2)) . #'isearch-yank-char-in-minibuffer) 

;;;;;; s-C- 
;;;;;;; s-C-3 
;;;;;;;; s-C-3-0 
  (("s-C-" . (3 0 -4)) . #'isearch-reverse-exit-minibuffer) 
  (("s-C-" . (3 0 4)) . #'isearch-forward-exit-minibuffer))

;;;; query-replace-map
`(query-replace-map 
;;;;; Coordinates
;;;;;;; 1
;;;;;;;; 1-0
  ((1 0 -2) . #'skip) 
  ((1 0 2) . #'act) 

;;;;;;; 3 
;;;;;;;; 3-0 
  ((3 0 1) . #'exit) 

;;;;;; C- 
;;;;;;; C-1 
;;;;;;;; C-1-0 
  (("C-" . (1 0 -2)) . #'skip) 
  (("C-" . (1 0 2)) . #'act))

;;;; widget-keymap
`(widget-keymap 
;;;;; Coordinates
;;;;;;; 1
;;;;;;;; 1-0
  ((1 0 -2) . #'widget-backward) 
  ((1 0 2) . #'widget-forward) 

;;;;;;; 3 
;;;;;;;; 3-0 
  ((3 0 1) . #'bury-buffer) 
  ((3 0 2) . #'widget-button-press))

;;;; rectangle-mark-mode-map
`(rectangle-mark-mode-map 
;;;;; Coordinates
;;;;;;; 1
;;;;;;;; 1-0
  ((1 0 -2) . #'string-rectangle) 
  ((1 0 2) . #'string-insert-rectangle)
  ((1 1 -3) . #'copy-rectangle-as-kill)
  ((1 1 3) . #'yank-rectangle)
  ((1 -1 -2) . #'rectangle-exchange-point-and-mark)
  ((1 -1 2) . #'clear-rectangle)
  ((2 1 -3) . #'kill-rectangle)
  ((1 -1 -3) . #'delete-rectangle)
  ((1 0 -1) . #'open-rectangle)
  ((1 0 1) . #'close-rectangle)
  ((1 0 -3) . #'replace-rectangle)
  ((5 1 -3) . #'copy-rectangle-to-register))

;;;; prog-mode-map
`(prog-mode-map 
;;;;; Coordinates
;;;;;;; 5
;;;;;;;; 5--1
  ((5 -1 4) . #'d-emacs-insert-lambda-string))

;;;; emacs-lisp-mode-map
`(emacs-lisp-mode-map 
;;;;; Coordinates
;;;;;; M-
;;;;;;; M-3
;;;;;;;; M-3--1
  (("M-" . (3 -1 -4)) . #'d-emacs-fill-docstrings-in-buffer) 
  (("M-" . (3 -1 -3)) . #'d-emacs-generate-declare-function) 
  (("M-" . (3 -1 3)) . #'d-emacs-generate-variable-definition) 
  (("M-" . (3 -1 4)) . #'d-emacs-fill-current-docstring))

;;;; debug
`(debug
  (debugger-mode-map 
;;;;; Coordinates
;;;;;;; 1
;;;;;;;; 1--1
   ((1 -1 -4) . #'debugger-eval-expression) 
   ((1 -1 3) . #'debugger-jump) 
   ((1 -1 4) . #'debugger-list-functions) 

;;;;;;;; 1-0 
   ((1 0 -4) . #'debugger-return-value) 
   ((1 0 -3) . #'backtrace-backward-frame) 
   ((1 0 1) . #'debugger-continue) 
   ((1 0 2) . #'debugger-step-through) 
   ((1 0 3) . #'backtrace-forward-frame) 

;;;;;;;; 1-1 
   ((1 1 -2) . #'debugger-frame-clear) 
   ((1 1 0) . #'backtrace-toggle-locals) 

;;;;;;; 3 
;;;;;;;; 3-0 
   ((3 0 1) . #'debugger-quit)))

;;; d-emacs-emacs-bindlists.el ends here
