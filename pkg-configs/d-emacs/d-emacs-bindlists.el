;;; d-emacs-bindlists.el --- d-emacs-bindlists for d-emacs  -*- lexical-binding: t; -*-
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

;; This file contains the keylists for maps of d-emacs. If daselt-d-emacs is t, it is parsed automatically when daselt-mode is started. Each element in this file should be either

;; 1. A LIST consisting of conses of whose car is a binding-location (see d--binding-location-p) binding-cars and values. This map list corresponds to  and should generally be at be at the top if it exists, though sometimes it makes sense to put it lower down if it references maps that are previously defined. When d-emacs-mode is activated and  is evaluated,  is backed up at d-emacs--mode-backup and the key combinations in this list are rebound to their corresponding values.

;; 2. A cons CONS1 whose car is a symbol SYMB whose cdr is a list as in 1. If SYMB is already bound when  is evaluated and d-emacs-mode is activated, then its value should be a keymap. This map is backed at d-emacs-SYMB-backup and then rebound as above. If SYMB is bound at that point, a new keymap is defined and bound to SYMB.

;; 3. A cons CONS2 whose car is a symbol or string and whose cdr is a list of conses like CONS1 as in 2. The first element of CONS2 denotes a file or feature that has to be evaluated before the keys in the bindlists in the cdr of CONS2 can be rebound.

;; Each of these options should be given using a backquote.

;;; Code:

;;;; d-emacs-mode-map
`(
;;;;; Strings
;;;;;; C-
  ("C-<return>" . #'d-emacs-insert-newline-forward) 
  ("C-RET" . #'d-emacs-insert-newline-forward)

;;;;; Coordinates 
;;;;;;; 4 
  ((4 -1 -1) . (if d-emacs-icicles #'beginning-of-line+ #'beginning-of-line)) 
  ((4 -1 1) . (if d-emacs-icicles #'end-of-line+ #'end-of-line)) 

;;;;;;;; 4-0 
  ((4 0 -3) . #'backward-up-list) 
  ((4 0 -2) . #'backward-list) 
  ((4 0 -1) . (if d-emacs-icicles #'beginning-of-visual-line+ #'beginning-of-visual-line)) 
  ((4 0 1) . (if d-emacs-icicles #'end-of-visual-line+ #'end-of-visual-line)) 
  ((4 0 2) . #'forward-list) 
  ((4 0 3) . #'up-list) 

;;;;;;; 7 
;;;;;;;; 7-0 
  ((7 0 -5) . (if d-lua-insert-normpairs #'d-lua-insert-normpair-yas-snippet)) 
  ((7 0 5) . (if d-lua-insert-normpairs #'d-lua-insert-othernormpair-yas-snippet)) 

;;;;;; C- 
;;;;;;; C-1 
;;;;;;;; C-1--1 
  (("C-" . (1 -1 -5)) . #'d-emacs-backward-transpose-lines) 
  (("C-" . (1 -1 -4)) . #'avy-goto-char-in-line) 
  (("C-" . (1 -1 -3)) . #'set-mark-command) 
  (("C-" . (1 -1 -2)) . (if d-emacs-undo-tree #'undo-tree-undo #'undo)) 
  (("C-" . (1 -1 -1)) . (if d-emacs-back-button #'back-button-local-backward #'pop-tag-mark)) 
  (("C-" . (1 -1 1)) . #'back-button-local-forward) 
  (("C-" . (1 -1 2)) . (if d-emacs-undo-tree #'undo-tree-redo #'redo)) 
  (("C-" . (1 -1 3)) . #'easy-mark) 
  (("C-" . (1 -1 4)) . #'avy-goto-word-1) 
  (("C-" . (1 -1 5)) . #'transpose-lines) 

;;;;;;;; C-1-0 
  (("C-" . (1 0 -5)) . (d-emacs-dynamic-binding "C-h")) 
  (("C-" . (1 0 -4)) . #'isearch-backward) 
  (("C-" . (1 0 -3)) unless d-stump #'d-emacs-C-1-0--3) 
  (("C-" . (1 0 -2)) unless d-stump #'d-emacs-C-1-0--2) 
  (("C-" . (1 0 -1)) unless d-stump #'d-emacs-C-1-0--1) 
  (("C-" . (1 0 1)) unless d-stump #'d-emacs-C-1-0-1) 
  (("C-" . (1 0 2)) unless d-stump #'d-emacs-C-1-0-2) 
  (("C-" . (1 0 3)) unless d-stump #'d-emacs-C-1-0-3) 
  (("C-" . (1 0 4)) . #'isearch-forward) 
  (("C-" . (1 0 5)) . #'eww) 

;;;;;;;; C-1-1 
  (("C-" . (1 1 -6)) . #'org-timer-start) 
  (("C-" . (1 1 -5)) . #'d-emacs-backward-kill-line) 
  (("C-" . (1 1 -4)) . #'avy-act-on-position-in-line) 
  (("C-" . (1 1 -3)) . #'easy-kill) 
  (("C-" . (1 1 0)) . #'universal-argument) 
  (("C-" . (1 1 2)) . #'execute-extended-command) 
  (("C-" . (1 1 3)) . #'d-emacs-yank-or-org-roam-node-insert) 
  (("C-" . (1 1 4)) . #'avy-act-on-position-word-1) 
  (("C-" . (1 1 5)) . #'kill-line) 
  (("C-" . (1 1 6)) . #'org-timer-pause-or-continue) 

;;;;;;; C-2 
;;;;;;;; C-2--1 
  (("C-" . (2 -1 -5)) . #'d-emacs-backward-transpose-words) 
  (("C-" . (2 -1 -4)) . #'eval-expression) 
  (("C-" . (2 -1 -3)) . #'backward-kill-word) 
  (("C-" . (2 -1 -2)) . #'bmkp-jump-map) 
  (("C-" . (2 -1 -1)) . #'avy-goto-line) 
  (("C-" . (2 -1 1)) . #'avy-goto-end-of-line) 
  (("C-" . (2 -1 2)) . #'bmkp-jump-other-window-map) 
  (("C-" . (2 -1 3)) . #'kill-word) 
  (("C-" . (2 -1 4)) . #'switch-to-buffer) 
  (("C-" . (2 -1 5)) . #'transpose-words) 

;;;;;;;; C-2-0 
  (("C-" . (2 0 -5)) . #'find-file) 
  (("C-" . (2 0 -4)) . #'isearch-backward-regexp) 
  (("C-" . (2 0 -3)) . #'backward-paragraph) 
  (("C-" . (2 0 -2)) . #'backward-word) 
  (("C-" . (2 0 -1)) . #'backward-sentence) 
  (("C-" . (2 0 1)) . #'forward-sentence) 
  (("C-" . (2 0 2)) . #'forward-word) 
  (("C-" . (2 0 3)) . #'forward-paragraph) 
  (("C-" . (2 0 4)) . #'isearch-forward-regexp) 
  (("C-" . (2 0 5)) . #'save-buffer) 

;;;;;;;; C-2-1 
  (("C-" . (2 1 -6)) . #'d-emacs-backward-transpose-subsentences) 
  (("C-" . (2 1 -5)) . #'d-emacs-backward-kill-subsentence) 
  (("C-" . (2 1 -4)) . #'avy-act-to-point-in-same-line) 
  (("C-" . (2 1 -3)) . #'completion-kill-region) 
  (("C-" . (2 1 -2)) . #'d-emacs-backward-subsentence) 
  (("C-" . (2 1 0)) . (d-emacs-dynamic-binding "C-c")) 
  (("C-" . (2 1 2)) . #'d-emacs-forward-subsentence) 
  (("C-" . (2 1 3)) . #'capitalize-dwim) 
  (("C-" . (2 1 4)) . #'avy-act-on-position) 
  (("C-" . (2 1 5)) . #'d-emacs-kill-subsentence) 
  (("C-" . (2 1 6)) . #'d-emacs-transpose-subsentences) 

;;;;;;; C-3 
;;;;;;;; C-3-0 
  (("C-" . (3 0 2)) . #'d-emacs-C-3-0-2) 

;;;;;;; C-4 
;;;;;;;; C-4--1 
  (("C-" . (4 -1 -5)) . #'d-emacs-backward-transpose-sexps) 
  (("C-" . (4 -1 -4)) . #'eval-last-sexp) 
  (("C-" . (4 -1 -3)) . #'sp-backward-kill-sexp) 
  (("C-" . (4 -1 -2)) . #'abbrev-prefix-mark) 
  (("C-" . (4 -1 -1)) . #'xref-go-back) 
  (("C-" . (4 -1 1)) . #'xref-go-forward) 
  (("C-" . (4 -1 2)) . #'embark-dwim) 
  (("C-" . (4 -1 3)) . #'sp-kill-sexp) 
  (("C-" . (4 -1 4)) . #'eval-defun) 
  (("C-" . (4 -1 5)) . #'transpose-sexps) 

;;;;;;;; C-4-0 
  (("C-" . (4 0 -4)) . (if d-emacs-smartparens #'d-emacs-backward-sp-down-sexp #'d-emacs-backward-down-list)) 
  (("C-" . (4 0 -3)) . #'d-emacs-backward-sp-up-sexp) 
  (("C-" . (4 0 -2)) . #'sp-backward-sexp) 
  (("C-" . (4 0 -1)) . #'beginning-of-defun) 
  (("C-" . (4 0 1)) . #'end-of-defun) 
  (("C-" . (4 0 2)) . #'sp-forward-sexp) 
  (("C-" . (4 0 3)) . #'sp-up-sexp) 
  (("C-" . (4 0 4)) . (if d-emacs-smartparens #'sp-down-sexp #'down-list)) 
  (("C-" . (4 0 5)) . #'ekg-show-notes-with-tag) 

;;;;;;;; C-4-1 
  (("C-" . (4 1 -6)) . #'previous-error) 
  (("C-" . (4 1 -5)) . #'d-emacs-backward-kill-defun) 
  (("C-" . (4 1 -4)) . #'avy-act-on-region-by-same-function) 
  (("C-" . (4 1 -3)) . #'d-emacs-kill-append) 
  (("C-" . (4 1 -2)) . (if d-emacs-avy-act #'avy-act-recenter-bottom-at-line #'d-emacs-recenter-bottom)) 
  (("C-" . (4 1 0)) . (if d-emacs-avy-act #'avy-act-recenter-middle-at-line #'recenter)) 
  (("C-" . (4 1 2)) . (if d-emacs-avy-act #'avy-act-recenter-top-at-line #'d-emacs-recenter-top)) 
  (("C-" . (4 1 4)) . #'avy-act-on-region) 
  (("C-" . (4 1 5)) . #'d-emacs-kill-defun) 
  (("C-" . (4 1 6)) . #'next-error) 

;;;;;;; C-5 
;;;;;;;; C-5--1 
  (("C-" . (5 -1 -5)) . #'d-emacs-backward-sp-transpose-sexp) 
  (("C-" . (5 -1 -4)) . #'sp-splice-sexp-killing-backward) 
  (("C-" . (5 -1 -3)) . #'d-emacs-backward-sp-kill-hybrid-sexp) 
  (("C-" . (5 -1 -2)) . #'citar-open) 
  (("C-" . (5 -1 -1)) . #'back-button-global-backward) 
  (("C-" . (5 -1 1)) . #'back-button-global-forward) 
  (("C-" . (5 -1 2)) . #'gt-do-translate) 
  (("C-" . (5 -1 3)) . #'sp-kill-hybrid-sexp) 
  (("C-" . (5 -1 4)) . #'sp-splice-sexp-killing-forward) 
  (("C-" . (5 -1 5)) . #'sp-transpose-sexp) 

;;;;;;;; C-5-0 
  (("C-" . (5 0 -2)) . #'sp-backward-slurp-sexp) 
  (("C-" . (5 0 -1)) . #'citar-insert-citation) 
  (("C-" . (5 0 1)) . #'citar-dwim) 
  (("C-" . (5 0 2)) . #'sp-forward-slurp-sexp) 
  (("C-" . (5 0 5)) . #'consult-org-roam-file-find) 

;;;;;;;; C-5-1 
  (("C-" . (5 1 -6)) . #'d-emacs-backward-sp-transpose-hybrid-sexp) 
  (("C-" . (5 1 -3)) . (if d-emacs-consult #'consult-register-store #'copy-to-register)) 
  (("C-" . (5 1 -2)) . #'sp-backward-barf-sexp) 
  (("C-" . (5 1 0)) . #'quoted-insert) 
  (("C-" . (5 1 2)) . #'sp-forward-barf-sexp) 
  (("C-" . (5 1 3)) . (if d-emacs-consult #'consult-register-load #'insert-register)) 
  (("C-" . (5 1 6)) . #'sp-transpose-hybrid-sexp) 

;;;;;;; C-6 
;;;;;;;; C-6--1 
  (("C-" . (6 -1 -5)) . #'d-emacs-backward-transpose-chars) 
  (("C-" . (6 -1 -4)) . #'outline-cycle-buffer) 
  (("C-" . (6 -1 -2)) . #'anchored-transpose) 
  (("C-" . (6 -1 2)) . #'anchored-transpose) 
  (("C-" . (6 -1 4)) . #'outline-cycle) 
  (("C-" . (6 -1 5)) . #'transpose-chars) 

;;;;;;;; C-6-0 
  (("C-" . (6 0 -5)) . #'d-emacs-toggle-variable) 
  (("C-" . (6 0 -4)) . #'outline-backward-same-level) 
  (("C-" . (6 0 -3)) . #'outline-up-heading) 
  (("C-" . (6 0 -2)) . #'outline-previous-visible-heading) 
  (("C-" . (6 0 -1)) . #'outline-promote) 
  (("C-" . (6 0 1)) . #'outline-demote) 
  (("C-" . (6 0 2)) . #'outline-next-visible-heading) 
  (("C-" . (6 0 3)) . #'d-emacs-outline-forward-up-heading) 
  (("C-" . (6 0 4)) . #'outline-forward-same-level) 
  (("C-" . (6 0 5)) . #'consult-outline) 

;;;;;;;; C-6-1 
  (("C-" . (6 1 -3)) . #'outline-headers-as-kill) 
  (("C-" . (6 1 -2)) . #'outline-previous-heading) 
  (("C-" . (6 1 2)) . #'outline-next-heading) 

;;;;;;; C-7 
;;;;;;;; C-7--1 
  (("C-" . (7 -1 -5)) . #'overwrite-mode) 
  (("C-" . (7 -1 -4)) . #'undelete-frame) 
  (("C-" . (7 -1 -3)) . #'make-frame-command) 
  (("C-" . (7 -1 -2)) . #'previous-frame) 
  (("C-" . (7 -1 -1)) . #'text-scale-decrease) 
  (("C-" . (7 -1 1)) . #'text-scale-increase) 
  (("C-" . (7 -1 2)) . #'other-frame) 
  (("C-" . (7 -1 3)) . #'delete-frame) 
  (("C-" . (7 -1 5)) . #'d-emacs-insert-and-return) 

;;;;;;;; C-7-0 
  (("C-" . (7 0 -4)) . #'tab-undo) 
  (("C-" . (7 0 -3)) . #'tab-new) 
  (("C-" . (7 0 -2)) . #'tab-previous) 
  (("C-" . (7 0 -1)) . #'tab-bar-history-back) 
  (("C-" . (7 0 1)) . #'tab-bar-history-forward) 
  (("C-" . (7 0 2)) . #'tab-next) 
  (("C-" . (7 0 3)) . #'tab-close) 
  (("C-" . (7 0 4)) . #'tab-switch) 
  (("C-" . (7 0 5)) . #'tabgo) 

;;;;;;;; C-7-1 
  (("C-" . (7 1 -5)) . #'org-roam-node-insert) 
  (("C-" . (7 1 -3)) . #'clone-buffer) 
  (("C-" . (7 1 -2)) . #'previous-buffer) 
  (("C-" . (7 1 0)) . #'d-emacs-exchange) 
  (("C-" . (7 1 2)) . #'next-buffer) 
  (("C-" . (7 1 3)) . #'kill-current-buffer) 

;;;;;; H- 
;;;;;;; H-2 
;;;;;;;; H-2-0 
  (("H-" . (2 0 -5)) . #'delete-other-windows) 
  (("H-" . (2 0 -4)) . #'split-window-below) 
  (("H-" . (2 0 -3)) . #'windmove-up) 
  (("H-" . (2 0 -2)) . #'windmove-left) 
  (("H-" . (2 0 -1)) . #'beginning-of-buffer) 
  (("H-" . (2 0 1)) . #'end-of-buffer) 
  (("H-" . (2 0 2)) . #'windmove-right) 
  (("H-" . (2 0 3)) . #'windmove-down) 
  (("H-" . (2 0 4)) . #'split-window-right) 
  (("H-" . (2 0 5)) . #'delete-window) 

;;;;;;;; H-2-1 
  (("H-" . (2 1 -3)) . #'icicle-switch-to/from-minibuffer) 
  (("H-" . (2 1 3)) . #'icicle-switch-to/from-minibuffer) 

;;;;;;;; H-4-0
  (("H-" . (4 -1 -4)) . #'swiper-all)
  (("H-" . (4 0 -4)) . #'swiper-backward)
  (("H-" . (4 0 4)) . #'swiper)
  (("H-" . (4 -1 4)) . #'swiper-avy)
  
;;;;;;;; H-4-0
  (("H-" . (4 0 -4)) . #'swiper-all-thing-at-point)
  (("H-" . (4 0 4)) . #'swiper-thing-at-point)

;;;;;;; H-8 
;;;;;;;; H-8-0 
  (("H-" . (8 0 -3)) . #'buf-move-up) 
  (("H-" . (8 0 -2)) . #'buf-move-left) 
  (("H-" . (8 0 -1)) . #'winner-undo) 
  (("H-" . (8 0 1)) . #'winner-redo) 
  (("H-" . (8 0 2)) . #'buf-move-right) 
  (("H-" . (8 0 3)) . #'buf-move-down) 

;;;;;; M- 
;;;;;;; M-0 
;;;;;;;; M-0-0 
  (("M-" . (0 0 6)) . #'rectangle-mark-mode) 

;;;;;;; M-1 
;;;;;;;; M-1--1 
  (("M-" . (1 -1 -5)) . #'d-emacs-mode) 
  (("M-" . (1 -1 -4)) . #'eval-region) 
  (("M-" . (1 -1 -3)) . #'delete-minibuffer-contents) 
  (("M-" . (1 -1 -2)) . #'exchange-point-and-mark) 
  (("M-" . (1 -1 -1)) . #'d-emacs-set-test-fun) 
  (("M-" . (1 -1 1)) . #'d-emacs-test) 
  (("M-" . (1 -1 2)) . #'undo-tree-visualize) 
  (("M-" . (1 -1 3)) . #'mark-whole-buffer) 
  (("M-" . (1 -1 4)) . #'recentf) 
  (("M-" . (1 -1 5)) . #'flyspell-correct-at-point) 

;;;;;;;; M-1-0 
  (("M-" . (1 0 -5)) . #'dired-jump) 
  (("M-" . (1 0 -4)) . #'d-emacs-replace-string-backward) 
  (("M-" . (1 0 -3)) . #'embark-bindings) 
  (("M-" . (1 0 -2)) . #'embark-act) 
  (("M-" . (1 0 -1)) . d-emacs-miscellaneous-map) 
  (("M-" . (1 0 1)) . #'save-buffers-kill-emacs) 
  (("M-" . (1 0 3)) . #'windresize) 
  (("M-" . (1 0 4)) . #'replace-string) 
  (("M-" . (1 0 5)) . #'org-brain-visualize) 

;;;;;;;; M-1-1 
  (("M-" . (1 1 -5)) . #'d-emacs-backward-mark-line) 
  (("M-" . (1 1 -3)) . #'avy-kill-ring-save-region) 
  (("M-" . (1 1 -2)) . #'ekg-capture) 
  (("M-" . (1 1 0)) . (d-emacs-dynamic-binding "C-x")) 
  (("M-" . (1 1 2)) . #'org-roam-capture) 
  (("M-" . (1 1 3)) . (if d-emacs-consult #'consult-yank-pop #'yank-pop)) 
  (("M-" . (1 1 4)) . #'icicle-switch-to-completions) 
  (("M-" . (1 1 5)) . #'what-cursor-position) 

;;;;;;; M-2 
;;;;;;;; M-2--1 
  (("M-" . (2 -1 -5)) . #'projectile-find-implementation-or-test) 
  (("M-" . (2 -1 -4)) . #'eval-expression) 
  (("M-" . (2 -1 -3)) . #'mark-word) 
  (("M-" . (2 -1 -2)) . #'bookmark-set)
  (("M-" . (2 -1 -1)) . #'goto-char)
  (("M-" . (2 -1 1)) . #'goto-line)
  (("M-" . (2 -1 2)) . #'list-bookmarks) 
  (("M-" . (2 -1 3)) . #'mark-paragraph) 
  (("M-" . (2 -1 4)) . #'list-registers) 

;;;;;;;; M-2-0 
  (("M-" . (2 0 -5)) . #'projectile-dired) 
  (("M-" . (2 0 -4)) . #'d-emacs-replace-regexp-backward) 
  (("M-" . (2 0 -1)) . #'transpose-regions) 
  (("M-" . (2 0 4)) . #'replace-regexp) 

;;;;;;;; M-2-1 
  (("M-" . (2 1 -5)) . #'d-emacs-mark-subsentence) 
  (("M-" . (2 1 -2)) . #'embark-act-all) 
  (("M-" . (2 1 2)) . #'embark-dwim) 
  (("M-" . (2 1 5)) . #'er/mark-sentence) 

;;;;;;; M-4 
;;;;;;;; M-4--1 
  (("M-" . (4 -1 -3)) . (if d-emacs-smartparens #'d-emacs-backward-sp-mark-sexp #'d-emacs-backward-mark-sexp)) 
  (("M-" . (4 -1 2)) . #'xref-find-references) 
  (("M-" . (4 -1 3)) . (if d-emacs-smartparens #'sp-mark-sexp #'mark-sexp)) 
  (("M-" . (4 -1 5)) . #'embark-live) 

;;;;;;;; M-4-0 
  (("M-" . (4 0 -5)) . #'ekg-show-notes-with-all-tags) 
  (("M-" . (4 0 -3)) . #'sp-wrap-round) 
  (("M-" . (4 0 -2)) . #'sp-backward-unwrap-sexp) 
  (("M-" . (4 0 -1)) . #'sp-split-sexp) 
  (("M-" . (4 0 1)) . #'sp-join-sexp) 
  (("M-" . (4 0 2)) . #'sp-unwrap-sexp) 
  (("M-" . (4 0 3)) . #'sp-wrap-curly) 
  (("M-" . (4 0 4)) . #'sp-wrap-square) 
  (("M-" . (4 0 5)) . #'ekg-show-notes-with-any-tags) 

;;;;;;;; M-4-1 
  (("M-" . (4 1 -5)) . #'d-emacs-backward-mark-defun) 
  (("M-" . (4 1 -4)) . #'ekg-show-notes-in-trash) 
  (("M-" . (4 1 -2)) . #'d-emacs-move-to-top) 
  (("M-" . (4 1 0)) . #'move-to-window-line) 
  (("M-" . (4 1 2)) . #'d-emacs-move-to-bottom) 
  (("M-" . (4 1 4)) . #'ekg-show-notes-in-drafts) 
  (("M-" . (4 1 5)) . #'mark-defun) 

;;;;;;; M-5 
;;;;;;;; M-5--1 
  (("M-" . (5 -1 -5)) . #'password-store-insert) 
  (("M-" . (5 -1 3)) . #'avy-kill-region) 
  (("M-" . (5 -1 5)) . #'password-store-copy) 

;;;;;;;; M-5-0 
  (("M-" . (5 0 -5)) . #'icicle-describe-option-of-type) 
  (("M-" . (5 0 -4)) . #'projectile-multi-occur) 
  (("M-" . (5 0 -4)) . #'gptel-extensions-refactor) 
  (("M-" . (5 0 -3)) . #'gptel-menu) 
  (("M-" . (5 0 -2)) . #'gptel-set-topic) 
  (("M-" . (5 0 -1)) . #'gptel-extensions-ask-document) 
  (("M-" . (5 0 1)) . #'gptel-extensions-rewrite-and-replace) 
  (("M-" . (5 0 2)) . #'copilot-complete) 
  (("M-" . (5 0 3)) . #'gptel) 

;;;;;;;; M-5-1 
  (("M-" . (5 1 0)) . #'sp-convolute-sexp) 
  (("M-" . (5 1 3)) . #'avy-resume) 

;;;;;;; M-6 
;;;;;;;; M-6--1 
  (("M-" . (6 -1 -5)) . #'outline-apply-default-state) 
  (("M-" . (6 -1 -4)) . #'outline-hide-leaves) 
  (("M-" . (6 -1 -3)) . #'outline-mark-subtree) 
  (("M-" . (6 -1 -2)) . #'outline-hide-other) 
  (("M-" . (6 -1 2)) . #'outline-show-only-headings) 
  (("M-" . (6 -1 3)) . #'outline-insert-heading) 
  (("M-" . (6 -1 4)) . #'outline-show-branches) 
  (("M-" . (6 -1 5)) . #'outline-show-children) 

;;;;;;;; M-6-0 
  (("M-" . (6 0 -5)) . #'outline-hide-sublevels) 
  (("M-" . (6 0 -4)) . #'outline-hide-by-heading-regexp) 
  (("M-" . (6 0 -4)) . #'d-emacs-do-not-search-invisible) 
  (("M-" . (6 0 -3)) . #'outline-hide-subtree) 
  (("M-" . (6 0 -2)) . #'outline-hide-entry) 
  (("M-" . (6 0 -1)) . #'outline-hide-body) 
  (("M-" . (6 0 1)) . #'outline-show-all) 
  (("M-" . (6 0 2)) . #'outline-show-entry) 
  (("M-" . (6 0 3)) . #'outline-show-subtree) 
  (("M-" . (6 0 4)) . #'outline-show-by-heading-regexp) 
  (("M-" . (6 0 4)) . #'d-emacs-search-invisible) 
  (("M-" . (6 0 5)) . #'outlineify-sticky) 

;;;;;;;; M-6-1 
  (("M-" . (6 1 -2)) . #'allout-open-supertopic) 
  (("M-" . (6 1 0)) . #'allout-open-sibtopic) 
  (("M-" . (6 1 2)) . #'allout-open-subtopic) 

;;;;;;; M-7 
;;;;;;;; M-7--1 
  (("M-" . (7 -1 -3)) . #'emms-shuffle) 
  (("M-" . (7 -1 -2)) . #'emms-add-url) 
  (("M-" . (7 -1 2)) . #'emms-play-url) 
  (("M-" . (7 -1 3)) . #'emms-sort) 
  (("M-" . (7 -1 4)) . #'emms-show) 

;;;;;;;; M-7-0 
  (("M-" . (7 0 -3)) . #'upcase-region) 
  (("M-" . (7 0 3)) . #'downcase-region) 

;;;;;;;; M-7-1 
  (("M-" . (7 1 -4)) . #'emms-play-find) 
  (("M-" . (7 1 -3)) . #'emms-play-playlist) 
  (("M-" . (7 1 -2)) . #'emms-play-directory) 
  (("M-" . (7 1 -1)) . #'emms-play-directory-tree) 
  (("M-" . (7 1 1)) . #'emms-add-directory-tree) 
  (("M-" . (7 1 2)) . #'emms-add-directory) 
  (("M-" . (7 1 3)) . #'emms-add-playlist) 
  (("M-" . (7 1 4)) . #'emms-add-find) 

;;;;;; M-C- 
;;;;;;; M-C-0 
;;;;;;;; M-C-0-2 
  (("M-C-" . (0 2 1)) . #'d-emacs-MetaSuper-next-cmd) 

;;;;;;; M-C-1 
;;;;;;;; M-C-1--1 
  (("M-C-" . (1 -1 -5)) . #'d-emacs-mode) 
  (("M-C-" . (1 -1 -4)) . #'embark-eval-replace) 
  (("M-C-" . (1 -1 -2)) . #'fill-region) 
  (("M-C-" . (1 -1 2)) . #'count-words-region) 

;;;;;;;; M-C-1-0 
  (("M-C-" . (1 0 -4)) . #'d-emacs-query-replace-string-backward) 
  (("M-C-" . (1 0 3)) . #'rename-file-and-buffer) 
  (("M-C-" . (1 0 4)) . #'query-replace) 
  (("M-C-" . (1 0 5)) . #'eww-search-words) 

;;;;;;;; M-C-1-1 
  (("M-C-" . (1 1 -6)) . #'org-timer-stop) 
  (("M-C-" . (1 1 -2)) . #'execute-extended-command-for-buffer) 
  (("M-C-" . (1 1 2)) . (if d-emacs-org-roam #'org-roam-capture #'org-capture)) 
  (("M-C-" . (1 1 3)) . #'avy-copy-line) 
  (("M-C-" . (1 1 4)) . #'count-lines) 

;;;;;;; M-C-2 
;;;;;;;; M-C-2--1 
  (("M-C-" . (2 -1 -5)) . #'projectile-find-implementation-or-test-other-window) 
  (("M-C-" . (2 -1 2)) . #'count-matches) 

;;;;;;;; M-C-2-0 
  (("M-C-" . (2 0 -5)) . #'projectile-dired-other-window) 
  (("M-C-" . (2 0 -4)) . #'d-emacs-query-replace-regexp-backward) 
  (("M-C-" . (2 0 4)) . #'query-replace-regexp) 
  (("M-C-" . (2 0 5)) . #'embark-export) 

;;;;;;; M-C-4 
;;;;;;;; M-C-4--1 
  (("M-C-" . (4 -1 -5)) . #'ekg-show-notes-with-tag-prefix) 
  (("M-C-" . (4 -1 3)) . #'undollar) 
  (("M-C-" . (4 -1 4)) . #'eval-buffer) 
  (("M-C-" . (4 -1 5)) . #'ekg-show-notes-for-today) 

;;;;;;;; M-C-4-0 
  (("M-C-" . (4 0 -5)) . #'ekg-capture-url) 
  (("M-C-" . (4 0 -4)) . #'projectile-ripgrep) 
  (("M-C-" . (4 0 4)) . #'ripgrep-regexp) 
  (("M-C-" . (4 0 5)) . #'ekg-browse-url) 

;;;;;;;; M-C-4-1 
  (("M-C-" . (4 1 -2)) . #'projectile-run-shell-command-in-root) 
  (("M-C-" . (4 1 2)) . #'projectile-run-async-shell-command-in-root) 

;;;;;;; M-C-5 
;;;;;;;; M-C-5--1 
  (("M-C-" . (5 -1 -3)) . #'citar-copy-reference) 

;;;;;;;; M-C-5-0 
  (("M-C-" . (5 0 -5)) . #'projectile-find-implementation-or-test) 
  (("M-C-" . (5 0 -1)) . #'citar-insert-bibtex) 
  (("M-C-" . (5 0 1)) . #'citar-insert-edit) 

;;;;;;;; M-C-5-1 
  (("M-C-" . (5 1 -1)) . #'citar-add-file-to-library) 
  (("M-C-" . (5 1 1)) . #'citar-create-note) 

;;;;;;; M-C-6 
;;;;;;;; M-C-6--1 
  (("M-C-" . (6 -1 -5)) . #'password-store-generate) 
  (("M-C-" . (6 -1 -5)) . #'password-store-edit) 
  (("M-C-" . (6 -1 -4)) . #'undo-tree-save-state-to-register) 
  (("M-C-" . (6 -1 -2)) . #'embark-select) 
  (("M-C-" . (6 -1 2)) . #'append-to-register) 
  (("M-C-" . (6 -1 3)) . #'avy-kill-whole-line) 
  (("M-C-" . (6 -1 4)) . #'undo-tree-restore-state-from-register) 

;;;;;;;; M-C-6-0 
  (("M-C-" . (6 0 -5)) . #'embark-become) 
  (("M-C-" . (6 0 5)) . #'embark-collect) 

;;;;;;;; M-C-6-1 
  (("M-C-" . (6 1 -3)) . #'password-store-copy-field) 

;;;;;;; M-C-7 
;;;;;;;; M-C-7-1 
  (("M-C-" . (7 1 -3)) . #'projectile-kill-buffers) 
  (("M-C-" . (7 1 2)) . #'projectile-switch-to-buffer-other-window) 

;;;;;; M-H- 
;;;;;;; M-H-2 
;;;;;;;; M-H-2--1 
  (("M-H-" . (2 -1 -5)) . #'projectile-find-implementation-or-test-other-frame) 

;;;;;;;; M-H-2-0 
  (("M-H-" . (2 0 -5)) . #'projectile-dired-other-frame) 

;;;;;; s- 
;;;;;;; s-0 
;;;;;;;; s-0-2 
  (("s-" . (0 2 0)) . #'d-emacs-insert-space-forward) 
  (("s-" . (0 2 0)) . #'d-emacs-insert-space-forward) 
  (("s-" . (0 2 1)) . #'d-emacs-MetaSuper-next-cmd) 

;;;;;;; s-1 
;;;;;;;; s-1--1 
  (("s-" . (1 -1 -5)) . #'read-only-mode) 
  (("s-" . (1 -1 -4)) . #'find-alternate-file) 
  (("s-" . (1 -1 -3)) . #'primary-to-secondary) 
  (("s-" . (1 -1 -2)) . #'mail) 
  (("s-" . (1 -1 1)) . #'flyspell-correct-buffer) 
  (("s-" . (1 -1 2)) . #'customize) 
  (("s-" . (1 -1 3)) . #'secondary-to-primary) 
  (("s-" . (1 -1 4)) . #'icicle-imenu) 
  (("s-" . (1 -1 5)) . #'dedicated-mode) 

;;;;;;;; s-1-0 
  (("s-" . (1 0 -5)) . #'icicle-read+insert-file-name) 
  (("s-" . (1 0 -4)) . #'d-emacs-replace-string-throughout-buffer) 
  (("s-" . (1 0 -3)) . #'kmacro-name-last-macro) 
  (("s-" . (1 0 -2)) . #'kmacro-start-macro-or-insert-counter) 
  (("s-" . (1 0 2)) . #'kmacro-end-or-call-macro) 
  (("s-" . (1 0 3)) . #'insert-kbd-macro) 
  (("s-" . (1 0 4)) . #'icicle-search-generic) 
  (("s-" . (1 0 5)) . #'desktop-save) 

;;;;;;;; s-1-1 
  (("s-" . (1 1 -5)) . #'kill-whole-line) 
  (("s-" . (1 1 -3)) . #'secondary-save-then-kill) 
  (("s-" . (1 1 0)) . #'secondary-swap-region) 
  (("s-" . (1 1 2)) . #'lacarte-execute-command) 
  (("s-" . (1 1 3)) . #'yank-secondary) 
  (("s-" . (1 1 4)) . #'comment-line) 
  (("s-" . (1 1 5)) . #'delete-blank-lines) 

;;;;;;; s-2 
;;;;;;;; s-2--1 
  (("s-" . (2 -1 -5)) . #'d-emacs-backward-transpose-paragraphs) 
  (("s-" . (2 -1 -4)) . #'org-agenda-file-to-front) 
  (("s-" . (2 -1 -3)) . #'d-emacs-backward-kill-paragraph) 
  (("s-" . (2 -1 -1)) . #'indent-region) 
  (("s-" . (2 -1 1)) . #'indent-sexp) 
  (("s-" . (2 -1 2)) . #'edit-bookmarks) 
  (("s-" . (2 -1 3)) . #'kill-paragraph) 
  (("s-" . (2 -1 4)) . #'org-remove-file) 
  (("s-" . (2 -1 5)) . #'transpose-paragraphs) 

;;;;;;;; s-2-0 
  (("s-" . (2 0 -5)) . #'projectile-find-file) 
  (("s-" . (2 0 -4)) . #'icicle-occur) 
  (("s-" . (2 0 4)) . #'icicle-search-keywords) 
  (("s-" . (2 0 5)) . #'projectile-find-related-file) 

;;;;;;;; s-2-1 
  (("s-" . (2 1 -6)) . #'d-emacs-backward-transpose-sentences) 
  (("s-" . (2 1 -5)) . #'backward-kill-sentence) 
  (("s-" . (2 1 0)) . d-emacs-theme-map) 
  (("s-" . (2 1 2)) . #'icicle-command-abbrev) 
  (("s-" . (2 1 5)) . #'kill-sentence) 
  (("s-" . (2 1 6)) . #'transpose-sentences) 

;;;;;;; s-4 
;;;;;;;; s-4--1 
  (("s-" . (4 -1 -3)) . #'just-one-space) 
  (("s-" . (4 -1 -2)) . #'xref-find-references) 
  (("s-" . (4 -1 2)) . #'xref-find-definitions) 
  (("s-" . (4 -1 3)) . #'delete-trailing-whitespace) 
  (("s-" . (4 -1 4)) . #'list-abbrevs) 

;;;;;;;; s-4-0 
  (("s-" . (4 0 -5)) . #'icicle-dired-saved-file-candidates) 
  (("s-" . (4 0 -4)) . #'d-emacs-isearch-backward-symbol) 
  (("s-" . (4 0 -3)) . #'add-global-abbrev) 
  (("s-" . (4 0 -2)) . #'add-mode-abbrev) 
  (("s-" . (4 0 2)) . #'inverse-add-mode-abbrev) 
  (("s-" . (4 0 3)) . #'inverse-add-global-abbrev) 
  (("s-" . (4 0 4)) . #'isearch-forward-symbol) 
  (("s-" . (4 0 5)) . #'abbrev-edit-save-buffer) 

;;;;;;;; s-4-1 
  (("s-" . (4 1 -5)) . #'d-emacs-backward-delete-all-space) 
  (("s-" . (4 1 -4)) . #'debug-on-entry) 
  (("s-" . (4 1 -3)) . #'append-next-kill) 
  (("s-" . (4 1 4)) . #'cancel-debug-on-entry) 
  (("s-" . (4 1 5)) . #'delete-all-space) 

;;;;;;; s-5 
;;;;;;;; s-5--1 
  (("s-" . (5 -1 -5)) . #'aya-clear-history) 
  (("s-" . (5 -1 -3)) . #'aya-delete-from-history) 
  (("s-" . (5 -1 2)) . #'aya-persist-snippet) 
  (("s-" . (5 -1 3)) . #'aya-yank-snippet) 

;;;;;;;; s-5-0 
  (("s-" . (5 0 -4)) . #'d-emacs-isearch-backward-symbol) 
  (("s-" . (5 0 -3)) . #'aya-previous-in-history) 
  (("s-" . (5 0 -2)) . #'aya-create) 
  (("s-" . (5 0 2)) . #'aya-expand) 
  (("s-" . (5 0 3)) . #'aya-next-in-history) 
  (("s-" . (5 0 4)) . #'isearch-forward-symbol) 

;;;;;;; s-7 
;;;;;;;; s-7--1 
  (("s-" . (7 -1 -1)) . #'variable-text-scale-decrease) 
  (("s-" . (7 -1 1)) . #'variable-text-scale-increase) 
  (("s-" . (7 -1 2)) . #'icicle-select-frame) 

;;;;;;;; s-7-0 
  (("s-" . (7 0 -3)) . #'flycheck-previous-error) 
  (("s-" . (7 0 -2)) . #'flycheck-clear) 
  (("s-" . (7 0 -1)) . #'flycheck-first-error) 
  (("s-" . (7 0 1)) . #'flycheck-list-errors) 
  (("s-" . (7 0 2)) . #'flycheck-display-error-at-point) 
  (("s-" . (7 0 3)) . #'flycheck-next-error) 

;;;;;;;; s-7-1 
  (("s-" . (7 1 -3)) . #'flycheck-copy-errors-as-kill) 
  (("s-" . (7 1 -2)) . #'projectile-previous-project-buffer) 
  (("s-" . (7 1 0)) . #'d-emacs-ireplace-listwise) 
  (("s-" . (7 1 2)) . #'projectile-next-project-buffer) 

;;;;;; s-C- 
;;;;;;; s-C-1 
;;;;;;;; s-C-1--1 
  (("s-C-" . (1 -1 -5)) . #'d-emacs-red-emacs) 
  (("s-C-" . (1 -1 -4)) . #'d-emacs-replace-string-throughout-buffer) 
  (("s-C-" . (1 -1 -3)) . #'d-emacs-backward-delete-char) 
  (("s-C-" . (1 -1 -2)) . #'fill-region-as-paragraph) 
  (("s-C-" . (1 -1 -1)) . #'d-emacs-bind-test-key-to-eval) 
  (("s-C-" . (1 -1 3)) . #'delete-char) 

;;;;;;;; s-C-1-0 
  (("s-C-" . (1 0 -5)) . #'ekg-capture-file) 
  (("s-C-" . (1 0 -4)) . #'avy-isearch) 
  (("s-C-" . (1 0 -3)) . #'d-emacs-open-file-in-new-emacs-no-init) 
  (("s-C-" . (1 0 2)) . #'icicle-kmacro) 
  (("s-C-" . (1 0 3)) . #'d-emacs-open-file-in-new-emacs) 
  (("s-C-" . (1 0 4)) . #'icicle-search-word) 

;;;;;;;; s-C-1-1 
  (("s-C-" . (1 1 -4)) . #'d-find-bindlists-file) 
  (("s-C-" . (1 1 2)) . #'lacarte-execute-menu-command) 
  (("s-C-" . (1 1 4)) . #'d-find-pkg-file-by-type) 

;;;;;;; s-C-2 
;;;;;;;; s-C-2--1 
  (("s-C-" . (2 -1 4)) . #'org-switchb) 

;;;;;;;; s-C-2-0 
  (("s-C-" . (2 0 -5)) . #'projectile-find-related-file-other-window) 
  (("s-C-" . (2 0 -4)) . #'d-emacs-replace-regexp-throughout-buffer) 
  (("s-C-" . (2 0 4)) . #'icicle-search-text-property) 
  (("s-C-" . (2 0 5)) . #'projectile-save-known-projects) 

;;;;;;;; s-C-2-1 
  (("s-C-" . (2 1 -4)) . #'avy-act-to-point) 
  (("s-C-" . (2 1 -3)) . #'projectile-test-project) 
  (("s-C-" . (2 1 0)) . #'projectile-compile-project) 
  (("s-C-" . (2 1 3)) . #'projectile-invalidate-cache) 
  (("s-C-" . (2 1 4)) . #'citar-open) 

;;;;;;; s-C-4 
;;;;;;;; s-C-4--1 
  (("s-C-" . (4 -1 4)) . #'projectile-find-related-file-other-frame) 
  (("s-C-" . (4 -1 5)) . #'projectile-find-file-dwim-other-frame) 

;;;;;;;; s-C-4-0 
  (("s-C-" . (4 0 -5)) . #'icicle-dired-saved-file-candidates-other-window) 
  (("s-C-" . (4 0 -4)) . #'d-emacs-isearch-backward-symbol-at-point) 
  (("s-C-" . (4 0 -2)) . #'projectile-find-references) 
  (("s-C-" . (4 0 4)) . #'isearch-forward-symbol-at-point) 

;;;;;;;; s-C-4-1 
  (("s-C-" . (4 1 -5)) . #'d-emacs-backward-delete-horizontal-space) 
  (("s-C-" . (4 1 5)) . #'delete-horizontal-space) 

;;;;;;; s-C-5 
;;;;;;;; s-C-5--1 
  (("s-C-" . (5 -1 -4)) . #'point-to-register) 
  (("s-C-" . (5 -1 -3)) . #'secondary-to-register) 
  (("s-C-" . (5 -1 3)) . #'aya-yank-snippet-from-history) 
  (("s-C-" . (5 -1 4)) . #'jump-to-register) 

;;;;;;;; s-C-5-0 
  (("s-C-" . (5 0 -4)) . #'d-emacs-isearch-backward-thing-at-point) 
  (("s-C-" . (5 0 -2)) . #'aya-create-one-line) 
  (("s-C-" . (5 0 2)) . #'aya-expand-from-history) 
  (("s-C-" . (5 0 4)) . #'isearch-forward-thing-at-point) 

;;;;;;;; s-C-5-1 
  (("s-C-" . (5 1 -3)) . #'window-configuration-to-register) 

;;;;;;; s-C-6 
;;;;;;;; s-C-6-0 
  (("s-C-" . (6 0 -1)) . #'outline-move-subtree-up) 
  (("s-C-" . (6 0 1)) . #'outline-move-subtree-down) 

;;;;;;; s-C-7 
;;;;;;;; s-C-7--1 
  (("s-C-" . (7 -1 -1)) . #'default-text-scale-decrease) 
  (("s-C-" . (7 -1 1)) . #'default-text-scale-increase) 

;;;;;;;; s-C-7-1 
  (("s-C-" . (7 1 -3)) . #'save-some-buffers) 
  (("s-C-" . (7 1 0)) . #'projectile-switch-to-buffer-other-frame) 
  (("s-C-" . (7 1 3)) . #'kill-some-buffers) 

;;;;;; s-H- 
;;;;;;; s-H-2 
;;;;;;;; s-H-2-0 
  (("s-H-" . (2 0 -5)) . #'projectile-find-file-other-frame)
  (("s-H-" . (2 0 5)) . #'projectile-find-related-file-other-frame) 

;;;;;;;; s-H-4-0 
  (("s-H-" . (4 0 -4)) . #'swiper-isearch-backward)
  (("s-H-" . (4 0 4)) . #'swiper-isearch)

;;;;;; s-M- 
;;;;;;; s-M-1 
;;;;;;;; s-M-1-0 
  (("s-M-" . (1 0 1)) . #'kill-buffer) 

;;;;;;; s-M-2 
;;;;;;;; s-M-2-0 
  (("s-M-" . (2 0 4)) . #'projectile-replace-regexp))

;;;; d-emacs-miscellaneous-map
`(d-emacs-miscellaneous-map 
;;;;; Strings
 ("5" . #'projectile-run-eshell) 
 ("C" . #'checkdoc) 
 ("F" . #'flycheck-mode) 
 ("G" . #'projectile-run-gdb) 
 ("L" . #'package-lint-current-buffer) 
 ("M" . #'d-emacs-toggle-mode-line) 
 ("P" . #'projectile-run-vterm) 
 ("S" . #'eshell) 
 ("[" . #'scroll-lock-mode) 
 ("\\" . #'projectile-run-term) 
 ("—" . #'projectile-run-shell) 
 ("c" . #'cdlatex-mode) 
 ("d" . #'display-line-numbers-mode) 
 ("f" . #'flyspell-mode) 
 ("g" . #'gnus) 
 ("i" . #'icicle-mode) 
 ("l" . #'list-packages) 
 ("m" . #'magit) 
 ("s" . #'shell) 
 ("t" . #'tab-bar-mode) 
 ("u" . #'undo-tree-mode) 
 ("v" . #'vterm))

;;;; d-emacs-theme-map
`(d-emacs-theme-map 
;;;;; Coordinates
;;;;;;; 1
;;;;;;;; 1--1
 ((1 -1 4) . #'modus-themes-select) 

;;;;;;;; 1-0 
 ((1 0 -2) . #'abyss-theme) 
 ((1 0 1) . #'color-theme-sanityinc-tomorrow-day) 
 ((1 0 5) . #'color-theme-sanityinc-tomorrow-bright))

;;; d-emacs-bindlists.el ends here
