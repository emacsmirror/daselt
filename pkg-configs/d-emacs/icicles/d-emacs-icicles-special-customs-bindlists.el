;;; d-emacs-icicles-special-customs-bindlists.el --- d-emacs-bindlists for icicles  -*- lexical-binding: t; -*-

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

;; This file contains the keylists for maps of icicles. 'If daselt-icicles is t, it is parsed automatically when daselt-mode is started. 'Each element in this file should be either

;; 1. 'A LIST consisting of conses of whose car is a binding-location (see d--binding-location-p) binding-cars and values. 'This map list corresponds to  and should generally be at be at the top if it exists, though sometimes it makes sense to put it lower down if it references maps that are previously defined. 'When d-emacs-mode is activated and  is evaluated,  is backed up at d-emacs--mode-backup and the key combinations in this list are rebound to their corresponding values.

;; 2. 'A cons CONS1 whose car is a symbol SYMB whose cdr is a list as in 1. 'If SYMB is already bound when  is evaluated and d-emacs-mode is activated, then its value should be a keymap. 'This map is backed at d-emacs-SYMB-backup and then rebound as above. 'If SYMB is bound at that point, a new keymap is defined and bound to SYMB.

;; 3. 'A cons CONS2 whose car is a symbol or string and whose cdr is a list of conses like CONS1 as in 2. 'The first element of CONS2 denotes a file or feature that has to be evaluated before the keys in the bindlists in the cdr of CONS2 can be rebound.

;; Each of these options should be given using a backquote.

;;; Code:

;;;; icicles-mode-map
`(
;;;;; Coordinates
;;;;;; C-
;;;;;;; C-3
;;;;;;;; C-3--1
  (("C-" . (3 -1 -4)) . 'icicle-search-from-isearch-keys) 
  (("C-" . (3 -1 -3)) . 'icicle-apropos-cycle-previous-keys) 
  (("C-" . (3 -1 -2)) . 'icicle-apropos-complete-keys) 
  (("C-" . (3 -1 3)) . 'icicle-apropos-cycle-next-keys) 

;;;;;;;; C-3-0 
  (("C-" . (3 0 -3)) . 'icicle-modal-cycle-up-keys) 
  (("C-" . (3 0 3)) . 'icicle-modal-cycle-down-keys) 

;;;;;;;; C-3-1 
  (("C-" . (3 1 -4)) . 'icicle-isearch-history-insert-keys) 
  (("C-" . (3 1 -3)) . 'icicle-prefix-cycle-previous-keys) 
  (("C-" . (3 1 -2)) . 'icicle-prefix-complete-keys) 
  (("C-" . (3 1 3)) . 'icicle-prefix-cycle-next-keys) 
  (("C-" . (3 1 4)) . 'icicle-isearch-complete-keys) 

;;;;;; s- 
;;;;;;; s-3 
;;;;;;;; s-3--1 
(("s-" . (3 -1 -3)) . 'icicle-apropos-cycle-previous-action-keys) 
(("s-" . (3 -1 3)) . 'icicle-apropos-cycle-next-action-keys) 

;;;;;;;; s-3-0 
(("s-" . (3 0 -3)) . 'icicle-modal-cycle-up-action-keys) 
(("s-" . (3 0 2)) . 'icicle-candidate-action-keys) 
(("s-" . (3 0 3)) . 'icicle-modal-cycle-down-action-keys) 

;;;;;;;; s-3-1 
(("s-" . (3 1 -3)) . 'icicle-prefix-cycle-previous-action-keys) 
(("s-" . (3 1 3)) . 'icicle-prefix-cycle-next-action-keys) 

;;;;;; s-C- 
;;;;;;; s-C-3 
;;;;;;;; s-C-3--1 
  (("s-C-" . (3 -1 -3)) . 'icicle-apropos-cycle-previous-help-keys) 
  (("s-C-" . (3 -1 -2)) . 'icicle-apropos-complete-no-display-keys) 
  (("s-C-" . (3 -1 3)) . 'icicle-apropos-cycle-previous-help-keys) 

;;;;;;;; s-C-3-0 
  (("s-C-" . (3 0 -5)) . 'icicle-candidate-help-keys) 
  (("s-C-" . (3 0 -3)) . 'icicle-modal-cycle-up-help-keys) 
  (("s-C-" . (3 0 3)) . 'icicle-modal-cycle-down-help-keys) 

;;;;;;;; s-C-3-1 
  (("s-C-" . (3 1 -3)) . 'icicle-prefix-cycle-previous-help-keys) 
  (("s-C-" . (3 1 -2)) . 'icicle-prefix-complete-no-display-keys) 
  (("s-C-" . (3 1 3)) . 'icicle-prefix-cycle-next-help-keys) 

;;;;;; s-H- 
;;;;;;; s-H-3 
;;;;;;;; s-H-3--1 
(("s-H-" . (3 -1 -3)) . 'icicle-apropos-cycle-previous-alt-action-keys) 
(("s-H-" . (3 -1 3)) . 'icicle-apropos-cycle-next-alt-action-keys) 

;;;;;;;; s-H-3-0 
(("s-H-" . (3 0 -3)) . 'icicle-modal-cycle-up-alt-action-keys) 
(("s-H-" . (3 0 3)) . 'icicle-modal-cycle-down-alt-action-keys) 

;;;;;;;; s-H-3-1 
(("s-H-" . (3 1 -3)) . 'icicle-prefix-cycle-previous-alt-action-keys) 
(("s-H-" . (3 1 3)) . 'icicle-prefix-cycle-next-alt-action-keys))

;;;; icicle-minibuffer-key-bindings
`(icicle-minibuffer-key-bindings 
;;;;; Coordinates
;;;;;; C-
;;;;;;; C-3
;;;;;;;; C-3-0
  (("C-" . (3 0 1)) . #'icicle-insert-string-at-point) 

;;;;;;; C-8 
;;;;;;;; C-8--1 
  (("C-" . (8 -1 4)) . #'icicle-multi-inputs-act) 

;;;;;;;; C-8-0 
  (("C-" . (8 0 -1)) . #'icicle-scroll-backward) 
  (("C-" . (8 0 1)) . #'icicle-scroll-forward) 

;;;;;; M- 
;;;;;;; M-1 
;;;;;;;; M-1-1 
  (("M-" . (1 1 -6)) . #'icicle-minibuffer-help) 

;;;;;;; M-3 
;;;;;;;; M-3--1 
  (("M-" . (3 -1 -3)) . #'icicle-erase-minibuffer) 
  (("M-" . (3 -1 3)) . #'icicle-erase-minibuffer-or-history-element) 

;;;;;;;; M-3-0 
  (("M-" . (3 0 -5)) . #'icicle-resolve-file-name) 

;;;;;;;; M-3-1 
  (("M-" . (3 1 -4)) . #'icicle-save-string-to-variable) 
  (("M-" . (3 1 4)) . #'icicle-insert-string-from-variable) 

;;;;;;; M-8 
;;;;;;;; M-8--1 
  (("M-" . (8 -1 -5)) . #'icicle-cycle-image-file-thumbnail) 
  (("M-" . (8 -1 -4)) . #'icicle-toggle-search-complementing-domain) 
  (("M-" . (8 -1 -2)) . #'icicle-toggle-sorting) 
  (("M-" . (8 -1 -1)) . #'icicle-toggle-ignored-extensions) 
  (("M-" . (8 -1 1)) . #'icicle-toggle-ignored-space-prefix) 
  (("M-" . (8 -1 2)) . #'icicle-toggle-alternative-sorting) 
  (("M-" . (8 -1 3)) . #'icicle-toggle-highlight-saved-candidates) 
  (("M-" . (8 -1 4)) . #'icicle-toggle-search-cleanup) 
  (("M-" . (8 -1 5)) . #'icicle-toggle-search-replace-common-match) 

;;;;;;;; M-8-0 
  (("M-" . (8 0 -5)) . #'icicle-minibuffer-help) 
  (("M-" . (8 0 -4)) . #'icicle-toggle-icomplete-mode) 
  (("M-" . (8 0 -3)) . #'icicle-toggle-regexp-quote) 
  (("M-" . (8 0 -2)) . #'icicle-toggle-annotation) 
  (("M-" . (8 0 -1)) . #'icicle-toggle-proxy-candidates) 
  (("M-" . (8 0 1)) . #'icicle-toggle-WYSIWYG-Completions) 
  (("M-" . (8 0 2)) . #'icicle-toggle-case-sensitivity) 
  (("M-" . (8 0 3)) . #'icicle-toggle-highlight-all-current) 
  (("M-" . (8 0 4)) . #'icicle-toggle-literal-replacement) 
  (("M-" . (8 0 5)) . #'icicle-toggle-highlight-historical-candidates) 

;;;;;;;; M-8-1 
  (("M-" . (8 1 -6)) . #'icicle-toggle-transforming) 
  (("M-" . (8 1 -5)) . #'icicle-toggle-show-multi-completion) 
  (("M-" . (8 1 -4)) . #'icicle-toggle-highlight-historical-candidates) 
  (("M-" . (8 1 -3)) . #'icicle-toggle-hiding-common-match) 
  (("M-" . (8 1 -2)) . #'icicle-toggle-completions-format) 
  (("M-" . (8 1 0)) . #'icicle-toggle-expand-to-common-match) 
  (("M-" . (8 1 2)) . #'icicle-toggle-expand-directory) 
  (("M-" . (8 1 3)) . #'icicle-toggle-hiding-non-matching-lines) 
  (("M-" . (8 1 4)) . #'icicle-toggle-highlight-saved-candidates) 
  (("M-" . (8 1 5)) . #'icicle-toggle-search-whole-word) 
  (("M-" . (8 1 6)) . #'icicle-toggle-WYSIWYG-Completions) 

;;;;;; M-C- 
;;;;;;; M-C-8 
;;;;;;;; M-C-8--1 
  (("M-C-" . (8 -1 3)) . #'icicle-insert-newline-in-minibuffer) 

;;;;;;;; M-C-8-0 
  (("M-C-" . (8 0 -4)) . #'icicle-insert-history-element) 

;;;;;;;; M-C-8-1 
  (("M-C-" . (8 1 -3)) . #'icicle-kill-region) 
  (("M-C-" . (8 1 -2)) . #'icicle-abort-recursive-edit) 
  (("M-C-" . (8 1 0)) . #'icicle-reverse-sort-order) 

;;;;;; s- 
;;;;;;; s-8 
;;;;;;;; s-8--1 
  (("s-" . (8 -1 -4)) . #'icicle-pp-eval-expression) 
  (("s-" . (8 -1 4)) . #'icicle-other-history) 

;;;;;;;; s-8-0 
  (("s-" . (8 0 5)) . #'icicle-multi-inputs-save) 

;;;;;;;; s-8-1 
  (("s-" . (8 1 -2)) . #'icicle-top-level) 

;;;;;; s-C- 
;;;;;;; s-C-3 
;;;;;;;; s-C-3-0 
  (("s-C-" . (3 0 -5)) . #'icicle-minibuffer-help) 

;;;;;; s-H- 
;;;;;;; s-H-8 
;;;;;;;; s-H-8--1 
  (("s-H-" . (8 -1 -3)) . #'icicle-clear-current-history))

;;;; icicle-completion-key-bindings
`(icicle-completion-key-bindings 
;;;;; Coordinates
;;;;;; C-
;;;;;;; C-1
;;;;;;;; C-1-0
  (("C-" . (1 0 -3)) . #'previous-line-or-history-element) 
  (("C-" . (1 0 3)) . #'next-line-or-history-element) 

;;;;;;; C-3 
;;;;;;;; C-3--1 
  (("C-" . (3 -1 -5)) . #'icicle-insert-key-description) 
  (("C-" . (3 -1 -1)) . #'icicle-apropos-complete-and-narrow) 
  (("C-" . (3 -1 2)) . #'icicle-apropos-complete-and-exit) 
  (("C-" . (3 -1 4)) . #'d-emacs-icicle-avy-jump) 

;;;;;;;; C-3-0 
  (("C-" . (3 0 -5)) . #'icicle-help-on-candidate) 
  (("C-" . (3 0 -2)) . #'icicle-roundup) 
  (("C-" . (3 0 -1)) . #'icicle-narrow-candidates) 
  (("C-" . (3 0 1)) . #'icicle-widen-candidates) 

;;;;;;;; C-3-1 
  (("C-" . (3 1 -4)) . #'icicle-candidate-set-swap) 
  (("C-" . (3 1 4)) . #'icicle-insert-list-join-string) 

;;;;;;; C-8 
;;;;;;;; C-8--1 
  (("C-" . (8 -1 -4)) . #'icicle-keep-only-past-inputs) 
  (("C-" . (8 -1 2)) . #'icicle-candidate-read-fn-invoke) 
  (("C-" . (8 -1 5)) . #'icicle-candidate-set-save-selected) 

;;;;;;;; C-8-0 
  (("C-" . (8 0 -5)) . #'icicle-candidate-set-retrieve) 
  (("C-" . (8 0 3)) . #'icicle-retrieve-next-input) 
  (("C-" . (8 0 4)) . #'icicle-save/unsave-candidate) 
  (("C-" . (8 0 5)) . #'icicle-candidate-set-save) 

;;;;;;;; C-8-1 
  (("C-" . (8 1 -5)) . #'icicle-candidate-set-retrieve-from-variable) 
  (("C-" . (8 1 5)) . #'icicle-candidate-set-save-to-variable) 

;;;;;; H- 
;;;;;;; H-3 
;;;;;;;; H-3--1 
  (("H-" . (3 -1 -3)) . #'d-emacs-icicle-previous-S-TAB-completion-method) 
  (("H-" . (3 -1 -2)) . #'icicle-candidate-set-intersection) 
  (("H-" . (3 -1 2)) . #'icicle-candidate-set-union) 
  (("H-" . (3 -1 3)) . #'icicle-next-S-TAB-completion-method) 

;;;;;;;; H-3-0 
  (("H-" . (3 0 -3)) . #'d-emacs-icicle-previous-completion-style-set) 
  (("H-" . (3 0 -2)) . #'icicle-candidate-set-complement) 
  (("H-" . (3 0 2)) . #'icicle-candidate-set-difference) 
  (("H-" . (3 0 3)) . #'icicle-next-completion-style-set) 

;;;;;;;; H-3-1 
  (("H-" . (3 1 -3)) . #'d-emacs-icicle-previous-TAB-completion-method) 
  (("H-" . (3 1 3)) . #'icicle-next-TAB-completion-method) 

;;;;;;; H-8 
;;;;;;;; H-8--1 
  (("H-" . (8 -1 -3)) . #'d-emacs-icicle-backward-cycle-incremental-completion) 
  (("H-" . (8 -1 3)) . #'icicle-cycle-incremental-completion) 

;;;;;;;; H-8-0 
  (("H-" . (8 0 -4)) . #'d-emacs-icicle-previous-sort-order) 
  (("H-" . (8 0 -1)) . #'icicle-scroll-Completions-backward) 
  (("H-" . (8 0 1)) . #'icicle-scroll-Completions-forward) 
  (("H-" . (8 0 4)) . #'icicle-change-sort-order) 

;;;;;;;; H-8-1 
  (("H-" . (8 1 -3)) . #'d-emacs-icicle-backward-cycle-expand-to-common-match) 
  (("H-" . (8 1 3)) . #'icicle-cycle-expand-to-common-match) 

;;;;;; H-C- 
;;;;;;; H-C-3 
;;;;;;;; H-C-3--1 
  (("H-C-" . (3 -1 -3)) . #'icicle-delete-candidate-object) 

;;;;;; M- 
;;;;;;; M-3 
;;;;;;;; M-3--1 
  (("M-" . (3 -1 4)) . #'icicle-history) 

;;;;;;;; M-3-0 
  (("M-" . (3 0 3)) . #'icicle-dispatch-M-comma) 

;;;;;;; M-8 
;;;;;;;; M-8-0 
  (("M-" . (8 0 -1)) . #'icicle-candidate-set-truncate) 

;;;;;; M-C- 
;;;;;;; M-C-3 
;;;;;;;; M-C-3--1 
  (("M-C-" . (3 -1 -4)) . #'icicle-display-candidates-in-Completions) 
  (("M-C-" . (3 -1 -2)) . #'icicle-recomplete-from-original-domain) 
  (("M-C-" . (3 -1 4)) . #'d-emacs-icicle-avy-insert) 

;;;;;;; M-C-8 
;;;;;;;; M-C-8--1 
  (("M-C-" . (8 -1 5)) . #'icicle-candidate-set-save-more-selected) 

;;;;;;;; M-C-8-0 
  (("M-C-" . (8 0 -5)) . #'icicle-candidate-set-retrieve-more) 
  (("M-C-" . (8 0 5)) . #'icicle-candidate-set-save-more) 

;;;;;; M-H- 
;;;;;;; M-H-3 
;;;;;;;; M-H-3-0 
  (("M-H-" . (3 0 -5)) . #'bmkp-retrieve-icicle-search-hits) 
  (("M-H-" . (3 0 -4)) . #'icicle-doremi-increment-swank-timeout+) 
  (("M-H-" . (3 0 -3)) . #'icicle-doremi-increment-variable+) 
  (("M-H-" . (3 0 -2)) . #'icicle-doremi-candidate-width-factor+) 
  (("M-H-" . (3 0 2)) . #'icicle-doremi-zoom-Completions+) 
  (("M-H-" . (3 0 3)) . #'icicle-doremi-increment-max-candidates+) 
  (("M-H-" . (3 0 4)) . #'icicle-doremi-inter-candidates-min-spaces+) 
  (("M-H-" . (3 0 5)) . #'bmkp-set-icicle-search-hits-bookmark) 

;;;;;;;; M-H-3-1 
  (("M-H-" . (3 1 0)) . #'icicle-doremi-increment-swank-prefix-length+) 

;;;;;; s- 
;;;;;;; s-3 
;;;;;;;; s-3--1 
  (("s-" . (3 -1 -3)) . #'icicle-remove-candidate) 
  (("s-" . (3 -1 -1)) . #'icicle-narrow-candidates-with-predicate) 
  (("s-" . (3 -1 1)) . #'icicle-apropos-complete-and-widen) 
  (("s-" . (3 -1 4)) . #'icicle-other-history) 

;;;;;;;; s-3-0 
  (("s-" . (3 0 -2)) . #'icicle-all-candidates-action) 

;;;;;;;; s-3-1 
  (("s-" . (3 1 -2)) . #'icicle-all-candidates-list-action) 

;;;;;;; s-8 
;;;;;;;; s-8-0 
  (("s-" . (8 0 -4)) . #'icicle-regexp-quote-input) 
  (("s-" . (8 0 -3)) . #'icicle-retrieve-last-input) 
  (("s-" . (8 0 4)) . #'icicle-plus-saved-sort) 

;;;;;;;; s-8-1 
  (("s-" . (8 1 5)) . #'icicle-save-predicate-to-variable) 

;;;;;; s-C- 
;;;;;;; s-C-3 
;;;;;;;; s-C-3--1 
  (("s-C-" . (3 -1 3)) . #'d-emacs-icicle-insert-anychar-regexp) 
  (("s-C-" . (3 -1 4)) . #'icicle-switch-to-Completions-buf) 

;;;;;;;; s-C-3-0 
  (("s-C-" . (3 0 -2)) . #'icicle-candidate-set-define) 

;;;;;;; s-C-8 
;;;;;;;; s-C-8-0 
  (("s-C-" . (8 0 -5)) . #'icicle-candidate-set-retrieve-persistent) 
  (("s-C-" . (8 0 -3)) . #'icicle-retrieve-previous-input) 
  (("s-C-" . (8 0 5)) . #'icicle-candidate-set-save-persistently) 

;;;;;; s-H- 
;;;;;;; s-H-3 
;;;;;;;; s-H-3-0 
(("s-H-" . (3 0 -2)) . #'icicle-all-candidates-alt-action) 
(("s-H-" . (3 0 2)) . #'icicle-candidate-alt-action) 

;;;;;;;; s-H-3-1 
  (("s-H-" . (3 1 -2)) . #'icicle-all-candidates-list-alt-action))

;;;; icicle-buffer-candidate-key-bindings
`(icicle-buffer-candidate-key-bindings 
;;;;; Coordinates
;;;;;; s-H-
;;;;;;; s-H-8
;;;;;;;; s-H-8--1
  (("s-H-" . (8 -1 -3)) . #'icicle-remove-buffer-cands-for-derived-mode) 
  (("s-H-" . (8 -1 -2)) . #'icicle-toggle-include-cached-files) 
  (("s-H-" . (8 -1 2)) . #'icicle-toggle-include-recent-files) 
  (("s-H-" . (8 -1 3)) . #'icicle-keep-only-buffer-cands-for-derived-mode) 

;;;;;;;; s-H-8-0 
  (("s-H-" . (8 0 -4)) . #'icicle-remove-buffer-cands-for-visible) 
  (("s-H-" . (8 0 -3)) . #'icicle-remove-buffer-cands-for-mode) 
  (("s-H-" . (8 0 -2)) . #'icicle-remove-buffer-cands-for-modified) 
  (("s-H-" . (8 0 -1)) . #'icicle-remove-buffer-cands-for-indirect) 
  (("s-H-" . (8 0 1)) . #'icicle-keep-only-buffer-cands-for-indirect) 
  (("s-H-" . (8 0 2)) . #'icicle-keep-only-buffer-cands-for-modified) 
  (("s-H-" . (8 0 3)) . #'icicle-keep-only-buffer-cands-for-mode) 
  (("s-H-" . (8 0 4)) . #'icicle-keep-only-buffer-cands-for-visible))

;;;; icicle-completion-list-key-bindings
`(icicle-completion-list-key-bindings 
;;;;; Coordinates
;;;;;; C-
;;;;;;; C-3
;;;;;;;; C-3-0
  (("C-" . (3 0 -5)) . #'icicle-help-on-candidate) 
  (("C-" . (3 0 -3)) . #'icicle-move-to-previous-completion) 
  (("C-" . (3 0 2)) . #'icicle-insert-completion) 
  (("C-" . (3 0 3)) . #'icicle-move-to-next-completion) 

;;;;;;; C-8 
;;;;;;;; C-8--1 
  (("C-" . (8 -1 5)) . #'icicle-candidate-set-save-selected) 

;;;;;;;; C-8-0 
  (("C-" . (8 0 -5)) . #'icicle-candidate-set-retrieve) 
  (("C-" . (8 0 -3)) . #'icicle-retrieve-previous-input) 
  (("C-" . (8 0 5)) . #'icicle-candidate-set-save) 

;;;;;; M- 
;;;;;;; M-1 
;;;;;;;; M-1-0 
  (("M-" . (1 0 -6)) . #'icicle-minibuffer-help) 

;;;;;; M-C- 
;;;;;;; M-C-8 
;;;;;;;; M-C-8--1 
  (("M-C-" . (8 -1 5)) . #'icicle-candidate-set-save-more-selected) 

;;;;;;;; M-C-8-0 
  (("M-C-" . (8 0 5)) . #'icicle-candidate-set-save-more) 

;;;;;;;; M-C-8-1 
  (("M-C-" . (8 1 -2)) . #'icicle-abort-recursive-edit))

;;; d-emacs-icicles-special-customs-bindlists.el ends here
