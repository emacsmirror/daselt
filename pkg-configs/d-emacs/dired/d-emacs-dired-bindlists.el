;;; d-emacs-dired-bindlists.el --- d-emacs-bindlists for dired  -*- lexical-binding: t; -*-

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

;; This file contains the keylists for maps of dired. If d-dired is t, it is parsed automatically when d-emacs-mode is started. Each element in this file should be either

;; 1. A LIST consisting of conses of whose car is a binding-location (see d--binding-location-p) binding-cars and values. This map list corresponds to  and should generally be at be at the top if it exists, though sometimes it makes sense to put it lower down if it references maps that are previously defined. When d-emacs-mode is activated and  is evaluated,  is backed up at d-emacs--mode-backup and the key combinations in this list are rebound to their corresponding values.

;; 2. A cons CONS1 whose car is a symbol SYMB whose cdr is a list as in 1. If SYMB is already bound when  is evaluated and d-emacs-mode is activated, then its value should be a keymap. This map is backed at d-emacs-SYMB-backup and then rebound as above. If SYMB is bound at that point, a new keymap is defined and bound to SYMB.

;; 3. A cons CONS2 whose car is a symbol or string and whose cdr is a list of conses like CONS1 as in 2. The first element of CONS2 denotes a file or feature that has to be evaluated before the keys in the bindlists in the cdr of CONS2 can be rebound.

;; Each of these options should be given using a backquote.

;;; Code:

;;;; dired-mode-map
`(
;;;;; Coordinates
;;;;;;; 0
;;;;;;;; 0-0
  ((0 2 0) . #'hydra-dired-quick-sort/body) 

;;;;;;; 1 
;;;;;;;; 1--1 
  ((1 -1 -5) . #'dired-do-chmod) 
  ((1 -1 -4) . #'avy-act-follow-in-new-tab) 
  ((1 -1 -3) . #'dired-unmark) 
  ((1 -1 -2) . #'dired-undo) 
  ((1 -1 -1) . #'dired-do-chown) 
  ((1 -1 1) . #'dired-mode-regexp-menu) 
  ((1 -1 2) . #'dired-redo) 
  ((1 -1 3) . #'dired-mark) 
  ((1 -1 4) . #'avy-act-follow) 
  ((1 -1 5) . #'dired-omit-mode) 

;;;;;;;; 1-0 
  ((1 0 -5) . #'dired-do-hardlink) 
  ((1 0 -4) . #'find-name-dired) 
  ((1 0 -3) . #'scroll-down-command) 
  ((1 0 -2) . #'dired-up-directory) 
  ((1 0 -1) . #'image-dired-display-thumb) 
  ((1 0 2) . #'dired-find-file) 
  ((1 0 3) . #'scroll-up-command) 
  ((1 0 4) . #'find-name-dired) 
  ((1 0 5) . #'dired-do-symlink) 

;;;;;;;; 1-1 
  ((1 1 -6) . #'dired-get-size) 
  ((1 1 -5) . #'dired-do-kill-lines) 
  ((1 1 -3) . #'dired-hide-details-mode) 
  ((1 1 -2) . #'revert-buffer) 
  ((1 1 2) . #'dired-do-shell-command) 
  ((1 1 3) . #'dired-copy-filename-as-kill) 

;;;;;;; 2 
;;;;;;;; 2--1 
  ((2 -1 -5) . #'dired-unmark-backward) 
  ((2 -1 -4) . #'dired-find-alternate-file) 
  ((2 -1 -3) . #'dired-unmark-all-files) 
  ((2 -1 3) . #'dired-mark-files-regexp) 

;;;;;;;; 2-0 
  ((2 0 -1) . #'image-dired-display-thumbs) 
  ((2 0 3) . #'dired-do-rename) 

;;;;;;;; 2-1 
  ((2 1 -5) . #'dired-do-compress) 
  ((2 1 -2) . #'dired-do-redisplay) 
  ((2 1 0) . #'image-dired-dired-display-external) 

;;;;;;; 3 
;;;;;;;; 3--1 
  ((3 -1 -5) . #'dired-do-byte-compile) 
  ((3 -1 -3) . #'dired-do-delete) 
  ((3 -1 2) . #'dired-goto-file) 
  ((3 -1 3) . #'dired-mark-sexp) 
  ((3 -1 5) . #'bmkp-dired-jump) 

;;;;;;;; 3-0 
  ((3 0 -5) . #'dired-do-info) 
  ((3 0 -4) . #'dired-do-find-regexp) 
  ((3 0 -3) . #'dired-hide-subdir) 
  ((3 0 -1) . #'dired-do-run-mail) 
  ((3 0 1) . #'quit-window) 
  ((3 0 2) . #'dired-find-file-other-window) 
  ((3 0 3) . #'dired-insert-subdir) 
  ((3 0 4) . #'dired-do-find-regexp-and-replace) 
  ((3 0 5) . #'browse-url-of-dired-file) 

;;;;;;;; 3-1 
  ((3 1 -6) . #'dired-show-file-type) 
  ((3 1 -3) . #'dired-do-touch) 
  ((3 1 3) . #'dired-flag-backup-files) 
  ((3 1 5) . #'dired-copy-file-recursive) 

;;;;;;; 4 
;;;;;;;; 4--1 
  ((4 -1 -4) . #'dired-do-load) 
  ((4 -1 -3) . #'dired-flag-file-deletion) 

;;;;;;;; 4-0 
  ((4 0 -3) . #'dired-hide-all) 

;;;;;;;; 4-1 
  ((4 1 3) . #'image-dired-dired-toggle-marked-thumbs) 

;;;;;;; 5 
;;;;;;;; 5--1 
  ((5 -1 -2) . #'dired-mode-mark-menu) 
  ((5 -1 4) . #'dired-do-chgrp) 

;;;;;;;; 5-1 
  ((5 1 2) . #'dired-do-async-shell-command) 
  ((5 1 5) . #'image-dired-delete-tag) 

;;;;;;; 6 
;;;;;;;; 6--1 
  ((6 -1 -4) . #'dired-do-print) 
  ((6 -1 -2) . #'d-dired-do-flagged-delete) 

;;;;;;;; 6-0 
  ((6 0 -2) . #'image-dired-dired-edit-comment-and-tags) 
  ((6 0 2) . #'image-dired-dired-display-image) 
  ((6 0 4) . #'elgrep-menu) 

;;;;;;;; 6-1 
  ((6 1 -3) . #'image-dired-mark-tagged-files) 
  ((6 1 -2) . #'image-dired-tag-files) 
  ((6 1 2) . #'image-dired-dired-comment-files) 
  ((6 1 4) . #'image-dired-jump-thumbnail-buffer) 
  ((6 1 5) . #'image-dired-dired-edit-comment-and-tags) 

;;;;;;; 7 
;;;;;;;; 7--1 
  ((7 -1 -3) . #'dired-display-file) 
  ((7 -1 3) . #'dired-mark-suffix) 

;;;;;;;; 7-0 
  ((7 0 -3) . #'dired-tree-up) 
  ((7 0 -2) . #'dired-prev-marked-file) 
  ((7 0 2) . #'dired-next-marked-file) 
  ((7 0 3) . #'dired-tree-down) 
  ((7 0 4) . #'dired-goto-subdir) 
  ((7 0 5) . #'dired-do-relsymlink) 

;;;;;;;; 7-1 
  ((7 1 -2) . #'dired-prev-subdir) 
  ((7 1 0) . #'dired-toggle-marks) 
  ((7 1 2) . #'dired-next-subdir) 
  ((7 1 5) . #'dired-smart-shell-command) 

;;;;;; M- 
;;;;;;; M-3 
;;;;;;;; M-3--1 
  (("M-" . (3 -1 -3)) . #'d-dired-permanentely-delete))

;;; d-emacs-dired-bindlists.el ends here
