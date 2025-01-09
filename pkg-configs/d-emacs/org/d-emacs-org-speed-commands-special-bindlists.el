;;; d-emacs-org-speed-commands-special-bindlists.el --- d-emacs map list for org speed-commands.  -*- lexical-binding: t; -*-

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

;; This file houses the map list for org speed-commands. Speed-commands don't have their own keymap, so they have to be read into org-speed-commands, but since they are still bound to keys they should have a bindlist that is sorted and formatted like other bindlists are.

;;; Code:

;;;; org-mode-map

;;;; org-mode-map
`(
;;;;; Coordinates
;;;;;;; 1
;;;;;;;; 1--1
  ((1 -1 -4) . org-columns) 
  ((1 -1 -2) org-speed-move-safe 'org-forward-heading-same-level) 
  ((1 -1 1) org-speed-move-safe 'org-backward-heading-same-level) 
  ((1 -1 3) . org-cut-subtree) 
  ((1 -1 4) . org-goto) 

;;;;;;;; 1-0 
  ((1 0 -3) org-speed-move-safe 'outline-up-heading) 
  ((1 0 -2) org-speed-move-safe 'org-previous-visible-heading) 
  ((1 0 2) org-speed-move-safe 'org-next-visible-heading) 

;;;;;;;; 1-1 
  ((1 1 0) . org-toggle-narrow-to-subtree) 
  ((1 1 2) org-refile '(4)) 
  ((1 1 3) . (lambda (m) (interactive "sMinutes before warning: ") (org-entry-put (point) "APPT_WARNTIME" m))) 
  ((1 1 4) . org-shifttab) 

;;;;;;; 2 
;;;;;;;; 2--1 
  ((2 -1 -2) org-priority) 
  ((2 -1 1) . org-todo) 

;;;;;;;; 2-0 
  ((2 0 -4) . org-sparse-tree) 
  ((2 0 -3) . org-set-tags-command) 
  ((2 0 -2) . org-next-block) 
  ((2 0 -1) . org-agenda) 
  ((2 0 1) . org-inc-effort) 
  ((2 0 2) . org-previous-block) 
  ((2 0 3) . org-set-effort) 
  ((2 0 4) . org-table) 

;;;;;;; 3 
;;;;;;;; 3--1 
  ((3 -1 -2) org-priority 32) 
  ((3 -1 2) org-priority 65) 
  ((3 -1 3) progn (forward-char 1) (call-interactively 'org-insert-heading-respect-content)) 
  ((3 -1 4) . org-metadown) 

;;;;;;;; 3-0 
  ((3 0 -4) org-priority 66) 
  ((3 0 4) org-priority 67) 

;;;;;;;; 3-1 
  ((3 1 -3) . org-cycle) 

;;;;;;; 4 
;;;;;;;; 4--1 
  ((4 -1 -3) . org-mark-subtree) 
  ((4 -1 2) . org-refile) 
  ((4 -1 3) . org-archive-subtree-default-with-confirmation) 
  ((4 -1 4) . org-metaleft) 

;;;;;;;; 4-0 
  ((4 0 -3) . org-metaright) 
  ((4 0 3) . org-metaup) 
  ((4 0 4) . org-sort) 
  ((4 0 5) . org-display-outline-path) 

;;;;;;;; 4-1 
  ((4 1 -5) . org-shiftmetaright) 
  ((4 1 2) . org-toggle-comment) 
  ((4 1 5) . org-shiftmetaleft) 

;;;;;;; 5 
;;;;;;;; 5-0 
  ((5 0 2) . org-open-at-point) 

;;;;;;; 6 
;;;;;;;; 6--1 
  ((6 -1 -2) . org-clock-in) 
  ((6 -1 2) . org-clock-out) 

;;;;;;;; 6-0 
  ((6 0 -2) org-agenda-set-restriction-lock 'subtree) 
  ((6 0 2) org-agenda-remove-restriction-lock))

;;; d-emacs-org-speed-commands-special-bindlists.el ends here
