;;; d-emacs-vertico-bindlists.el --- Daselt-bindlist for Vertico  -*- lexical-binding: t; -*-

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

;; 

;;; Code:

;;;; vertico-map
`(vertico-map
;;;;; Coordinates
;;;;;; C-
;;;;;;; C-3
;;;;;;;; C-3--1
  (("C-" . (3 -1 -4)) . #'vertico-quick-exit) 
  (("C-" . (3 -1 -3)) . #'vertico-directory-delete-char) 
  (("C-" . (3 -1 -2)) . #'vertico-directory-up) 
  (("C-" . (3 -1 2)) . #'vertico-directory-enter) 
  (("C-" . (3 -1 3)) . #'vertico-directory-delete-word) 
  (("C-" . (3 -1 4)) . #'vertico-quick-jump) 

;;;;;;;; C-3-0 
  (("C-" . (3 0 -5)) . #'vertico-repeat) 
  (("C-" . (3 0 -1)) . #'vertico-scroll-up) 
  (("C-" . (3 0 -1)) . #'vertico-previous-group) 
  (("C-" . (3 0 1)) . #'vertico-scroll-down) 
  (("C-" . (3 0 1)) . #'vertico-next-group) 
  (("C-" . (3 0 2)) . #'vertico-exit) 
  (("C-" . (3 0 5)) . #'vertico-suspend)
  
;;;;;;;; C-3-1 
  (("C-" . (3 1 -3)) . #'vertico-save) 
  (("C-" . (3 1 -2)) . #'vertico-exit-input) 

;;;;;; M- 
;;;;;;; M-3 
;;;;;;;; M-3-0 
  (("M-" . (3 0 -3)) . #'vertico-indexed-mode) 
  (("M-" . (3 0 -2)) . #'vertico-flat-mode) 
  (("M-" . (3 0 -1)) . #'vertico-multiform-mode) 
  (("M-" . (3 0 1)) . #'vertico-mouse-mode) 
  (("M-" . (3 0 2)) . #'vertico-grid-mode) 
  (("M-" . (3 0 3)) . #'vertico-unobtrusive-mode) 

;;;;;; s- 
;;;;;;; s-3 
;;;;;;;; s-3--1 
  (("s-" . (3 -1 -2)) . #'vertico-prescient-mode) 

;;;;;;;; s-3-0 
  (("s-" . (3 0 -3)) . #'vertico-repeat-previous) 
  (("s-" . (3 0 -2)) . #'vertico-repeat-select) 
  (("s-" . (3 0 -1)) . #'vertico-first) 
  (("s-" . (3 0 1)) . #'vertico-last) 
  (("s-" . (3 0 3)) . #'vertico-repeat-next) 

;;;;;;;; s-3-1 
  (("s-" . (3 1 -2)) . #'vertico-quick-insert))

(provide 'd-emacs-vertico-bindlists)
;;; d-emacs-vertico-bindlists.el ends here
