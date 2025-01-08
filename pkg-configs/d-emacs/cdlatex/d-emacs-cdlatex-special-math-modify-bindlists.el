;;; d-emacs-cdlatex-special-math-modify-bindlists.el --- bindlist for cdlatex-math-modify-alist-default.  -*- lexical-binding: t; -*-

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

;; The bindlists for cdlatex-math-modify-alist-default. Read in in d-emacs-cdlatex-constants.el.

;;; Code:

;;;; cdlatex-mode-map
`(
;;;;; Coordinates
;;;;;;; 1
;;;;;;;; 1-0
  ((1 0 -2) "\\mathbb" nil t nil nil) 

;;;;;;;; 1-1 
  ((1 1 -3) "\\mathfrak" nil t nil nil) 

;;;;;;; 2 
;;;;;;;; 2-0 
  ((2 0 3) "\\red" nil t nil nil) 

;;;;;;;; 2-1 
  ((2 1 -4) "\\yellow" nil t nil nil) 
  ((2 1 5) "\\blue" nil t nil nil) 

;;;;;;; 3 
;;;;;;;; 3--1 
  ((3 -1 -5) "\\cancel" nil t nil nil) 
  ((3 -1 -3) "\\mathsmaller" "\\textsmaller" t nil nil) 
  ((3 -1 3) "\\mathlarger" "\\textlarger" t nil nil) 

;;;;;;; 4 
;;;;;;;; 4--1 
  ((4 -1 -4) "\\reflectbox" nil t nil nil) 

;;;;;;;; 4-1 
  ((4 1 -2) "\\underline" nil t nil nil) 
  ((4 1 2) "\\overline" nil t nil nil) 

;;;;;;; 6 
;;;;;;;; 6--1 
  ((6 -1 -3) "\\xuparrow" nil t nil nil) 
  ((6 -1 -2) "\\xleftarrow" nil t nil nil) 
  ((6 -1 2) "\\xrightarrow" nil t nil nil) 
  ((6 -1 3) "\\xdownarrow" nil t nil nil))

(provide 'd-emacs-cdlatex-special-math-modify-bindlists)
;;; d-emacs-cdlatex-special-math-modify-bindlists.el ends here
