;;; d-stump-iresize-special-bindlists.el --- bindlists for the d-stump init.  -*- lexical-binding: t; -*-

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

;; This file contains the keylist for iresize in StumpWM.

;;; Code:


;;;; stumpwm-mode-map
`((
;;;;; Strings
   ("ESC") 
   ("RET") 

;;;;; Coordinates   
;;;;;; H- 
;;;;;;; H-1 
;;;;;;;; H-1-0 
   (("H-" . (1 0 1)))) 
;;;;; Coordinates
;;;;;;; 1
;;;;;;;; 1-0
  ((1 0 -3) "resize-direction up") 
  ((1 0 -2) "resize-direction left") 
  ((1 0 2) "resize-direction right") 
  ((1 0 3) "resize-direction down"))

;;; d-stump-iresize-special-bindlists.el ends here
