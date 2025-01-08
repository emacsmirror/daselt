 ;;; d-emacs-avy-act-special-recenter-bindlists.el --- d-emacs-bindlists for avy-act-recenter-keys  -*- lexical-binding: t; -*-

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

;; Special bindlist for avy-act-recenter-at-cur-line-keys that is read in in d-emacs-avy-act-constants.
;;; Code:

;;;; avy-act-mode-map
`(
;;;;; Coordinates
;;;;;; C-
;;;;;;; C-1
;;;;;;;; C-1-0
  (("C-" . (1 0 -3))) 
  (("C-" . (1 0 3))))

(provide 'd-emacs-avy-act-special-recenter-bindlists)
;;; d-emacs-avy-act-special-recenter-bindlists.el ends here
