;;; d-emacs-tabgo-advicelists.el --- d-emacs-bindlists for emacs  -*- lexical-binding: t; -*-

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

;; Advice that is supposed to be wrapped around tabgo-functions when d-emacs-mode is activated. Each object in the code of this file should start with a backquote and evaluate to either 

;;   a LIST of entries of the form

;;   (SYMBOL HOW FUNCTION &optional PROPS)

;;   where each term means the same as for #'advice-add. Don't forget the initial backquote.

;; or

;;   a CONS whose car is an evaluation condition for #'with-eval-after-load and whose cdr is a LIST as above. If no other evaluation condition is given, the name of the containing directory is used.

;;; Code:

`(((tabgo) :around (d-emacs-keep-tab-bar-status)))

(provide 'd-emacs-tabgo-advice.el)
;;; d-emacs-tabgo-advice.el.el ends here
