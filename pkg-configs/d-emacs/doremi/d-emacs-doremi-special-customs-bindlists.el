 ;;; d-emacs-doremi-special-customs-bindlists.el.el --- d-emacs-bindlists for doremi  -*- lexical-binding: t; -*-

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

;; This file contains the bindlist for doremi key customs. If daselt-doremi is t, it is parsed automatically when daselt-mode is started and to each custom that is the cdr of a binding in this bindlist the bindingplace of the binding is added as an element.

;;; Code:

`((("C-" . (3 0 -3)) . doremi-up-keys)
  (("C-" . (3 0 3)) . doremi-down-keys)
  (("C-" . (3 0 -1)) . doremi-boost-up-keys)
  (("C-" . (3 0 1)) . doremi-boost-down-keys))

(provide 'd-emacs-doremi-special-customs-bindlists.el)
;;; d-emacs-doremi-special-customs-bindlists.el.el ends here
