;;; daselt.el --- Module for the Daselt configuration scheme  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Alexander Prähauser

;; Author: Alexander Prähauser <ahprae@protonmail.com>
;; Package-Requires: ((emacs "30.1"))
;; Version: 1.0
;; Keywords: tools
;; URL: https://gitlab.com/nameiwillforget/d-emacs/

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

;; This package is Daselt's Emacs module. Daselt is a global configuration
;; scheme for a GNU/Linux system, providing, among other things, an optimized
;; keyboard layout and a shortcut meta-layout. The main purpose of this package
;; is to provide an implementation of this shortcut meta-layout in Emacs, but it
;; contains several parts, which can be used independently to create and share
;; other configurations:

;; - daselt-base provides a host of functions for the other components that
;;   might be useful for building other packages as well.

;; - daselt-coords provides functions to for the coordinatization of layouts.

;; - daselt-xkb can import custom xkb-layouts into Emacs.

;; - daselt-dfk can generate Dual Function Keys configurations from coordinates
;; and add them to layouts as a zeroth layer.

;; - daselt-bind allows Emacs to read bindings that use coordinates and store
;; and manipulate them in bindlists.

;; - daselt-dirs provides functions to recursively act on files in a directory
;; and automatically apply actions on file save.

;; - daselt-mode provides a mode that implements Daselt's shortcut layout in
;; Emacs.

;; - daselt-stump can generate StumpWM configurations from bindlists.

;; - daselt-tri can generate Tridactyl configurations from bindlists.

;; For more information, please visit the main page of Daselt:
;; https://gitlab.com/nameiwillforget/daselt.

;; Installation:

;; Most of the components of daselt can be used on their own but are mainly
;; auxiliary tools for the creation of other packages and configurations.
;; daselt-mode contains the actual configuration to allow you to have the
;; Daselt experience in Emacs, but to use it, Daselt's xkb and dfk components
;; have to be configured on your system. To achieve this, please follow the
;; README in the Daselt repository and use the Daselt configure script.

;;; Code:


(defconst daselt-emacs-dir
  (file-name-directory (or load-file-name (buffer-file-name)))
  "Directory Daselt's Emacs component is installed in.")
  
(require 'daselt-base)
(require 'daselt-coords)
(require 'daselt-xkb)
(require 'daselt-dfk)
(require 'daselt-bind)
(require 'daselt-dirs)
(require 'daselt-mode)


(provide 'daselt)
;;; daselt.el ends here
