;;; d-emacs.el --- Daselt's Emacs module  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Alexander Prähauser

;; Author: Alexander Prähauser <ahprae@protonmail.com>
;; Package-Requires: ((emacs "29.1"))
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

;;; Commentary: The main file for Daselt's Emacs module.
;; Only exists to load other files.

;; Main file for d-emacs. Only loads other files.

;;; Code:

(require 'd-emacs-base)
(require 'd-emacs-coords)
(require 'd-emacs-xkb)
(require 'd-emacs-dfk)
(require 'd-emacs-bind)
(require 'd-emacs-dirs)
(require 'd-emacs-mode)

(provide 'd-emacs)
;;; d-emacs.el ends here
