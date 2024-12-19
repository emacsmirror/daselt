;;; d-tri-customs.el --- Daselt's Emacs module              -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Alexander Prähauser

;; Author: Alexander Prähauser <ahprae@protonmail.com>
;; Keywords: tools
;; Version: 0.8

;; This file is part of Daselt.

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

;; This file houses the d-tri-customization options for Daselt.

;;; Code:

(defgroup d-tri
  nil
  "Customization group for for d-tri in Emacs."
  :group 'Daselt
  :prefix "d-tri-")

(defcustom d-tri
  t
  "The Daselt custom corresponding to d-tri. Toggle off if you're not using it."
  :type 'boolean
  :group 'd-tri)

(provide 'd-tri-customs)
;;; d-tri-customs.el ends here
