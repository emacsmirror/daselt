;;; d-stump-constants.el --- Daselt's Emacs module              -*- lexical-binding: t; -*-

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

;; This file houses the d-stump-constants for Daselt.

;;; Code:

(defconst d-stump-modules
  '("binwarp" "spatial-groups" "swm-emacs" "winner-mode" "clipboard-history" "pamixer" "screenshot-maim" "acpi-backlight" "notifications")
  "Modules for which d-stump-configurations exist.")

(provide 'd-stump-constants)
;;; d-stump-constants.el ends here
