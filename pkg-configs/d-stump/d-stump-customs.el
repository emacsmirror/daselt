;;; d-stump-customs.el --- Daselt's Emacs module              -*- lexical-binding: t; -*-

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

;; This file houses the d-emacs-customization options for Daselt.

;;; Code:

(require 'd-customs)

(defgroup d-stump
  nil
  "Customization options for d-stump in Emacs."
  :group 'Daselt
  :prefix "d-stump-")

(defcustom d-stump
  nil
  "Non-nil means StumpWM integration with d-stump is active."
  :type 'boolean
  :group 'd-stump)

(when d-stump
  (defcustom d-stump-keymaps
    '(*emacs-map* *run-app-map* *quit-map*)
    "List of maps that should be defined in the d-stump-init."
    :type 'list
    :group 'd-stump)

  (defcustom d-stump-contrib
    t
    "Toggle if you have stumpwm-contrib installed and want to use d-stump's contrib-functions."
    :type 'boolean)
  
  (if d-stump-contrib
      (progn (require 'd-stump-constants)
             (mapcar (lambda (module)
                       (eval `(defcustom ,(intern (concat "d-stump-" module))
                                t
                                "d-stump-option for ,module. If you're not using ,module you can deactivate it to make a simpler d-stump-init."
                                :type 'boolean
                                :group d-stump)))
                     d-stump-modules)))
  
  (defcustom d-stump-remap-exceptions-alist
    (remq nil `(,(if d-stump-binwarp `("binwarp" . 'binwarp:*binwarp-mode-p*))))
    "List of cons cells defining exceptions for key remapping in StumpWM modes.
Each cons cell consists of a string representing a directory name in `d-emacs-directory/pkg-configs/d-stump/`
(without its path) and a symbol representing a mode for which key remappings should be suspended."
    :type 'list
    :group 'd-stump))

(provide 'd-stump-customs)
;;; d-stump-customs.el ends here
