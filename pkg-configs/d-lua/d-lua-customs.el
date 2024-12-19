;;; d-lua-customs.el --- Daselt's Emacs module customization options              -*- lexical-binding: t; -*-

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

(defgroup d-lua
  nil
  "`d-lua'-customization group."
  :group 'Daselt)

(defcustom d-lua
  nil
  "Set to t if you want to use d-lua.
Useful if you want to write TeX-documents."
  :type 'boolean
  :group 'd-lua)

(defcustom d-lua-insert-normpairs
  d-emacs-avy
  "If non-nil, insert a norm-pair yasnippet for specific inputs.
Inserts a norm-pair yasnippet when typing `(7 0 -5)` and `(7 0 5)`.
Refer to `d-lua-insert-normpair-yasnippet` for details."
  :group 'd-lua
  :type 'boolean)

(defcustom d-lua-abbrev-math
  t
  "If non-nil, abbreviate certain LaTeX math commands in Daselt.
When enabled, commands like `mathopen' and `mathclose' are replaced
with `mo' and `mc', respectively, while `mathlarger' and 
`mathsmaller' are replaced with `ml' and `ms', making the LaTeX 
buffer more legible. This is useful if you are using DLua or have 
defined the following in your LaTeX configuration:

\\newcommand{\\mo}{\\mathopen}
\\newcommand{\\mc}{\\mathclose}
\\newcommand{\\ml}{\\mathlarger}
\\newcommand{\\ms}{\\mathsmaller}"
  :group 'd-lua
  :type 'boolean)

(defcustom d-lua-envs d-lua-default-envs
  "List of TeX environments for Daselt."
  :group 'd-lua
  :type '(repeat string))

(defcustom d-lua-add-tex-envs
  t
  "If non-nil, allows `add-tex-envs' to generate environments from `d-tex-envs'.
This option must be enabled together with `d-add-tex-envs'."
  :group 'd-lua
  :type 'boolean)

(provide 'd-lua-customs)
;;; d-lua-customs.el ends here
