;;; d-dfk-customs.el --- Daselt's Emacs module              -*- lexical-binding: t; -*-

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

;; This file houses the customization options for Daselt in d-emacs.

;;; Code:

(defgroup d-dfk
  nil
  "Customization group for d-dfk."
  :group 'Convenience
  :group 'Daselt
  :prefix "d-dfk")

(with-eval-after-load 'd-xkb-customs
  (defcustom d-dfk-dual-functions-outside-main-keys
    (if (not (eq d-xkb-layout 'd-xkb-main-layout))
        t
      nil)
    "Non-nil if dual functions on main keys are not used.

If you do not use dual functions on the main keys (i.e., keys that usually house
letters), set this variable to t. This variable is non-nil by default unless
`d-xkb-layout` is set to `d-xkb-main-layout`, as most other layouts place
letters on the keys within the reduced layout. Having dual functions on letter
keys can often lead to typing errors."
    :type 'boolean
    :group 'd-dfk))

(provide 'd-dfk-customs)
;;; d-dfk-customs.el ends here
