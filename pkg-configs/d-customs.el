;;; d-customs.el --- Daselt's Emacs module              -*- lexical-binding: t; -*-

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

(defgroup Daselt
  nil
  "Customization group for Daselt."
  :group 'Convenience
  :group 'External
  :prefix "d-")

(defcustom d-directory
      (condition-case nil (file-name-parent-directory
                       (file-name-parent-directory (file-name-directory (buffer-file-name))))

    ;; During startup, the file of a buffer can't be found, thus this failsave.
    (error (cl-loop for loaded in load-history
                    for loadpath = (car loaded)
                    if (string-match "/Daselt/" loadpath)
                    do (cl-return (substring loadpath 0 (match-end 0))))))
      "Directory where Daselt is housed.

This directory should include a trailing slash.

By default, it is set to the grandparent directory of the buffer containing the 
custom. If this is nil during startup, it is derived from `load-history` by
looking for \"Daselt\"."
      :group 'Daselt 
      :type 'directory)



(defcustom d-quick-key-coords-base-list
  '((0 2) (0 3) (-1 3) (0 4) (-1 4) (0 1) (-1 2) (0 5) (1 1) (1 2) (1 3) (-1 5) (1 4) (-1 1) (1 0))
  "Coordinates of the right-hand keys for quick-key selections.

This list is used in `d-special-quick-keys-bindlists' for generating bindlists
used in quick-key selections. The left-hand keys and coordinates from
`d-quick-key-layers-list' are added in a way that ensures balanced
distribution."
  :type '(repeat coords)
  :group 'Daselt)

(defcustom d-quick-key-layers-list
  (d-emacs-cardinal 7 t)
  "List of key layers for generating quick-key selection bindlists.

Used by `d-special-quick-keys-bindlists` to create constants for quick-key 
selection mechanisms."
  :type 'boolean)

(defcustom d-emacs-debug
  nil
  "Enable debugging options in Daselt.

When non-nil, functions will print additional debugging messages."
  :type 'boolean
  :group 'Daselt)

(defcustom d-emacs-keep-read-buffers
  nil
  "Keep buffers open when starting `d-emacs-mode`.

If non-nil, previously read buffers will not be closed."
  :type 'boolean
  :group 'Daselt)

(defcustom d-mention-unmatched
  nil
  "Notify when a suffix in a keybind is not in `d-emacs-xkb-layout'.

Useful for users who import their keybinds, as it highlights unmatched suffixes."
  :type 'boolean
  :group 'Daselt)

(defcustom d-sort-save-and-apply-bindlists-at-file-save
  t
  "Default behavior for bindlists on file save.

Sets `d-sort-bindlists-at-file-save`, `d-save-bindlists-at-file-save` and 
`d-emacs-apply-regular-bindlists-at-file-save` to t, unless overwritten by
the user."
  :type 'boolean
  :group 'Daselt)

(dolist (str '("sort" "save" "apply"))
  (eval `(defcustom ,(intern (concat "d-" str "-bindlists-at-file-save"))
           d-sort-save-and-apply-bindlists-at-file-save
           "Whenever a `bindlists'-file is saved, ,str the bindlists in it."
           :type 'boolean
           :group 'Daselt)))

(defcustom d-show-tutorial
  t
  "Show the Daselt tutorial when `d-emacs-mode' is started.

If non-nil, the tutorial will be displayed upon entering the mode."
  :type 'boolean
  :group 'Daselt)

(provide 'd-customs)
;;; d-customs.el ends here
