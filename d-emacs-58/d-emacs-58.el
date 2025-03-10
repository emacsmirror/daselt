;;; d-emacs-58.el -- Functions for the Daselt-config for the Lily58  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Alexander Prähauser

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

;; 

;;; Code:

(defgroup d-emacs-58
  nil
  "Group for d-emacs-58."
  :group d-emacs)

(defcustom d-emacs-58-directory
  (concat d-emacs-directory "d-emacs-58/")
  "Directory for d-emacs-58."
  :type 'directory
  :group 'd-emacs-58)

(defun d-emacs-58-dfk ()
  "Generate the Lily58-config for `dual-function-keys'."
  (interactive)
  (d-emacs-dfk-generate-config
   (car (d-emacs-dirs-act-on-sexps-in-file (concat d-emacs-58-directory
                                                   "d-emacs-58.dbl")
                                           #'d-emacs-base-read-region))
   "d-dfk-58")) 

(provide 'd-emacs-58)
;;; d-emacs-58.el ends here
