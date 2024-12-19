 ;;; d-emacs-avy-act-constants.el --- Daselt's avy-act module constants             -*- lexical-binding: t; -*-

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

;;  d-emacs avy-act variables. The contents of this file are conses consisting of the name of the variable and its contents under Daselt. This file is later parsed and all variables in it are backed up and re-set. To add a new variable, it suffices to put it in this file.

`(avy-act-recenter-at-cur-line-keys
  . ,(flatten-list
      (d--act-on-bindlists-in-file
       (concat (file-name-directory (buffer-file-name)) "d-emacs-avy-act-special-recenter-bindlists.el")
       (lambda () (let ((blist (d--extract-bindlist)))
               (mapcar (lambda (bind)
                         (let ((kbdbind (kbd (d--extract-binding-string bind t t))))
                           (if (stringp kbdbind) ; Allow non-characters to also be bound.
                               (string-to-char kbdbind)
                             kbdbind)))
                       blist))))))

(provide 'd-emacs-avy-act-constants)
;; d-emacs-avy-act-constants.el ends here.
