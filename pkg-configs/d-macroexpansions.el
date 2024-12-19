;;; d-macroexpansions.el --- Functions generated using Macro Expansions for Daselt  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Alexander Prähauser

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

(require 'd-functions)
(require 'd-constants)

(cl-loop for typestr in d-pkg-file-types-list
         do (eval
             `(defun ,(intern (concat "d--" typestr "-p")) (filepath)
                ,(format "This function matches the filepaths of Daselt-%s-files, meaning .el-files whose base name ends with \"%s\" and that don't contain # or ~." typestr typestr)
                (and (d--standard-el-file-p filepath)
                     (string-match-p (rx ,typestr string-end) (file-name-base filepath))))))

(cl-loop for modstr in d-pkg-type-modifiers-list
         do (eval
             `(defun ,(intern (concat "d--" modstr "-file-p")) (filepath)
                ,(format "This function matches the filepaths of %s Daselt-files, meaning .el-files whose base name contains \"%s\" and that don't contain # or ~." modstr modstr)
                (and (d--standard-el-file-p filepath)
                     (string-match-p ,modstr (file-name-base filepath)))))

         finally do (defun d--regular-file-p (filepath)
                      "Match the filepaths of regular Daselt-files.
This means .el-files whose base name doesn't contain \"special\", # or ~."
                      (and (d--standard-el-file-p filepath)
                           (not (string-match-p "special" (file-name-base filepath))))))

(provide 'd-macroexpansions)
;;; d-macroexpansions.el ends here
