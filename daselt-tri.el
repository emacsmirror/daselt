;;; daselt-tri.el --- Functions to generate Tridactyl configs  -*- lexical-binding: t; -*-

;; Copyright (C) 2024, 2025  Alexander Prähauser

;; Author: Alexander Prähauser <ahprae@protonmail.com>
;; Version: 1.0
;; Keywords: tools, external
;; URL: https://gitlab.com/nameiwillforget/d-emacs/-/blob/master/daselt-tri/daselt-tri.el

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

;; This package provides the function `daselt-tri-generate-config', which can
;; generate a Tridactyl-keyconfig from a daselt-bindlist, and bindlist
;; housing Daselt's standard Tridactyl config.

;;; Code:

(require 'daselt-base)
(require 'daselt-coords)
(require 'daselt-bind)

;; Automatically generated
(defvar daselt-xkb-layouts)

(defvar daselt-emacs-dir)
(defvar daselt-mode-quick-key-string)
(defcustom daselt-tri-bindlist-file
  (condition-case nil (concat daselt-emacs-dir "tri-configs/daselt-tri.dbl")
    (error nil))
  "The bindlist file from which the Tridactyl config is generated."
  :type  'directory
  :group 'daselt)

(defcustom daselt-tri-config-directory
  nil
  "The directory that the config should be written in."
  :type 'directory
  :group 'daselt)

(defun daselt-tri-generate-config (&optional filename)
  "Generate Daselt's Tridactyl config from the d-tri-bindlists file.

FILENAME should be the name of the file that is generated. It is `d-tri' by
default."
  (declare (ftype (function (&optional string) string)))
  (interactive)
  (let* ((print-level nil)
         (print-length nil)
         (filename (or filename "d-tri"))
         (buffer (find-file (concat daselt-tri-config-directory filename))))
    (delete-region (point-min) (point-max))
    (daselt-dirs-act-on-sexps-in-file
     daselt-tri-bindlist-file
     (lambda () (let* ((blist (daselt-base-read-region))
                  (elblist (mapcar (lambda (bind)
                                     (daselt-bind-elaborate-on-binding bind))
                                   blist))
                  (strlist (mapcar (lambda (elbind)
                                     (let* ((mods (daselt-base-remove-indices (caaar elbind)))
                                            (coords (cdar elbind))
                                            (sfx (if coords
                                                     (daselt-coords-binding coords)
                                                   (cdaar elbind)))
                                            (nsfx (cond ((string= sfx "<")
                                                         "<<")
                                                        ((string= sfx ">")
                                                         ">>")
                                                        (t sfx)))
                                            (value (cdr elbind)))
                                       (concat "bind "
                                               (if mods "<")
                                               (if mods (daselt-bind-modifiers-to-string mods))
                                               nsfx
                                               (if mods ">")
                                               " "
                                               value
                                               "\n")))
                                   elblist)))
             (cl-flet ((frmtstrlst (lststr) (string-replace
                                             "\")" ""
                                             (string-replace
                                              "(\"" ""
                                              (string-replace "\" \"" "" lststr)))))
               (set-buffer buffer)
               (insert (frmtstrlst (format "%S" strlist)))
               (insert (format "set hintchars %S\n" daselt-mode-quick-key-string))
               (insert "set hintchars false")
               (save-buffer)))))))

(defun daselt-tri-generate-all-configs ()
    "Execute `daselt-tri-generate-config' for each layout in `daselt-xkb-layouts'.

Add in layer 0 to each layout first, just to be sure."
    (declare (ftype (function () (list string))))
    (daselt-xkb-generate-layouts)
    (daselt-coords-for-layouts-in (lambda (layoutsym)
                                    (let ((namecore (daselt-base-namecore
                                                   layoutsym "daselt-dfk-" "-layout")))
                                    (daselt-tri-generate-config (concat "d-tri-" namecore))))
                                (mapcar (lambda (layoutsym) (eval `(daselt-dfk-import-layout ,layoutsym)))
                                        daselt-xkb-layouts)))

(provide 'daselt-tri)
;;; daselt-tri.el ends here
