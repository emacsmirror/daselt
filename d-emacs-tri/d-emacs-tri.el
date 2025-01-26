;;; d-emacs-tri.el -- d-tri functions           -*- lexical-binding: t; -*-

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

;; This package provides the function `d-emacs-tri-generate-config', whichcan
;; generate a Tridactyl-keyconfig from a d-emacs-bindlist, and bindlist
;; housing Daselt's standard Tridactyl config.

;;; Code:

(defcustom d-emacs-tri-bindlist-file
  (condition-case nil (concat (file-name-directory (buffer-file-name)) "d-emacs-tri.dbl")
    (error nil))
  "The bindlist file from which the Tridactyl config is generated."
  :type  'directory
  :group 'd-emacs)

(defcustom d-emacs-tri-config-directory
  nil
  "The directory that the config should be written in."
  :type 'directory
  :group 'd-emacs)

(defun d-emacs-tri-generate-config (&optional filename)
  "Generate Daselt's Tridactyl config from the d-tri-bindlists file.

FILENAME should be the name of the file that is generated. It is `d-tri' by
default."
  (declare (ftype (function (&optional string) string)))
  (interactive)
  (let* ((print-level nil)
         (print-length nil)
         (filename (or filename "d-tri"))
         (buffer (find-file (concat d-emacs-tri-config-directory filename))))
    (delete-region (point-min) (point-max))
    (d-emacs-dirs-act-on-sexps-in-file
     d-emacs-tri-bindlist-file
     (lambda () (let* ((blist (d-emacs-base-read-region))
                  (elblist (mapcar (lambda (bind)
                                     (d-emacs-bind-elaborate-on-binding bind))
                                   blist))
                  (strlist (mapcar (lambda (elbind)
                                     (let* ((mods (d-emacs-base-remove-indices (caaar elbind)))
                                            (coords (cdar elbind))
                                            (sfx (if coords
                                                     (d-emacs-coords-binding coords)
                                                   (cdaar elbind)))
                                            (nsfx (cond ((string= sfx "<")
                                                         "<<")
                                                        ((string= sfx ">")
                                                         ">>")
                                                        (t sfx)))
                                            (value (cdr elbind)))
                                       (concat "bind "
                                               (if mods "<")
                                               (if mods (d-emacs-bind-modifiers-to-string mods))
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
               (save-buffer)))))))

(defun d-emacs-tri-generate-all-configs ()
  "Execute `d-emacs-tri-generate-config' for each layout in `d-emacs-xkb-layouts'.

Add in layer 0 to each layout first, just to be sure."
  (declare (ftype (function () (list string))))
  (d-emacs-coords-for-layouts-in (lambda (layoutsym)
                                   (let ((namecore (d-emacs-base-namecore
                                                    layoutsym "d-emacs-dfk-" "-layout")))
                                     (d-emacs-tri-generate-config (concat "d-tri-" namecore))))
                                 (mapcar (lambda (layoutsym) (eval `(d-emacs-dfk-import-layout ,layoutsym)))
                                         d-emacs-xkb-layouts)))

(provide 'd-emacs-tri)
;;; d-emacs-tri.el ends here
