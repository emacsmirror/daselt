;;; d-tri-functions.el --- d-tri functions           -*- lexical-binding: t; -*-

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

(defun d-tri--generate-config ()
  "Generate Daselt's Tridactyl config from the d-tri-bindlists file."
  (interactive)
  (let* ((bindfile (concat d-emacs-directory "pkg-configs/d-tri/d-tri-bindlists.el"))
         (configfile (concat d-directory "d-tri"))
         (print-level nil)
         (print-length nil)
         (buffer (find-file configfile)))
    (delete-region (point-min) (point-max))
    (d--act-on-bindlists-in-file
     bindfile
     (lambda () (let* ((blist (d--extract-bindlist))
                       (elblist (mapcar (lambda (bind)
                                          (d--elaborate-on-binding bind))
                                        blist))
                       (strlist (mapcar (lambda (elbind)
                                          (let* ((mods (d-remove-indices (caaar elbind)))
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
                                                    (if mods (d-string-together-modifiers mods))
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
                    (insert (frmtstrlst (format "%S" strlist)))))))))

(provide 'd-tri-functions)
;;; d-tri-functions.el ends here
