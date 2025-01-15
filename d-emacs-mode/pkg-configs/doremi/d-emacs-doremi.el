;;; d-emacs-doremi.el -- d-emacs-functions for d-emacs-doremi  -*- lexical-binding: t; -*-

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

;; This file contains functions for functions used in doremi. If daselt-doremi is t, it is parsed automatically when daselt-mode is started or doremi is evaluated, depending on what comes first. Each element in this file should be a function or macro definition.

;;; Code:

(defun d-emacs-doremi--set-key-customs-from-bindlists ()
  "Append to doremi customs their entries in d-emacs bindlists in this directory
  that contain \"customs\" in their name."
  (d-emacs-dirs-recurse-through-directory
   (lambda (filepath) (d-emacs-dirs-act-on-sexps-in-file
                       filepath
                       (lambda () 
                         (mapcar (lambda (bind)
                                   (let* ((cust (cdr bind))
                                          (bindstr (d-emacs-bind-string bind t t))
                                          (bindkbd (kbd bindstr))
                                          (custbup (intern (concat "d-emacs-" (symbol-name
                                                                               cust)
                                                                   "-backup"))))
                                     (if (bound-and-true-p custbup)
                                         (add-to-list cust bindkbd)
                                       (progn (set custbup cust)
                                              (add-to-list cust bindkbd)))))
                                 blist)
                         )))
   (concat d-emacs-directory "pkg-configs/d-emacs/other/doremi/")
   (lambda (filepath) (and (d--bindlists-p filepath)
                           (string-match-p "customs" (file-name-base filepath))))))

(provide 'd-emacs-doremi)
;;; d-emacs-doremi.el ends here
