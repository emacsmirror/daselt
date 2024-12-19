 ;;; d-emacs-org-constants.el --- Daselt's Emacs module org-mode constants             -*- lexical-binding: t; -*-

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

;;  d-emacs org variables. The contents of this file are conses consisting of the name of the variable and its contents under Daselt. This file is later parsed and all variables in it are backed up and re-set. To add a new variable, it suffices to put it in this file.

`(org-latex-compiler . "lualatex")
`(org-preview-latex-default-process . 'imagemagic-lua)
`(org-use-speed-commands . t)
;; `(org-speed-commands
;;   . ,(d--act-on-bindlists-in-file (concat d-emacs-directory "pkg-configs/d-emacs/emacs-native/org/d-emacs-org-speed-commands-special-bindlists.el")
;;                                   (lambda () (let ((blist (d--extract-bindlist)))
;;                                           (mapcar (lambda (binding)
;;                                                     (cons (d--extract-binding-string binding)
;;                                                           (cdr binding)))
;;                                                   blist)))))

(provide 'd-emacs-org-constants)
;; d-emacs-org-constants.el ends here.
