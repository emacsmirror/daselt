;;; d-emacs-cdlatex-constants.el --- Daselt's cdlatex module              -*- lexical-binding: t; -*-

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

;;  d-emacs cdlatex variables. The contents of this file are conses consisting of the name of the variable and its contents under Daselt. This file is later parsed and all variables in it are backed up and re-set. To add a new variable, it suffices to put it in this file.

;;; Code:
`(cdlatex-math-symbol-alist-default
  . ,(let ((filepath (concat d-emacs-directory "pkg-configs/d-emacs/cdlatex/d-emacs-cdlatex-special-math-symbol-bindlists.el")))
       (prog1 (d--act-on-bindlists-in-file
               filepath
               (lambda () (let ((blist (d--extract-bindlist)))
                            (mapcar (lambda (binding)
                                      (list (string-to-char
                                             (d--extract-binding-string binding))
                                            (cdr binding)))
                                    blist))))
         (unless (or d-emacs-debug d-emacs-keep-read-buffers) (kill-buffer (get-file-buffer filepath))))))

`(cdlatex-math-modify-alist-default
  . ,(let ((filepath (concat d-emacs-directory "pkg-configs/d-emacs/cdlatex/d-emacs-cdlatex-special-math-modify-bindlists.el")))
       (prog1 (d--act-on-bindlists-in-file
               filepath
               (lambda () (let ((blist (d--extract-bindlist)))
                            (mapcar (lambda (binding)
                                      (cons (string-to-char
                                             (d--extract-binding-string binding))
                                            (cdr binding)))
                                    blist))))
         (unless (or d-emacs-debug d-emacs-keep-read-buffers) (kill-buffer (get-file-buffer filepath))))))

 ;;; d-emacs-cdlatex-constants.el ends here
