;;; d-stump-stumpwm-init-functions.el --- bindlists for the d-stump init.  -*- lexical-binding: t; -*-

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

;; This file contains the functions that should be put into the StumpWM init file.

;;; Code:

(defun d-cl-forall-p (list)
  "This function returns LIST if and only if all of its members fulfill PREDICATE otherwise it returns nil."
  (if (member nil list)
      nil
      t))

(defun d-stump-test-for-modes (modes)
  "Test whether all MODES are active."
  (d-cl-forall-p (mapcar (lambda (mode) (symbol-value mode))
                         modes)))

(defun d-stump-test-for-window-name-and-modes (win names modes)
  "This common-lisp function tests whether the window-class of WIN is a member of the list NAMES using string-equal. If MODES are given, it also tests whether MODES are true."
  (and (member (window-class win)
               names
               :test #'string-equal)
       (if modes (d-stump-test-for-modes modes)
           t))) 

;; (eval `(defun d-stump--letter-to-number (str)
;;          (cond ((string= ,(d-xkb--binding-from-coords '(1 0 -2)) str) 0)
;;                ((string= ,(d-xkb--binding-from-coords '(1 0 2)) str) 1)
;;                ((string= ,(d-xkb--binding-from-coords '(1 0 -3)) str) 2)
;;                ((string= ,(d-xkb--binding-from-coords '(1 0 3)) str) 3)
;;                ((string= ,(d-xkb--binding-from-coords '(1 0 -4)) str) 4)
;;                ((string= ,(d-xkb--binding-from-coords '(1 0 4)) str) 5)
;;                ((string= ,(d-xkb--binding-from-coords '(1 0 -5)) str) 6)
;;                ((string= ,(d-xkb--binding-from-coords '(1 0 5)) str) 7)
;;                ((string= ,(d-xkb--binding-from-coords '(1 0 -1)) str) 8)
;;                ((string= ,(d-xkb--binding-from-coords '(1 0 1)) str) 9))))

(defun d-stump--letter-to-number (str) (cond ((string= "a" str) 0) ((string= "t" str) 1) ((string= "e" str) 2) ((string= "r" str) 3) ((string= "i" str) 4) ((string= "n" str) 5) ((string= "h" str) 6) ((string= "s" str) 7) ((string= "o" str) 8) ((string= "d" str) 9)))

;;; modes-stump-stumpwm-init-functions.el ends here

