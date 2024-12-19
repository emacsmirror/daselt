;;; d-emacs-org-eval-commands.el --- d-emacs-commands for org  -*- lexical-binding: t; -*-

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

(defun d-emacs-org-insert-superheading (&optional number)
  "Insert a heading one or several levels above the previous one.
By default, go one level up. If a prefix number is given,
that number of levels are gone up."
  (interactive "p")
  (org-insert-heading)
  (left-char)
  (if number
      (delete-char (- number))
    (delete-char -1))
  (right-char))

(defun d-emacs-org-insert-todo-superheading (&optional number)
  "Insert a todo heading one or several levels above the previous one.
By default, go one level up. If a prefix number is given,
that number of levels are gone up."
  (interactive "p")
  (call-interactively #'org-insert-todo-heading)
  (left-word)
  (left-char)
  (if number
      (delete-char (- number))
    (delete-char -1))
  (right-word)
  (insert " "))

(defun d-emacs-org-insert-todo-heading-after-current ()
  "Insert a todo heading one level above the previous one."
  (interactive)
  (org-insert-heading-after-current)
  (org-todo))

(provide 'd-emacs-org-eval-commands)
;;; d-emacs-org-eval-commands.el ends here
