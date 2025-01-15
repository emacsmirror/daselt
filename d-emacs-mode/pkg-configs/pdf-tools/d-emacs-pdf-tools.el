;;; d-emacs-pdf-tools.el -- d-emacs-commands for pdf-tools  -*- lexical-binding: t; -*-

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

(defun d-emacs-pdf-view-scroll-chunk-up ()
  "Scroll up a chunk when reading a pdf."
  (interactive)
  (pdf-view-previous-line-or-previous-page 10))

(defun d-emacs-pdf-view-scroll-chunk-down ()
  "Scroll down a chunk when reading a pdf."
  (interactive)
  (pdf-view-next-line-or-next-page 10))

(provide 'd-emacs-pdf-tools)
;;; d-emacs-pdf-tools.el ends here
