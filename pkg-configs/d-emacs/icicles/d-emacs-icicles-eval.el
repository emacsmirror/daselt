;;;  d-emacs-icicles-eval.el ---  Evaluated code for Daselt's Icicles Module      -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Alexander Prähauser

;; Author: Alexander Prähauser <ahprae@protonmail.com>
;; Keywords: convenience, extensions

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

(d-emacs-icicles--set-key-customs-from-bindlists)
(add-hook 'd-emacs-mode-hook (lambda () (unless d-emacs-mode (d-emacs-icicles--reset-key-customs-from-backups))))

(provide ' d-emacs-icicles-eval)
;;;  d-emacs-icicles-eval.el ends here
