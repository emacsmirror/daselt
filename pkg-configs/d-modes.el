;;;  d-modes.el --- Daselt's Emacs module              -*- lexical-binding: t; -*-

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

;;  Daselt modes.

;;; Code:

(define-derived-mode d-bindlists-mode emacs-lisp-mode "d-bindlists"
  "A mode for editing Daselt bindlists."
  (if d-sort-bindlists-at-file-save
      (add-hook 'before-save-hook  #'d--sort-and-format-bindlists-in-file 97 t))
  (if d-save-bindlists-at-file-save
      (add-hook 'before-save-hook #'d--save-bindlists-in-file-as-variables 98 t))
  (if (and d-apply-bindlists-at-file-save (d--regular-file-p (buffer-file-name)))
      (add-hook 'before-save-hook
                #'d-emacs--with-eval-backup-and-apply-bindlists-in-file
                99 t)))

(add-to-list 'auto-mode-alist
             `(,(rx "d-" (zero-or-more (not "/")) "-bindlists.el" string-end)
               . d-bindlists-mode))

(provide 'd-modes)
;;; d-modes.el ends here
