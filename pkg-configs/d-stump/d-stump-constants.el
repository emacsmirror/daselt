;;; d-stump-constants.el --- Daselt's Emacs module              -*- lexical-binding: t; -*-

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

;; This file houses the d-stump-constants for Daselt.

;;; Code:

(defconst d-stump-modules
  '("binwarp" "spatial-groups" "swm-emacs" "winner-mode" "clipboard-history" "pamixer" "screenshot-maim" "acpi-backlight" "notifications")
  "Modules for which d-stump-configurations exist.")

(with-eval-after-load 'd-stump-customs
  (if d-stump
      (defconst d-stump-emacs-key-translations-alist
        (let* ((base-file-path "pkg-configs/d-stump/stumpwm/d-stump-remapped-keys-special-bindlists.el")
               (user-file-path "pkg-configs/d-stump/stumpwm/d-stump-remapped-keys-user-defined-special-bindlists.el")
               (file-path (if (file-exists-p (concat d-emacs-directory user-file-path))
                              user-file-path
                            base-file-path))

               (blist (car (remq nil (d--act-on-bindlists-in-file
                                      (concat d-emacs-directory file-path)
                                      (lambda () (let* ((blist (eval (d-read-region)))
                                                   (head (d-head-if-exists blist)))
                                              (if (string= head "emacs")
                                                  blist)))))))
               (body (cdr blist))
               (transconses
                (mapcar (lambda (bind)
                          (cons (d--extract-binding-string bind)
                                (let* ((val (cdr bind))
                                       (formval (if (member val
                                                            d-xkb-special-key-names)
                                                    (d-xkb--format-special-key val)
                                                  val)))
                                  formval)))
                        body)))

          transconses)
        "Alist of key combinations that are translated by StumpWM before they reach Emacs. Automatically generated from the contents of the remapped-keys-file.")))

(provide 'd-stump-constants)
;;; d-stump-constants.el ends here
