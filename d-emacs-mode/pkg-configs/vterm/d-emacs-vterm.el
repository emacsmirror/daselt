;;; d-emacs-vterm.el -- d-emacs-modes for vterm-maps  -*- lexical-binding: t; -*-

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

;; This file contains maps for maps used in vterm. If d-vterm is t, it is parsed automatically when d-emacs-mode is started or vterm is evaluated, depending on what comes first. Each element in this file should be a minor mode.

;;; Code:
(defcustom d-vterm-insert-exclude-key-combinations-list
  `("C-`" "C-χ")
  "List of key combinations that should not be inserted into vterm when d-vterm-insert-mode is on. Should at least contain \"C-`\"."
  :type 'list)

(define-minor-mode d-vterm-insert-mode
  "This mode is used by Daselt to pass through keys to vterm. When it is active, its map overrides all regular maps."
  :lighter vt-ins
  :keymap '(("C-`" #'d-vterm-insert-mode)
            ("<kp-enter>" #'vterm--self-insert)
            ("RET" #'vterm--self-insert)
            ("<XF86Launch5>" #'vterm-copy-mode))
  (if d-vterm-insert-mode
      (progn (d-vterm-configure-insert
              (d-emacs-complement d-C-key-combinations-list
                                  d-vterm-insert-exclude-key-combinations-list))
             (add-to-list 'emulation-mode-map-alists
                          `((d-vterm-insert-mode . ,d-vterm-insert-mode-map))))))

(provide 'd-emacs-vterm)
;;; d-emacs-vterm.el ends here
