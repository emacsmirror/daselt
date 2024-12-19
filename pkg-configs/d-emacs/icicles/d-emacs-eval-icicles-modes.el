;;; d-emacs-eval-icicles-modes.el --- d-emacs-modes for icicles-modes  -*- lexical-binding: t; -*-

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

;; This file contains modes for modes used in icicles. If daselt-icicles is t, it is parsed automatically when daselt-mode is started or icicles is evaluated, depending on what comes first. Each element in this file should be a minor mode.

;;; Code:

;; (define-minor-mode d-emacs-icicle-mode
;;   "Mode for Daselt's Icicle implementation."
;;   :interactive t
;;   :global t
;;   (if (bound-and-true-p d-emacs-icicle-mode)
;;       (progn
;;         ;; the keymaps in `emulation-mode-map-alists' take precedence over
;;         ;; `minor-mode-map-alist'
;;         (add-to-list 'emulation-mode-map-alists
;;                      `((d-emacs-icicle-minibuffer-mode
;;                         . ,d-emacs-icicle-minibuffer-mode-map)))
;;         (add-hook 'minibuffer-mode-hook #'d-emacs-icicle-minibuffer-mode))
;;     (remove-hook 'minibuffer-mode-hook #'d-emacs-icicle-minibuffer-mode)))

;; (define-minor-mode d-emacs-icicle-minibuffer-mode
;;   "Mode for Daselt's Icicle implementation."
;;   :interactive nil)

(provide 'd-emacs-eval-icicles-modes)
;;; d-emacs-eval-icicles-modes.el ends here
