;;; d-lua-eval-commands.el --- Daselt's Emacs module              -*- lexical-binding: t; -*-

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

;;  d-lua commands.

;;; Code:

(defun d-lua-kill-ring-delete-dollars ()
  "This function deletes dollar signs in the highest entry in the kill ring."
  (interactive)
  (let ((new
         (string-replace "$" nil (car kill-ring))))
    (kill-new new t)))

(defun d-lua-kill-ring-delete-dollars-number (x)
  "This function deletes dollar signs in the xth entry in the kill ring."
  (interactive "n  Number:")
  (let ((new
         (string-replace "$" nil (nth x kill-ring))))
    (kill-new new t)))

(defalias 'd-lua-indent-item
    (kmacro "<kp-enter> C-S-<backspace> C-a")
    "This macro automatically re-indents items and similar entries in LaTeX-mode.")


(defun d-lua-isearchp-within-mathmode (a)
  "This function does a search restricted to mathmode."
  (interactive)
  (isearchp-add-filter-predicate check-math-mode)
  (call-interactively #'isearch-forward))

(defun d-lua-remove-modifier (ENV)
  "This command removes a TeX modifier."
  (interactive "sEnvironment: ")
  (replace-regexp (format "\\\\%s{\\(.*?\\)}" ENV) "\\1"))

;; From here: https://www.reddit.com/r/emacs/comments/5f99nv/help_with_auctex_how_to_delete_an_environment/
(defun d-lua-LaTeX-delete-environment-outside ()
  (interactive)
  (when (LaTeX-current-environment)
    (save-excursion
      (let* ((begin-start (save-excursion
                            (LaTeX-find-matching-begin)
                            (point)))
             (begin-end (save-excursion
                          (goto-char begin-start)
                          (search-forward-regexp "begin{.*?}")))
             (end-end (save-excursion
                        (LaTeX-find-matching-end)
                        (point)))
             (end-start (save-excursion
                          (goto-char end-end)
                          (1- (search-backward-regexp "\\end")))))
        ;; delete end first since if we delete begin first it shifts the
        ;; location of end
        (delete-region end-start end-end)
        (delete-region begin-start begin-end)))))

(defun d-lua-change-mathvar (a b)
  "This function changes a variable that stands on its own in mathmode."
  (interactive "sfrom: \nsto: ")
  (while (re-search-forward
          "\\(\\\\(\\|\\\\\\[\\|[^\\\\]\$\$?\\|\\\\begin{equation}\\|\\\\begin{align}\\)" nil 1)
    (query-replace-regexp a  b t  (point) 
                          (progn (re-search-forward 
                                  "\\(\\\\)\\|\\\\\\]\\|[^\\\\]\$\$?\\|\\\\end{equation}\\|\\\\end{align}\\)" nil 1) (point)))))

(defun d-lua-change-mathvar-withinstring (a b)
  "This function changes a variable that appears within a string in mathmode."
  (interactive "sfrom: \nsto: ")
  (goto-char (point-min))
  (while (re-search-forward
          "\\(\\\\(\\|\\\\\\[\\|[^\\\\]\$\$?\\|\\\\begin{equation}\\|\\\\begin{align}\\)" nil 1)
    (query-replace-regexp a  b nil  (point) 
                          (progn (re-search-forward 
                                  "\\(\\\\)\\|\\\\\\]\\|[^\\\\]\$\$?\\|\\\\end{equation}\\|\\\\end{align}\\)" nil 1) (point)))))


(defun d-lua-insert-normpair-yas-snippet (&optional ARG)
  "This command inserts a correctly spaced norm pair yas-snippet. If the region is active
  and usable, it inserts the norm pair around the region. It is bound to ∣ in LaTeX-mode,
  because their spacing otherwise is awful.

  Without a prefix argument, it inserts a snippet like \\mathopen{∣}-\\mathclose{∣}. If
  d-lua-abbrev-math is true, it inserts an abbreviation using mo and mc instead of
  mathopen and mathclose. This only makes sense if you have something like

\\newcommand{\\mo}{\\mathopen} \\newcommand{\\mc}{\\mathclose}

in your LaTeX-config or are using DLua.

With a universal prefix argument C-u it just inserts a normal norm sign. Take out of
d-LaTeX-Mode-Bindlist if you don't want that.

With -2 as a prefix, it inserts a snippet like
\\mo{\\ms{\\ms{∣}}}${1:-}\\mc{\\ms{\\ms{∣}}}.

With -1 as a prefix, it inserts a snippet like \\mo{\\ms{∣}}${1:-}\\mc{\\ms{∣}}.

With 0 as a prefix, it inserts a snippet like \\left\\vert${1:-}\\right\\vert.

With 2 as a prefix, it inserts \\big\\vert${1:-}\\big\\vert.

With 3 as a prefix, it inserts \\bigg\\vert${1:-}\\bigg\\vert."
  (interactive "p")
  (let ((snippet (if d-lua-abbrev-math
                     (cond ((= ARG -2)
                            "\\mo{\\ms{\\ms{∣}}}${1:-}\\mc{\\ms{\\ms{∣}}}")
                           ((= ARG -1)
                            "\\mo{\\ms{∣}}${1:-}\\mc{\\ms{∣}}")
                           ((= ARG 0)
                            "\\left\\vert${1:-}\\right\\vert")
                           ((= ARG 2)
                            "\\big\\vert${1:-}\\big\\vert")
                           ((= ARG 3)
                            "\\bigg\\vert${1:-}\\bigg\\vert")
                           (t "\\mo{∣}${1:-}\\mc{∣}"))
                   (cond ((= ARG -2)
                          "\\mathsmaller{\\mathsmaller{\\mathopen{∣}}}${1:-}\\mathsmaller{\\mathsmaller{\\mathclose{∣}}}")
                         ((= ARG -1)
                          "\\mathsmaller{\\mathopen{∣}}${1:-}\\mathsmaller{\\mathclose{∣}}")
                         ((= ARG 0)
                          "\\left\\vert${1:-}\\right\\vert")
                         ((= ARG 2)
                          "\\big\\vert${1:-}\\big\\vert")
                         ((= ARG 3)
                          "\\bigg\\vert${1:-}\\bigg\\vert")
                         (t "\\mathopen{∣}${1:-}\\mathclose{∣}")))))
    (cond ((eq ARG 4)
           (self-insert-command 1))
          ((use-region-p)
           (progn
             (call-interactively #'kill-region)
             (yas-expand-snippet snippet)
             (yank)))
          (t (yas-expand-snippet snippet)))))

(defun d-lua-insert-othernormpair-yas-snippet (&optional ARG)
  "This command inserts a correctly spaced ∥-norm pair yas-snippet. If the region is
  active and usable, it inserts the norm pair around the region. It is bound to ∥ in
  LaTeX-mode, because their spacing otherwise is awful.

  Without a prefix argument, it inserts a snippet like \\mathopen{∥}-\\mathclose{∥}. If
  d-lua-abbrev-math is true, it inserts an abbreviation using mo and mc instead of
  mathopen and mathclose. This only makes sense if you have something like

\\newcommand{\\mo}{\\mathopen} \\newcommand{\\mc}{\\mathclose}

in your LaTeX-config or are using DLua.

With a universal prefix argument C-u it just inserts a normal norm sign. Take out of
d-LaTeX-Mode-Bindlist if you don't want that.

With -2 as a prefix, it inserts a snippet like
\\mo{\\ms{\\ms{∥}}}${1:-}\\mc{\\ms{\\ms{∥}}}.

With -1 as a prefix, it inserts a snippet like \\mo{\\ms{∥}}${1:-}\\mc{\\ms{∥}}.

With 0 as a prefix, it inserts a snippet like \\left\\|${1:-}\\right\\|.

With 2 as a prefix, it inserts \\big\\|${1:-}\\big\\|.

With 3 as a prefix, it inserts \\bigg\\|${1:-}\\bigg\\|."
  (interactive "p")
  (let ((snippet (if d-lua-abbrev-math
                     (cond ((= ARG -2)
                            "\\mo{\\ms{\\ms{∥}}}${1:-}\\mc{\\ms{\\ms{∥}}}") 
                           ((= ARG -1)
                            "\\mo{\\ms{∥}}${1:-}\\mc{\\ms{∥}}")
                           ((= ARG 0)
                            "\\left\\|${1:-}\\right\\|")
                           ((= ARG 2)
                            "\\big\\|${1:-}\\big\\|")
                           ((= ARG 3)
                            "\\bigg\\|${1:-}\\bigg\\|")
                           (t "\\mo{∥}${1:-}\\mc{∥}"))
                   (cond ((= ARG -2)
                          "\\mathopen{\\mathsmaller{\\mathsmaller{∥}}}${1:-}\\mathclose{\\mathsmaller{\\mathsmaller{∥}}}")
                         ((= ARG -1)
                          "\\mathopen{\\mathsmaller{∥}}${1:-}\\mathclose{\\mathsmaller{∥}}")
                         ((= ARG 0)
                          "\\left\\|${1:-}\\right\\|")
                         ((= ARG 2)
                          "\\big\\|${1:-}\\big\\|")
                         ((= ARG 3)
                          "\\bigg\\|${1:-}\\bigg\\|")
                         (t "\\mathopen{∥}${1:-}\\mathclose{∥}")))))
    (cond ((eq ARG 4)
           (self-insert-command 1))
          ((use-region-p)
           (progn
             (call-interactively #'kill-region)
             (yas-expand-snippet snippet)
             (yank)))
          (t (yas-expand-snippet snippet)))))

(provide 'd-lua-eval-commands)
;;; d-lua-eval-commands.el ends here
