;;; d-lua-eval.el --- Code that should be evaluated for d-lua  -*- lexical-binding: t; -*-

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

(add-hook 'LaTeX-mode-hook 'd-lua-add-to-prettify)
(if d-lua-add-tex-envs (with-eval-after-load 'cdlatex
                         (with-eval-after-load 'auctex
                           (with-eval-after-load 'reftex
                             (d-lua-add d-lua-envs)))))

(provide 'd-lua-eval)
;;; d-lua-eval.el ends here
