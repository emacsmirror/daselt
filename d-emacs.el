;;; d-emacs.el --- d-emacs main file                 -*- lexical-binding: t; -*-

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

;; Main file for d-emacs. Only loads other files.

;;; Code:


(require 'd-functions nil t)
(require 'd-constants nil t)
(require 'd-customs nil t)
(require 'd-commands nil t)
(require 'd-modes nil t)
(require 'd-macroexpansions nil t)
(require 'd-dfk-customs nil t)
(require 'd-xkb-functions nil t)
(require 'd-xkb-customs nil t)
(require 'd-xkb-constants nil t)
(require 'd-xkb-commands nil t)
(require 'd-emacs-functions nil t) 
(require 'd-emacs-customs nil t)
(require 'd-emacs-constants nil t)
(require 'd-emacs-commands nil t)
(require 'd-emacs-maps nil t)
(require 'd-emacs-modes nil t)
(require 'd-stump-customs nil t)
(require 'd-stump-constants nil t)
(require 'd-stump-functions nil t)
(require 'd-lua-constants nil t)
(require 'd-lua-customs nil t)
(require 'd-lua-commands nil t)
(require 'd-lua-functions nil t)
(require 'd-tri-customs nil t)
(require 'd-tri-functions nil t)

(provide 'd-emacs)
;;; d-emacs.el ends here
