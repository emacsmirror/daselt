;;; d-special-quick-keys-bindlists.el --- Daselt's quick-keys map list.  -*- lexical-binding: t; -*-

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

;; This file houses the map list describing Daselt's quick keys. Note that this is not exactly a fully formed bindlist, but a recipe for one, which has to be evaluated before it can be used.

;;; Code:


;;;; pkg-configs-mode-map
;; Duplicate entries in d-quick-key-coords-base-list unless they are in the middle and add layer coordinates for each layer in d-quick-key-layers-list.
`(apply #'append
        (mapcar
         (lambda (layer)
           (mapcar (lambda (coords)
                     (cons (append (list layer) coords) nil))
                   (let ((runlist))
                     (cl-loop for rightside in d-quick-key-coords-base-list
                              for index from 0
                              for leftside = (list (car rightside)
                                                   (- (nth 1 rightside)))
                              do (setq runlist
                                       (append
                                        runlist
                                        (if (= 0 (nth 1 rightside)) ;If in the middle, just return unduplicated.
                                            (list rightside)
                                          (if (cl-evenp index)
                                              (list leftside rightside)
                                            (list rightside leftside)))))
                              finally return runlist))))
         d-quick-key-layers-list))


(provide 'd-special-quick-keys-bindlists)
;;; d-special-quick-keys-bindlists.el ends here
