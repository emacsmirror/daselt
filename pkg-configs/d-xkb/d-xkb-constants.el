;;; d-xkb-constants.el --- Constants for Daselt's Xkb module              -*- lexical-binding: t; -*-

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

;;  d-xkb variables.

;;; Code:

;;;; Preamble:

(require 'd-xkb-functions)

(defconst d-xkb-layer-numbers-list
  (d-cardinal 8 t)
  "Number of layers in d-xkb-layouts.")

(defconst d-xkb-special-key-names
  '("Delete" "BackSpace" "Tab" "Escape" "Print" "Space" "Up" "Left" "Right" "Down" "Home" "End")
  "List of control characters and function key names used in Daselt.")

(defconst d-xkb-remaining-char-mappings
  '(("question" . "?") ("quotedbl" . "\"") ("exclam" . "!") ("grave" . "`") ("acute" . "´") ("asciitilde" . "~") ("bar" . "|") ("percent" . "%") ("equal" . "=") ("dollar" . "$") ("EuroSign" . "€") ("ellipsis" . "…") ("parenleft" . "(") ("numbersign" . "#") ("braceleft" . "{") ("bracketleft" . "\[") ("emdash" . "—") ("division" . "÷") ("parenright" . ")") ("underscore" . "_") ("at" . "@") ("braceright" . "}") ("bracketright" . "]") ("multiply" . "×") ("partialderivative" . "∂") ("radical" . "√") ("minus" . "-") ("plus" . "+") ("Return" . "<return>")))

(d-xkb--generate-layouts)

;; Generate layout constants.
(let ((layrx (rx string-start
                 "d-xkb-"
                 (group (zero-or-more (not "-")))
                 "-layout" string-end)))

  (cl-flet* ((namecore (layout) (let ((layname (symbol-name layout)))
                                  (prog2 (string-match layrx layname)
                                      (substring layname
                                                 (match-beginning 1)
                                                 (match-end 1)))))
             (extlaysymb (layout) (let ((core (namecore layout)))
                                    (intern (concat "d-xkb-extended-"
                                                    core
                                                    (unless (string-empty-p core) "-")
                                                    "layout")))))

    (defconst d-xkb-layouts
      (apropos-internal layrx (lambda (potlay) (and (boundp potlay)
                                               (not (string-match-p
                                                     "-extended-"
                                                     (symbol-name potlay))))))
      "List of Daselt layouts in unextended form. Generated automatically.")

    (with-eval-after-load 'd-xkb-customs
      (defconst d-xkb-extended-layout
        (intern (concat "d-xkb-extended-"
                        (namecore d-xkb-layout)
                        "-layout"))
        "Name of the extended version of d-xkb-layout.")

      (defconst d-xkb-modifier-layer
        (list (list "H" "s" "5/<tab>" "7/<compose>" "C" "M" "C" "7/<compose>" "5/<tab>" "s" "H")
              (list "6/F11" "" "" "3" "" "" "" "" "3" "" "" "6/F11")
              (list "4/<XF86Launch5>" "" "" "" "" "" "" "" "" "" "" "4/<XF86Launch5>")
              (list "⟨I⟩dA/<f8>" "8" "" "" "" "" (if (eq d-xkb-layout 'd-xkb-main-layout) "H" "") "" "" "" "" "8" "dA/<f8>")
              (list "2/dM/⟨C⟩ds/<XF86Launch6>" "C/SPC" "2/dM/⟨C⟩ds/<XF86Launch6>"))
        "Daselt's modifier layout.")

      (defconst d-xkb-reduced-modifier-layer
        (list (list "H" "s" "5/<tab>" "3/<compose>" "7" "M" "7" "3/<compose>" "5/<tab>" "s" "H")
              (list "6/F11" "" "" "" "" "" "" "" "" "" "" "6/F11")
              (list "4/<XF86Launch5>" "" "" "" "" "" "" "" "" "" "" "4/<XF86Launch5>")
              (list "8" "" "" "" "" "" "" "" "" "" "8")
              (list "2/dM/⟨C⟩ds/<XF86Launch6>" "C/SPC" "2/dM/⟨C⟩ds/<XF86Launch6>"))
        "Daselt's modifier layout. Version where all modifiers are outside letter keys.")

      (defvaralias 'd-xkb-layer-0 (if d-dfk-dual-functions-outside-main-keys
                                      'd-xkb-reduced-modifier-layer
                                    'd-xkb-modifier-layer))

      (mapc (lambda (layout)
              (eval 
               `(defconst ,(extlaysymb layout)
                  (append (list d-xkb-layer-0) ,layout)
                  (format "Extended version of %s. 
Generated automatically." layout))))
            d-xkb-layouts)

      (defconst d-xkb-key-coords 
        (mapcar (lambda (indrow)
                  (mapcar (lambda (indplace)
                            (let ((fullcoords (d-xkb--rel-from-abs-coords
                                               (list 0 (car indrow) (car indplace)))))
                              (list (nth 1 fullcoords)
                                    (nth 2 fullcoords))))
                          (d-add-list-indices (cdr indrow))))
                (d-add-list-indices (car (symbol-value d-xkb-layout))))
        "This is the coordinate system of keys of d-xkb.
Generated from the first layer of the main layout.")

      (defconst d-xkb-extended-key-coords 
        (mapcar (lambda (indrow)
                  (mapcar (lambda (indplace)
                            (let ((fullcoords (d-xkb--rel-from-abs-coords
                                               (list 0 (car indrow) (car indplace))
                                               t)))
                              (list (nth 1 fullcoords)
                                    (nth 2 fullcoords))))
                          (d-add-list-indices (cdr indrow))))
                (d-add-list-indices (car (symbol-value d-xkb-extended-layout))))
        "This is the extended coordinate system of keys of d-xkb.
Generated from the first layer of the main layout.")

      (defconst d-xkb-layer-boundaries
        (let* ((allcoords (d-flatten-until d-xkb-key-coords
                                           (lambda (lst) ; Flatten until the first entry is a coordinate.
                                             (d-xkb--coordinates-p (car lst)))))
               (allrows (mapcar (lambda (coords)
                                  (car coords))
                                allcoords))
               (allcols (mapcar (lambda (coords)
                                  (car (last coords)))
                                allcoords)))
          (cons (cons (apply #'min allrows)
                      (apply #'max allrows))
                (cons (apply #'min allcols)
                      (apply #'max allcols))))
        "A cons of conses that keep the minimum and maximum row and column
        coordinates in `d-xkb-key-coords`.")

      (defconst d-xkb-layer-0-boundaries
        (let* ((allcoords (d-flatten-until d-xkb-extended-key-coords
                                           (lambda (lst) ; Flatten until the first entry is a coordinate.
                                             (d-xkb--coordinates-p (car lst)))))
               (allrows (mapcar (lambda (coords)
                                  (car coords))
                                allcoords))
               (allcols (mapcar (lambda (coords)
                                  (car (last coords)))
                                allcoords)))
          (cons (cons (apply #'min allrows)
                      (apply #'max allrows))
                (cons (apply #'min allcols)
                      (apply #'max allcols))))
        "A cons of conses that keep the minimum and maximum row and column
        coordinates in `d-xkb-extended-key-coords`.")

      (defconst d-xkb-coordinates
        (mapcar (lambda (layer)
                  (mapcar (lambda (row)
                            (mapcar (lambda (place)
                                      (append (list layer) place))
                                    row))
                          d-xkb-key-coords))
                (d-cardinal 8 t))
        "The coordinates of all places in normal d-xkb-layers.
Saved here for lookup in help-commands.")

      (defconst d-xkb-extended-coordinates
        (append (mapcar (lambda (row)
                          (mapcar (lambda (place)
                                    (append (list 0) place))
                                  row))
                        d-xkb-extended-key-coords)
                d-xkb-coordinates)
        "The coordinates of all places in all d-xkb-layers, including the 0-th
        layer.")))) 

(provide 'd-xkb-constants)
;;; d-xkb-constants.el ends here

