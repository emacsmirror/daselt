;;; d-emacs-xkb.el --- Create and draw a coordinate system from an xkb-layout -*- lexical-binding: t; -*-

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

;;  d-emacs-xkb functions.

;;; Code:

;;;; Preamble
(require 'cl-macs)
(require 'table)
(require 'org-compat)
(require 'd-emacs-base)

(declare-function org-table-align "org-table" nil)
(declare-function org-sublist "org-compat" (list start end))
(declare-function d-execute-in-maximized-maybe-temp-buffer "d-emacs-base" (bufname fun))
(declare-function d-remove-list-index "d-emacs-base" (lst idx))
(declare-function d-filter-by-predicate "d-emacs-base" (lst pred))
(declare-function d-string-exists-and-nonempty "d-emacs-base" (str))
(declare-function d-forall-p "d-emacs-base" (list predicate))
(declare-function d-add-list-indices "d-emacs-base" (list &optional fromone))
(declare-function d-mark-line "d-emacs-base" (&optional arg))
(declare-function d-cardinal "d-emacs-base" (n &optional fromone))
(declare-function d-uppercase-p "d-emacs-base" (str))

;; (defvar d-emacs-xkb-layouts)
;; (defvar d-debug)
(defvar d-keep-read-buffers)

;;;; Customs
(defgroup d-emacs-xkb
                  nil
                  "Customization group for d-emacs-xkb."
                  :group 'Daselt
                  :prefix "d-emacs-xkb-")

(defcustom d-emacs-xkb-file
        "/usr/share/X11/xkb/symbols/dxkb"
        "Location of the file housing Daselt's xkb layout."
        :type 'directory
        :group 'd-emacs-xkb)

(defcustom d-emacs-xkb-rowlist
  '("D" "C" ("LSGT" "B"))
  "List containing the information for the rows to be read from the layout.
Each element is supposed to be either a one-letter string corresponding to a row
letter in xkb (usually E-B) or a list.

If it is a list, it describes either a list with unusual length or special keys.
In this case, if the first element of the list is a number, it is assumed to be
the length of the list. The other elements of the list should be strings.
Exactly one of them should be a one-letter string. This is the row letter. The
strings before and after the one-letter string are taken to correspond to
special keys before resp. after the regularly named keys in the row. They are
not counted in the row length.

For instance, ISO-layouts contain an additional key in the lower key row (the
B-row) next to left Shift that is named `LSGT'. Thus, the entry for this row in
the default for `d-emacs-xkb-rowlist' is a list consisting of the strings `LSGT'
and `B'. Since no number is at the beginning of the list, the number of keys in
the row without the special key `LSGT' is taken to be
`d-emacs-xkb-rows-length'."
  :type 'list
  :group 'd-emacs-xkb)

(defcustom d-emacs-xkb-rows-length
                                            10
                                            "Length of normal rows in d-xkb-layouts.
Can be overwritten for any particular row using `d-emacs-xkb-rowlist'."
                                            :type 'natnum
                                            :group 'd-emacs-xkb)

(defcustom d-emacs-xkb-special-key-names
                            '("Delete" "BackSpace" "Tab" "Escape" "Print" "Space" "Up" "Left" "Right" "Down" "Home" "End" "Return")
                            "List of control characters and function key names used in Daselt."
                            :type '(repeat 'string)
                            :group 'd-emacs-xkb)

(defcustom d-emacs-xkb-remaining-char-mappings
  '(("question" . "?") ("quotedbl" . "\"") ("exclam" . "!") ("grave" . "`") ("acute" . "´") ("asciitilde" . "~") ("bar" . "|") ("percent" . "%") ("equal" . "=") ("dollar" . "$") ("EuroSign" . "€") ("ellipsis" . "…") ("parenleft" . "(") ("numbersign" . "#") ("braceleft" . "{") ("bracketleft" . "\[") ("emdash" . "—") ("division" . "÷") ("parenright" . ")") ("underscore" . "_") ("at" . "@") ("braceright" . "}") ("bracketright" . "]") ("multiply" . "×") ("partialderivative" . "∂") ("radical" . "√") ("minus" . "-") ("plus" . "+") ("degree" . "°"))
  "Alist of symbol names in xkb that have to be translated into symbols in Emacs."
  :type '(repeat (cons 'string 'string))
  :group 'd-emacs-xkb)

(defcustom d-emacs-xkb-read-layer-function
                                                                        'd-emacs-xkb--generate-layer
                                                                        "Function used to generate a d-emacs-xkb-layer from an xkb-file.
Default is `d-emacs-xkb--generate-layer'.
Has to be adapted for different layouts."
                                                                        :type 'symbol
                                                                        :group 'd-emacs-xkb)

;;;; Functions
;;;;; Reading d-emacs-xkb-file
(defun d-emacs-xkb--get-key-binding (beg end num keyname)
  "Extract the key binding on layer NUM for the key with name KEYNAME.
The region searched is that from BEG to END. Returns nil if no binding is
found."
  (goto-char beg)
  (if (search-forward keyname end t)
                                                              (progn (beginning-of-line)
             (search-forward "\{" end t)
             (mark-sexp)
             (nth (1+ num) ; First two entries are brackets, absolute layers start at zero.
                  (split-string
                   (buffer-substring-no-properties (region-beginning)
                                                   (region-end))
                   "[[:space:]]+" t)))
    nil))

(defun d-emacs-xkb--format-special-key (str)
                "Format a string STR describing a control character or function key.
Return a form suitable for Emacs."
                (concat "<" (replace-regexp-in-string "_" "-" (downcase str)) ">"))

(defun d-emacs-xkb--format-xkb-signal-name (rawsigname)
    "Format the signal name RAWSIGNAME read by d-emacs-xkb into a form usable by
Emacs.
Removes spaces and NoSymbol's, converts Unicode characters, converts keypad
signal names, and replaces other signal names with their Emacs equivalents."
    (let ((signame rawsigname))
    (if rawsigname
            (progn
            ;; Remove tabs, spaces and commas if they exist.
            (setq signame (replace-regexp-in-string "\t" "" signame))
            (setq signame (remove ?\  signame))
            (setq signame (remove ?, signame))

            ;; Remove NoSymbol's.
            (cond
           ((string= signame "NoSymbol")
            (setq signame nil))

           ;; If it's a keypad-signal or a control character, downcase, put <> around it and replace _ with -.
           ((or (string-match-p "KP" signame)
                (member signame d-emacs-xkb-special-key-names))
            (setq signame (d-emacs-xkb--format-special-key signame)))

           ;; Try inserting the signal as a character.
           ((char-from-name signame t)
            (setq signame (char-to-string (char-from-name signame t))))

           ;; If the string starts with a U, is longer than one symbol and all letters are uppercase it's safe to assume it's the code of a unicode symbol and convert it to that symbol.
           ((and (d-uppercase-p signame) (string-match-p "U" (substring signame 0 1))
                 (> (length signame) 1))
            (setq signame (char-to-string (string-to-number (substring signame 1) 16))))

           ;; If it's a Greek letter, create the corresponding name and use it for insertion. Uppercase first.
           ((string-match-p "Greek" signame)
            (setq signame (let ((letter (nth 1 (split-string signame "_"))))
                            (if (d-uppercase-p letter)
                                    (char-to-string
                                 (char-from-name (concat "greek capital letter " letter) t))
                              (char-to-string
                               (char-from-name (concat "greek small letter " letter) t))))))

           ;; Replace other symbols
           (t (cl-loop for cand in d-emacs-xkb-remaining-char-mappings
                       do (if (equal (car cand) signame)
                                  (setq signame (cdr cand)))))
           )
            signame)
      nil)))

(defun d-emacs-xkb--inherit-from-parent-map (beg end laynum rownum key)
  "Lookup the key binding of a KEY in the parent map of the current map.
The current map is that within the range defined by BEG to END. The key position
is given by LAYNUM, ROWNUM and KEY.

KEY can be a number, corresponding to KEYs with a number (AB01 and such) or a
string, in which case the string is assumed to be its name.

This works only if the parent map appears earlier in the file."
  (goto-char beg)
  (if (search-forward "include" end t)
                                                  (progn (d-mark-line)
             (let* ((includelinelist (remove "" (split-string (remove ?\" (buffer-substring-no-properties (region-beginning) (region-end)))
                                                              "\\(\(\\|\)\\|[ ]+\\|\_\\)" t)))
                    (parent (nth 2 includelinelist))
                    (parentlayout (symbol-value
                                   (intern (concat "d-emacs-xkb-" parent "-layout"))))
                    (parlayer (nth laynum parentlayout))
                    (maxrowlength (apply #'max (mapcar #'length parlayer)))
                    (parrow (nth rownum parlayer))
                    (parrowlength (length parrow)))
               (nth (- key (- maxrowlength parrowlength))
                    parrow)))
    nil))

;;;;; Layout generation functions
(defun d-emacs-xkb--generate-layer (beg end laynum)
                  "Generate a list from a d-emacs-xkb layer.
The layer is defined by the region from BEG to END in `d-emacs-xkb-file' and the
layer number LAYNUM."
                  (mapcar (lambda (indrowinfo)
                                  (let* ((rowidx (car indrowinfo))
                   (rowinfo (cdr indrowinfo))
                   (rowinfo (if (atom rowinfo) ; Redefine if it's not a list.
                                                                            (list rowinfo)
                                                    rowinfo))
                   (rowlength (if (numberp (car rowinfo))
                                                                              (car rowinfo)
                                                      d-emacs-xkb-rows-length))
                   (idx 0)
                   (numbershift (progn (while (not (= 1 (length (nth idx rowinfo))))
                                         (setq idx (1+ idx)))
                                       idx)))
              (cl-loop for eltnum from 0 to (1- (length rowinfo))
                       for elt = (nth eltnum rowinfo)
                       append
                       (if (= 1 (length elt)) ; If it's a row letter.
                                                                       (let* ((rowprefix (concat "A" elt)))
                             (mapcar (lambda (keynum)
                                                             (let* ((keynumstr (if (>= keynum 10)
                                                                                                         (number-to-string keynum)
                                                                                 (concat
                                                            "0" (number-to-string
                                                                 keynum))))
                                              (keyname (concat rowprefix keynumstr))
                                              (binding (d-emacs-xkb--format-xkb-signal-name
                                                        (d-emacs-xkb--get-key-binding
                                                         beg end laynum keyname))))
                                         (or binding
                                             (let ((keynum (+ numbershift (1- keynum))))
                                               (d-emacs-xkb--inherit-from-parent-map
                                                beg end laynum rowidx
                                                keynum)))))
                                     (d-cardinal rowlength t)))
                         
                         (let ((binding (d-emacs-xkb--format-xkb-signal-name
                                         (d-emacs-xkb--get-key-binding
                                          beg end laynum elt))))
                           (list (or binding
                                     (d-emacs-xkb--inherit-from-parent-map
                                      beg end laynum rowidx eltnum))))))))
          (d-add-list-indices d-emacs-xkb-rowlist))) 

(defun d-emacs-xkb--generate-layouts ()
    "Generate lists from layouts defined in the d-emacs-xkb file.
This function searches for `xkb_symbols', marks the line, extracts layout names,
and processes them with `d-emacs-xkb--generate-layer' to define the layout."
    (let ((dxkbbuf (find-file-noselect d-emacs-xkb-file)))
    (set-buffer dxkbbuf)
    (goto-char (point-min))
    (while (search-forward "xkb_symbols" nil t)
      (let ((linebeg (prog2 (beginning-of-line) (point))))
        (progn (d-mark-line)
               (let ((laynameful
                      (nth 1 (split-string
                              (buffer-substring (region-beginning) (region-end)) " "))))
                 (if (string-match-p "base" laynameful)
                                                 (let ((layname (remove ?\" (car (split-string laynameful "_")))))
                       (progn (goto-char linebeg)
                              (search-forward "\{" nil t)
                              (backward-char)
                              (mark-sexp)
                              (let ((laybeg (region-beginning)) (layend (region-end)))
                                (set (intern (concat "d-emacs-xkb-" layname "-layout"))
                                     (mapcar (lambda (laynum)
                                                             (d-emacs-xkb--generate-layer laybeg layend laynum))
                                             d-emacs-coords-layer-numbers-list))))))))))
    (unless (or d-debug d-keep-read-buffers) (kill-buffer dxkbbuf))))

;;;; Generated Constants
(d-emacs-xkb--generate-layouts)


;; Generate layout constants.

(defconst d-emacs-xkb-layouts
  (apropos-internal "d-emacs-xkb-.*-layout" (lambda (sym) (boundp sym)))
  "List of d-emacs-xkb-layouts in unextended form. Generated automatically.")

(defcustom d-emacs-xkb-layout
  'd-emacs-xkb-main-layout
  "The keyboard-layout you're using.
Should be one of the layouts in the `d-emacs-xkb-file'."
  :group 'Daselt
  :type 'symbol
  :options d-emacs-xkb-layouts)

(defconst d-emacs-xkb-coordinates
  (let* ((layout (symbol-value d-emacs-xkb-layout))
         (coordlayout (d-emacs-coords-coordinatize-layout layout)))
    (d-flatten-until (mapcar (lambda (coordlayer)
                               (mapcar (lambda (coordrow)
                                         (mapcar #'car coordrow))
                                       coordlayer))
                             coordlayout)
                     (lambda (lst) (d-emacs-coords-p (car lst)))))
  "All coordinates in `d-emacs-xkb-layout'.
Saved here to be called in calculations of drawing functions.")

;;;; Provide
(provide 'd-emacs-xkb)
;;; d-emacs-xkb.el ends here
