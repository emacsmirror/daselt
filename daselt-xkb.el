;;; daselt-xkb.el --- Create and draw a coordinate system from an xkb-layout -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Alexander Prähauser

;; Author: Alexander Prähauser <ahprae@protonmail.com>
;; Version: 1.0
;; Keywords: tools, external
;; URL: https://gitlab.com/nameiwillforget/d-emacs/-/blob/master/daselt-xkb.el

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

;; daselt-xkb.el is a component of the Daselt Emacs configuration that enables users
;; to create and visualize a keyboard coordinate system based on their X Keyboard
;; Extension (xkb) layout. This module parses specified xkb layout files to extract key
;; bindings across multiple layers, organizing them into Emacs-compatible data structures.
;;
;; Key features include:
;; - **Customization:** Users can specify the location of their xkb layout file, define
;;   row configurations, handle special keys, and map xkb symbol names to Emacs-friendly
;;   representations through various customizable variables.
;;
;; - **Layer Management:** The package supports multiple keyboard layers, allowing for
;;   complex key binding schemes. It can inherit key bindings from parent layouts, ensuring
;;   consistency and reducing redundancy in configurations.
;;
;; - **Symbol Translation:** daselt-xkb.el translates xkb symbol names into symbols that
;;   Emacs can interpret, handling special characters, Unicode symbols, and function keys.
;;
;; - **Automatic Layout Generation:** Upon loading, the module automatically reads the
;;   specified xkb file, processes the layout definitions, and generates corresponding
;;   Emacs constants. This ensures that the Emacs environment accurately mirrors the
;;   user's physical keyboard layout.
;;
;; By integrating with the broader Daselt system, daselt-xkb.el provides a foundation
;; for advanced key binding strategies and coordinate-based operations within Emacs.
;; This enhances customization capabilities and improves the efficiency of the user's
;; workflow by allowing Emacs to understand and utilize the physical layout of the
;; keyboard effectively.
;;
;; Overall, daselt-xkb is designed to bridge the gap between the system's keyboard
;; configuration and Emacs, offering a seamless and customizable experience tailored to
;; the user's specific keyboard setup.

;; **Main Functions:** The main function provided by this package is
;; `daselt-xkb-generate-layouts', which reads an xkb-file and converts each of
;; its layouts into a daselt-coords-layout. The exact behavior of
;; `daselt-xkb-generate-layouts' can be tailored by setting the customs in the
;; group `daselt-xkb'.

;;; Code:

;;;; Preamble
(require 'daselt-base)

(require 'cl-lib)
;; (declare-function daselt-base-mark-line "daselt-base" (&optional arg))
;; (declare-function daselt-base-uppercase-p "daselt-base" (str))
;; (declare-function daselt-base-cardinal "daselt-base" (n &optional fromone))

(defvar daselt-xkb-keep-read-buffers (bound-and-true-p daselt-dirs-keep-read-buffers))

;; Automatically generated
(defvar daselt-xkb-layouts)

;;;; Customs
(defgroup daselt-xkb
  nil
  "Customization group for daselt-xkb."
  :group 'daselt
  :prefix "daselt-xkb-")

(defcustom daselt-xkb-file
        "/usr/share/X11/xkb/symbols/dxkb"
        "Location of the file housing Daselt's xkb layout."
        :type 'directory
        :group 'daselt-xkb)

(defcustom daselt-xkb-rowlist
  '("D" "C" ("LSGT" "B"))
  "List containing the information for the rows to be read from the layout.

Each element should be either a one-letter string corresponding to a row
letter in XKB (usually E-B) or a list.

If the element is a list, it describes either a row with an unusual length
or includes special keys. In this case:

- If the first element of the list is a number, it is assumed to be the
  length of the row.

- The remaining elements should be strings, with exactly one being a
  one-letter string representing the row letter.

- Strings before and after the one-letter string correspond to special keys
  before and after the regular keys in the row. These special keys are not
  counted in the row length.

For example, ISO layouts contain an additional key in the lower key row
\(the B-row) next to the left Shift key named `LSGT`. Thus, the entry for
this row in the default value of `daselt-xkb-rowlist' is a list containing
the strings `LSGT` and `B`. Since there is no number at the beginning of the
list, the number of keys in the row without the special key `LSGT` is taken
to be `daselt-xkb-rows-length'."
  :type '(repeat
          (choice
           (string :tag "Row Letter")
           (list
            (optional integer)
            (repeat string))))
  :group 'daselt-xkb)

(defcustom daselt-xkb-rows-length
  10
  "Length of normal rows in d-xkb-layouts.

Can be overwritten for any particular row using `daselt-xkb-rowlist'."
  :type 'natnum
  :group 'daselt-xkb)

(defcustom daselt-xkb-special-key-names
  '("Delete" "BackSpace" "Tab" "Escape" "Print" "Space" "Up" "Left" "Right" "Down" "Home" "End" "Return")
  "List of control characters and function key names used in Daselt."
  :type '(repeat string)
  :group 'daselt-xkb)

(defcustom daselt-xkb-remaining-char-mappings
  '(("question" . "?") ("quotedbl" . "\"") ("exclam" . "!") ("grave" . "`") ("acute" . "´") ("asciitilde" . "~") ("bar" . "|") ("percent" . "%") ("equal" . "=") ("dollar" . "$") ("EuroSign" . "€") ("ellipsis" . "…") ("parenleft" . "(") ("numbersign" . "#") ("braceleft" . "{") ("bracketleft" . "\[") ("emdash" . "—") ("division" . "÷") ("parenright" . ")") ("underscore" . "_") ("at" . "@") ("braceright" . "}") ("bracketright" . "]") ("multiply" . "×") ("partialderivative" . "∂") ("radical" . "√") ("minus" . "-") ("plus" . "+") ("degree" . "°"))
  "Alist of symbol names in xkb that have to be translated into symbols in Emacs."
  :type '(repeat (cons string string))
  :group 'daselt-xkb)

(defcustom daselt-xkb-read-layer-function
          'daselt-xkb--generate-layer
          "Function used to generate a daselt-xkb-layer from an xkb-file.

Default is `daselt-xkb--generate-layer'. Has to be adapted for different
layouts."
          :type 'symbol
          :group 'daselt-xkb)

(defcustom daselt-xkb-layer-numbers-list
  (daselt-base-cardinal 8 t)
  "Layers of the xkb-layout you want to import."
  :type 'natnum
  :group 'daselt-xkb)

;;;; Functions
;;;;; Reading daselt-xkb-file
(defun daselt-xkb--get-key-binding (beg end num keyname)
  "Extract the key binding on layer NUM for the key with name KEYNAME.

The region searched is that from BEG to END. Returns nil if no binding is
found."
  (declare (ftype (function (integer integer integer string) string))
           (side-effect-free t))
  (goto-char beg)
  (if (search-forward keyname end t)
      (progn (beginning-of-line)
             (search-forward "\{" end t)
             (mark-sexp)
             (nth num ; First two entries are brackets, absolute layers start at zero.
                  (split-string
                   (buffer-substring-no-properties (region-beginning)
                                                   (region-end))
                   "[[:space:]]+" t)))
    nil))

(defun daselt-xkb--format-special-key (string)
  "Format a STRING describing a control character or function key.

Return a form suitable for Emacs."
  (declare (ftype (function (string) string))
           (pure t))
  (concat "<" (replace-regexp-in-string "_" "-" (downcase string)) ">"))

(defun daselt-xkb--format-xkb-signal-name (rawsigname)
  "Format the signal name RAWSIGNAME read by daselt-xkb for Emacs.

Removes spaces and NoSymbol's, converts Unicode characters, converts keypad
signal names, and replaces other signal names with their Emacs equivalents."
  (declare (ftype (function (string) string))
           (pure t))
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
                (member signame daselt-xkb-special-key-names))
            (setq signame (daselt-xkb--format-special-key signame)))

           ;; Try inserting the signal as a character.
           ((char-from-name signame t)
            (setq signame (char-to-string (char-from-name signame t))))

           ;; If the string starts with a U, is longer than one symbol and all letters are uppercase it's safe to assume it's the code of a unicode symbol and convert it to that symbol.
           ((and (daselt-base-uppercase-p signame) (string-match-p "U" (substring signame 0 1))
                 (> (length signame) 1))
            (setq signame (char-to-string (string-to-number (substring signame 1) 16))))

           ;; If it's a Greek letter, create the corresponding name and use it for insertion. Uppercase first.
           ((string-match-p "Greek" signame)
            (setq signame (let ((letter (nth 1 (split-string signame "_"))))
                            (if (daselt-base-uppercase-p letter)
                                (char-to-string
                                 (char-from-name (concat "greek capital letter " letter) t))
                              (char-to-string
                               (char-from-name (concat "greek small letter " letter) t))))))

           ;; Replace other symbols
           (t (cl-loop for cand in daselt-xkb-remaining-char-mappings
                       do (if (equal (car cand) signame)
                              (setq signame (cdr cand))))))
          signame)
      nil)))

(defun daselt-xkb--inherit-from-parent-map (beg end laynum rownum key)
  "Lookup the key binding of a KEY in the parent map of the current map.

The current map is that within the range defined by BEG to END. The key position
is given by LAYNUM, ROWNUM and KEY.

KEY can be a number, corresponding to KEYs with a number (AB01 and such) or a
string, in which case the string is assumed to be its name.

This works only if the parent map appears earlier in the file."
  (declare (ftype (function (integer integer integer integer (or integer string)) string))
           (side-effect-free t))
  (goto-char beg)
  (if (search-forward "include" end t)
      (progn (daselt-base-mark-line)
             (let* ((includelinelist (remove "" (split-string (remove ?\" (buffer-substring-no-properties (region-beginning) (region-end)))
                                                              "\\(\(\\|\)\\|[ ]+\\|\_\\)" t)))
                    (parent (nth 2 includelinelist))
                    (parentlayout (symbol-value
                                   (intern (concat "daselt-xkb-" parent "-layout"))))
                    (parlayer (nth (1- laynum) parentlayout)) ; Layers start at 1
                    (maxrowlength (apply #'max (mapcar #'length parlayer)))
                    (parrow (nth rownum parlayer))
                    (parrowlength (length parrow)))
               (nth (- (1+ key) (- maxrowlength parrowlength))
                    parrow)))
    nil))

;;;;; Layout generation functions
(defun daselt-xkb--generate-layer (beg end laynum)
  "Generate a list from an xkb-layer.

The layer is defined by the region from BEG to END in `daselt-xkb-file' and the
layer number LAYNUM."
  (declare (ftype (function (integer integer integer) list))
           (side-effect-free t))
  (mapcar (lambda (indrowinfo)
            (let* ((rowidx (car indrowinfo))
                   (rowinfo (cdr indrowinfo))
                   (rowinfo (if (atom rowinfo) ; Redefine if it's not a list.
                                (list rowinfo)
                              rowinfo))
                   (rowlength (if (numberp (car rowinfo))
                                  (car rowinfo)
                                daselt-xkb-rows-length))
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
                                              (binding (daselt-xkb--format-xkb-signal-name
                                                        (daselt-xkb--get-key-binding
                                                         beg end laynum keyname))))
                                         (or binding
                                             (let ((keynum (+ numbershift (1- keynum))))
                                               (daselt-xkb--inherit-from-parent-map
                                                beg end laynum rowidx
                                                keynum)))))
                                     (daselt-base-cardinal rowlength t)))
                         
                         (let ((binding (daselt-xkb--format-xkb-signal-name
                                         (daselt-xkb--get-key-binding
                                          beg end laynum elt))))
                           (list (or binding
                                     (daselt-xkb--inherit-from-parent-map
                                      beg end laynum rowidx eltnum))))))))
          (daselt-base-index daselt-xkb-rowlist)))

(defun daselt-xkb-generate-layouts ()
  "Generate lists from layouts defined in the `daselt-xkb-file'.

This function searches for `xkb_symbols', marks the line, extracts layout names,
and processes them with `daselt-xkb--generate-layer' to define the layout.

The structure of xkb-layouts can be faily complex, so this function makes
various simplifying assumptions. In particular it is assumed that all layouts in
the file have the same number of layers."
  (declare (ftype (function () t )))
  (let ((dxkbbuf (find-file-noselect daselt-xkb-file))
        defined-layouts)  ; This way we don't overwrite layouts with a base and a full version.
    (set-buffer dxkbbuf)
    (daselt-base-goto-min)
    (while (re-search-forward (rx "partial" (* blank)
                                  "alphanumeric_keys" (* not-newline) (* (or "\n" blank))
                                  "xkb_symbols")
                              nil t)
      (let ((linebeg (prog2 (beginning-of-line) (point))))
        (progn (daselt-base-mark-line)
               (let* ((laynameful
                       (nth 1 (split-string
                               (buffer-substring (region-beginning) (region-end)) " ")))
                      (layname (remove ?\" (if (string-match-p "base" laynameful)
                                               (car (split-string laynameful "_"))
                                             laynameful))))

                 (progn (goto-char linebeg)
                        (search-forward "\{" nil t)
                        (backward-char)
                        (mark-sexp)
                        (unless (daselt-base-exists-p defined-layouts
                                                      (lambda (previous-layname)
                                                        (string= layname previous-layname)))
                          (let ((laybeg (region-beginning)) (layend (region-end)))
                            (set (intern (concat "daselt-xkb-" layname "-layout"))
                                 (mapcar (lambda (laynum)
                                           (daselt-xkb--generate-layer laybeg layend laynum))
                                         daselt-xkb-layer-numbers-list)))
                          (push layname defined-layouts)))))))
    (unless daselt-xkb-keep-read-buffers (kill-buffer dxkbbuf))))
                                        ;
;;;;; Set layouts variable
(defun daselt-xkb-set-layouts-list ()
  "Set `daselt-xkb-layouts' from symbols matching `daselt-xkb-.*layout'."
  (declare (ftype (function () t)))
  (defconst daselt-xkb-layouts
    (apropos-internal "daselt-xkb-.*-layout" (lambda (sym) (boundp sym)))
    "List of daselt-xkb-layouts in unextended form. Generated automatically."))

;;;; Generated Constants
(defcustom daselt-xkb-layout
  'daselt-xkb-main-layout
  "The keyboard-layout you're using.

Should be one of the layouts in the `daselt-xkb-file'."
  :group 'daselt-xkb
  :type 'symbol)

;;;; Provide
(provide 'daselt-xkb)
;;; daselt-xkb.el ends here
