;;; d-emacs-customs.el --- Daselt's Emacs module customization options              -*- lexical-binding: t; -*-

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

;; This file houses the d-emacs-customization options for Daselt.

;;; Code:

(require 'd-customs)

(defgroup d-emacs
  nil
  "This group houses all customization options for d-emacs."
  :group 'Daselt
  :prefix "d-emacs-")

(defcustom d-emacs
    t
    "Enable or disable d-emacs customizations. If set to nil, no customizations are undertaken by d-emacs."
    :type 'boolean
    :group 'd-emacs)

(defcustom d-emacs-directory
  (condition-case nil (file-name-parent-directory
                       (file-name-parent-directory
                        (file-name-directory (buffer-file-name))))
    (error (cl-loop for loaded in load-history
                    for loadpath = (car loaded)
                    if (string-match "/d-emacs/" loadpath)
                    do (cl-return (substring loadpath 0 (match-end 0))))))
  "The directory containing d-emacs. Include a trailing slash. 
By default, this directory is set to the parent directory of
the parent directory of the file containing these customs. 
During start-up, it is set instead by checking `load-history'"
  :group 'd-emacs
  :type 'directory)

(defcustom d-emacs-apply-regular-bindlists-at-file-save
  d-sort-save-and-apply-bindlists-at-file-save
  "If non-nil, apply regular `bindlists' when a regular `bindlists' file is saved."
  :type 'boolean
  :group 'd-emacs)

(defcustom d-emacs-put-d-emacs-mode-map-into-emulation
  t
  "If non-nil, add `d-emacs-mode-map' to `emulation-mode-map-alists'.
Maps in this list supersede most other keymaps."
  :type 'boolean
  :group 'd-emacs)

(defcustom d-emacs-globalize-d-emacs-mode-map
  nil
  "If non-nil, set `global-mode-map' to `d-emacs-mode-map' while `d-emacs-mode' is active.
Not needed if `d-emacs-put-d-emacs-mode-map-into-emulation' is non-nil."
  :type 'boolean
  :group 'd-emacs)

(defcustom d-emacs-translate-C-1-1--2-C-g
  nil
  "If non-nil, translate C-g to C-(1 1 -2) and vice versa.
Note that the C-g function to stop running processes cannot be translated,
so the option is disabled by default."
  :type 'boolean)

(defcustom d-emacs-key-translations-alist
  `(("C-m" . "C-á") ("C-i" . "C-ĥ") ("C-[" . "C-é"))
  "List of key translations to circumvent terminal interference.
Each element is a cons cell where the car is a key combination
to be translated and the cdr is the desired translation. For
example, on terminals like xterm, `C-i' may be translated to TAB.
Setting `d-emacs-translate-keys' to t will use these translations
to preserve intended key bindings."
  :type '(repeat (cons string string)))

(defcustom d-emacs-double-symbs-alist
  '((?Ͳ . ?ͳ) (?Ϙ . ?ϙ))
  "Alist of symbol translations for elaborate binding suffixes.
If the first symbol in a cons cell is the suffix of the elaborate
form of a binding in a bindlist, the same binding should apply to the
second symbol as well."
  :type '(repeat (cons character character)))

(defcustom d-emacs-overwrite-translate-keys
  nil
  "Determine if keys in `d-emacs-key-translations-alist' should be overwritten.
When non-nil, bindings with the original keys will be overridden.
When nil, bindings will use an A-Modifier instead of a C-modifier."
  :type 'boolean
  :group 'd-emacs)

(defcustom d-emacs-translate-keys
  (if (> (string-to-number (substring emacs-version 0 (string-match-p "\\." emacs-version))) 29) t nil)
  "Enable translation of keys defined in `d-emacs-key-translations-alist'.
This translation is intended for Emacs versions 30 or higher (29 may
also work) to address terminal translations that conflict with key
bindings. When active, use the translated key combinations in bindings."
  :type 'boolean
  :group 'd-emacs)

(defcustom d-emacs-replace-multiple-choice
  t
  "Replace 'y' and 'n' in multiple-choice queries with alternative values.
If a query uses symbols at coordinates (1 0 2) or (1 0 -2), replace them with
the values at coordinates (5 0 2) or (5 0 -2), typically unused Greek letters."
  :type 'boolean
  :group 'd-emacs)

(defcustom d-emacs-C-3-0-2-contexts-list
  '((minibuffer-mode . minibuffer-complete-and-exit)
    (Info-mode . Info-follow-nearest-node)
    (eww-mode . eww-follow-link)
    (help-mode . push-button)
    (LaTeX-mode . latex/forward-environment)
    (vterm-mode . vterm-send-tab)
    (emacs-lisp-mode . sp-next-sexp)

    (org-mode . (lambda () (let ((buffer (buffer-name))) 
                        (cond ((org-footnote-at-reference-p)
                               org-footnote-action)

                              ((org-at-item-p)
                               (unless (run-hook-with-args-until-success 'org-open-at-point-functions)
                                 (let* ((context
	                                 ;; Only consider supported types, even if they are not the
	                                 ;; closest one.
	                                 (org-element-lineage
	                                  (org-element-context)
	                                  '(citation citation-reference clock comment comment-block
                                                     footnote-definition footnote-reference headline
                                                     inline-src-block inlinetask keyword link node-property
                                                     planning src-block timestamp)
	                                  t))
	                                (type (org-element-type context))
	                                (value (org-element-property :value context)))
                                   (if type
                                       org-open-at-point
                                     org-next-item))))

                              (t org-open-at-point))))))
  "A list defining actions for `d-emacs-C-3-0-2' across different modes."
  :type '(repeat (cons symbol sexp))
  :group 'd-emacs)

(defcustom d-emacs-C-1-0-2-contexts-list
  '((rectangle-mark-mode . (lambda () (rectangle-forward-char 1)))
    (minibuffer-mode . d-emacs-move-right-or-exit)
    (image-mode . image-forward-hscroll)
    (pdf-view-mode . image-forward-hscroll)
    (vterm-mode . vterm-send-right))
  "Actions for `d-emacs-C-1-0-2' in different modes."
  :type '(repeat (cons symbol sexp))
  :group 'd-emacs)

(defcustom d-emacs-C-1-0--2-contexts-list
  '((rectangle-mark-mode . (lambda () (rectangle-backward-char 1)))
    (image-mode . image-backward-hscroll)
    (pdf-view-mode . image-backward-hscroll)
    (vterm-mode . vterm-send-left))
  "Actions for `d-emacs-C-1-0--2' in different modes."
  :type '(repeat (cons symbol sexp))
  :group 'd-emacs)

(defcustom d-emacs-C-1-0--3-contexts-list
  '((vterm-copy-mode . previous-line)
    (vterm-mode . vterm-send-up)
    (minibuffer-mode . previous-line-or-history-element)
    (rectangle-mark-mode . (lambda () (rectangle-previous-line 1)))
    (pdf-view-mode . (lambda () pdf-view-scroll-down-or-previous-page 1))
    (image-mode . image-previous-line)
    (eshell-hist-mode . eshell-previous-input))
  "Actions for `d-emacs-C-1-0--3' across different modes."
  :type '(repeat (cons symbol sexp))
  :group 'd-emacs)

(defcustom d-emacs-C-1-0-3-contexts-list
  '((vterm-copy-mode . next-line)
    (vterm-mode . vterm-send-down)
    (minibuffer-mode . next-line-or-history-element)
    (rectangle-mark-mode . (lambda () rectangle-next-line 1))
    (pdf-view-mode . (lambda () pdf-view-scroll-up-or-next-page 1))
    (image-mode . image-next-line)
    (eshell-hist-mode . eshell-next-input))
  "Actions for `d-emacs-C-1-0-3' across different modes."
  :type '(repeat (cons symbol sexp))
  :group 'd-emacs)

(defcustom d-emacs-C-1-0--1-contexts-list
  '((pdf-view-mode . pdf-view-previous-page-command)
    (minibuffer-mode . previous-line-or-history-element)
    (image-mode . image-previous-frame)
    (vterm-copy-mode . scroll-down-command)
    (vterm-mode . vterm-send-prior))
  "Actions for `d-emacs-C-1-0--1' across different modes."
  :type '(repeat (cons symbol sexp))
  :group 'd-emacs)

(defcustom d-emacs-C-1-0-1-contexts-list
  '((pdf-view-mode . pdf-view-next-page-command)
    (minibuffer-mode . next-line-or-history-element)
    (image-mode . image-next-frame)
    (vterm-copy-mode . scroll-up-command)
    (vterm-mode . vterm-send-next))
  "Actions for `d-emacs-C-1-0-1' across different modes."
  :type '(repeat (cons symbol sexp))
  :group 'd-emacs)

;;;; Pkg customs
(defgroup d-emacs-pkgs
  nil
  "Options for configuring d-emacs support for other packages."
  :group 'd-emacs
  :prefix "d-")

(require 'd-emacs-functions)
(require 'd-emacs-constants)

(d-emacs--create-pkg-customization-options)

(with-eval-after-load 'd-stump
  (defcustom d-emacs-include-imitation-commands
    d-stump
    "Turn to nil to remove the imitation commands on keys C-(1 0 [-2-2]) in the `d-emacs-mode-map'."
    :type 'boolean
    :group 'd-emacs))

(provide 'd-emacs-customs)
;;; d-emacs-customs.el ends here
