;;; d-emacs-mode.el -- Modes for d-emacs           -*- lexical-binding: t; -*-

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

;; Define `d-emacs-mode' and provide commands bound to its keys and related to
;; it.

;;; Code:
;;;; Preamble
(declare-function d-emacs-dirs-act-on-pkg-files-by-type-and-maybe-kill "d-emacs-dirs" (funtypes &optional dir customt))
(declare-function d-emacs-dirs-create-pkg-customization-options "d-emacs-dirs" (&optional dir group deffun))
(declare-function d-emacs-stump-translated-emacs-keys "d-emacs-stump" nil)
(declare-function d-emacs-stump-translated-emacs-keys "d-emacs-stump" nil)
(declare-function d-emacs-dfk-import-current-layout "d-emacs-dfk" nil)
(declare-function d-emacs-xkb--generate-layouts "d-emacs-xkb" nil)
(declare-function d-emacs-read-region "d-emacs-base" (&optional properties))
(declare-function d-emacs-dirs-act-on-sexps-in-file "d-emacs-dirs" (filepath function &optional untangle))
(declare-function d-emacs-bind-string "d-emacs-bind" (binding &optional translate csectoshft doublebind))
(declare-function d-emacs-minor-mode-key-binding "d-emacs-mode-ext" (key))
(declare-function d-emacs-cardinal "d-emacs-base" (n &optional fromone))

(defvar d-emacs-global-map-backup)
(defvar undo-tree-auto-save-history)
(defvar d-emacs-dirs-keep-read-buffers)
(defvar d-emacs-debug)
(defvar d-emacs-dirs-pkg-configs-directory)
(defvar d-emacs-bind-key-translations-alist)
(defvar d-emacs-replace-untranslated-keys)
(defvar d-emacs-bind-translate-keys)
(defvar d-emacs-bind-translate-C-1-1--2-C-g)
(defvar d-emacs-stump)

;; (require 'd-emacs-base)
;; (require 'd-emacs-coords)
;; (require 'd-emacs-xkb)
;; (require 'd-emacs-dfk)
;; (require 'd-emacs-bind)
;; (require 'd-emacs-dirs)
;; (require 'd-emacs-mode-ext)
(require 'cl-seq)

;;;; Initialization
;;;;; Customs
(defgroup d-emacs-mode
  nil
  "This group houses all customization options for d-emacs."
  :group 'd-emacs
  :prefix "d-emacs-mode-")

(defcustom d-emacs-mode-put-d-emacs-mode-map-into-emulation
  t
  "If non-nil, add `d-emacs-mode-map' to `emulation-mode-map-alists'.

Maps in this list supersede most other keymaps."
  :type 'boolean
  :group 'd-emacs-mode)

(defcustom d-emacs-mode-globalize-d-emacs-mode-map
  nil
  "Set `global-mode-map' to `d-emacs-mode-map' while `d-emacs-mode' is active.

Not needed if `d-emacs-mode-put-d-emacs-mode-map-into-emulation' is non-nil."
  :type 'boolean
  :group 'd-emacs-mode)

(defcustom d-emacs-mode-quick-key-coords-base-list
  '((0 2) (0 3) (-1 3) (0 4) (-1 4) (0 1) (-1 2) (0 5) (1 1) (1 2) (1 3) (-1 5) (1 4) (-1 1) (1 0))
  "Coordinates of the right-hand keys for quick-key selections.

This list is used in `d-special-quick-keys-bindlists' for generating bindlists
used in quick-key selections. The left-hand keys and coordinates from
`d-emacs-mode-quick-key-layers-list' are added in a way that ensures balanced
distribution."
  :type '(repeat coords)
  :group 'd-emacs-mode)

(defcustom d-emacs-mode-quick-key-layers-list
  (d-emacs-cardinal 8 t)
  "List of key layers for generating quick-key selection bindlists.

Used by `d-special-quick-keys-bindlists` to create constants for quick-key
selection mechanisms."
  :type 'boolean
  :group 'd-emacs-mode)

(defcustom d-emacs-mode-show-tutorial
  t
  "Show the Daselt tutorial when `d-emacs-mode' is started.

If non-nil, the tutorial will be displayed upon entering the mode."
  :type 'boolean
  :group 'd-emacs-mode)

;;;;; Maps
(defvar-keymap d-emacs-miscellaneous-map)
(defvar-keymap d-emacs-theme-map)

;;;;; Rebinder functions, taken from Abdulla Bubshait's rebinder package: https://github.com/darkstego/rebinder.el
(defun d-emacs-dynamic-binding (key &optional toggle)
  "Create a dynamic binding for KEY in the current context.

This function generates a menu item that computes the actual binding
dynamically. If TOGGLE is non-nil, it allows switching from Ctrl mappings to
standard key mappings."
  `(menu-item
    ,""
    nil
    :filter
    (lambda (&optional _)
      ,`(d-emacs-key-binding ,key ,toggle))))

;; might need to do keymap inheretence to perserve priority
(defun d-emacs-key-binding (key &optional toggle)
  "Retrieve the keymap associated with KEY.

If TOGGLE is non-nil, the Ctrl status of all bindings in the resulting keymap
will be inverted."
  (let ((map (make-composed-keymap (list (d-emacs-minor-mode-key-binding key) (local-key-binding (kbd key)) (global-key-binding (kbd key))))))
    (if toggle
	(mapcar 'd-emacs-toggle-ctrl map)
      map)))

(defun d-emacs-toggle-ctrl (item)
  "Toggle the Ctrl status of ITEM key binding.

If ITEM is a list, recursively toggle the Ctrl status of its elements.
If ITEM is a key event, the modifiers are toggled appropriately."
  (cond
   ((and (listp item)
	 (not (listp (cdr item))))
    (cons (d-emacs-toggle-ctrl (car item)) (cdr item)))
   ((listp item)
    (mapcar 'd-emacs-toggle-ctrl item))
   ((event-basic-type item)
    (let ((mods (event-modifiers item))
	  (key (event-basic-type item)))
      (if (member 'control mods)
	  (event-convert-list (append (remove 'control mods) (list key)))
	(event-convert-list (append (append mods '(control)) (list key))))))
   (t item)))

(defun d-emacs-rebind (keylist)
  "Rebind keys according to KEYLIST, where each entry is a cons cell.

The car of each cons cell is a prefix key combination and the cdr is the new key
combination that acts as the prefix key."
  (mapcar (lambda (i) (define-key global-map (kbd (car i)) (d-emacs-dynamic-binding (nth 1 i))))
          keylist))


;;;;; Functions
(defun d-emacs-mode--set-replace-strings-alist ()
  "Update `d-emacs-replace-binding-strings-alist` based on current bindings.

Uses `d-emacs-bind-translate-C-1-1--2-C-g`, `d-emacs-bind-translate-keys`,
and `d-emacs-replace-untranslated-keys` to determine the relevant bindings."
  (remq nil (append (unless (or (and (boundp d-emacs-stump) d-emacs-stump) d-emacs-bind-translate-C-1-1--2-C-g)
                      `(("C-g" . ,(d-emacs-bind-string `(("C-" . (1 1 -2)))))))

                    (unless (or d-emacs-bind-translate-keys
                                (not d-emacs-replace-untranslated-keys))
                      (mapcar (lambda (cns)
                                (let ((str (car cns)))
                                  (cons str (string-replace "C-" "A-" str))))
                              d-emacs-bind-key-translations-alist)))))

(defun d-emacs-mode--generate-quick-key-variables ()
      "Generate quick key variables used in Daselt configurations.

Utilizes `d-special-quick-keys-bindlist` as a foundation for generation."
      (let* ((filepath
          (concat d-emacs-dirs-pkg-configs-directory "quick-keys.dbf"))

         (keylist (prog1 (cl-remove-duplicates
                          (flatten-list (d-emacs-dirs-act-on-sexps-in-file
                                         filepath
                                         (lambda () (let ((blist (eval (d-emacs-read-region))))
                                                 (remq nil (mapcar (lambda (bind)
                                                                         (let ((sig (d-emacs-bind-string bind)))
                                                                       (if (= 1 (length sig))
                                                                                   (string-to-char
                                                                            sig))))
                                                                   blist)))))))
                    (let ((filebuffer (get-file-buffer filepath))) ; `get-file-buffer' can get tripped up by symlinks.
                      (unless (or d-emacs-debug
                                  d-emacs-dirs-keep-read-buffers
                                  (not filebuffer))
                        (kill-buffer filebuffer)))))

         runlist
         (permutedlist (cl-loop for n from 0 to (- (length keylist) 2)
                                do (setq runlist
                                         (append runlist
                                                 (list (if (cl-evenp n)
                                                                   (nth (1+ n) keylist)
                                                             (nth (1- n) keylist)))))
                                finally return runlist))

         (keystring (mapconcat #'char-to-string keylist))

         (keystringpair (cons keystring
                              (mapconcat #'char-to-string permutedlist))))

    (defvar d-quick-key-list keylist
          "Quick key list for Daselt.
Auto-generated using `d-emacs-mode--generate-quick-key-variables.'")

    (defvar d-quick-key-string keystring
          "Quick key string for Daselt.
Auto-generated using `d-emacs-mode--generate-quick-key-variables.'")

    (defvar d-quick-key-string-cons keystringpair
          "Quick key string pair for Daselt.
Auto-generated using `d-emacs-mode--generate-quick-key-variables.'")))

(defun d-emacs-mode-generate-tutorial ()
  "Generate the Daselt-tutorial."
  (interactive)
  (let ((tutfile (concat (file-name-directory (buffer-file-name))
                         "d-emacs-tutorial.el"))
        (display-buffer-alist '((".*" display-buffer-full-frame))))
    (find-file tutfile)
    (goto-char (point-min))
    (search-forward ";;; Code:")
    (search-forward "\(")
    (backward-char)
    (mark-sexp)
    (let ((tuttext (eval (d-emacs-read-region))))
      (pop-to-buffer "*daselt-tutorial*")
      (delete-minibuffer-contents)
      (org-mode)
      (setq visual-line-mode t)
      (insert tuttext)
      (goto-char (point-min)))))

;;;; Mode
(define-minor-mode d-emacs-mode
  "Daselt's minor mode."
  :init-value nil
  :global t
  :interactive t
  :lighter "Daselt"
  (if d-emacs-mode
      (progn (if d-emacs-bind-translate-keys
                 ;; Add the key translations for C-g and ("C-" . (1 1 -2)) if they aren't there yet.
                 (progn (if d-emacs-bind-translate-C-1-1--2-C-g
                            (let ((transcons
                                   `(,(d-emacs-bind-string `(("C-" . (1 1 -2)))) . "C-g"))
                                  (revtranscons
                                   `("C-g" . ,(d-emacs-bind-string `(("C-" . (1 1 -2)))))))
                              (add-to-list 'd-emacs-bind-key-translations-alist transcons)
                              (add-to-list 'd-emacs-bind-key-translations-alist revtranscons)))
                        (mapc
                         (lambda (cns) (key-translate (car cns) (cdr cns)))
                         d-emacs-bind-key-translations-alist)))

             ;; Refresh the d-emacs-xkb-layouts in case someone has changed bindings.
             (d-emacs-xkb--generate-layouts)

             ;; Generate d-emacs-dfk-layout from the current d-emacs-xkb-layout.
             (d-emacs-dfk-import-current-layout)

             ;; Set constants
             (if d-emacs-stump (setopt d-emacs-bind-outside-translations-alist
                                       (d-emacs-stump-translated-emacs-keys)))
             
             ;; Add all files in pkg-configs to the load-path.
             (let ((default-directory d-emacs-dirs-pkg-configs-directory))
               (normal-top-level-add-to-load-path '("."))
               (normal-top-level-add-subdirs-to-load-path))

             ;; Add pkg-config-options
             (d-emacs-dirs-create-pkg-customization-options)

             ;; Quick keys
             (d-emacs-mode--generate-quick-key-variables)
             
             ;; Has to be defined here so later `emulation'-maps are put on top.
             (defvar-keymap d-emacs-mode-map)
             (if d-emacs-mode-put-d-emacs-mode-map-into-emulation
                 (add-to-list 'emulation-mode-map-alists
                              `((d-emacs-mode . ,d-emacs-mode-map))))

             (let ((undo-tree-auto-save-history nil) ; Saving undo-state of opened files is useless here and slows down startup.
                   )
               (d-emacs-dirs-act-on-pkg-files-by-type-and-maybe-kill
                `((d-emacs-dirs-with-eval-load-elc-or-file .  "del")
                  (d-emacs-dirs-with-eval-apply-bindlists-in-file
                   . ("dbl" "regular"))
                  
                  ;; Do rebinding before other operations, that way if something goes wrong, at least the layout is defined
                  (d-emacs-dirs--save-bindlists-in-file-as-variables . "dbl")
                  (d-emacs-dirs-save-bindforms-in-file-as-variables . "dbf")
                  (d-emacs-dirs--with-eval-backup-and-set-constantlists-in-file . ("dcl" "regular"))
                  (d-emacs-dirs-with-eval-add-advicelists-in-file . ("dal" "regular"))
                  (d-emacs-dirs-with-eval-add-adviceforms-in-file . ("daf" "regular")))))

             (if d-emacs-mode-globalize-d-emacs-mode-map
                 (progn (unless (boundp d-emacs-global-map-backup)
                          (setq d-emacs-global-map-backup global-map))
                        (setq global-map d-emacs-mode-map)))

             (if d-emacs-mode-show-tutorial (d-emacs-mode-generate-tutorial)))

    ;; Reset variables and remove advice
    (d-emacs-dirs-act-on-pkg-files-by-type-and-maybe-kill
     `((d-emacs-dirs--with-eval-remove-advicelists-in-file . ("dal" "regular"))
       (d-emacs-dirs--with-eval-reset-constantlists-in-file . ("dcl" "regular"))))))

;;;; Provide
(provide 'd-emacs-mode)
;;; d-emacs-mode.el ends here
