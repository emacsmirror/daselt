;;; d-emacs-mode.el --- Provides d-emacs-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Alexander Prähauser

;; Author: Alexander Prähauser <ahprae@protonmail.com>
;; Package-Requires: ((emacs "29.1"))
;; Version: 1.0
;; Keywords: tools
;; URL: https://gitlab.com/nameiwillforget/d-emacs/d-emacs-mode/

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

;; Define `d-emacs-mode', the implementation of the Daselt shortcut-layout in
;; Emacs and provide commands needed to start it. Commands bound to keys are in
;; `pkg-configs/d-emacs/d-emacs-ext.del'.

;;; Code:
;;;; Preamble
(declare-function d-emacs-dirs-act-on-pkg-files-by-type-and-maybe-kill "d-emacs-dirs" (funtypes &optional dir customt sortfun pfx))
(declare-function d-emacs-dirs-create-pkg-customization-options "d-emacs-dirs" (&optional dir group deffun))
(declare-function d-emacs-stump-translated-emacs-keys "d-emacs-stump" nil)
(declare-function d-emacs-stump-translated-emacs-keys "d-emacs-stump" nil)
(declare-function d-emacs-dfk-import-current-layout "d-emacs-dfk" nil)
(declare-function d-emacs-xkb-generate-layouts "d-emacs-xkb" nil)
(declare-function d-emacs-base-read-region "d-emacs-base" (&optional properties))
(declare-function d-emacs-dirs-act-on-sexps-in-file "d-emacs-dirs" (filepath function &optional untangle))
(declare-function d-emacs-bind-string "d-emacs-bind" (binding &optional translate csectoshft doublebind))
(declare-function d-emacs-minor-mode-key-binding "d-emacs-mode-ext" (key))
(declare-function d-emacs-base-cardinal "d-emacs-base" (n &optional fromone))

(defvar d-emacs-global-map-backup)
(defvar undo-tree-auto-save-history)
(defvar d-emacs-dirs-keep-read-buffers)
(defvar d-emacs-base-debug)
(defvar d-emacs-dirs-pkg-configs-directory)
(defvar d-emacs-bind-key-translations-alist)
(defvar d-emacs-replace-untranslated-keys)
(defvar d-emacs-bind-translate-keys)
(defvar d-emacs-bind-translate-C-1-1--2-C-g)
(defvar d-emacs-stump)

(require 'd-emacs-base)
(require 'd-emacs-coords)
(require 'd-emacs-xkb)
(require 'd-emacs-dfk)
(require 'd-emacs-bind)
(require 'd-emacs-dirs)
(require 'cl-lib)
(require 'org-table)


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
  (d-emacs-base-cardinal 8 t)
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


;; This is a custom mostly to have it saved between sessions.
(defcustom d-emacs-mode-pkg-configs-directory
  nil
  "The pkg-configs-directory of d-emacs-mode.

This should be the directory named `pkg-configs' in the folder in which
d-emacs-mode is installed."
  :type 'directory
  :group 'd-emacs-mode)

(defcustom d-emacs-mode-keep-tab-bar-status
  nil
  "If t, keep the tab bar disabled when commands like `tab-new' are used.

Uses advice."
  :type 'boolean
  :group 'd-emacs-mode)

(defcustom d-emacs-mode-include-imitation-commands
  (not (bound-and-true-p d-emacs-stump))
  "Include the imitation commands on keys C-(1 0 [-2-2]) in `d-emacs-mode-map'.

Unnecessary if you are using a program like StumpWM to translate these key
combinations."
  :type 'boolean
  :group 'd-emacs-mode)

(defcustom d-emacs-mode-redaselt
  (d-emacs-base-namecore d-emacs-xkb-layout "d-emacs-xkb-" "-layout")
  "Have the `redaselt' shell script run with this string as arguments when
`d-emacs-mode' starts.

If you are using some other means of setting Daselt's layout, then you can turn
this off."
  :type 'string
  :group 'd-emacs-mode)

(defcustom d-emacs-mode-undaselt
    nil
    "Have the `undaselt' shell script run with these arguments when
`d-emacs-mode' ends.

You can use this to specify the layout you're using outside of Daselt, if any.

Note that the default of the `undaselt' script is `en us'."
    :type 'string
    :group 'd-emacs-mode)

(defcustom d-emacs-mode-redaselt-time
  1
  "The time the redaselt-script should wait between (re)starting udevmon and
setting the xkb-layout.

You only need to set this by hand if the current amount of time is too little
for your computer to properly start up udevmon. In that case, the layout will
not be set."
  :type 'number
  :group 'd-emacs-mode)

(defcustom d-emacs-mode-exchange-H-2-H-6
  nil
  "Set to t to exchange H-2 with H-6 bindings.

On some systems this might be necessary because H in combination with other
modifiers acts as a level 5 shift for currently unknown reasons."
  :type 'boolean
  :group 'd-emacs-mode)

;;;;; Maps
(defvar-keymap d-emacs-mode-miscellaneous-map)
(defvar-keymap d-emacs-mode-theme-map)

(defvar-keymap d-emacs-mode-map)

;;;;; Rebinder functions, taken from Abdulla Bubshait's rebinder package: https://github.com/darkstego/rebinder.el
(defun d-emacs-mode-dynamic-binding (key &optional toggle)
  "Create a dynamic binding for KEY in the current context.

This function generates a menu item that computes the actual binding
dynamically. If TOGGLE is non-nil, it allows switching from Ctrl mappings to
standard key mappings."
  `(menu-item
    ,""
    nil
    :filter
    (lambda (&optional _)
                        ,`(d-emacs-mode-key-binding ,key ,toggle))))

;; might need to do keymap inheretence to perserve priority
(defun d-emacs-mode-key-binding (key &optional toggle)
  "Retrieve the keymap associated with KEY.

If TOGGLE is non-nil, the Ctrl status of all bindings in the resulting keymap
will be inverted."
  (let ((map (make-composed-keymap (list (d-emacs-minor-mode-key-binding key) (local-key-binding (kbd key)) (global-key-binding (kbd key))))))
    (if toggle
	(mapcar 'd-emacs-mode-toggle-ctrl map)
      map)))

(defun d-emacs-mode-toggle-ctrl (item)
  "Toggle the Ctrl status of ITEM key binding.

If ITEM is a list, recursively toggle the Ctrl status of its elements. If ITEM
is a key event, the modifiers are toggled appropriately."
  (cond
   ((and (listp item)
	 (not (listp (cdr item))))
    (cons (d-emacs-mode-toggle-ctrl (car item)) (cdr item)))
   ((listp item)
    (mapcar 'd-emacs-mode-toggle-ctrl item))
   ((event-basic-type item)
    (let ((mods (event-modifiers item))
	  (key (event-basic-type item)))
      (if (member 'control mods)
	  (event-convert-list (append (remove 'control mods) (list key)))
	(event-convert-list (append (append mods '(control)) (list key))))))
   (t item)))

(defun d-emacs-mode-rebind (keylist)
  "Rebind keys according to KEYLIST, where each entry is a cons cell.

The car of each cons cell is a prefix key combination and the cdr is the new key
combination that acts as the prefix key."
  (mapcar (lambda (i) (define-key global-map (kbd (car i)) (d-emacs-mode-dynamic-binding (nth 1 i))))
          keylist))


;;;;; Functions
(defun d-emacs-mode--generate-replace-strings ()
  "Update `d-emacs-bind-replace-binding-strings-alist' based on current bindings.

Uses `d-emacs-bind-translate-C-1-1--2-C-g', `d-emacs-bind-translate-keys', and
`d-emacs-replace-untranslated-keys' to determine the relevant bindings."
  (declare (ftype (function () list))
           (side-effect-free t))
  (remq nil (append (unless (or (and (boundp d-emacs-stump) d-emacs-stump)
                                d-emacs-bind-translate-C-1-1--2-C-g)
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
  (declare (ftype (function () string)))
  (let* ((filepath
          (concat d-emacs-mode-pkg-configs-directory "quick-keys.dbf"))

         (keylist (prog1 (cl-remove-duplicates
                          (flatten-list (d-emacs-dirs-act-on-sexps-in-file
                                         filepath
                                         (lambda ()
                                           (let ((blist (eval (d-emacs-base-read-region))))
                                             (remq nil (mapcar (lambda (bind)
                                                                 (let ((sig (d-emacs-bind-string bind)))
                                                                   (if (= 1 (length sig))
                                                                       (string-to-char
                                                                        sig))))
                                                               (cdr blist))))))))
                    (let ((filebuffer (get-file-buffer filepath))) ; `get-file-buffer' can get tripped up by symlinks.
                      (unless d-emacs-dirs-keep-read-buffers
                        (not filebuffer)
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
    
    (defvar d-emacs-mode-quick-key-list keylist
      "Quick key list for Daselt.
Auto-generated using `d-emacs-mode--generate-quick-key-variables.'")

    (defvar d-emacs-mode-quick-key-string keystring
      "Quick key string for Daselt.
Auto-generated using `d-emacs-mode--generate-quick-key-variables.'")

    (defvar d-emacs-mode-quick-key-string-cons keystringpair
      "Quick key string pair for Daselt.
Auto-generated using `d-emacs-mode--generate-quick-key-variables.'")))

(defun d-emacs-mode-generate-tutorial (&optional no-refresh)
  "Generate the Daselt-tutorial.

NO-REFRESH is or optimization-purposes: `d-emacs-mode' already refreshes
`d-emacs-dfk', so it's unnecessary to do it again."
  (declare (ftype (function (&optional boolean)
                            ;; void  ; Compiler complains.
                            t)))
  (interactive)
  (unless no-refresh
    (d-emacs-dfk-import-current-layout))
  (let ((display-buffer-alist-backup display-buffer-alist))
    (condition-case report
        (let ((tutfile (concat d-emacs-mode-pkg-configs-directory "d-emacs-mode/d-emacs-mode.tut"))
              (display-buffer-alist '((".*" display-buffer-full-frame))))
          (find-file tutfile)
          (d-emacs-base-goto-min)
          (mark-sexp)
          (let ((tuttext (eval (d-emacs-base-read-region))))
            (deactivate-mark)
            (pop-to-buffer "*daselt-tutorial*")
            (delete-minibuffer-contents)
            (org-mode)
            (visual-line-mode)
            (insert tuttext)
            (d-emacs-base-goto-min)
            nil))
      (error (progn (setq display-buffer-alist display-buffer-alist-backup)
                    (error (error-message-string report)))))))

(defun d-emacs-mode-redaselt ()
  "Run the `redaselt'-bash-script to switch your keyboard layout.

The keyboard-layout loaded is the d-xkb-variant specified in
the option `d-emacs-mode-redaselt'."
  (interactive)
  (async-shell-command (d-emacs-base-concat-with-separators " "
                                                            "redaselt"
                                                            d-emacs-mode-redaselt
                                                            (number-to-string d-emacs-mode-redaselt-time))))

(defun d-emacs-mode-undaselt ()
  "Run the `redaselt'-bash-script to switch your keyboard layout.

The keyboard-layout loaded is the d-xkb-variant specified by
`d-emacs-xkb-layout'."
  (interactive)
  (async-shell-command (concat "undaselt "
                               d-emacs-mode-undaselt)))

;;;;; The pkg-configs-issue
(defun d-emacs-mode--pkg-configs-directory-test (dir)
  "Test whether DIR looks like d-emacs-mode's pkg-configs-directory."
  (declare (ftype (function (str) boolean))
           (pure t))
  (and dir
       (file-exists-p dir)
       (file-exists-p (concat dir "d-emacs-mode/"))))

(defun d-emacs-mode--pkg-configs-directory-enter-manually ()
  "Specify manually where the pkg-configs-directory is."
  (declare (ftype (function () string)))
  (let* ((use-file-dialog nil) ; Dialog box doesn't let you select folder (or I was doing something wrong).
         (filename (read-file-name "Please point d-emacs-mode to its pkg-configs directory (in the directory where d-emacs-mode is installed, include trailing backslash): "
                                   nil nil
                                   #'d-emacs-mode--pkg-configs-directory-test)))
    (customize-save-variable 'd-emacs-mode-pkg-configs-directory
                             filename)
    filename))

(defun d-emacs-mode--find-pkg-configs-directory ()
  "Find d-emacs-mode's pkg-configs-directory.

Set the corresponding option so it's saved for future sessions.

If the option already points to something that looks like the right directory,
don't do anything."
  (declare (ftype (function () string)))
  (unless (d-emacs-mode--pkg-configs-directory-test d-emacs-mode-pkg-configs-directory)
    (condition-case nil (let ((current-pkg-dir
                               (concat (file-name-directory
                                        (buffer-file-name))
                                       "pkg-configs/")))
                          (if (d-emacs-mode--pkg-configs-directory-test current-pkg-dir)
                              (customize-save-variable 'd-emacs-mode-pkg-configs-directory
                                                       current-pkg-dir)
                            (d-emacs-mode--pkg-configs-directory-enter-manually)))
      (error (d-emacs-mode--pkg-configs-directory-enter-manually)))))

;;;;; Find, set and save the directory
(d-emacs-mode--find-pkg-configs-directory)

;;;; Mode
;;;###autoload
(define-minor-mode d-emacs-mode
  "Daselt's minor mode.

Rebinds most keys and revamps Emacs to implement Daselt's shortcut layout.

`d-emacs-mode' uses a pkg-configs-directory as defined in `d-emacs-dirs' to
store its configuration. When `d-emacs-mode' is started, it has to read all
files in this directory. Depending on your hardware and installed packages this
might take between a few seconds and maybe a minute. If you plan on toggling
`d-emacs-mode' several times you can set `d-emacs-dirs-keep-read-buffers' to t
to reduce the startup time."
  :init-value nil
  :global t
  :interactive t
  :lighter "Daselt"
  (if d-emacs-mode
      (progn  ;; Set `d-emacs-dirs-pkg-configs-directory' to `d-emacs-mode-pkg-configs-directory' while `d-emacs-mode' is on.
        (unless (d-emacs-mode--pkg-configs-directory-test d-emacs-dirs-pkg-configs-directory)
          (if (bound-and-true-p d-emacs-dirs-pkg-configs-directory)
              (progn (defvar d-emacs-d-emacs-dirs-pkg-configs-directory-backup)
                     (setq d-emacs-d-emacs-dirs-pkg-configs-directory-backup
                           d-emacs-dirs-pkg-configs-directory)))
          (setopt d-emacs-dirs-pkg-configs-directory
                  d-emacs-mode-pkg-configs-directory))

        ;; `d-emacs-mode' without translated keys is borderline unusable.
        (setq d-emacs-bind-translate-keys t)

        ;; Find out if it's an ansi keyboard
        (unless (or (not d-emacs-mode-show-tutorial)
                    (not (eq (custom-variable-state 'd-emacs-dfk-keyboard-layout-type t)
                             'standard)))
          (customize-save-variable 'd-emacs-dfk-keyboard-layout-type
                                   (d-emacs-base-remove-text-properties-from-string
                                    (completing-read "Do you have an ansi or iso-keyboard (you have ansi if your left Shift-key is larger than CapsLock)? " d-emacs-dfk-supported-layout-types))))

        (if d-emacs-bind-translate-keys
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
        (d-emacs-xkb-generate-layouts)

        ;; Choose the layout
        (unless (or (not d-emacs-mode-show-tutorial)
                    (not (eq (custom-variable-state 'd-emacs-xkb-layout t)
                             'standard)))
          (customize-save-variable 'd-emacs-xkb-layout
                                   (d-emacs-base-intern-from-parts
                                    "d-emacs-xkb"
                                    (completing-read
                                     "Please pick the Daselt layout you want to use: "
                                     (mapcar (lambda (sym)
                                               (d-emacs-base-namecore sym
                                                                      "d-emacs-xkb-"
                                                                      "-layout"))
                                             d-emacs-xkb-layouts))
                                    "layout")))

        (if d-emacs-mode-redaselt
            (if (file-exists-p "/usr/share/X11/xkb/symbols/dxkb")
                (d-emacs-mode-redaselt)
              (error "Please put the dxkb-file into `/usr/share/X11/xkb/symbols/'")))
        
        ;; For a non-main layout put modifiers outside the layout unless they have been put in by hand. For the main layout, do it the other way around.
        (unless (not (eq (custom-variable-state 'd-emacs-dfk-outside-mods t)
                         'standard))
          (if (eq (symbol-value 'd-emacs-xkb-layout)
                  'd-emacs-xkb-main-layout)
              (setopt d-emacs-dfk-outside-mods nil)
            (setopt d-emacs-dfk-outside-mods t)))

        ;; Generate d-emacs-dfk-layout from the d-emacs-xkb-layout.
        (d-emacs-dfk-import-current-layout)

        ;; Set constants
        (when (bound-and-true-p d-emacs-stump)
          (setopt d-emacs-bind-outside-translations-alist
                  (d-emacs-stump-translated-emacs-keys))

          ;; We can also apply the d-emacs-stump-bindlists here.
          (d-emacs-dirs-act-on-pkg-files-by-type-and-maybe-kill
           `(((lambda (filename)
                (d-emacs-dirs-save-bindlists-in-file filename "d-emacs-stump")) . "dbl")
             ((lambda (filename)
                (d-emacs-dirs-save-bindforms-in-file filename "d-emacs-stump")) . "dbf"))
           d-emacs-stump-pkg-configs-directory t nil "d-emacs-stump"))

        ;; Calculate layer boundaries to simplify calculations by `d-emacs-bind-draw-bindings-from-regexps'.
        (setq d-emacs-bind-boundaries
              (list (d-emacs-coords-boundaries
                     (mapcar (lambda (placeval) (car placeval))
                             (d-emacs-base-flatten-until
                              (d-emacs-coords-get-layer
                               (d-emacs-coords-coordinatize-layout
                                (symbol-value d-emacs-dfk-layout))
                               0)
                              (lambda (lst) (d-emacs-coords-p (caar lst))))))

                    (d-emacs-coords-boundaries
                     (mapcar (lambda (placeval) (car placeval))
                             (d-emacs-base-flatten-until
                              (d-emacs-coords-get-layer
                               (d-emacs-coords-coordinatize-layout
                                (symbol-value d-emacs-xkb-layout))
                               1)
                              (lambda (lst) (d-emacs-coords-p (caar lst))))))))

        ;; Add all files in pkg-configs to the load-path.
        (let ((default-directory d-emacs-dirs-pkg-configs-directory))
          (normal-top-level-add-to-load-path '("."))
          (normal-top-level-add-subdirs-to-load-path))

        ;; Add pkg-config-options
        (d-emacs-dirs-create-pkg-customization-options-by-variable d-emacs-mode-pkg-configs-directory)

        (setopt d-emacs-d-emacs-mode t) ; This should really be enabled (otherwise it won't recurse into the corresponding directory).

        ;; Quick keys
        (d-emacs-mode--generate-quick-key-variables)

        ;; Add to emulation
        (if d-emacs-mode-put-d-emacs-mode-map-into-emulation
            (add-to-list 'emulation-mode-map-alists
                         `((d-emacs-mode . ,d-emacs-mode-map))))

        (let ((undo-tree-auto-save-history nil) ; Saving undo-state of opened files is useless here and slows down startup.
              )
          (d-emacs-dirs-act-on-pkg-files-by-type-and-maybe-kill
           `((d-emacs-dirs-with-eval-load-elc-or-lispcode-in-file .  "del")
             (d-emacs-dirs-save-and-with-eval-apply-bindlists-in-file
              . ("dbl" "regular"))

             ;; Do rebinding before other operations, that way if something goes wrong, at least the layout is defined.
             (d-emacs-dirs-save-bindlists-in-file . ("dbl" "special"))
             (d-emacs-dirs-save-bindforms-in-file . "dbf")
             (d-emacs-dirs-with-eval-set-constantlists-in-file . ("dcl" "regular"))
             (d-emacs-dirs-with-eval-add-advicelists-in-file . ("dal" "regular"))
             (d-emacs-dirs-with-eval-add-adviceforms-in-file . ("daf" "regular")))
           nil t))

        (if d-emacs-mode-globalize-d-emacs-mode-map
            (progn (unless (boundp d-emacs-global-map-backup)
                     (setq d-emacs-global-map-backup global-map))
                   (setq global-map d-emacs-mode-map)))

        (if d-emacs-mode-show-tutorial (d-emacs-mode-generate-tutorial t)))

    ;; Reset variables and remove advice We have to recurse again instead of
    ;; simply setting the original variables to their backup values because
    ;; there is the possibility that otherwise an eval-condition might be
    ;; triggered after the mode was exited, overriding the restored
    ;; backup-value.
    (d-emacs-dirs-act-on-pkg-files-by-type-and-maybe-kill
     `((d-emacs-dirs-with-eval-remove-advicelists-in-file . ("dal" "regular"))
       (d-emacs-dirs-with-eval-reset-bindlists-in-file . ("dbl" "regular"))
       (d-emacs-dirs-with-eval-reset-constantlists-in-file . ("dcl" "regular"))))

    ;; Let's also reset all global variables that have not been found by the recursion (because their entry was deleted).
    (d-emacs-dirs--reset-backed-up-variables)

    ;; Restore the keyboard layout.
    (if d-emacs-mode-undaselt (d-emacs-mode-undaselt))))

;;;; Provide
(provide 'd-emacs-mode)
;;; d-emacs-mode.el ends here
