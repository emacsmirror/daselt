;;; daselt-mode.el --- Provides daselt-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2024, 2025  Alexander Prähauser

;; Author: Alexander Prähauser <ahprae@protonmail.com>
;; Version: 1.0
;; Keywords: tools
;; URL: https://gitlab.com/nameiwillforget/d-emacs/-/blob/master/daselt-dirs.elhttps://gitlab.com/nameiwillforget/d-emacs/daselt-mode/

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

;; Define `daselt-mode', the implementation of the Daselt shortcut-layout in
;; Emacs and provide commands needed to start it. Commands bound to keys are in
;; `mode-configs/daselt-mode/daselt-ext.del'.

;;; Code:
;;;; Preamble
(declare-function daselt-minor-mode-key-binding "daselt-mode-ext" (key))
(declare-function daselt-stump-translate-daselt-keys "daselt-stump" nil)

(require 'daselt-base)
(require 'daselt-coords)
(require 'daselt-xkb)
(require 'daselt-dfk)
(require 'daselt-bind)
(require 'daselt-dirs)
(require 'cl-lib)
(require 'org-table)

(defvar daselt-replace-untranslated-keys)
(defvar daselt-global-map-backup)
(defvar undo-tree-auto-save-history)

;; If daselt-stump isn't loaded, this variable will not be bound…
(defvar daselt-stump)

;; and this will never be called
(defvar daselt-stump-pkg-configs-directory)

;; These are generated when starting the mode.
(defvar daselt-xkb-layouts)
(defvar daselt-dfk-layout)

(defvar daselt-mode-eval-log nil
        "Log for `daselt-mode' eval conditions.")
(defvar daselt-emacs-dir)
;;;; Initialization
;; ;;;;; Let's read in the dxkb-file
;; (daselt-xkb-generate-layouts)

;;;;; Customs
(defgroup daselt-mode
  nil
  "This group houses all customization options for daselt."
  :group 'daselt
  :prefix "daselt-mode-")

(defcustom daselt-mode-put-daselt-mode-map-into-emulation
  t
  "If non-nil, add `daselt-mode-map' to `emulation-mode-map-alists'.

Maps in this list supersede most other keymaps."
  :type 'boolean
  :group 'daselt-mode)

(defcustom daselt-mode-globalize-daselt-mode-map
  nil
  "Set `global-mode-map' to `daselt-mode-map' while `daselt-mode' is active.

Not needed if `daselt-mode-put-daselt-mode-map-into-emulation' is non-nil."
  :type 'boolean
  :group 'daselt-mode)

(defcustom daselt-mode-quick-key-coords-base-list
  '((0 2) (0 3) (-1 3) (0 4) (-1 4) (0 1) (-1 2) (0 5) (1 1) (1 2) (1 3) (-1 5) (1 4) (-1 1) (1 0))
  "Coordinates of the right-hand keys for quick-key selections.

This list is used in `d-special-quick-keys-bindlists' for generating bindlists
used in quick-key selections. The left-hand keys and coordinates from
`daselt-mode-quick-key-layers-list' are added in a way that ensures balanced
distribution."
  :type '(repeat coords)
  :group 'daselt-mode)

(defcustom daselt-mode-quick-key-layers-list
  (daselt-base-cardinal 8 t)
  "List of key layers for generating quick-key selection bindlists.

Used by `d-special-quick-keys-bindlists` to create constants for quick-key
selection mechanisms."
  :type 'boolean
  :group 'daselt-mode)

(defcustom daselt-mode-show-tutorial
          t
          "Show the Daselt tutorial when `daselt-mode' is started.

If non-nil, the tutorial will be displayed upon entering the mode."
          :type 'boolean
          :group 'daselt-mode)


;; This is a custom mostly to have it saved between sessions.
(defcustom daselt-mode-pkg-configs-directory
  nil
  "The pkg-configs-directory of `daselt-mode'.

This should be the directory named `mode-configs' in the root directory
of Daselt."
  :type 'directory
  :group 'daselt-mode)

(defcustom daselt-mode-keep-tab-bar-status
  nil
  "If t, keep the tab bar disabled when commands like `tab-new' are used.

Uses advice."
  :type 'boolean
  :group 'daselt-mode)

(defcustom daselt-mode-include-imitation-commands
  (not (bound-and-true-p daselt-stump))
  "Include the imitation commands on keys C-(1 0 [-2-2]) in `daselt-mode-map'.

Unnecessary if you are using a program like StumpWM to translate these key
combinations."
  :type 'boolean
  :group 'daselt-mode)

(defcustom daselt-mode-redaselt
  t
  "Have the `redaselt' shell script run when `daselt-mode' starts.

The argument to the script is the part of the value of `daselt-xkb-layout'
between the prefix and the suffix.

If you are using some other means of setting Daselt's layout, then you can turn
this off."
  :type 'boolean
  :group 'daselt-mode)

(defcustom daselt-mode-undaselt
  nil
  "Have the `undaselt' shell script run with these arguments when
`daselt-mode' ends.

You can use this to specify the layout you're using outside of Daselt, if any.

Note that the default of the `undaselt' script is `en us'."
  :type '(choice string boolean)
  :group 'daselt-mode)

(defcustom daselt-mode-redaselt-time
  1
  "The time the `redaselt'-script should wait between (re)starting udevmon and
setting the xkb-layout.

You only need to set this by hand if the current amount of time is too little
for your computer to properly start up udevmon. In that case, the layout will
not be set."
  :type 'number
  :group 'daselt-mode)

(defcustom daselt-mode-global-udevmon
  nil
  "If set to t, run `redaselt' with a --global flag.

This makes systemctl search for a global udevmon-service.

Requires sudo and slows down the starting process a bit.

Doesn't have any effect if neither of the options `daselt-mode-redaselt' or
`daselt-mode-undaselt' are set."
  :type 'boolean
  :group 'daselt-mode)

(defcustom daselt-mode-exchange-H-2-H-6
  nil
  "Set to t to exchange H-2 with H-6 bindings.

On some systems this might be necessary because H in combination with other
modifiers acts as a level 5 shift for currently unknown reasons."
  :type 'boolean
  :group 'daselt-mode)

(defcustom daselt-insert-latex-normpairs
  t
  "If non-nil, insert a norm-pair yasnippet for specific inputs.

Inserts a norm-pair yasnippet when typing (8 0 -5) and (8 0 5).
Refer to `daselt-latex-insert-normpair-yas-snippet' for details."
  :type 'boolean
  :group 'daselt-mode)


;;;;; Maps
(defvar-keymap daselt-mode-miscellaneous-map)
(defvar-keymap daselt-mode-theme-map)

(defvar-keymap daselt-mode-map)

;;;;; Rebinder functions, taken from Abdulla Bubshait's rebinder package: https://github.com/darkstego/rebinder.el
(defun daselt-mode-dynamic-binding (key &optional toggle)
  "Create a dynamic binding for KEY in the current context.

This function generates a menu item that computes the actual binding
dynamically. If TOGGLE is non-nil, it allows switching from Ctrl mappings to
standard key mappings."
  `(menu-item
    ,""
    nil
    :filter
    (lambda (&optional _)
                        ,`(daselt-mode-key-binding ,key ,toggle))))

;; might need to do keymap inheretence to perserve priority
(defun daselt-mode-key-binding (key &optional toggle)
  "Retrieve the keymap associated with KEY.

If TOGGLE is non-nil, the Ctrl status of all bindings in the resulting keymap
will be inverted."
  (let ((map (make-composed-keymap (list (daselt-minor-mode-key-binding key) (local-key-binding (kbd key)) (global-key-binding (kbd key))))))
    (if toggle
	(mapcar #'daselt-mode-toggle-ctrl map)
      map)))

(defun daselt-mode-toggle-ctrl (item)
  "Toggle the Ctrl status of ITEM key binding.

If ITEM is a list, recursively toggle the Ctrl status of its elements. If ITEM
is a key event, the modifiers are toggled appropriately."
  (cond
   ((and (listp item)
	 (not (listp (cdr item))))
    (cons (daselt-mode-toggle-ctrl (car item)) (cdr item)))
   ((listp item)
    (mapcar #'daselt-mode-toggle-ctrl item))
   ((event-basic-type item)
    (let ((mods (event-modifiers item))
	  (key (event-basic-type item)))
      (if (member 'control mods)
	  (event-convert-list (append (remove 'control mods) (list key)))
	(event-convert-list (append (append mods '(control)) (list key))))))
   (t item)))

(defun daselt-mode-rebind (keylist)
  "Rebind keys according to KEYLIST, where each entry is a cons cell.

The car of each cons cell is a prefix key combination and the cdr is the new key
combination that acts as the prefix key."
  (mapcar (lambda (i) (define-key global-map (kbd (car i)) (daselt-mode-dynamic-binding (nth 1 i))))
          keylist))


;;;;; Functions
(defun daselt-mode--generate-replace-strings ()
  "Update `daselt-bind-replace-binding-strings-alist' based on current bindings.

Uses `daselt-bind-translate-C-1-1--2-C-g', `daselt-bind-translate-keys', and
`daselt-replace-untranslated-keys' to determine the relevant bindings."
  (declare (ftype (function () list))
           (side-effect-free t))
  (remq nil (append (unless (or (and (boundp daselt-stump) daselt-stump)
                                daselt-bind-translate-C-1-1--2-C-g)
                      `(("C-g" . ,(daselt-bind-string `(("C-" . (1 1 -2)))))))

                    (unless (or daselt-bind-translate-keys
                                (not daselt-replace-untranslated-keys))
                      (mapcar (lambda (cns)
                                (let ((str (car cns)))
                                  (cons str (string-replace "C-" "A-" str))))
                              daselt-bind-key-translations-alist)))))

(defun daselt-mode--generate-quick-key-variables ()
  "Generate quick key variables used in Daselt configurations.

Utilizes `d-special-quick-keys-bindlist` as a foundation for generation."
  (declare (ftype (function () string)))
  (let* ((filepath
          (concat daselt-mode-pkg-configs-directory "quick-keys.dbf"))

         (keylist (prog1 (cl-remove-duplicates
                          (flatten-list (daselt-dirs-act-on-sexps-in-file
                                         filepath
                                         (lambda ()
                                           (let ((blist (eval (daselt-base-read-region))))
                                             (remq nil (mapcar (lambda (bind)
                                                                 (let ((sig (daselt-bind-string bind)))
                                                                   (if (= 1 (length sig))
                                                                       (string-to-char
                                                                        sig))))
                                                               (cdr blist))))))))
                    (let ((filebuffer (get-file-buffer filepath))) ; `get-file-buffer' can get tripped up by symlinks.
                      (unless daselt-dirs-keep-read-buffers
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
    
    (defvar daselt-mode-quick-key-list keylist
      "Quick key list for Daselt.
Auto-generated using `daselt-mode--generate-quick-key-variables.'")

    (defvar daselt-mode-quick-key-string keystring
      "Quick key string for Daselt.
Auto-generated using `daselt-mode--generate-quick-key-variables.'")

    (defvar daselt-mode-quick-key-string-cons keystringpair
      "Quick key string pair for Daselt.
Auto-generated using `daselt-mode--generate-quick-key-variables.'")))

(defun daselt-mode-generate-tutorial (&optional no-refresh)
  "Generate the Daselt-tutorial.

NO-REFRESH is or optimization-purposes: `daselt-mode' already refreshes
`daselt-dfk', so it's unnecessary to do it again."
  (declare (ftype (function (&optional boolean)
                            ;; void  ; Compiler complains.
                            t)))
  (interactive)
  (unless no-refresh
    (daselt-dfk-import-current-layout))
  (let ((display-buffer-alist-backup display-buffer-alist))
    (condition-case report
        (let ((tutfile (concat daselt-mode-pkg-configs-directory "daselt-mode/daselt-mode.tut"))
              (display-buffer-alist '((".*" display-buffer-full-frame))))
          (find-file tutfile)
          (daselt-base-goto-min)
          (mark-sexp)
          (let ((tuttext (eval (daselt-base-read-region))))
            (deactivate-mark)
            (pop-to-buffer "*daselt-tutorial*")
            (delete-minibuffer-contents)
            (org-mode)
            (visual-line-mode)
            (insert tuttext)
            (daselt-base-goto-min)
            nil)
          (kill-buffer (get-file-buffer tutfile)))
      (error (progn (setq display-buffer-alist display-buffer-alist-backup)
                    (error (error-message-string report)))))))

(defun daselt-mode-redaselt ()
  "Run the `redaselt'-bash-script to switch your keyboard layout.

The keyboard-layout loaded is the d-xkb-variant specified in
the option `daselt-mode-redaselt'."
  (interactive)
  (let ((cmdstr (daselt-base-concat-with-separators
                 " "
                 "redaselt"
                 (if daselt-mode-global-udevmon "--global" "")
                 (daselt-base-namecore daselt-xkb-layout "daselt-xkb-" "-layout")
                 (number-to-string daselt-mode-redaselt-time))))
    (let ((display-buffer-alist
           '(("\\*Async Daselt Buffer\\*"
              (display-buffer-no-window)))))
      (if daselt-mode-global-udevmon
          (shell-command cmdstr "*Async Daselt Buffer*")
        (async-shell-command cmdstr "*Async Daselt Buffer*")))))

(defun daselt-mode-undaselt ()
  "Run the `redaselt'-bash-script to switch your keyboard layout.

The keyboard-layout loaded is the d-xkb-variant specified by
`daselt-xkb-layout'."
  (interactive)
  (let ((cmdstr (daselt-base-concat-with-separators
                 "undaselt"
                 (if daselt-mode-global-udevmon "--global" "")
                 daselt-mode-undaselt)))
    (let ((display-buffer-alist
           '(("\\*Async Daselt Buffer\\*"
              (display-buffer-no-window)))))
      (if daselt-mode-global-udevmon
          (shell-command cmdstr "*Async Daselt Buffer*")
        (async-shell-command cmdstr "*Async Daselt Buffer*")))))

;;;;; The pkg-configs-issue
(defun daselt-mode--pkg-configs-directory-test (dir)
  "Test whether DIR looks like daselt-mode's pkg-configs-directory."
  (declare (ftype (function (str) boolean))
           (pure t))
  (and dir
       (file-exists-p dir)
       (file-exists-p (concat dir "daselt-mode/"))))

(defun daselt-mode--pkg-configs-directory-enter-manually ()
  "Specify manually where the pkg-configs-directory is."
  (declare (ftype (function () string)))
  (let* ((use-file-dialog nil) ; Dialog box doesn't let you select folder (or I was doing something wrong).
         (filename (read-file-name "Please point daselt-mode to its pkg-configs directory (in the root directory of Daselt, include trailing backslash): "
                                   nil nil
                                   #'daselt-mode--pkg-configs-directory-test)))
    (customize-set-variable 'daselt-mode-pkg-configs-directory
                            filename)
    filename))

(defun daselt-mode--find-pkg-configs-directory ()
  "Find daselt-mode's pkg-configs-directory.

Set the corresponding option so it's saved for future sessions.

If the option already points to something that looks like the right directory,
don't do anything."
  (declare (ftype (function () string)))
  (unless (daselt-mode--pkg-configs-directory-test daselt-mode-pkg-configs-directory)
    (condition-case nil (let ((current-pkg-dir
                               (concat daselt-emacs-dir "mode-configs/")))
                          (if (daselt-mode--pkg-configs-directory-test current-pkg-dir)
                              (customize-set-variable 'daselt-mode-pkg-configs-directory
                                                      current-pkg-dir)
                            (daselt-mode--pkg-configs-directory-enter-manually)))
      (error (daselt-mode--pkg-configs-directory-enter-manually)))))

;;;; Mode
;;;###autoload
(define-minor-mode daselt-mode
  "Daselt's minor mode.

Rebinds most keys and revamps Emacs to implement Daselt's shortcut layout.

Note that this assumes you have Daselt installed (most simply using Daselt's
configure script from the repository):

https://gitlab.com/nameiwillforget/d-emacs

During startup, daselt-mode will ask you to set the options
`daselt-xkb-layout' and `daselt-dfk-keyboard-layout-type', unless these
options are non-standard or `daselt-mode-show-tutorial' is nil. In that
case, it will assume you have already set these options correctly. If
the option `daselt-mode-redaselt' is t, it will then run the `redaselt'
shell-script with the values specified.

`daselt-mode' uses a pkg-configs-directory as defined in `daselt-dirs' to
store its configuration. When `daselt-mode' is started, it has to read all
files in this directory. Depending on your hardware and installed packages this
might take between a few seconds and maybe a minute. If you plan on toggling
`daselt-mode' several times you can set `daselt-dirs-keep-read-buffers' to t
to reduce the startup time.

When you quit `daselt-mode', all constants are reset. If the option
`daselt-mode-undaselt' is non-nil, the `undaselt' shell-script is run,
resetting the keyboard layout as well."
  :init-value nil
  :global t
  :interactive t
  :lighter " Daselt"
  (if daselt-mode
      (progn
        ;; Just to be sure that `daselt-emacs-dir' is set (if possible).
        (require 'daselt)

        ;; Set `daselt-dirs-pkg-configs-directory' to `daselt-mode-pkg-configs-directory' while `daselt-mode' is on.
        (unless (daselt-mode--pkg-configs-directory-test daselt-dirs-pkg-configs-directory)
          (daselt-mode--find-pkg-configs-directory)
          (if (bound-and-true-p daselt-dirs-pkg-configs-directory)
              (progn (defvar daselt-daselt-dirs-pkg-configs-directory-backup)
                     (setq daselt-daselt-dirs-pkg-configs-directory-backup
                           daselt-dirs-pkg-configs-directory)))
          (setopt daselt-dirs-pkg-configs-directory
                  daselt-mode-pkg-configs-directory))

        ;; `daselt-mode' without translated keys is borderline unusable.
        (setq daselt-bind-translate-keys t)

        ;; Find out if it's an ansi keyboard
        (unless (or (not daselt-mode-show-tutorial)
                    (not (eq (custom-variable-state 'daselt-dfk-keyboard-layout-type t)
                             'standard)))
          (customize-save-variable 'daselt-dfk-keyboard-layout-type
                                   (daselt-base-remove-text-properties-from-string
                                    (completing-read "Do you have an ansi or iso-keyboard (you have ansi if your left Shift-key is larger than CapsLock)? " daselt-dfk-supported-layout-types nil t))))

        (if daselt-bind-translate-keys
            ;; Add the key translations for C-g and ("C-" . (1 1 -2)) if they aren't there yet.
            (progn (if daselt-bind-translate-C-1-1--2-C-g
                       (let ((transcons
                              `(,(daselt-bind-string `(("C-" . (1 1 -2)))) . "C-g"))
                             (revtranscons
                              `("C-g" . ,(daselt-bind-string `(("C-" . (1 1 -2)))))))
                         (add-to-list 'daselt-bind-key-translations-alist transcons)
                         (add-to-list 'daselt-bind-key-translations-alist revtranscons)))
                   (mapc
                    (lambda (cns) (key-translate (car cns) (cdr cns)))
                    daselt-bind-key-translations-alist)))

        ;; Refresh the daselt-xkb-layouts in case someone has changed bindings.
        (daselt-xkb-generate-layouts)
        (daselt-xkb-set-layouts-list)
        (put 'daselt-xkb-layout 'custom-options daselt-xkb-layouts)

        ;; Choose the layout
        (if (or (not (boundp 'daselt-xkb-layout))
                (and daselt-mode-show-tutorial
                     (eq (custom-variable-state 'daselt-xkb-layout t)
                         'standard)))
            (customize-save-variable 'daselt-xkb-layout
                                     (daselt-base-intern-from-parts
                                      "daselt-xkb"
                                      (completing-read
                                       "Please pick the Daselt layout you want to use: "
                                       (mapcar (lambda (sym)
                                                 (daselt-base-namecore sym
                                                                       "daselt-xkb-"
                                                                       "-layout"))
                                               daselt-xkb-layouts))
                                      "layout")))

        (if daselt-mode-redaselt
            (if (file-exists-p "/usr/share/X11/xkb/symbols/dxkb")
                (daselt-mode-redaselt)
              (error "Please put the dxkb-file into `/usr/share/X11/xkb/symbols/' or deactivate daselt-mode-redaselt")))
        
        ;; For a non-main layout put modifiers outside the layout unless they have been put in by hand. For the main layout, do it the other way around.
        (when (eq (custom-variable-state 'daselt-dfk-outside-mods t)
                  'standard)
          (if (eq (symbol-value 'daselt-xkb-layout)
                  'daselt-xkb-main-layout)
              (setopt daselt-dfk-outside-mods nil)
            (setopt daselt-dfk-outside-mods t)))

        ;; Generate daselt-dfk-layout from the daselt-xkb-layout.
        (daselt-dfk-import-current-layout)

        ;; Set constants
        (when (bound-and-true-p daselt-stump)
          (require 'daselt-stump)
          (setopt daselt-bind-outside-translations-alist
                  (daselt-stump-translate-daselt-keys))

          ;; We can also apply the daselt-stump-bindlists here.
          (daselt-dirs-act-on-pkg-files-by-type-and-maybe-kill
           `(((lambda (filename)
                (daselt-dirs-save-bindlists-in-file filename "daselt-stump")) . "dbl")
             ((lambda (filename)
                (daselt-dirs-save-bindforms-in-file filename "daselt-stump")) . "dbf"))
           daselt-stump-pkg-configs-directory t nil "daselt-stump"))

        ;; Calculate layer boundaries to simplify calculations by `daselt-bind-draw-bindings-from-regexps'.
        (setq daselt-bind-boundaries
              (list (daselt-coords-boundaries
                     (mapcar (lambda (placeval) (car placeval))
                             (daselt-base-flatten-until
                              (daselt-coords-get-layer
                               (daselt-coords-coordinatize-layout
                                (symbol-value daselt-dfk-layout))
                               0)
                              (lambda (lst) (daselt-coords-p (caar lst))))))

                    (daselt-coords-boundaries
                     (mapcar (lambda (placeval) (car placeval))
                             (daselt-base-flatten-until
                              (daselt-coords-get-layer
                               (daselt-coords-coordinatize-layout
                                (symbol-value daselt-xkb-layout))
                               1)
                              (lambda (lst) (daselt-coords-p (caar lst))))))))

        ;; Add all files in pkg-configs to the load-path.
        (let ((default-directory daselt-dirs-pkg-configs-directory))
          (normal-top-level-add-to-load-path '("."))
          (normal-top-level-add-subdirs-to-load-path))

        ;; Add pkg-config-options
        (daselt-dirs-create-pkg-customization-options-function daselt-mode-pkg-configs-directory)

        (setopt daselt-daselt-mode t) ; This should really be enabled (otherwise it won't recurse into the corresponding directory).

        ;; Quick keys
        (daselt-mode--generate-quick-key-variables)

        ;; Add to emulation
        (if daselt-mode-put-daselt-mode-map-into-emulation
            (add-to-list 'emulation-mode-map-alists
                         `((daselt-mode . ,daselt-mode-map))))

        (let ((undo-tree-auto-save-history nil) ; Saving undo-state of opened files is useless here and slows down startup.
              ;; (daselt-bind-eval-log 'daselt-mode-eval-log) ; Currently unused.
              )
          (daselt-dirs-act-on-pkg-files-by-type-and-maybe-kill
           `((daselt-dirs-with-eval-load-elc-or-lispcode-in-file .  "del")
             (daselt-dirs-save-and-with-eval-apply-bindlists-in-file
              . ("dbl" "-special"))

             ;; Do rebinding before other operations, that way if something goes wrong, at least the layout is defined.
             (daselt-dirs-save-bindlists-in-file . ("dbl" "special"))
             (daselt-dirs-save-bindforms-in-file . "dbf")
             (daselt-dirs-with-eval-set-constantlists-in-file . ("dcl" "-special"))
             (daselt-dirs-with-eval-add-advicelists-in-file . ("dal" "-special"))
             (daselt-dirs-with-eval-add-adviceforms-in-file . ("daf" "-special")))
           nil t))

        (if daselt-mode-globalize-daselt-mode-map
            (progn (unless (boundp daselt-global-map-backup)
                     (setq daselt-global-map-backup global-map))
                   (setq global-map daselt-mode-map)))

        (if daselt-mode-show-tutorial (daselt-mode-generate-tutorial t)))

    ;; Remove all eval forms that have been set by `daselt-mode'.
    (daselt-bind--remove-from-after-load-alist)

    ;; Let's also reset all global variables that have not been found by the recursion.
    (daselt-dirs--reset-backed-up-variables)

    ;; Restore the keyboard layout.
    (if daselt-mode-undaselt (daselt-mode-undaselt))))

;;;; Provide
(provide 'daselt-mode)
;;; daselt-mode.el ends here
