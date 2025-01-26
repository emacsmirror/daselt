;;; d-emacs-special-example-user-defined-bindlists.el --- d-emacs-special-example-user-defined-bindlists for d-emacs  -*- lexical-binding: t; -*-
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

;; This file is an example `bindlists'-file for d-emacs or Daselt in general. Each bindlists-file should have a name whose suffix is .el and whose base name ends with `bindlists'. Each `bindlists'-FILE in D-EMACS-DIRECTORY/pkg-configs/d-emacs/ is parsed when `d-emacs-mode' starts and its bindings are read into an Emacs-keymap, except if the name of FILE contains the string `special'. Special `bindlists'-files are used for bindings are not read into a keymap but fulfill some other purpose. For instance, this file is a special `bindlists'-file so its bindings are not applied when `d-emacs-mode' is started. Another purpose of special bindlists is to define constants that are about values of keys, such as `avy-keys'. Daselt-files that do not contain `special' in their filename are also known as `regular' Daselt-files.

;; User-defined bindlists should also have the string `user-defined' in their name. This ensures that they are read after any other `bindlists'-file and thus override bindings in them.

;; Each bindlist in a `bindlists'-file should start with a backquote \`. This is how they are recognized when the `bindlists'-file is parsed. It can then be either

;; 1. A LIST consisting of `bindings', meaning conses whose car is a binding-location (see `d--binding-location-p'), usually either a string as if provided to `kbd', a coordinate or a pair of strings and coordinates. The cdr in a special bindlist is essentially arbitrary, it just cannot contain another binding. In a regular bindlist, the cdr of a binding should evaluate to a command. See below for examples.

;; A LIST like this is known as a `headless' bindlist. The bindings in this bindlist are applied to `DIR-mode-map', where `DIR' is the name of the directory containing the `bindlists'-file that houses LIST.

;; 2. A cons CONS1 whose car is a symbol SYMB and whose cdr is a list as in 1. The car of CONS1 known as its `head' while its cdr is known as its `body'. The head of such a list should be the symbol of the keymap to which the bindings in the body should be applied.

;; When the feature whose name is the name of the containing DIR is loaded, both LISTS as in 1. and CONSES as in 2. are applied to their respective keymaps, after these have been backed up. So, for instance, a headless regular bindlist in the folder `avy' is applied to `avy-mode-map' once `avy' is loaded.

;; Sometimes it is necessary to specify a different eval-condition. Thus, a third bindlist form exists:

;; 3. A cons CONS2 whose car is a symbol or string and whose cdr is a list of conses like CONS1 as in 2. The first element of CONS2 denotes a file or feature that has to be evaluated before the keys in the bindlists in the cdr of CONS2 can be rebound.

;; The bindlists in a `bindlists'-file can be automatically sorted using `d--sort-and-format-bindlists-in-file'. They can be applied using `d-emacs--with-eval-backup-and-apply-bindlists-in-file' and saved as a variable using `d--save-bindlists-in-file-as-variables' so they can be recalled by d-emacs-help-functions. As mentioned, all regular `bindlists' in `D-EMACS-DIRECTORY/pkg-configs/d-emacs/' are read when `d-emacs-mode' is started es well.

;; Below are examples of all three bindlist-types.

;;; Code:

`(((0 0 6) . #'forward-char) ; Bind `<XF86Launch5>' to `forward-char' in `d-emacs-mode-map'.
  ((1 0 2)) ; Unbind (1 0 2).
  (("M-C-" . (5 0 2)) #'backward-char) ; This is how you specify modifiers.
  ) 

`(d-emacs-miscellaneous-map ; Apply bindings to `d-emacs-miscellaneous-map'.
  ((1 0 2) . (if (package-installed-p 'treemacs) #'treemacs-mode #'term-mode)) ; Such clauses are allowed in the cdr. When the bindlist is applied, the cdr is evaluated, yielding a command name.

  ("t" . #'treemacs) ; This is how you specify binding strings.
  ("C-t" . #'term) ; You can put modifiers directly into the binding string.
  )

`(flycheck ; After `flycheck' is loaded…
  (flycheck-mode-map ; In `flycheck-mode-map'…
   (("M-" . (3 -1 1)) . #'treemacs-mode)) ; Bind #' M-(3 -1 1) to treemacs mode.
  (flycheck-command-map
   (("H-C-" . (3 -1 -1)) . #'projectile-compile-project)) ; And H-C-(3 -1 -1) to `projectile-compile-project'.
  )

;; You can see the effect of `d--sort-and-format-bindlists-in-file' by using it here. Note however that it will destroy the comments in bindlists (though you can restore them with an `undo').

;;; d-emacs-special-example-user-defined-bindlists.el ends here
