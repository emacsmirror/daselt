;;; d-emacs-dirs.el --- Functions to act on files throughout directory by type  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Alexander Prähauser

;; Author: Alexander Prähauser <ahprae@protonmail.com>
;; Package-Requires: ((emacs "29.1"))
;; Version: 1.0
;; Keywords: tools
;; URL: https://gitlab.com/nameiwillforget/d-emacs/d-emacs-dirs/

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

;; d-emacs-dirs is a comprehensive utility module designed to manage and manipulate
;; configuration files within a directory.

;; Part of the Daselt suite, this module provides a robust set of functions and
;; derived modes tailored for recursion through directories and handling of
;; various types of configuration files, ensuring efficient organization and
;; streamlined workflows. In particular, it allows the writing of
;; (not-necessarily Emacs) configurations whose application is dependent on
;; installed packages and that can be switched on and off, making it possible to
;; share configurations similarly to how themes are shared.

;; **Key Features:**

;; - **Recursive Directory Traversal:** Efficiently traverses directories,
;;   applying user-defined functions to files based on specified conditions and
;;   file types.

;; - **File Type Management:** Supports multiple configuration file types,
;;   including bindlists (`.dbl'), bindforms (`.dbf'), constantlists (`.dcl'),
;;   advicelists (`.dal'), and adviceforms (`.daf'). Each file type has
;;   dedicated functions and derived modes to facilitate specialized operations.

;; - **Customization Options:** Offers a variety of customizable settings,
;;   allowing users to tailor behaviors such as sorting, saving, and applying
;;   bindlists based on their preferences. These settings are easily adjustable
;;   through Emacs’ customization interface.

;; - **File Saving actions:** Provides file saving actions for supported file
;;   types. These are generated using the macro
;;   `d-emacs-dirs-create-pkg-customization-options', which generates options
;;   that allow for each file saving action an option that controls whether it
;;   is applied when a file of that type is saved. This macro can be used by
;;   other packages or users to automatically generate saving actions for file
;;   types.

;; - **File Operations Utilities:** Provides a suite of utilities for backing
;;   up, setting and re-setting of configuration options.

;; - **Interactive File Selection:** Facilitates easy navigation and selection
;;   of configuration files through interactive prompts, supporting filtering by
;;   file type and type modifiers.

;; - **Error Handling and Buffer Management:** Incorporates robust error
;;   handling mechanisms and offers options to manage buffer states, such as
;;   keeping buffers open after operations or automatically closing them to
;;   maintain a clean workspace.

;; **Usage Scenario:**

;; d-emacs-dirs.el allows the creation of configurations that can easily be
;; shared without requiring that all configured packages are installed in the
;; end user's Emacs. It allows users to apply and re-set provided configurations
;; automatically. The paradigmatic example for this is `d-emacs-mode', which is
;; provided in a separate package. configurations, the module minimizes
;; redundancy, ensures consistency, and enhances the overall efficiency of the
;; user's Emacs environment.

;; **Integration:**

;; To write a configuration that can be read using d-emacs-dirs.el, ensure that
;; your configuration files are organized in a directory DIR. The macro
;; `d-emacs-dirs-create-pkg-customization-options' generates a boolean
;; customization option for each (non-hidden) subdirectory SDIR of DIR whose
;; default state is usually provided by running `package-installed-p' on the
;; name of SDIR (other conditions are possible). When
;; `d-emacs-dirs-act-on-pkg-files-by-type' recurses through the config
;; directory, it usually enters a subdirectory if and only if its custom is set
;; to t (a different behavior can be specified).

;; The configurations that can be written and shared this way are not
;; necessarily Emacs-configurations and not every file type has to be officially
;; supported. An example of a configuration with unsupported filetypes of a
;; non-Emacs program is given by the package `d-emacs-stump', which uses
;; `d-emacs-dirs' to provide a StumpWM configuration along with a script that
;; compiles it into a StumpWM init.

;; Supported filetypes are:

;; - `del' files, which are like `el'-files but have a different ending so
;; saving-actions can be configured for them without having to impact
;; `emacs-lisp-mode'. When the configuration is applied these are evaluated
;; using `d-emacs-dirs-load-elc-or-lispcode-in-file'. Saving actions for them
;; are `d-emacs-dfk-fill-docstrings-of-lispcode-in-file',
;; `d-emacs-dirs-trim-lines-of-lispcode-in-file',
;; `d-emacs-dirs-byte-compile-lispcode-in-file' and
;; `d-emacs-dirs-load-elc-or-lispcode-in-file'.

;; - `dbl' files containing bindlists as defined in `d-emacs-bind'. These are
;; - applied when the configuration is applied. The maps they are applied to are
;; - backed up so they can be re-set later. Saving actions for them are
;; - `d-emacs-dirs-with-eval-apply-bindlists-in-file' and
;; - `d-emacs-dirs-save-bindlists-in-file',
;; - `d-emacs-dirs-sort-and-format-bindlists-in-file'.

;; - `dcl' files containing alists of constant names and what should evaluate to
;; - their values. The given constants are backed up and set to the provided
;; - values. Saving actions for them are
;; - `d-emacs-dirs-with-eval-set-constantlists-in-file.'

;; - `dal' files containig advicelists. See
;; - `d-emacs-dirs-with-eval-add-advicelist' for documentation on the form these
;; - advicelists should have. Saving actions for them are
;; - `d-emacs-dirs-with-eval-add-advicelist'.

;; - `dbf' and `daf' files. These are like `dbl' and `dal' files, except that
;; - every balanced expression in them should evaluate to a bindlist resp. an
;; - advicelist instead of already being one. Saving actions for them are
;; - similar to those of `dal' and `dbl' files, except bindforms cannot be
;; - sorted.

;; All saving actions can be turned on and off using the options prefixed by
;; `d-emacs-dirs-' and suffixed by `-save'.

;; For an example of how a config for `d-emacs-dirs' can look in practice, see
;; the `pkg-configs'-directory of `d-emacs-mode'.

;; This module is distributed under the GNU General Public License v3.0 and is
;; maintained as part of the Daselt project. Contributions and enhancements are
;; welcome to further expand its capabilities and compatibility.

;;; Code:
;;;; Preamble
(require 'd-emacs-base)
(require 'd-emacs-coords)

(declare-function d-emacs-base-fill-string-like-docstring "d-emacs-commands" (str))
(declare-function d-emacs-bind-change-coords-in-bindlist "d-emacs-bind" (blist coordlistlist))
(declare-function d-emacs-bind-save-bindlist-as-variable "d-emacs-bind" (blist &optional pfx))
(declare-function d-emacs-base-read-region "d-emacs-base" (&optional properties))
(declare-function d-emacs-base-trim-lines "nil" nil)
(declare-function d-emacs-bind--sort-and-format-marked-bindlist-string "d-emacs-bind" (&optional coordsonly prefun modlist))
(declare-function d-emacs-base-goto-min "d-emacs-base" nil)
(declare-function d-emacs-base-containing-directory-base-name "d-emacs-base" (filepath))
(declare-function d-emacs-base-geq-p "d-emacs-base" (seq1 seq2))
(declare-function d-emacs-base-compare-by-sequential-predicates "d-emacs-base" (arg1 arg2 &rest predicates))
(declare-function d-emacs-base-funcalls-recursively "d-emacs-base" (obj funtests &optional recursetest formatfun eltcolfun lstcolfun restargs restargfun contt debug))
(declare-function d-emacs-base-def-by-forms "d-emacs-base" (templates &rest mappings))

;;;; Customs
(defgroup d-emacs-dirs
  nil
  "Customization group for d-emacs-dirs-."
  :group 'd-emacs)

(defcustom d-emacs-dirs-pkg-configs-directory
  nil
  "The directory `d-emacs-dirs-act-on-files-by-pkg-type' should act on.

From this directory the different file types are also read that allow for
navigation using `d-emacs-dirs-find-pkg-file-by-type'."
  :type 'directory
  :group 'd-emacs-dirs)

(defcustom d-emacs-dirs-save-default
  t
  "Default behavior for bindlists on file save.

Sets `d-sort-bindlists-at-file-save`, `d-save-bindlists-at-file-save` and
`d-emacs-apply-regular-bindlists-at-file-save` to t, unless overwritten by the
user."
  :type 'boolean
  :group 'd-emacs)

(defcustom d-emacs-dirs-keep-read-buffers
              nil
              "Keep buffers open after d-emacs-functions read them.

If non-nil, previously read buffers will not be closed."
              :type 'boolean
              :group 'd-emacs-dirs)

;;;; Constants
(defconst d-emacs-dirs-pkg-type-modifiers-list
                                        '("user-defined" "special")
                                        "Type modifiers that files in `d-emacs-dirs-pkg-configs-directory ' can have.")

(defconst d-emacs-dirs-supported-type-data-list
  `(("del" "lispcode" ("trim-lines-of" "fill-docstrings-of" "add-el-symlink-for") ("byte-compile" "load-elc-or"))
    ("dal" "advicelists" ("add"))
    ("dbl" "bindlists" ("apply" "save" "sort-and-format"))
    ("dcl" "constantlists" ("set"))
    ("daf" "adviceforms" ("add"))
    ("dbf" "bindforms" ("apply" "save")))
  "List of supported data types along with their save operations.

Each element should be a list consisting of

- the file extension.

- the objects that should be in the files.

- a list of operations that should be applied when the files are saved (if their
  options are t).

All of these atoms should be strings.")

;;;; Functions
;;;;; General
(defun d-emacs-dirs-recurse-through-directory (dir funtests &optional dirtest lstcolfun allfiles sortfun contt)
  "Recursively apply functions to files in DIR based on condition tests.

This function traverses the directory structure of DIR and applies specified
functions to files that satisfy condition tests. The tests and functions are
specified in FUNTESTS, a list of condition-function pairs (like FUNCTION .
TEST). Each TEST takes two arguments: IDX, the index of the position of the file
in the folder, and LIST, the list of file names. Each FUNCTION takes one
argument, the file name.

Optional behavior and customization:

- DIRTEST: A function that determines if a directory should be recursed into. If
  provided, it takes a directory name as input and returns non-nil to recurse
  into that directory; otherwise, all directories are recursed into by default.

- LSTCOLFUN: Collects results using this function if given, otherwise defaults
  to `list', preserving directory structure.

- ALLFILES: Include hidden files if set to non-nil.

- SORTFUN: A sorting function for directory contents, applied if provided.

- CONTT: If non-nil, continue applying tests even after the first match.

The behavior is achieved by using the `d-emacs-base-funcalls-recursively'
function, which handles collecting results and determines whether to recurse
further into directory contents. The results are built into a list reflecting
the processed structure of DIR."
  (declare (ftype (function (string ; DIR
                             list
                             ;; (list (cons (function (string) t)  ; FUNTESTS  ; Compiler complains.
                             ;;             (function (integer (list string)) boolean)))
                             &optional (function (string) boolean) ; DIRTEST
                             (function (list t) list) ; LSTCOLFUN
                             boolean ; ALLFILES
                             (function (list) list)
                             ;; (function (list string) (list string)) ; SORTFUN  ; Compiler complains.
                             boolean) ; CONTT
                            t))) 
  (d-emacs-base-funcalls-recursively
   dir
   funtests
   (lambda (idx lst)
             (and (file-directory-p (nth idx lst))
          (if dirtest (funcall dirtest (nth idx lst)) t)))
   (lambda (directory)
             (let ((maybesorted (directory-files directory t
                                         (if allfiles
                                                             (rx (or (: (not ".") (* not-newline))
                                                     (: "." (+ (not ".")))))
                                           "\\`[^.]"
                                           ) sortfun)))
       (if sortfun
                           (funcall sortfun maybesorted)
                 maybesorted)))
   (lambda (lst result)
             (if result
                         (append lst (list result))
               lst))
   lstcolfun
   nil
   nil
   contt))

(defun d-emacs-dirs--sort-files-for-actions (filelist)
  "Sort FILELIST based on a series of comparison tests.

This function orders the files in FILELIST using several tests defined in
`d-emacs-base-compare-by-sequential-predicates'. The sorting is determined by
the following criteria, in order of primacy:

1. Directories come last.

2. User-defined files come later. A user-defined file is recognized by having
`user-defined' in its name.

3. Evaluation files come earliest possible (`d--eval-file-p').

4. Longer-named files come before shorter ones."
  (declare (ftype (function (list) list)
                  ;; (function ((list string)) (list string)) ; Compiler complains.
                  )
           (pure t))
  (sort filelist (lambda (fname1 fname2)
                   (let ((compval (d-emacs-base-compare-by-sequential-predicates
                                   fname1 fname2
                                   (lambda (fname)
                                     (not (file-directory-p fname)))
                                   (lambda (fname)
                                     (not (string-match-p "user-defined" fname)))
                                   (lambda (fname)
                                     (string-match-p "eval" fname))
                                   #'d-emacs-base-geq-p)))
                     (if compval
                         (car compval))))))

;;;;; Predicates
(defun d-emacs-dirs-custom-t-p (dirpath basedir pfx)
  "Check if the directory DIRPATH has its corresponding custom set to t.

BASEDIR is automatically set to t.

Each option should be named PFX-DIRNAME, where DIRNAME is the filename of the
directory at DIRPATH."
  (declare (ftype (function (string string string) boolean))
           (side-effect-free t))
  (if (string= dirpath basedir)
      t
    (let* ((end (file-name-base (directory-file-name dirpath)))
           (symbol (d-emacs-base-intern-from-parts pfx end)))
      (if (boundp symbol)
          (symbol-value symbol)))))

(defun d-emacs-dirs--standard-file-p (filename)
  "Check if the FILENAME corresponds to a standard file.

This means its base name doesn’t start with a dot and does not contain # or ~."
  (declare (ftype (function (string) boolean))
           (pure t))
  (not (or (string-match-p (rx (or (: string-start ".") "#" "~"))
                           (file-name-base filename))
           (string-match-p (rx (or "#" "~"))
                           (car (last (split-string filename "\\.")))))))

;;;;; pkg-configs
(defun d-emacs-dirs-act-on-pkg-files-by-type (funtypes &optional dir customt sortfun pfx)
  "Apply functions to files of specified types in `pkg-configs'.

This function operates on files located in the `pkg-configs' directory. It
applies each function specified in FUNTYPES to files that match a corresponding
type.

Parameters:

- FUNTYPES: A list of cons cells where each cell contains a function and a type
  \(e.g., (FUNCTION . TYPE)).

- FUN should be a function with one argument that is given the file names of
  files of the right type as input during recursion.

- TYPE can be a string or a predicate function. If it is a string, it is checked
  against the file name extension of each file. It can also be a cons whose car
  is a file name extension and whose cdr consists of type modifier strings.
  These strings are checked against the file name base. If one of the type
  modifiers is `regular', the base name is checked for whether it does not
  contain the word `special'.

- DIR: If provided,execute in this directory.

- CUSTOMT: If non-nil, apply the function only to files in directories whose
  corresponding custom is t, determined by `d-emacs-dirs-custom-t-p'.

- SORTFUN: Function used to sort files in a folder before they are acted on. By
  default this is `d-emacs-dirs--sort-files-for-actions'.

- PFX is only applicable if CUSTOMT is on. In that case it is forwarded to
  `d-emacs-dirs-custom-t-p' as the prefix that the options should have that
  `d-emacs-dirs-custom-t-p' examines. It is `d-emacs' by default.

Collects results using `#'append'.

This process utilizes `d-emacs-dirs-recurse-through-directory' to manage
recursion through directories and applies file sorting using
`d-emacs-dirs--sort-files-for-actions', which ensures user-defined files always
come before others. The function is versatile, operating on files that pass
given type predicates and options.

Usage example:

`(d-emacs-dirs-act-on-pkg-files-by-type `(((lambda (fname)
\(d-emacs-dirs-load-file fname)) . (\"el\" \"regular\"))))'

This loads the code of all regular files in
`d-emacs-dirs-pkg-configs-directory'."
  (declare (ftype (function (list
                             ;; (list (cons (function (string) t) ; Compiler complains.
                             ;;             (or string list
                             ;;                 (list string) 
                             ;;                 (function (string) boolean))))
                             &optional string boolean (function (list) list)
                             ;; (function ((list string) (list string))) ; Compiler complains.
                             string)
                            t)))
  (let ((pfx (or pfx "d-emacs"))
        (dir (or dir d-emacs-dirs-pkg-configs-directory))
        (sortfun (or sortfun #'d-emacs-dirs--sort-files-for-actions))
        (recurseargs
         (mapcar (lambda (funtype)
                   (let* ((fun (car funtype))
                          (type (cdr funtype))
                          (maintype (if (or (stringp type)
                                            (functionp type))
                                        type
                                      (car type)))
                          (typemods (if (and (consp type)
                                             (not (functionp type)))
                                        (cdr type))))
                     (cons fun
                           (lambda (idx filelst)
                             (let ((filepath (nth idx filelst)))
                               (and (d-emacs-dirs--standard-file-p filepath)
                                    (if maintype
                                        (if (stringp maintype)
                                            (string= maintype (file-name-extension filepath))
                                          (if (functionp maintype)
                                              (funcall maintype filepath)))
                                      t)
                                    (cl-every (lambda (typemod)
                                                (if (string= typemod "regular")
                                                    (not (string-match-p "special"
                                                                         filepath))
                                                  (string-match-p typemod filepath)))
                                              typemods)))))))
                 funtypes)))
    (d-emacs-dirs-recurse-through-directory
     dir
     recurseargs
     (lambda (filepath) (if customt (d-emacs-dirs-custom-t-p filepath dir pfx) t))
     #'append
     nil
     sortfun
     t)))

(defun d-emacs-dirs--execute-and-maybe-kill-file-buffer (filename fun)
  "Execute FUN with FILEPATH as an argument.

Kill the buffer corresponding to FILENAME unless D-EMACS-DIRS-KEEP-READ-BUFFERS
is t."
  (declare (ftype (function (string (function (string) t)) t)))
  (prog1 (funcall fun filename)
    (unless d-emacs-dirs-keep-read-buffers (kill-buffer (find-file-noselect filename)))))

(defun d-emacs-dirs-act-on-pkg-files-by-type-and-maybe-kill (funtypes &optional dir customt sortfun pfx)
  "Wrapper around `d-emacs-dirs-act-on-pkg-files-by-type'.

Wraps functions in FUNTYPES in
`d-emacs-dirs--execute-and-maybe-kill-file-buffer'. Convert errors produced by
them into warnings, then send them to `d-act-on-pkg-files-by-type'. See
``d-act-on-pkg-files-by-type'.' for further documentation."
  (let ((newfuntypes (mapcar (lambda (funtype)
                               (let ((fun (car funtype))
                                     (type (cdr funtype)))
                                 (cons (lambda (filepath)
                                         (condition-case err
                                             (d-emacs-dirs--execute-and-maybe-kill-file-buffer
                                              filepath fun)
                                           (error (warn "Error in %s:\n%s"
                                                        filepath
                                                        (error-message-string err)))))
                                       type)))
                             funtypes)))
    (d-emacs-dirs-act-on-pkg-files-by-type newfuntypes dir customt sortfun pfx)))

(defun d-emacs-dirs--file-extensions ()
  "Return all file extensions in `d-emacs-dirs-pkg-configs-directory'."
  (declare (ftype (function () list
                            ;; (list string)
                            ))
           (side-effect-free t))
  (cl-remove-duplicates
   (flatten-list
    (d-emacs-dirs-recurse-through-directory
     d-emacs-dirs-pkg-configs-directory
     `(((lambda (filepath)
          (file-name-extension filepath))
        . (lambda (idx lst) (let ((elt (nth idx lst)))
                         (and (d-emacs-dirs--standard-file-p elt)
                              (not (file-directory-p elt)))))))))
   :test #'string=))

;;;;; File operations
;;;;;; General
(defun d-emacs-dirs-act-on-sexps-in-file (filepath function &optional untangle)
  "Process all lists in the file with FILEPATH by executing FUNCTION on each.

Map lists are identified by starting with a newline and backquote (`). The
results can be collected using `list' or `append' if UNTANGLE is non-nil."
  (declare (ftype (function (string (function () t) &optional boolean) t)))
  (let ((buffer (current-buffer)))
    (set-buffer (find-file-noselect filepath))
    (prog1 (save-excursion
             (cl-labels ((mark-and-unmark-sexp ()
                           (mark-sexp)
                           (prog1 (funcall function)
                             (progn (deactivate-mark)
                                    (forward-sexp)))))

               (d-emacs-base-goto-min)
               (cl-loop while (search-forward "\(" nil t)
                        do (backward-char)
                        if untangle
                        append (mark-and-unmark-sexp)
                        else collect (mark-and-unmark-sexp))))
      (set-buffer buffer))))

;;;;;; del
(defun d-emacs-dirs-fill-docstrings-of-lispcode-in-file (fname)
  "Fill the docstrings in FNAME in the way docstrings should be filled."
  (declare (ftype (function (string)
                            ;; void  ; Compiler complains.
                            t)))
  (with-current-buffer (find-file-noselect fname)
    (d-emacs-base-fill-docstrings-in-buffer)))

(defun d-emacs-dirs-trim-lines-of-lispcode-in-file (fname)
  "Trim the line ends in FNAME."
  (declare (ftype (function (string) void)))
  (with-current-buffer (find-file-noselect fname)
    (d-emacs-base-trim-lines)))

(defun d-emacs-dirs-load-elc-or-lispcode-in-file (fname)
  "If an `elc'-version of FNAME exists, load it.

Otherwise, load FNAME."
  (declare (ftype (function (string) boolean)))
  (let ((elcname (concat (file-name-sans-extension fname) ".elc")))
    (if (file-exists-p elcname)
        (load elcname)
      (load fname))))

(defun d-emacs-dirs-byte-compile-lispcode-in-file (fname)
  "Byte-compile `del'-file FNAME."
  (declare (ftype (function (string) t)))
  (let ((byte-compile-dest-file-function
         (lambda (fname)
           (if (string-match-p (rx ".del" string-end) fname)
               (concat (file-name-sans-extension fname) ".elc")
             (error "Should only be used on a `del'-file")))))
    (byte-compile-file fname)))

(d-emacs-base-def-by-forms
 ((let ((operation-core (d-emacs-base-namecore operation "d-emacs-dirs-")))
    `(defun ,(intern (concat "d-emacs-dirs-with-eval-" operation-core "-lispcode-in-file")) (filepath)
       ,(d-emacs-base-fill-string-like-docstring
         (format "%s FILEPATH after the feature of the parent folder name was loaded.

If the base name of FILEPATH contains the string `-init-', skip the eval condition."
                 operation-core))
       (declare (ftype (function (string) t)))
       (d-emacs-bind-with-eval-unless-init
        filepath #',(intern (concat (symbol-name operation) "-lispcode-in-file"))))))
 (operation d-emacs-dirs-fill-docstrings-of d-emacs-dirs-trim-lines-of d-emacs-dirs-byte-compile d-emacs-dirs-load-elc-or))

(defun d-emacs-dirs-add-el-symlink-for-lispcode-in-file (fname)
  "Add a symlink to FNAME from an eponymous name with an el-extension.

This way, Emacs's help functions can still find definitions in FNAME while you
can open the file as a del-file so that the save hooks are in place.

If an el-file with that name already exists, no new link is added."
  (declare (ftype (function (string) t)))
  (let ((el-name (concat (file-name-sans-extension fname) ".el")))
    (unless (file-exists-p el-name)
      (f-symlink fname el-name))))

;;;;;; dbl
(defun d-emacs-dirs--delete-duplicate-comment-lines ()
                                                      "Delete duplicate comment lines separated by blank lines in current buffer."
                                                      (declare (ftype (function () void)))
                                                      (save-excursion
                                                        (d-emacs-base-goto-min)
                                                        (while (not (eobp))
      (let* ((curline (thing-at-point 'line t))
             (curpos (line-end-position))
             (curtrimline (string-trim curline))
             (nextline "")
             (nexttrimline ""))
        (if (string-match-p (rx string-start ";") curtrimline)
                                                                                                                    (cl-loop while (and (not (eobp))
                                (not (string-match-p (rx string-start (not ";"))
                                                     nexttrimline)))
                     do (progn (forward-line)
                               (setq nextline (thing-at-point 'line t))
                               (setq nexttrimline (string-trim nextline))
                               (if (string= curtrimline nexttrimline)
                                                                                                                                           (delete-region curpos (line-end-position))))))
        (goto-char curpos)
        (unless (eobp)
          (forward-line))))))

(defun d-emacs-dirs-sort-and-format-bindlists-in-file (&optional blistfile coordsonly prefun modifierlist)
  "Sort and format bindlists in specified FILE.

If BLISTFILE is nil, the file corresponding to the current buffer is used. If
PREFUN is provided, it will be applied to each bindlist after elaboration but
before sorting. MODIFIERLIST is a list of modifiers for formatting and ordering
prefix strings. Its default is `d-modifiers-list'. If COORDSONLY is non-nil,
replace matching binding strings with prefix-coords pairs."
  (declare (ftype (function (&optional string boolean (function (list) list) list
                                       ;; (list integer) ; Compiler complains.
                                       )
                            void)))
  (interactive (list (d-emacs-dirs--pick-pkg-file-by-type "bindlists")
                     (yes-or-no-p "Replace suffixes by coordinates? ")))
  (let ((blistfile (or blistfile (buffer-file-name))))
    (d-emacs-dirs-act-on-sexps-in-file
     blistfile
     (lambda () (d-emacs-bind--sort-and-format-marked-bindlist-string
            coordsonly prefun modifierlist)))

    ;; Do some buffer formatting
    (let ((buffer (current-buffer))
          (pos (point)))
      (find-file blistfile)
      (d-emacs-dirs--delete-duplicate-comment-lines)
      (d-emacs-base-trim-lines)

      ;; Remove unnecessary empty lines
      (d-emacs-base-goto-min)
      (while (re-search-forward (rx "\n" (one-or-more "\n")) nil t)
        (replace-match "\n\n"))

      (set-buffer buffer)
      (goto-char pos)))
  nil)

(defun d-emacs-dirs-with-eval-apply-bindlist (blist &optional backuppfx)
  "A wrapper around `d-emacs-dirs-with-eval-apply-bindlist' which sets WITHEVAL
  to t.

Evaluation can still be stopped by putting `-init-' into the base file name of
the containing file.

See `d-emacs-bind-apply-bindlist' for more documentation."
  (d-emacs-bind-apply-bindlist blist backuppfx t))

(defun d-emacs-dirs-with-eval-apply-bindlists-in-file (&optional blistfile backuppfx)
  "Backup and bind all the bindlists in the file BLISTFILE.

BLISTFILE can be selected interactively from available bindlists in
`d-emacs-directory/pkg-configs/d-emacs/'. If BLISTFILE is nil, defaults to the
current buffer's file name. BACKUPPFX is forwarded to
`d-emacs-dirs-with-eval-apply-bindlist'."
  (declare (ftype (function (&optional string string) void)))
  (interactive  (list (d-emacs-dirs--pick-pkg-file-by-type '("dbl" "regular"))))
  (let ((blistfile (or blistfile (buffer-file-name))))
    (d-emacs-dirs-act-on-sexps-in-file
     blistfile (lambda () (d-emacs-dirs-with-eval-apply-bindlist
                      (d-emacs-base-read-region) backuppfx)))))

(defun d-emacs-dirs-save-bindlists-in-file (&optional blistfile pfx)
  "Save all bindlists in BLISTFILE as variables.

If BLISTFILE is nil, uses the current buffer's filename.

PFX is forwarded to `d-emacs-bind-save-bindlist-as-variable'."
  (declare (ftype (function (&optional string string) void)))
  (interactive  (list (d-emacs-dirs--pick-pkg-file-by-type "dbl")))
  (let ((blistfile (or blistfile (buffer-file-name))))
    (d-emacs-dirs-act-on-sexps-in-file
     blistfile
     (lambda () (d-emacs-bind-save-bindlist-as-variable (d-emacs-base-read-region) pfx))))
  nil)

(defun d-emacs-dirs-save-and-with-eval-apply-bindlists-in-file (&optional blistfile pfx)
  "Save all bindlists in BLISTFILE and with eval apply them.

PFX is the prefix for the save and backup variables."
  (declare (ftype (function (&optional string string) void)))
  (let ((blistfile (or blistfile (buffer-file-name))))
    (d-emacs-dirs-act-on-sexps-in-file
     blistfile
     (lambda () 
       (let ((blist (d-emacs-base-read-region)))
         (cl-flet ((apply-from-symbol (symbol)
                     (d-emacs-dirs-with-eval-apply-bindlist (symbol-value symbol))))
           (let ((symbol-s (d-emacs-bind-save-bindlist-as-variable blist pfx)))
             (if (listp symbol-s) ; If `d-emacs-bind-save-bindlist-as-variable' returns a list we know there is a non-trivial eval condition.
                 (with-eval-after-load (d-emacs-bind-head blist)
                   (mapcar #'apply-from-symbol symbol-s))
               (apply-from-symbol symbol-s)))))))
    nil))

(defun d-emacs-dirs-with-eval-reset-bindlists-in-file (&optional blistfile backuppfx)
                              "Backup and bind all the bindlists in the file BLISTFILE.

BLISTFILE can be selected interactively from available bindlists in
`d-emacs-directory/pkg-configs/d-emacs/'. If BLISTFILE is nil, defaults to the
current buffer's file name. BACKUPPFX is forwarded to
`d-emacs-dirs-with-eval-apply-bindlist'."
                              (declare (ftype (function (&optional string string) void)))
                              (interactive  (list (d-emacs-dirs--pick-pkg-file-by-type '("dbl" "regular"))))
                              (let ((blistfile (or blistfile (buffer-file-name)))
        (backuppfx (or backuppfx "d-emacs-")))
    (cl-flet ((restorefun (blist)
                                            (let* ((map (or (d-emacs-bind-head blist)
                                (d-emacs-base-intern-from-parts
                                 (d-emacs-base-containing-directory-base-name
                                  blistfile)
                                 "-mode-map")))
                       (mapname (symbol-name map))
                       (backup (d-emacs-base-intern-from-parts
                                backuppfx mapname "-backup")))
                  (when (boundp backup)
                    (set mapname backup)
                    (makunbound backup)))))
      (d-emacs-dirs-act-on-sexps-in-file
       blistfile (lambda () (let* ((blist (d-emacs-base-read-region))

                              ;; If it has a head and its second element is not a binding,
                              ;; the head should be an eval condition.
                              (double-head-p (and (d-emacs-bind-head blist)
                                                  (not (d-emacs-bind-p (nth 1 blist)))
                                                  (d-emacs-bind-head (car blist))))
                              (evalcond (if double-head-p (d-emacs-bind-head blist))))
                         (d-emacs-bind-with-eval-unless-init
                          blistfile (if double-head-p
                                                                                                (lambda (blist) (mapcar #'restorefun (cdr blist)))
                                                                  (lambda (blist) (restorefun blist)))
                          evalcond))))))
                              nil)

;;;;;; dbf
(defun d-emacs-dirs-save-bindforms-in-file (&optional bformfile pfx)
  "Evaluate each balanced expression in BFORMFILE and feed the output to
`d-emacs-bind-save-bindlist-as-variable'.

PFX is forwarded to `d-emacs-bind-save-bindlist-as-variable'."
  (declare (ftype (function (&optional string string) nil)))
  (interactive  (list (d-emacs-dirs--pick-pkg-file-by-type '("dbf"))))
  (let ((bformfile (or bformfile (buffer-file-name))))
    (d-emacs-dirs-act-on-sexps-in-file
     bformfile
     (lambda () (d-emacs-bind-save-bindlist-as-variable (eval (d-emacs-base-read-region)) pfx))))
  nil)

(defun d-emacs-dirs-with-eval-apply-bindforms-in-file (&optional bformfile backuppfx)
  "Evaluate each balanced expression in BFORMFILE and feed the output to
`d-emacs-dirs-with-eval-apply-bindlist'.

BACKUPPFX is forwarded to `d-emacs-dirs-with-eval-apply-bindlist'."
  (declare (ftype (function (&optional string string) void)))
  (interactive  (list (d-emacs-dirs--pick-pkg-file-by-type '("dbl" "regular"))))
  (let ((bformfile (or bformfile (buffer-file-name))))
    (d-emacs-dirs-act-on-sexps-in-file
     bformfile
     (lambda () (d-emacs-dirs-with-eval-apply-bindlist
            (eval (d-emacs-base-read-region)) backuppfx))))
  nil)

;;;;;; dcl
(defun d-emacs-dirs-with-eval-set-constantlists-in-file (&optional constfile pfx configname)
  "Set all constants in constantlists according to their values in a CONSTFILE.
Utilizes the buffer's file when CONSTFILE is nil. Parameters are set using
`setopt', ensuring customs are set as appropriate.

PFX is the prefix given to the backup. It is `d-emacs-' by default.

To the documentation string of a changed constant is appended a string `This
constant's value was changed by CONFIGNAME in FILE'. The default for CONFIGNAME
is PFX with the last character removed.

The constants are only set once an evaluation condition is fulfilled. If the
first element in a constantlist is not a cons, it is used as the evaluation
condition. Otherwise, the name of the containing directory is used."
  (declare (ftype (function (&optional string string string) void)))
  (interactive  (list (d-emacs-dirs--pick-pkg-file-by-type "constants")))
  (let* ((constfile (or constfile (buffer-file-name)))
         (pfx (or pfx "d-emacs-"))
         (configname (or configname (substring pfx 0 -1))))
    (d-emacs-dirs-act-on-sexps-in-file
     constfile
     (lambda () (let* ((constlist (d-emacs-base-read-region))
                  (head (unless (consp (car constlist)) (car constlist)))
                  (pkgname (d-emacs-base-containing-directory-base-name
                            (buffer-file-name)))
                  (pkgsymb (intern pkgname))
                  (evalcond (if head head pkgsymb)))
             (with-eval-after-load evalcond
               (mapc (lambda (constcons)
                       (let* ((constsymb (car constcons))
                              (constname (symbol-name constsymb))
                              (constval (eval (cdr constcons)))
                              (backupname (concat pfx constname "-backup"))
                              (backupsymb (intern backupname))
                              (symbdoc (get constsymb 'variable-documentation)))
                         (unless (or (boundp backupsymb) ; Don't overwrite an existing backup.
                                     (not (boundp constsymb)))
                           (set backupsymb (symbol-value constsymb))
                           (put backupsymb 'variable-documentation symbdoc))
                         (set constsymb constval)
                         (put constsymb 'variable-documentation (concat symbdoc (format "\n\nThis symbol's value was changed by %s. in %s" configname (buffer-file-name))))))
                     constlist))))))
  nil)

(defun d-emacs-dirs-with-eval-reset-constantlists-in-file (&optional constfile pfx)
  "For each constant in a constantlist in this file, reset the constant if a
  backup exists.

The backup should be a variable of the form PFX-CONSTNAME-backup.

Resetting is only done once an evalcondition is fulfilled. The evalcondition is
calculated the same way as by
`d-emacs-dirs--with-eval-backup-and-set-constants-in-file'. See there for more
documentation."
  (declare (ftype (function (&optional string string) void)))
  (interactive  (list (d-emacs-dirs--pick-pkg-file-by-type "constants")))
  (let* ((constfile (or constfile (buffer-file-name)))
         (pfx (or pfx "d-emacs-")))
    (d-emacs-dirs-act-on-sexps-in-file
     constfile
     (lambda () (let* ((constlist (d-emacs-base-read-region))
                  (head (unless (consp (car constlist)) (car constlist)))
                  (pkgname (d-emacs-base-containing-directory-base-name
                            (buffer-file-name)))
                  (pkgsymb (intern pkgname))
                  (evalcond (if head head pkgsymb)))
             (with-eval-after-load evalcond
               (mapc (lambda (constcons)
                       (let* ((constsymb (car constcons))
                              (constname (symbol-name constsymb))
                              (backupname (concat pfx constname "-backup"))
                              (backupsymb (intern backupname)))
                         (when (eval `(bound-and-true-p ,backupsymb))
                           (setopt--set constsymb (symbol-value backupsymb))
                           (put constsymb 'variable-documentation
                                (get backupsymb 'variable-documentation))
                           (makunbound backupsymb))))
                     constlist))))))
  nil)

;;;;;; dal
(d-emacs-base-def-by-forms ((let ((addp (string= str "add")))
                              `(defun ,(intern (concat "d-emacs-dirs-with-eval-" str "-advicelist")) (adlist)
                                 ,(d-emacs-base-fill-string-like-docstring
                                   (format "For all all advice combinations in ADLIST,
%s the advice to the corresponding function after EVALCOND is fulfilled.
EVALCOND is given either as the head of the list if its car is a head (meaninig
not a cons) or by the name of the containing directory.

ADLIST should consist of

- an optional EVALCOND (a string or symbol).

- lists ADENTRY such that

  - the first element of ADENTRY should be a list of SYMBOLS as in the
    documentation of `advice-add' that the advice should be applied to.

  - the second element should be a symbol like the HOW in `advice-add'.

  - the third entry should be a list of FUNCTIONS as in `advice-add' that should
    be applied to all SYMBOLS." str))
                                 (declare (ftype (function (list)
                                                           ;; void  ; Compiler complains.
                                                           t)))
                                 (let* ((head (unless (consp (car adlist)) (car adlist)))
                                        (pkgname (d-emacs-base-containing-directory-base-name
                                                  (buffer-file-name)))
                                        (pkgsymb (intern pkgname))
                                        (evalcond (if head head pkgsymb))
                                        (radlist (if head (cdr adlist) adlist)))
                                   (with-eval-after-load evalcond
                                     (mapcar (lambda (adv)
                                               (let (,@(remq nil
                                                             (list
                                                              `(advfun #',(intern (concat "advice-" str)))
                                                              `(symbs (car adv))
                                                              `(funs (caddr adv))
                                                              (if addp '(how (cadr adv)))
                                                              (if addp '(props (cadddr adv))))))
                                                 (mapcar (lambda (symb)
                                                           (mapcar
                                                            (lambda (fun)
                                                              (apply advfun
                                                                     (remq nil (list
                                                                                symb
                                                                                ,(if addp `how)
                                                                                fun
                                                                                ,(if addp `props)))))
                                                            funs))
                                                         symbs)))
                                             radlist))
                                   nil)))

                            `(defun ,(intern (concat "d-emacs-dirs-with-eval-" str "-advicelists-in-file")) (&optional alfile)
                               ,(d-emacs-base-fill-string-like-docstring
                                 (format "%s all advices in the d-emacs-advices-file
whose name is ALFILE. Treats the alfile of the current buffer as a
default." str))
                               (declare (ftype (function (&optional string) void)))
                               (interactive (list (d-emacs-dirs--pick-pkg-file-by-type "dal")))
                               (let ((alfile (or alfile (buffer-file-name))))
                                 (d-emacs-dirs-act-on-sexps-in-file
                                  alfile
                                  (lambda ()
                                    (,(intern (concat "d-emacs-dirs-with-eval-" str "-advicelist"))
                                     (d-emacs-base-read-region)))))
                               nil)

                            `(defun ,(intern (concat "d-emacs-dirs-with-eval-" str "-adviceforms-in-file")) (&optional alffile)
                               ,(d-emacs-base-fill-string-like-docstring
                                 (format "Evaluate every balanced expression in ALFFILE and feed the output to `d-emacs-dirs-with-eval-%s-advicelist'." str))
                               (declare (ftype (function (&optional string)
                                                         ;; void
                                                         t)))
                               (interactive (list (d-emacs-dirs--pick-pkg-file-by-type "daf")))
                               (let* ((alffile (or alffile (buffer-file-name))))
                                 (d-emacs-dirs-act-on-sexps-in-file
                                  alffile
                                  (lambda () (let ((adlist (eval (d-emacs-base-read-region))))
                                          (funcall #',(intern (concat "d-emacs-dirs-with-eval-"
                                                                      str
                                                                      "-advicelist"))
                                                   adlist)))))
                               nil))
                           (str . ("add" "remove")))

;;;;; Global operations
;;;;;; del
(defun d-emacs-dirs-compile-del-files (&optional directory)
  "Byte-compile all `del'-files throughout DIRECTORY.

DIRECTORY is `d-emacs-dirs-pkg-configs-directory' by default."
  (declare (ftype (function (&optional string)
                            ;; void
                            t)))
  (interactive)
  (d-emacs-dirs-act-on-pkg-files-by-type
   `(((lambda (filename)
        (byte-compile-file filename))
      .
      "del"))
   (if directory directory))
  nil)

;;;;;; dbl
(defun d-emacs-dirs--exchange-coordinates (coordlistlist &optional modlist coordsonly directory)
  "Exchange coordinates in all bindlists in `d-emacs/pkg-configs' or DIRECTORY.

COORDLISTLIST is a list of lists where the kth entry in the nth contained list
specifies the value that is to be used to replace the values matching the k-1th
entry. Entries matching the k-th entry are to be replaced by the k+1-th entry.
The last entry in the list is not replaced by any other entry but just remains
how it is if it is matched.

- For instance, to permute the first coordinate, specify the first list of
  COORDSLISTLIST as a list starting and ending with the same entry.

If you don't want to change a coordinate, you can feed the function nil instead
of a COORDLIST. The optional arguments are directly forwarded to
d-emacs-dirs--sort-and-format-bindlists, see the documentation there for their
function.

MODLIST, COORDSONLY and DIRECTORY are forwarded to
`d-sort-and-format-bindlists'."
  (declare (ftype (function ((list (list number)) &optional list
                             ;; (list integer) ; Compiler complains.
                             boolean string)
                            void)))
  (d-emacs-dirs--sort-and-format-bindlists
   coordsonly
   (lambda (blist) (d-emacs-bind-change-coords-in-bindlist blist coordlistlist))
   modlist
   directory)
  nil)

(defun d-emacs-dirs--sort-and-format-bindlists (&optional coordsonly prefun modlist directory)
  "Recurse through `d-emacs/pkg-configs' and format all bindlists within.
If PREFUN is specified, it denotes a function to run on each bindlist once its
bidings are in an elaborate form.

If COORDSONLY is t, prefer coordinates to suffixes when reducing elaborate
bindings.

 If MODLIST is given, use it to order modifiers instead of
 `d-emacs-bind-modifiers-list'.

If `DIRECTORY' is given, it should be a subdirectory of `pkg-configs'. In that
case, recurse through `DIRECTORY'."
  (declare (ftype (function (&optional boolean (function (list) list) list
                                       ;; (list integer) ; Compiler complains.
                                       string)
                            void)))
  (interactive)
  (d-emacs-dirs-act-on-pkg-files-by-type
   `(((lambda (filename) (d-emacs-dirs-sort-and-format-bindlists-in-file
                     filename ,coordsonly ,prefun ,modlist))
      .
      "bindlists"))
   (if directory directory))
  nil)

;;;;;; dcl
(defun d-emacs-dirs--reset-backed-up-variables (&optional pfx)
  "Restore each variable named VAR for which a variable `PFX-VAR-backup' exists.

Unbinds the backup-symbols.

PFX is `d-emacs-' by default."
  (declare (ftype (function (&optional string)
                            ;; void
                            t)))
  (mapc (lambda (symb)
          (let* ((pfx (or pfx "d-emacs-"))
                 (symbname (symbol-name symb))
                 (pfxend (progn (string-match pfx symbname)
                                (match-end 0)))
                 (sfxbgn (progn (string-match "-backup" symbname)
                                (match-beginning 0)))
                 (origsymbname (substring symbname pfxend sfxbgn))
                 (origsymb (intern origsymbname)))
            (if (boundp symb)
                (progn (set origsymb (symbol-value symb))
                       (makunbound symb)))))
        (apropos-internal "d-emacs-.*-backup"))
  nil)

;;;;; Finding files
(defun d-emacs-dirs--pick-pkg-file-by-type (type &optional subdir nodefault)
  "Select a file in pkg-configs by TYPE.

TYPE can be any expression that can act as a type specifier for
`d-emacs-dirs-act-on-pkg-files-by-type'. Restrict to files in SUBDIR if
specified. Return nil if no file is chosen. If NODEFAULT is nil, mention a
default in the prompt."
  (declare (ftype (function (t
                             ;; (or string list
                             ;;     ;; (list string) ; Compiler complains.
                             ;;     (function (string) boolean))
                             &optional string boolean)
                            string)))
  (let* ((filelist (d-emacs-dirs-act-on-pkg-files-by-type
                    `((identity . ,type))
                    subdir))
         (redfilelist (mapcar #'file-name-base filelist))
         (chosenfile (concat (completing-read
                              (concat "Choose file " (unless nodefault
                                                       "(default this file): "))
                              redfilelist)
                             "." (if (consp type) (car type) type)))
         (match (if (string-empty-p chosenfile)
                    (buffer-file-name)
                  (cl-loop for filepath in filelist
                           do (if (string= chosenfile
                                           (file-name-nondirectory filepath))
                                  (cl-return filepath))))))
    match))

(defun d-emacs-dirs-find-pkg-file-by-type (type &optional typemodifiers)
  "Pick a file of a Daselt-type TYPE with modifiers TYPEMODIFIERS and visit it.

The only difference to `find-file' is in the interactive completion, which asks
for a filetype in `d-pkg-file-types-list' and some type modifiers in
`d-emacs-dirs-pkg-type-modifiers-list', then displays all files of that type
with those modifiers."
  (declare (ftype (function (string &optional list
                                    ;; (list string) ; Compiler complains.
                                    )
                            ;; void
                            t)))
  (interactive (let* ((type (completing-read "Main type: "
                                             (d-emacs-dirs--file-extensions)))
                      (typemodifiers (cl-loop for repl = (completing-read "Type modifier (empty to exit): "
                                                                          d-emacs-dirs-pkg-type-modifiers-list)
                                              while (not (string-empty-p repl))
                                              collect repl)))
                 (list type typemodifiers)))
  (let ((filepath (d-emacs-dirs--pick-pkg-file-by-type (cons type typemodifiers))))
    (find-file filepath)))

(defun d-emacs-dirs-find-bindlists-file ()
  "Visit a bindlists file.

With a prefix argument, only regular bindlists files are considered."
  (declare (ftype (function () void)))
  (interactive)
  (d-emacs-dirs-find-pkg-file-by-type "dbl" (if current-prefix-arg '("regular"))))

;;;;; Macros
(defmacro d-emacs-dirs-create-pkg-customization-options (&optional dir group deffun)
  "Create Boolean customization options from the folders in DIR.

By default, DIR is `d-emacs-dirs-pkg-configs-directory'.

The group for the options is GROUP, which is `d-emacs' by default. All options
are prefixed with `GROUP-'.

DEFFUN should evaluate to a condition that determines whether a generated custom
is enabled by default. Its default is `featurep'."
  (let* ((dir (or dir d-emacs-dirs-pkg-configs-directory))
         (group (or group 'd-emacs))
         (pfx (concat (symbol-name group) "-"))
         (deffun (or deffun (lambda (pkg) (let ((val (featurep pkg)))
                                       (if val t)))))
         (customlist (mapcar (lambda (pkg)
                                                 `(defcustom ,(intern (concat pfx (symbol-name pkg)))
                                                    ,(funcall deffun pkg)
                                                    ,(d-emacs-base-fill-string-like-docstring
                                    (format "Set to t to have `d-emacs-dirs-act-on-pkg-files-by-type' recurse into the %sdirectory whose name is %s." pfx pkg))
                                                    :type 'boolean
                                                    :group ',group))
                             (d-emacs-dirs-recurse-through-directory
                              dir
                              `(((lambda (filepath) (intern (file-name-base filepath)))
                                 .
                                 (lambda (idx lst) (file-directory-p (nth idx lst)))))
                              nil #'append))))
    (append '(progn) customlist)))

(defmacro d-emacs-dirs-create-pkg-customization-options-by-variable (&optional dirvar pfx defval)
  "Like `d-emacs-dirs-create-pkg-customization-options', but DIRVAR should be a
  variable containing a directory path.

See `d-emacs-dirs-create-pkg-customization-options' for more documentation."
  `(d-emacs-dirs-create-pkg-customization-options ,(symbol-value dirvar) ,pfx ,defval))

(defmacro d-emacs-dirs-create-save-customized-modes-30 (typelist &optional pfx defaultvalfun)
  "Create the prerequisites for the save behavior of file types in TYPELIST.

TYPELIST is expected to be a list where each entry has the format: (TYPE OBJECT
[BEFORE_OPERATIONS] [AFTER_OPERATIONS] [MODE] [OVERRIDE])

 - TYPE is the filename-extension.

 - OBJECT is the name of the objects that are supposed to be in the files in
   question (e.g. `bindlists' `lispcode'). Can be in plural.

 - BEFORE_OPERATIONS is a list of operations that should be applied to the file
   before it is saved (see below for the exact naming scheme).

 - AFTER_OPERATIONS is a list of operations that should be applied to the file
   after it is saved.

 - MODE is the mode from which the major mode for the file type derives
   (`emacs-lisp-mode' by default).

 - OVERRIDE is a boolean that specifies that the new mode should be put into
   `auto-mode-alist' as the default mode even is another mode is already used
   for this file extension.

PFX is the prefix that all generated objects should have.

DEFAULTVALFUN is an optional function that produces the default value for a save
option based on the TYPE.

For each TYPE, this macro generates

 - An option `PFX-TYPE-save-default'.

 - A major mode `PFX-OBJECT-mode', which is automatically added to
   `auto-mode-alist' unless another mode is used for that file ending and
   OVERRIDE is not t. `PFX-OBJECT-mode' is derived from MODE and does nothing on
   its own except add the functions `PFX-TYPE-TIMEPOINT-save-function' to the
   before- and after-save-hooks for file buffer of file type TYPE.

 - For each BEFORE-OPERATION in the BEFORE-OPERATIONS of TYPE an option
   `PFX-BEFORE-OPERATION-OBJECT-before-TYPE-save'. The default value of these is
   calculated using DEFAULTVALFUN. If DEFAULTVALFUN is nil, it is the value of
   `PFX-TYPE-save-default'.

 - For each AFTER-OPERATION in the AFTER-OPERATIONS of TYPE an option
   `PFX-AFTER-OPERATION-OBJECT-after-TYPE-SAVE'.

 - Functions `PFX-TYPE-TIMEPOINT-save-function' (with TIMEPOINT being `before'
   or `after'). These retrieve the bound variables named
   `PFX-OPERATION-OBJECT-TIMEPOINT-TYPE-save', check for each value whether it
   is t and, if so, look if `PFX-with-eval-OPERATION-OBJECT-in-file' is
   function-bound. If so, they execute it. If not, they look if
   `PFX-OPERATION-OBJECT-in-file' is function-bound. If so they execute it. If
   not, they throw an error."
  (let ((defaultvalfun (or defaultvalfun (lambda (&optional type _objects _before-operation)
                                           (symbol-value
                                            (d-emacs-base-intern-from-parts
                                             "d-emacs-dirs"
                                             type "save-default")))))
        (pfx (or pfx "d-emacs-dirs"))
        (general-creation-notice "This %s was generated by the macro `d-emacs-dirs-create-save-customized-modes'"))
    (cl-flet ((creation-notice (definition-type) (format general-creation-notice definition-type)))

      `(d-emacs-base-def-by-forms

        ;; Default custom
        (`(defcustom ,(d-emacs-base-intern-from-parts ,pfx type "save-default")
            d-emacs-dirs-save-default
            (d-emacs-base-fill-string-like-docstring
             ,(format "Default for file-save-options of %s-files.

%s" type ,(creation-notice "option")))
            :type 'boolean
            :group (intern ,,pfx))

         ;; Major mode
         ,(let* ((time-expressions (mapcar (lambda (timepoint)
                                             (list `(d-emacs-base-intern-from-parts ,timepoint "save-hook")
                                                   `(d-emacs-base-intern-from-parts ,pfx type ,timepoint "save-function")))
                                           '("before" "after")))
                 (before-hook-expression (caar time-expressions))
                 (after-hook-expression (car (nth 1 time-expressions)))
                 (before-function-expression (nth 1 (car time-expressions)))
                 (after-function-expression (nth 1 (nth 1 time-expressions))))
            ``(define-derived-mode ,(d-emacs-base-intern-from-parts ,pfx objects "mode")
                emacs-lisp-mode ,type
                (d-emacs-base-fill-string-like-docstring
                 ,(format "A mode for `%s'-files.

Derived from emacs-lisp-mode, but adds %s and %s to %s and %s respectively.

%s" type ,before-function-expression ,after-function-expression ,before-hook-expression ,after-hook-expression ,(creation-notice "mode")))
                (add-hook ',,before-hook-expression ',,before-function-expression 99 t)
                (add-hook ',,after-hook-expression ',,after-function-expression 99 t)))

         ;; Add to auto-mode-alist
         `(let* ((extension-rx ,(rx-to-string `(: ,type string-end) t))
                 (mode-auto (cons extension-rx ',(d-emacs-base-intern-from-parts ,pfx objects "mode"))))
            (if (or ,override (not (cl-member mode-auto auto-mode-alist
                                              :test (lambda (mode-auto member)
                                                      (string= (car mode-auto) (car member))))))
                (add-to-list 'auto-mode-alist mode-auto)))

         ;; Before-operations-customs
         (if before-operation
             `(defcustom ,(d-emacs-base-intern-from-parts ,pfx before-operation objects "before" type "save")
                (funcall ,,defaultvalfun ,type)
                (d-emacs-base-fill-string-like-docstring ,(format "Whenever a %s-file is saved, %s the %s in it.

%s" type before-operation objects ,(creation-notice "custom")))
                :type 'boolean
                :group (intern ,,pfx)))

         ;; After-operations-customs
         (if after-operation
             `(defcustom ,(d-emacs-base-intern-from-parts ,pfx after-operation objects "after" type "save")
                (funcall ,,defaultvalfun ,type)
                (d-emacs-base-fill-string-like-docstring
                 ,(format "Whenever a %s-file is saved, %s the %s in it.

%s" type after-operation objects ,(creation-notice "custom")))
                :type 'boolean
                :group (intern ,,pfx)))

         ;; Before- and after-save functions
         ,@(mapcar (lambda (time-point)
                     ``(defun ,(d-emacs-base-intern-from-parts ,pfx type ,time-point "save-function") ()
                         (d-emacs-base-fill-string-like-docstring
                          (format "Function run %s %s-files are saved.

Finds all OPTIONs fulfilling the regexp `%s-dirs-\(.*\)-%s-before-%s-save',
where the regexp-group is some OPERATION.

If OPTION is t, it checks whether `d-emacs-dirs-with-eval-OPERATION-%s-in-file' is function-bound
and, if so, calls it with the name of the current file. Otherwise, it calls `d-emacs-dirs-OPERATION-%s-in-file'.

%s" ,,time-point ,type ,,pfx ,objects ,type ,objects ,objects ,,(creation-notice "function")))
                         (let ((namerx ,(rx-to-string `(: ,,pfx "-"
                                                          (group (zero-or-more not-newline))
                                                          "-" ,(d-emacs-base-concat-with-separators
                                                                "-" objects ,time-point type "save"))
                                                      t)))
                           (dolist (cust (d-emacs-base-filter-obarray (lambda (sym)
                                                                        (and (string-match-p namerx (symbol-name sym))
                                                                             (boundp sym)))))
                             (let ((operation (progn (string-match namerx (symbol-name cust))
                                                     (match-string 1 (symbol-name cust)))))
                               (when (symbol-value cust)
                                 (let ((evalfunsym
                                        (d-emacs-base-intern-from-parts
                                         ,,pfx "with-eval" operation ,objects "in-file"))
                                       (nonevalfunsym (d-emacs-base-intern-from-parts
                                                       ,,pfx operation ,objects "in-file")))
                                   (funcall (if (fboundp evalfunsym)
                                                evalfunsym
                                              (if (fboundp nonevalfunsym)
                                                  nonevalfunsym
                                                (error "No function exists that corresponds to option %s (should be either %s or %s)"
                                                       cust evalfunsym nonevalfunsym)))
                                            (buffer-file-name)))))))))
                   '("before" "after")))
        (type ,@(mapcar #'car typelist))
        (objects ,@(mapcar (lambda (lst) (nth 1 lst)) typelist))
        (before-operation ,@(mapcar (lambda (lst) (nth 2 lst)) typelist))
        (after-operation ,@(mapcar (lambda (lst) (nth 3 lst)) typelist))
        (mode ,@(mapcar (lambda (lst) (nth 4 lst)) typelist))
        (override ,@(mapcar (lambda (lst) (nth 5 lst)) typelist))))))

(defmacro d-emacs-dirs-create-save-customized-modes-29 (typelist &optional pfx)
  "Create the prerequisites for the save behavior of file types in TYPELIST.

TYPELIST is expected to be a list where each entry has the format: (TYPE OBJECT
[BEFORE_OPERATIONS] [AFTER_OPERATIONS] [MODE] [OVERRIDE])

 - TYPE is the filename-extension.

 - OBJECT is the name of the objects that are supposed to be in the files in
   question (e.g. `bindlists' `lispcode'). Can be in plural.

 - BEFORE_OPERATIONS is a list of operations that should be applied to the file
   before it is saved (see below for the exact naming scheme).

 - AFTER_OPERATIONS is a list of operations that should be applied to the file
   after it is saved.

 - MODE is the mode from which the major mode for the file type derives
   (`emacs-lisp-mode' by default).

 - OVERRIDE is a boolean that specifies that the new mode should be put into
   `auto-mode-alist' as the default mode even is another mode is already used
   for this file extension.

PFX is the prefix that all generated objects should have.

For each TYPE, this macro generates

 - An option `PFX-TYPE-save-default'.

 - A major mode `PFX-OBJECT-mode', which is automatically added to
   `auto-mode-alist' unless another mode is used for that file ending and
   OVERRIDE is not t. `PFX-OBJECT-mode' is derived from MODE and does nothing on
   its own except add the functions `PFX-TYPE-TIMEPOINT-save-function' to the
   before- and after-save-hooks for file buffer of file type TYPE.

 - For each BEFORE-OPERATION in the BEFORE-OPERATIONS of TYPE an option
   `PFX-BEFORE-OPERATION-OBJECT-before-TYPE-save'. The default value of these is
   calculated using DEFAULTVALFUN. If DEFAULTVALFUN is nil, it is the value of
   `PFX-TYPE-save-default'.

 - For each AFTER-OPERATION in the AFTER-OPERATIONS of TYPE an option
   `PFX-AFTER-OPERATION-OBJECT-after-TYPE-SAVE'.

 - Functions `PFX-TYPE-TIMEPOINT-save-function' (with TIMEPOINT being `before'
   or `after'). These retrieve the bound variables named
   `PFX-OPERATION-OBJECT-TIMEPOINT-TYPE-save', check for each value whether it
   is t and, if so, look if `PFX-with-eval-OPERATION-OBJECT-in-file' is
   function-bound. If so, they execute it. If not, they look if
   `PFX-OPERATION-OBJECT-in-file' is function-bound. If so they execute it. If
   not, they throw an error."
  (let ((pfx (or pfx "d-emacs-dirs"))
        (general-creation-notice "This %s was generated by the macro `d-emacs-dirs-create-save-customized-modes'"))
    (cl-flet ((creation-notice (definition-type) (format general-creation-notice definition-type)))

      `(d-emacs-base-def-by-forms

        ;; Default custom
        (`(defcustom ,(d-emacs-base-intern-from-parts ,pfx type "save-default")
            d-emacs-dirs-save-default
            (d-emacs-base-fill-string-like-docstring
             ,(format "Default for file-save-options of %s-files.

%s" type ,(creation-notice "option")))
            :type 'boolean
            :group (intern ,,pfx))

         ;; Major mode
         ,(let* ((time-expressions (mapcar (lambda (timepoint)
                                             (list `(d-emacs-base-intern-from-parts ,timepoint "save-hook")
                                                   `(d-emacs-base-intern-from-parts ,pfx type ,timepoint "save-function")))
                                           '("before" "after")))
                 (before-hook-expression (caar time-expressions))
                 (after-hook-expression (car (nth 1 time-expressions)))
                 (before-function-expression (nth 1 (car time-expressions)))
                 (after-function-expression (nth 1 (nth 1 time-expressions))))
            ``(define-derived-mode ,(d-emacs-base-intern-from-parts ,pfx objects "mode")
                emacs-lisp-mode ,type
                (d-emacs-base-fill-string-like-docstring
                 ,(format "A mode for `%s'-files.

Derived from emacs-lisp-mode, but adds %s and %s to %s and %s respectively.

%s" type ,before-function-expression ,after-function-expression ,before-hook-expression ,after-hook-expression ,(creation-notice "mode")))
                (add-hook ',,before-hook-expression ',,before-function-expression 99 t)
                (add-hook ',,after-hook-expression ',,after-function-expression 99 t)))

         ;; Add to auto-mode-alist
         `(let* ((extension-rx ,(rx-to-string `(: ,type string-end) t))
                 (mode-auto (cons extension-rx ',(d-emacs-base-intern-from-parts ,pfx objects "mode"))))
            (if (or ,override (not (cl-member mode-auto auto-mode-alist
                                              :test (lambda (mode-auto member)
                                                      (string= (car mode-auto) (car member))))))
                (add-to-list 'auto-mode-alist mode-auto)))

         ;; Before-operations-customs
         (if before-operation
             `(defcustom ,(d-emacs-base-intern-from-parts ,pfx before-operation objects "before" type "save")
                (symbol-value
                 (d-emacs-base-intern-from-parts
                  "d-emacs-dirs"
                  ,type "save-default"))
                (d-emacs-base-fill-string-like-docstring ,(format "Whenever a %s-file is saved, %s the %s in it.

%s" type before-operation objects ,(creation-notice "custom")))
                :type 'boolean
                :group (intern ,,pfx)))

         ;; After-operations-customs
         (if after-operation
             `(defcustom ,(d-emacs-base-intern-from-parts ,pfx after-operation objects "after" type "save")
                (symbol-value
                 (d-emacs-base-intern-from-parts
                  "d-emacs-dirs"
                  ,type "save-default"))
                (d-emacs-base-fill-string-like-docstring
                 ,(format "Whenever a %s-file is saved, %s the %s in it.

%s" type after-operation objects ,(creation-notice "custom")))
                :type 'boolean
                :group (intern ,,pfx)))

         ;; Before- and after-save functions
         ,@(mapcar (lambda (time-point)
                     ``(defun ,(d-emacs-base-intern-from-parts ,pfx type ,time-point "save-function") ()
                         (d-emacs-base-fill-string-like-docstring
                          (format "Function run %s %s-files are saved.

Finds all OPTIONs fulfilling the regexp `%s-dirs-\(.*\)-%s-before-%s-save',
where the regexp-group is some OPERATION.

If OPTION is t, it checks whether `d-emacs-dirs-with-eval-OPERATION-%s-in-file' is function-bound
and, if so, calls it with the name of the current file. Otherwise, it calls `d-emacs-dirs-OPERATION-%s-in-file'.

%s" ,,time-point ,type ,,pfx ,objects ,type ,objects ,objects ,,(creation-notice "function")))
                         (let ((namerx ,(rx-to-string `(: ,,pfx "-"
                                                          (group (zero-or-more not-newline))
                                                          "-" ,(d-emacs-base-concat-with-separators
                                                                "-" objects ,time-point type "save"))
                                                      t)))
                           (dolist (cust (d-emacs-base-filter-obarray (lambda (sym)
                                                                        (and (string-match-p namerx (symbol-name sym))
                                                                             (boundp sym)))))
                             (let ((operation (progn (string-match namerx (symbol-name cust))
                                                     (match-string 1 (symbol-name cust)))))
                               (when (symbol-value cust)
                                 (let ((evalfunsym
                                        (d-emacs-base-intern-from-parts
                                         ,,pfx "with-eval" operation ,objects "in-file"))
                                       (nonevalfunsym (d-emacs-base-intern-from-parts
                                                       ,,pfx operation ,objects "in-file")))
                                   (funcall (if (fboundp evalfunsym)
                                                evalfunsym
                                              (if (fboundp nonevalfunsym)
                                                  nonevalfunsym
                                                (error "No function exists that corresponds to option %s (should be either %s or %s)"
                                                       cust evalfunsym nonevalfunsym)))
                                            (buffer-file-name)))))))))
                   '("before" "after")))
        (type ,@(mapcar #'car typelist))
        (objects ,@(mapcar (lambda (lst) (nth 1 lst)) typelist))
        (before-operation ,@(mapcar (lambda (lst) (nth 2 lst)) typelist))
        (after-operation ,@(mapcar (lambda (lst) (nth 3 lst)) typelist))
        (mode ,@(mapcar (lambda (lst) (nth 4 lst)) typelist))
        (override ,@(mapcar (lambda (lst) (nth 5 lst)) typelist))))))

(defmacro d-emacs-dirs-create-save-customized-modes-by-variable (typelistsym &optional pfx defaultvalfun)
  "Like `d-emacs-dirs-create-save-customized-modes', but TYPELISTSYM should be a
          symbol bound to a typelist.

See `d-emacs-dirs-create-save-customized-modes' for more documentation."
  (remq nil `(d-emacs-dirs-create-save-customized-modes ,(symbol-value typelistsym) ,pfx ,defaultvalfun)))

;; Macro expansion works differently between 29 and 30
(defalias 'd-emacs-dirs-create-save-customized-modes
                        (if (< emacs-major-version 30)
                                                  'd-emacs-dirs-create-save-customized-modes-29
                          'd-emacs-dirs-create-save-customized-modes-30))


;;;;;; Macro call
(d-emacs-dirs-create-save-customized-modes-by-variable
 d-emacs-dirs-supported-type-data-list)

;;;; Provide
(provide 'd-emacs-dirs)
;;; d-emacs-dirs.el ends here
