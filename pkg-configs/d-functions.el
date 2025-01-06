;;; d-functions.el --- Daselt's Emacs module              -*- lexical-binding: t; -*-

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

;; The Emacs functions for Daselt d-emacs.

;;; Code:

;;;; Preamble
(require 'cl-macs)
(require 'cl-lib)
(require 'subr-x)

(declare-function d-emacs-coords-coordinatize-layout "d-emacs-xkb-functions" (layout &optional extt))
(declare-function d--special-file-p "d-macroexpansions" (filepath))
(declare-function d-emacs-coords-binding "d-emacs-xkb-functions" (coords &optional extlayout))
(declare-function d--user-defined-file-p "d-macro-expansions.el" (filename) t)
(declare-function d--eval-file-p "d-macro-expansions.el" (filename) t)
(declare-function d-emacs-coords-p (lst))

(defvar d-mention-unmatched)
(defvar d-modifiers-list)
(defvar d-emacs-no-shift-list)
(defvar d-emacs-double-symbs-alist)
(defvar d-stump)
(defvar d-stump-emacs-key-translations-alist)
(defvar d-discrete-modifiers-list)
(defvar d-emacs-key-translations-alist)
(defvar d-emacs-translate-keys)
(defvar d-emacs-directory)
(defvar d-debug)
(defvar d-keep-read-buffers)
(defvar d-escape-kbd-regexps-list)
(defvar d-emacs-xkb-layout)

;;;; General purpose functions
(defun d-cardinal (n &optional fromone)
                "Generate a list of integers from 0 to N-1.
If optional argument FROMONE is non-nil, return a list starting from 1 to N
instead."
                (append (unless fromone (list 0)) (cl-loop for k from 1 to (1- n)
                                             collect k)
          (if fromone (list n))))

(defun d-add-list-indices (list &optional fromone)
  "Cons each element of LIST with its position in LIST.
If optional argument FROMONE is non-nil, indices start from 1; otherwise, they
start from 0."
  (cl-mapcar (lambda (index elt)
               (cons index elt))
             (d-cardinal (length list) fromone) list))

(defun d-containing-directory-base-name (filepath)
  "Retrieve the base name of the containing directory of FILEPATH.
This function does not include the full path or trailing slashes in the result."
  (file-name-nondirectory (directory-file-name (file-name-parent-directory filepath))))

(defun d-filter-obarray-by-predicate (predicate)
  "Filter symbols in the obarray by a given PREDICATE."
  (let (filtered-symbols)
    (mapatoms (lambda (sym)
                (when (funcall predicate sym)
                  (push sym filtered-symbols))))
    filtered-symbols))

;;;;; Region operations
(defun d-read-region (&optional properties)
  "Read and return the contents of the current region as a Lisp expression.
The region is defined by the currently selected text in the buffer. Unless
PROPERTIES is t, read without properties."
  (let ((beg (region-beginning))
        (end (region-end)))
    (read (if properties
              (buffer-substring beg end)
            (buffer-substring-no-properties beg end)))))

(defun d-replace-region-with-arg (arg)
  "Replace the currently selected region with the content of ARG.
The text currently in the region is deleted, and ARG is inserted at the end of
the selection."
  (let* ((beg (region-beginning))
         (end (region-end)))
    (goto-char end)
    (insert arg)
    (delete-region beg end)))

;;;;; Lists
(defun d-remove-indices (indlst)
  "Remove indices of the elements of INDLST."
  (mapcar (lambda (indelt)
            (cdr indelt))
          indlst))

(defun d-filter-by-predicate (lst pred)
  "Return a new list of elements from LST filtered by PREDICATE.
The function tests each entry in LST using PRED."
  (remq nil (mapcar (lambda (item)
                      (if (funcall pred item)
                          item))
                    lst)))

(defun d-lisp-file-code (filename)
  "Extract and return the code section from a Lisp file specified by FILENAME.
The extraction is done by reading the content between `;;; Code:' and `;;; .*
ends here'. This function assumes the file follows standard Elisp formatting but
may work on other Lisp file formats as well."
  (set-buffer (find-file-noselect filename))
  (goto-char (point-min))
  (let ((region-beg (prog2 (re-search-forward ";;; Code:")
                        (match-end 0)))
        (region-end (prog2 (re-search-forward ";;; .*? ends here")
                        (match-beginning 0))))
    (buffer-substring-no-properties region-beg region-end)))

(defun d-emacs-make-list-if-not (obj)
  "Return a one-element list containing OBJ if it's not already a list.
If OBJ is already a list, return it unchanged."
  (if (listp obj) obj (list obj)))

(defun d-emacs-mapcar*-or-only (fun &rest objs)
  "Invoke FUN with elements from the provided Lisp objects OBJS.
If all objects in OBJS are lists, apply FUN with their elements as arguments
using `cl-mapcar'. If some OBJS are not lists, they are converted into
one-element lists before the application."
  (apply #'cl-mapcar fun
         (mapcar (lambda (obj)
                   (d-emacs-make-list-if-not obj))
                 objs)))

(defun d-cons-to-list (cns)
  "Convert the cons cell CNS into a list containing its elements.
This creates a list where the first element is the car of CNS, and the second
element is its cdr."
  (list (car cns) (cdr cns)))

(defun d-flatten-until (lst cnd)
  "Flatten the list LST until the condition CND becomes true.
CND should be a function accepting one argument to check flattenings of LST."
  (let ((rlist lst))
    (cl-loop until (funcall cnd rlist)
             do (setq rlist (apply #'append rlist))
             finally return rlist)))

(defun d-flatten-n-times (lst &optional n)
  "Flatten LST N times, concatenating all lists within LST.
If N is not provided, the function flattens LST once."
  (let ((n (if n n 1))
        (runlst lst))
    (cl-loop for k from 1 to n
             do (setq runlst (apply #'append runlst))
             finally return runlst)))

(defun d-reverse-alist-get (key alist &optional default)
  "Return the car of the first cons in ALIST whose cdr equals KEY.
If nothing is found, return DEFAULT."
  (catch 'found
    (dolist (item alist)
      (when (equal (cdr item) key)
        (throw 'found (car item))))
    default))

(defun d-sexp-end-position (&optional beg)
  "Return the ending position of the sexp beginning at BEG.
If BEG is not given, it is set using `point'."
  (let ((beg (or beg (point))))
    (save-excursion
      (goto-char beg)
      (forward-sexp)
      (point))))

;;;;; Strings
(defun d--escape-chars-in-str (str)
  "Escape characters in STR that are defined in `d-escape-kbd-regexps-list'.
This function modifies instances of the defined regex patterns."
  (let ((escaped-char-str
         (cl-loop for rx in d-escape-kbd-regexps-list
                  if (string-match-p rx str)
                  return (replace-regexp-in-string rx
                                                   "\\\\\\1"
                                                   str
                                                   nil)
                  finally return str)))
    (if escaped-char-str escaped-char-str str)))

(defun d-remove-surrounding-brackets (str)
  "Remove the initial and final bracket in STR."
  (let ((inibrapos (string-search "\(" str))
        (finbrapos (- (string-search "\)" (reverse str)))))
    (substring str (1+ inibrapos) (1- finbrapos))))

(defun d-remove-text-properties-from-string (str)
  "Return a new string based on STR with all text properties removed."
  (let ((no-prop-str (copy-sequence str)))
    (set-text-properties 0 (length no-prop-str) nil no-prop-str)
    no-prop-str))

(defun d-string-exists-and-nonempty (str)
                                                "Return t if STR exists and is not empty."
                                                (and str (not (string-empty-p
                 str))))


;; Taken from s.el
(defun d-uppercase-p (str)
                        "Return t if all letters in STR are uppercase."
                        (declare (side-effect-free t))
                        (let ((case-fold-search nil))
    (not (string-match-p "[[:lower:]]" str))))

;;;;; Lines
;; Taken from min,sc-cmds.el (Icicle library).
(defun d-mark-line (&optional arg)
  "Put mark at end of line, point at beginning.
A numeric prefix ARG means move forward (backward if negative) that
many lines, thus marking a line other than the one point was
originally in."
  (interactive "P")
  (setq arg  (if arg (prefix-numeric-value arg) 0))
  (forward-line arg)
  (push-mark nil t t)
  (goto-char (line-end-position)))

(defun d-read-line ()
                                                                                            "Read the current line."
                                                                                            (buffer-substring-no-properties (line-beginning-position)
                                  (line-end-position)))

(defun d-generate-newlines (k)
  "Generate a string containing K newlines."
  (cl-loop for i from 1 to k
           concat "\n"))

(defun d-surround-by-newlines (k l str)
  "Prepend STR with K newlines and append it with L newlines."
  (concat (d-generate-newlines k) str (d-generate-newlines l)))

(defun d-prepend-newlines (k str)
  "Prepend K newlines before STR."
  (d-surround-by-newlines k 0 str))

(defun d-append-newlines (k str)
  "Append K newlines before STR."
  (d-surround-by-newlines 0 k str))

(defun d-search-at-line-start (str)
  "Search for an occurrence of STR at the start of a line.
Unlike normal `search-forward', `d-search-at-line-start' returns nil if no match
is found and does not cause an error."
  (re-search-forward (eval `(rx line-start ,str)) nil t))

;;;;; Logical and set-theoretic operations
(defun d-forall-p (list predicate)
                                                                              "Return LIST if all elements satisfy PREDICATE; otherwise, return nil."
                                                                              (cl-loop for elt in list
           do (if (not (funcall predicate elt))
                                                                                                                                                                          (cl-return nil))
           finally return t))

(defun d-exists-p (list predicate)
  "Return LIST if at least one element satisfies PREDICATE.
Otherwise return nil."
  (cl-loop for elt in list
           do (if (funcall predicate elt) (cl-return t))))

(defun d-complement (list1 list2 &optional compfun)
  "Return a new list containing elements of LIST1 that are not in LIST2.
The operationis non-destructive, preserving the original lists. Use COMPFUN for
comparisons, defaulting to `equal'."
  (cl-remove-if (lambda (element)
                  (cl-member element list2 :test (or compfun #'equal)))
                list1))

(defun d-powerlist (list &optional elt)
  "Generate the power list of SET represented by LIST.
Returns a list of all sublists of LIST with elements ordered like in LIST. ELT
is used for recursion and should normally not be set by the user."
  (let ((powerlist (list nil))
        (cutlist list))
    (if elt (mapcar (lambda (sublist) (append (list elt) sublist))
                    (d-powerlist (cl-loop for index from 0 to (cl-position elt list)
                                          do (setq cutlist (remq (nth index list) cutlist))
                                          finally return cutlist)))
      (cl-loop for elt in list
               do (setq powerlist (append powerlist (d-powerlist list elt)))
               finally return powerlist))))

(defun d-remove-list-index (lst idx)
  "Remove the element at index IDX from LST and return the resulting list.
The operation does not modify the original list."
  (let (runlst)
    (cl-loop for runidx from 0 to (1- (length lst))
             do (unless (= runidx idx)
                  (setq runlst (append runlst (list (nth runidx lst)))))
             finally return runlst)))

;;;;; Comparison
(defun d-string-shorter-or-samep (str1 str2)
  "Return t if STR1 is shorter than or equal in length to STR2."
  (<= (length str1)
      (length str2)))

(defun d-string-longer-or-samep (str1 str2)
  "Return t if STR1 is longer than or equal in length to STR2."
  (>= (length str1)
      (length str2)))

(defun d-compare-if-decidable (test arg1 arg2)
  "Compare ARG1 and ARG2 using the function TEST.
If TEST takesone argument, return `(t)' if only ARG1 satisfies TEST, `(nil)' if
only ARG2 does, or nil if both or neither do. If TEST takes more arguments,
apply it with ARG1 and ARG2 as the first and second argument, then switched
around and provide output like in the one-argument case."
  (if (= (car (func-arity test)) 1)
      (let ((val1 (funcall test arg1))
            (val2 (funcall test arg2)))
        (cond ((and val1
                    (not val2))
               '(t))
              ((and val2
                    (not val1))
               '(nil))
              (t nil)))
    (let ((val1 (funcall test arg1 arg2))
          (val2 (funcall test arg2 arg1)))
      (cond ((and val1
                  (not val2))
             '(t))
            ((and val2
                  (not val1))
             '(nil))
            (t nil)))))

(defun d-compare-by-sequential-predicates (arg1 arg2 &rest predicates)
  "Compare ARG1 and ARG2 using the sequence of PREDICATES.
Return (t) if ARG1 fulfills the i-th predicate and ARG2 doesn't, return (nil) if
ARG2 fulfills the i-th predicate and ARG1 doesn't. If both or neither fulfill
the i-th predicate, compare using the next predicate. If both or neither fulfill
all predicates, return nil."
  (cl-loop for pred in predicates
           do (let ((compval (d-compare-if-decidable pred arg1 arg2)))
                (if compval (cl-return compval)))))

;;;;; Insertion
(defun d-capture-inserted-text (fn &rest args)
  "Capture the text inserted by FN called with ARGS and return it as a string."
  (with-temp-buffer
    (apply fn args)
    (buffer-string)))


;;;; Recursion
(defun d-funcalls-recursively (obj funtests &optional recursetest formatfun eltcolfun lstcolfun restargs restargfun contt debug)
  "Recursively apply functions to elements of OBJ based on condition tests.

This function processes OBJ using FORMATFUN to produce a list LST. If FORMATFUN
is not provided, it defaults to `identity'. The main operations of this function
involve iterating over LST's elements, applying tests and functions, and
potentially recursing into elements if they satisfy a recursion test.

FUNTESTS is a list of cons cells where each cell contains a function and a test
\(e.g., \(TEST . FUNCTION\)\). For each element ELT in LST, if its corresponding
TEST returns non-nil (using optional RESTARGS if provided), FUNCTION is applied
to ELT, and the result is collected into RUNLIST using ELTCOLFUN.

If RECURSETEST returns non-nil for an element ELT, `d-funcalls-recursively' is
applied to ELT with the same original parameters, allowing for deep processing
of nested structures.

Function output is accumulated: - Into RUNLIST by ELTCOLFUN for individual
element results. - With LSTCOLFUN for overall results.

Defaults for optional arguments: - RECURSETEST is `proper-list-p'. - ELTCOLFUN
appends results to make a flat list. - LSTCOLFUN is `list'. - RESTARGFUN
processes additional arguments for each recursion level. - CONT is non-nil to
continue tests after one is satisfied. - DEBUG enables logging for diagnostic
output.

OBJ can represent structured data such as folders, where elements are evaluated
and results collected based on hierarchy and matching tests."
  (let* ((recursetest (or recursetest (lambda (idx lst &optional _restargs)
                                        (proper-list-p (nth idx lst)))))
         (formatfun (or formatfun #'identity))
         (eltcolfun (or eltcolfun (lambda (lst result) (append lst (list result)))))
         (lstcolfun (or lstcolfun #'list))
         (restargfun (or restargfun (lambda (_lst restargs) restargs)))

         (lst (funcall formatfun obj))
         (restargs (funcall restargfun lst restargs)) ; Immediately add new arguments to RESTARGS

         runlist)

    ;; This somewhat awkward construction is necessary to allow test1 and the other functions to not need an input corresponding to RESTARGS if RESTARGS is not given.
    (cl-flet* ((apply-with-restargs-if-given (fun arg1 &optional arg2)
                 (eval (append `(funcall fun arg1)
                               (if arg2 `(arg2))
                               (if restargs `(restargs)))
                       `((fun . ,fun)
                         (arg1 . ,arg1)
                         (arg2 . ,arg2)
                         (restargs . ,restargs)))))

      (if debug (message "Restargs: %s" restargs))
      (if debug (message "Formatfun: %s" formatfun))
      (if debug (message "Objectlist: %s" lst))
      (cl-loop for idx from 0 to (1- (length lst))
               for elt = (nth idx lst)

               do (if debug (message "Element: %s" elt))
               do (cl-loop for funtest in funtests
                           for fun = (car funtest)
                           for test = (cdr funtest)

                           do (if debug (message "Fun: %s \nTest: %s \nElement Collection Function: %s"
                                                 fun test eltcolfun))
                           do (if (apply-with-restargs-if-given test idx lst)
                                  (let ((result (apply-with-restargs-if-given fun elt)))
                                    (if debug (message "\nRunlist: %s \nResult: %s" runlist result))
                                    (setq runlist (funcall eltcolfun runlist result))
                                    (unless contt (cl-return)))))

               do (if (apply-with-restargs-if-given recursetest idx lst)
                      (setq runlist
                            (funcall lstcolfun runlist
                                     (d-funcalls-recursively
                                      elt funtests recursetest formatfun eltcolfun lstcolfun
                                      restargs restargfun contt debug))))
               
               finally return runlist))))

(defun d-funcall-recursively (obj fun test  &optional recursetest formatfun eltcolfun lstcolfun restargs restargfun contt debug)
  "Recursively apply FUN to elements that satisfy TEST.
This wraps the contouring of arguments and collections found in
`d-funcalls-recursively'.
See there for further explanation."
  (d-funcalls-recursively obj `((,fun . ,test)) recursetest formatfun eltcolfun lstcolfun restargs restargfun contt debug))

;;;;; Directories and files
(defun d-recurse-through-directory (dir funtests &optional dirtest lstcolfun allfiles sortfun contt)
  "Recursively apply functions to files in DIR based on condition tests.

This function traverses the directory structure of DIR and applies specified
functions to files that satisfy condition tests. The tests and functions are
specified in FUNTESTS, a list of condition-function pairs (like TEST .
FUNCTION). Each TEST takes two arguments: IDX, the index of the position of the
file in the folder, and LIST, the list of file names. Each FUNCTION takes one
argument, the file name.

Optional behavior and customization: - DIRTEST: A function that determines if a
directory should be recursed into. If provided, it takes a directory name as
input and returns non-nil to recurse into that directory; otherwise, all
directories are recursed into by default. - LSTCOLFUN: Collects results using
this function if given, otherwise defaults to `list', preserving directory
structure. - ALLFILES: Include hidden files if set to non-nil. - SORTFUN: A
sorting function for directory contents, applied if provided. - CONT: If
non-nil, continue applying tests even after the first match.

The behavior is achieved by using the `d-funcalls-recursively' function, which
handles collecting results and determines whether to recurse further into
directory contents. The results are built into a list reflecting the processed
structure of DIR.

CONTT is as in `d-funcalls-recursively'."
  (d-funcalls-recursively
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

(defun d--act-on-pkg-files-by-type (funtypes &optional subdir customt)
  "Apply functions to files of specified types in `pkg-configs'.

This function operates on files located in the `pkg-configs' directory under
`d-emacs-directory'. It applies each function specified in FUNTYPES to files
that match a corresponding type. The types are matched against
`d-distinguished-file-types'.

Parameters: - FUNTYPES: A list of cons cells where each cell contains a function
and a type (e.g., (FUNCTION . TYPE)). - TYPE can be a string, symbol, or a
predicate function. It can also be a cons whose car is a file type and whose cdr
consists of type modifiers from `d-pkg-type-modifiers-list'. - SUBDIR: If
provided, restrict the execution to this subdirectory within `pkg-configs'. -
CUSTOMT: If non-nil, apply the function only to files in directories whose
corresponding custom is t, determined by `d--dir-custom-t-p'.

Execution logic: - If REGULAR is t, process all regular files of TYPE. - If
USER-DEFINED is t, process all user-defined files of TYPE. - The function checks
files based on predicates derived from TYPE and its modifiers. - Collects
results using `#'append'.

This process utilizes `d-recurse-through-directory' to manage recursion through
directories and applies file sorting using `d--sort-files-for-actions'. The
function is versatile, operating on files that pass given type predicates and
options."
  (let ((recurseargs
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
                               (and (if maintype
                                        (if (stringp maintype)
                                            (funcall (d--type-predicate maintype) filepath)
                                          (if (functionp maintype)
                                              (funcall maintype filepath)))
                                      t)
                                    (if typemods
                                        (cl-every (lambda (typemod)
                                                    (funcall
                                                     (d--type-modifier-predicate typemod)
                                                     filepath))
                                                  typemods)
                                      (d--standard-file-p filepath))))))))
                 funtypes)))
    
    (d-recurse-through-directory
     (concat d-emacs-directory "pkg-configs/" (if subdir subdir))
     recurseargs
     (lambda (filepath) (if customt (d--dir-custom-t-p filepath) t))
     #'append
     nil
     #'d--sort-files-for-actions
     t)))

(defun d--sort-files-for-actions (filelist)
  "Sort FILELIST based on a series of comparison tests.

This function orders the files in FILELIST using several tests defined in
`d-compare-by-sequential-predicates'. The sorting is determined thus:

1. Directories come last.
2. User-defined files come later (`d--user-defined-file-p').
3. Evaluation files come earliest possible (`d--eval-file-p').
4. Longer-named files come before shorter ones."
  (sort filelist :lessp (lambda (fname1 fname2)
                          (let ((compval (d-compare-by-sequential-predicates
                                          fname1 fname2
                                          (lambda (fname)
                                            (not (file-directory-p fname)))
                                          #'d--user-defined-file-p
                                          #'d--eval-file-p
                                          #'d-string-longer-or-samep)))
                            (if compval
                                (car compval))))))

(defun d--execute-and-maybe-kill-file-buffer (filename fun)
  "Execute FUN with FILEPATH as an argument.
Kill the buffer corresponding to FILENAME unless D-KEEP-READ-BUFFERS is t."
  (prog1 (funcall fun filename)
    (unless d-keep-read-buffers (kill-buffer (find-file-noselect filename)))))

(defun d--act-on-pkg-files-by-type-and-maybe-kill (funtypes &optional subdir customt)
  "Wrapper around `d--execute-on-pkg-files-by-type'.
Wraps functions in FUNTYPES in `d--execute-and-maybe-kill-file-buffer'.
Convert errors produced by them into warnings, then send them
to `d--execute-on-pkg-files-by-type'. Primarily designed for use when starting
`d-emacs-mode', hence CUSTOMT is assumed to be automatically enabled.
See `d--execute-on-pkg-files-by-type' for further documentation."
  (let ((newfuntypes (mapcar (lambda (funtype)
                               (let ((fun (car funtype))
                                     (type (cdr funtype)))
                                 (cons (lambda (filepath)
                                         (condition-case err
                                             (d--execute-and-maybe-kill-file-buffer
                                              filepath fun)
                                           (error (warn "Error in %s:\n%s"
                                                        filepath
                                                        (error-message-string err)))))
                                       type)))
                             funtypes)))
    (d--act-on-pkg-files-by-type newfuntypes subdir customt)))

(defun d-require-file (filepath)
  "Find file with FILEPATH and evaluate the corresponding buffer."
  (with-eval-after-load (intern (d-containing-directory-base-name filepath))
    (require (intern (file-name-base filepath)))))

;;;;; Bindlists
(defun d--act-on-bindlists-in-file (filepath function &optional untangle)
  "Process all lists in the file with FILEPATH by executing FUNCTION on each.
Map lists areidentified by starting with a newline and backquote (`). The
results can be collected using `list' or `append' if UNTANGLE is non-nil."
  (let ((buffer (current-buffer)))
    (set-buffer (find-file-noselect filepath))
    (prog1 (let ((pos (point)))
             (cl-labels ((mark-and-unmark-bindlist ()
                           (progn (backward-char)
                                  (mark-sexp)
                                  (prog1 (funcall function)
                                    (progn (deactivate-mark)
                                           (forward-char))))))

               (goto-char (point-min))
               (prog1 (cl-loop until (not (re-search-forward (rx (or bos
                                                                     bol) "`")
                                                             nil t))
                               if untangle
                               append (mark-and-unmark-bindlist)
                               else collect (mark-and-unmark-bindlist))
                 (goto-char pos))))
      (set-buffer buffer))))

(defun d--recursively-act-on-bindings (blist fun &optional nooutput)
  "Recursively apply FUN to all bindings in BLIST.

This function traverses BLIST, which is expected to be a structure containing
bindings, and applies the function FUN to each binding it encounters. It
determines elements that qualify as bindings using `d--binding-p'.

Parameters: - BLIST: The list or structure containing potential bindings. - FUN:
The function to apply to each binding. - NOOUTPUT: If non-nil, do not collect
the output in a list.

The function uses `d-funcall-recursively' to manage traversal: - It checks if
each element is a binding using `d--binding-p'. - Elements that are not atoms
and do not qualify as bindings are further recursed into as lists. - If NOOUTPUT
is nil, collected results are combined using `cons'.

The results are conditionally collected based on whether NOOUTPUT is set. Head
elements of lists are determined using `d-head-if-exists' and added to RESTARGS
so they can be used by FUN."
  (d-funcall-recursively blist
                         fun
                         (lambda (idx lst &optional _heads)
                           (let ((elt (nth idx lst)))
                             (d--binding-p elt)))
                         (lambda (idx lst &optional _heads)
                           (let ((elt (nth idx lst)))
                             (and (not (atom elt))
                                  (not (d--binding-p elt)))))
                         nil
                         (if nooutput nil #'cons)
                         (if nooutput nil #'cons)
                         nil
                         (lambda (lst heads)
                           (append heads (let ((newhead (d-head-if-exists lst)))
                                           (if newhead (list newhead)))))
                         nil))

;;;;; Conses
(defun d-recursively-act-on-proper-conses (list fun &optional lstcolfun)
  "Recursively apply FUN to all non-list cons cells in LIST.

This function traverses through LIST and applies the function FUN to each cons
cell that is not considered a proper list. The goal is to process individual
cons cells while ignoring proper lists composed of them.

Parameters: - LIST: The structure containing cons cells and lists. - FUN: The
function to be applied to each non-list cons cell. - LSTCOLFUN: An optional
function to collect results; defaults to `append'.

The function uses `d-funcall-recursively' to handle traversal: - It identifies
and applies FUN to cons cells that are not proper lists. - Recursion occurs into
elements that are proper lists.

Results are collected using the specified LSTCOLFUN function, with a default
behavior of concatenating results via `append', which should return them in a
flat list."
  (let ((lstcolfun (or lstcolfun #'append)))
    (d-funcall-recursively list
                           fun
                           (lambda (idx lst)
                             (let ((elt (nth idx lst)))
                               (and (consp elt) ; Check element is a cons
                                    (not (proper-list-p elt))))) ; and not a proper list

                           (lambda (idx lst) (proper-list-p (nth idx lst)))
                           nil
                           (lambda (lst result)
                             (if result
                                 (push result lst)
                               lst))
                           lstcolfun)))

(defun d-recursive-get-cons (obj allist &optional testfn reverse)
  "This function applies itself to each element ELT contained in ALLIST.
If ELT is a list, it applies itself to that list. For each ELT that is a cons
but not a proper list, it tests whether OBJ matches the car of that cons. It
returns the list of matches. Matching is done using TESTFN or, if none is given,
using equal. If REVERSE is t cdrs are tested instead of cars."
  (d-recursively-act-on-proper-conses
   allist
   (lambda (cns) (if (funcall (if testfn testfn #'equal)
                              obj
                              (funcall (if reverse #'cdr #'car) cns))
                     cns))))

;;;;; Aliases
(defalias 'd--act-on-constants-in-file #'d--act-on-bindlists-in-file
  "Process all constant values in the file at FILEPATH by executing the specified
FUNCTION.
Constant values are recognized by starting with a newline and an initial
backquote `. The results are collected using `list', or, if `untangle' is on,
using `append'.")

(defalias 'd--act-on-advicelists-in-file #'d--act-on-bindlists-in-file
  "Process all advicelists in the file at FILEPATH by executing the
specifiedFUNCTION.
Advicelists are recognized by starting with a newline and an initial backquote
`. The results are collected using `list', or, if `untangle' is on, using
`append'")

(defun d-emacs-regexp-replace-listwise (llist)
  "Go through a LLIST of regexps.
Map subexpressions of the second regexp to subexpressions of the first and so
on. Input is a list LLIST of lists RXLIST, where the car of RXLIST is a
REGEXP.

The second entry of RXLIST is a either a number or a list of numbers. If it is a
list, it specifies all the subexpressions of REGEXP that are supposed to be
replaced, the insertion places. If it is a number, it specifies the only
subexpression of REGEXP that is supposed to be replaced.

The third entry of RXLIST is either a string or a list of strings. It specifies
the strings that should be inserted into the insertion places of the previous
RXLIST-1 and so should have either the same length as the second entry of
RXLIST-1 or be a string if that entry was not a list but a number.

The optional fourth entry is t if the search for REGEXP should be case-sensitive
and nil otherwise.

The optional fifth entry is t if the replacement should treat case literally.

 The function then works similar to d-emacs-ireplace-listwise, replacing the
 subexp of REGEXP specified in the second list entry with the subexp of REGEXP2
 specified in the third and so on. In case the last regexp has not appeared as a
 previous argument, it replaces instances of its specified subexp with an empty
 string. This way, if the last argument is equal to the first, it cycles through
 the given arguments, while if the last argument did not appear previously it
 just removes it. As a special case, if the inputs are A, B and A, instances of
 the subexp of A are exchanged with instances of that of B.

Since the content of the subexpressions of REGEXP2 that are selected for
insertion in REGEXP would depend on the match of REGEXP2 and the matches of
REGEXP2 might not be equal to those of REGEXP, only the first match of REGEXP2
is used for calculating the subexpressions that are inserted into REGEXP.

Please note that this function uses words based on πλαχεηολδερ as a placeholder.
If for some reason you have this word in your text, change the placeholder."
  (let ((pos (point)))
    (progn (goto-char (point-min))
           (cl-loop for n from 0 to (1- (length llist))
                    for rxlist = (nth n llist)
                    do (let* ((oldrx (car rxlist))
                              (insplaces (d-emacs-make-list-if-not (nth 1 rxlist)))
                              (case-fold-search (if (> (length rxlist) 3)
                                                    (not (nth 2 rxlist))
                                                  case-fold-search))
                              (replace-case (if (> (length rxlist) 4)
                                                (nth 3 rxlist)
                                              nil)))
                         (while (re-search-forward oldrx nil t)
                           (cl-loop for k from 0 to (1- (length insplaces))
                                    do (replace-match (format "πλαχεηολδερ%s%s" n k)
                                                      replace-case nil nil (nth k insplaces)))))
                    do (goto-char (point-min)))
           (cl-loop for n from 0 to (1- (1- (length llist)))
                    for oldrxlist = (nth n llist)
                    for newrxlist = (nth (1+ n) llist)
                    do (let* ((subplaces (d-emacs-make-list-if-not (nth 2 newrxlist)))
                              (case-fold-search t)
                              (case-replace (if (> (length oldrxlist) 4)
                                                (nth 3 oldrxlist)
                                              nil)))
                         (cl-loop for k from 0 to (1- (length subplaces))
                                  do (while (search-forward (format "πλαχεηολδερ%s%s" n k) nil t)
                                       (replace-match (nth k subplaces) case-replace t))))
                    do (goto-char (point-min)))
           (while (re-search-forward "πλαχεηολδερ.." nil t)
             (replace-match "" nil t))
           (if pos (goto-char pos)))))

;;;; Predicates
(defun d--dir-custom-t-p (dirpath)
  "Check if the directory DIRPATH has its corresponding custom set to t.
DIRPATH should include the portion of the path from
`d-emacs-directory/pkg-configs' onward."
  (if (or (string= "pkg-configs" (file-name-base (directory-file-name dirpath))))
      t
    (let* ((splittedpath (split-string dirpath "/"))
           (base (cl-loop for strnum from 0 to (1- (length splittedpath))
                          do (if (string= (nth strnum splittedpath) "pkg-configs")
                                 (cl-return (nth (1+ strnum) splittedpath)))))
           (end (file-name-base (directory-file-name dirpath)))
           (symbol (intern (concat base "-" end))))
      (if (boundp symbol)
          (symbol-value symbol)
        t))))

(defun d--recursively-check-if-binding-cons-p (cns)
  "Check if CNS looks like the car of a binding.
If not, look at whether CAR is a cns, and, if so, apply yourself to it.
Moreover, if the CNS has more than one element, apply yourself to the second
element. This is necessary for a binding predicate that still allows the cdr of
the binding to be arbitrary, here with the restriction that it cannot contain
another binding form."
  (and (consp cns)
       (or (d--binding-location-p cns)
           (d--recursively-check-if-binding-cons-p (car cns))
           (if (proper-list-p (cdr cns))
               (d--recursively-check-if-binding-cons-p
                (car (cdr cns)))))))

(defun d--binding-suffix-form-p (cns)
  "Return t if CNS looks like a binding in suffix form.
This means its car is a string, and it is either not a proper list or its second
element is not a binding."
  (condition-case nil
      (and (consp cns)
           (atom (car cns))
           (stringp (car cns)))
    (error nil)))

(defun d--binding-prefix-suffix-form-p (cns)
  "Return t if CNS looks like a binding in prefix-suffix-form.
This means its car is a cons of two strings, and it is either not a proper list
or its second element is not a binding."
  (condition-case nil
      (and (consp cns)
           (consp (car cns))
           (stringp (caar cns))
           (stringp (cdar cns)))
    (error nil)))

(defun d--binding-coords-form-p (cns)
  "Return t if CNS looks like a binding in coords-form.
This means its car is a cns for which `d-emacs-coords-p' is t, and it is
either not a proper list or its second element is not a binding."
  (condition-case nil
      (and (consp cns)
           (d-emacs-coords-p (car cns)))
    (error nil)))

(defun d--binding-prefix-coords-form-p (cns)
  "Return t if CNS is a binding in prefix-coords form.
This means that the car must be a cons of a string (the prefix) and a d-emacs-xkb
coordinate list."
  (and (consp cns)
       (consp (car cns))
       (stringp (caar cns))
       (d-emacs-coords-p (cdar cns))))

(defun d--binding-prefix-suffix-coords-form-p (cns)
  "Return t if CNS looks like a binding in prefix-suffix-coords-form.
This means its car is cons whose car is a cons of two strings and whose cdr is
either nil or a cns for which d-emacs-coords-p is t, and it is either not a
proper list or its second element is not a binding."
  (condition-case nil (and (consp (car cns)) (consp (caar cns))
                           (stringp (caaar cns)) (stringp (cdaar cns))
                           (or (not (cdar cns))
                               (d-emacs-coords-p (cdar cns))))
    (error nil)))

(defun d--binding-elaborate-form-p (cns)
  "Return t if CNS looks like a binding in elaborate form.
This means its car is cons whose car is a cons of a list and a string and whose
cdr is either nil or a cns for which d-emacs-coords-p is t, and it is either
not a proper list or its second element is not a binding."
  (condition-case nil (and (consp (car cns)) (consp (caar cns))
                           (listp (caaar cns)) (stringp (cdaar cns))
                           (or (not (cdar cns))
                               (d-emacs-coords-p (cdar cns))))
    (error nil)))

(defun d--elaborate-unmatched-binding-p (cns)
  "Return t if CNS is an elaborate unmatched binding.
This means `d--binding-elaborate-form-p' is t and it has no coordinates."
  (and (d--binding-elaborate-form-p cns)
       (not (cdar cns))))

(defun d--binding-location-p (cns)
  "Return t if the car of CNS is a binding location.

A binding location consists of either

- a string, like a normal string fed to `kbd',

- a d-emacs-xkb coordinate list (see `d-emacs-coords-p'),

a cons whose car is a string of prefixes like `M-C-' and a suffix which is the
name of the signal that is sent from the keyboard without any applied modifiers
\(so a letter name or a name like `<kp-add>'),

- a cons whose car is a prefix and whose cdr is a d-emacs-xkb-coordinate-list,

- a cons

  - whose car is a cons consisting of a prefix and a suffix

  - and whose cdr is a d-emacs-xkb-coordinate-list,

- a cons whose car is

  - a cons whose car is a list of modifiers, given as characters, like `C', `M'
    etc. and whose cdr is a suffix,

- and whose cdr is is a d-emacs-xkb-coordinate-list.

The last two forms are redundant and so usually not needed, although the last
form (the so-called elaborate form) is used by some daselt-functions, such as
`d--compare-elaborate-bindings'."
  (and (consp cns) (and (or
                         (d--binding-suffix-form-p cns)
                         (d--binding-prefix-suffix-form-p cns)
                         (d--binding-coords-form-p cns)
                         (d--binding-prefix-coords-form-p cns)
                         (d--binding-prefix-suffix-coords-form-p cns)
                         (d--binding-elaborate-form-p cns)))))

(defun d--binding-p (cns)
  "This function returns t if CNS has the form ofa Daselt-binding.
This means it is a cons whose car is a binding car and if CNS is not a list that
contains any other binding forms."
  (and (consp cns)
       (d--binding-location-p cns)
       (not (if (proper-list-p (cdr cns))
                (d--recursively-check-if-binding-cons-p (car (cdr cns)))))))

(defun d--standard-file-p (filename)
  "Check if the FILENAME corresponds to a standard file.
This means its base name doesn’t start with a dot and does not contain # or ~."
  (not (or (string-match-p (rx (or (: string-start ".") "#" "~")) 
                           (file-name-base filename))
           (string-match-p (rx (or "#" "~")) 
                           (car (last (split-string filename "\\.")))))))

(defun d--standard-el-file-p (filename)
    "Verify if FILENAME follows the standards for elisp files.
This means its base name should not have special characters and it should end
with '.el'."
    (and (d--standard-file-p filename)
       (let ((ext (file-name-extension filename)))
         (if ext (string-match-p "el" ext)))))

;; Generate predicates for types of pkg-configs-files.

(defun d--type-predicate (str)
  "Return the symbol of the type predicate corresponding to STR.
STR should bean element of \"d-pkg-file-types-list\"."
  (intern (concat "d--" str "-p")))

(defun d--type-modifier-predicate (str)
  "Get the symbol for the type modifier predicate associated with STR.
STR should come from `d-pkg-type-modifiers-list' or be `regular'."
  (intern (concat "d--" str "-file-p")))

(defun d--bindlist-p (cand)
  "Return t if CAND is a bindlist.
The way used to test this is by recursing through CAND until a binding is
found."
  (if (atom cand)
      nil
    (cl-loop for elt in cand
             do (if (d--binding-p elt)
                    (cl-return t)
                  (unless (atom elt)
                    (if (d--bindlist-p elt)
                        (cl-return t)))))))

(defun d--bindlist-symb-p (symb)
  "Return t if SYMB is a bindlist symbol.
This is tested by looking at whether the name of SYMB ends in `-bindlist', SYMB
is a bound variable and the value of SYMB returns t when tested with
`d--bindlist-p'."
  (and (string-match-p (rx "-bindlist" string-end)
                       (symbol-name symb))
       (boundp symb)
       (d--bindlist-p (symbol-value symb))))

(defun d--string-binding-p (cns)
  "Return t if CNS is a binding given by a binding string."
  (and (d--binding-p cns)
       (not (d-emacs-coords-p (car cns)))
       (not (d-emacs-coords-p (cdar cns)))))


;;;; Bindlist formatting
;;;;; General
(defun d-head-if-exists (list)
  "Check if LIST has a head.
An element counts as a head if it isn't identified as a binding."
  (if (and (proper-list-p list) (not (d--binding-p list)))
      (if (d--binding-p (car list))
          nil
        (car list))))

;;;;; Modifiers
(defun d-index-prefix-modifiers (prefix &optional modlist)
  "Return a list of indexed modifiers in PREFIX.
The indexing is done according to the position of the modifier in MODLIST. If
MODLIST is not specified, `d-modifiers-list' is used."
  (unless (not prefix)
    (let ((modlist (if modlist modlist d-modifiers-list))
          (case-fold-search nil))
      (remq nil (mapcar (lambda (indmodifier)
                          (if (string-match-p (concat (char-to-string (cdr indmodifier))
                                                      "-")
                                              prefix)
                              indmodifier))
                        (d-add-list-indices modlist))))))

(defun d-index-and-sort-modifiers (mods &optional indexed modlist)
  "Index the modifiers in MODS based on their position in MODLIST and sort them.
The default MODLIST is `d-emacs-xkb-modifiers-list'.
If INDEXED is t, assume the MODS are already indexed and don't index them again."
  (let* ((modlist (or modlist d-modifiers-list))
         (indmods (if indexed
                      mods
                    (d-filter-by-predicate (d-add-list-indices modlist)
                                           (lambda (indmod) (member (cdr indmod) mods))))))
    (sort indmods
          :lessp (lambda (indmod1 indmod2)
                   (< (car indmod1)
                      (car indmod2))))))

(defun d-sort-modifiers (mods &optional retainidx modlist)
  "Sort modifiers in MODS; returns the modifiers without indices.
If RETAINIDX is true, retain the indices in the output.
MODLIST is the list of modifiers used for sorting, by default it is
`d-modifiers-list'."
  (let ((indmods (d-index-and-sort-modifiers mods retainidx modlist)))
    (mapcar (lambda (indmod)
              (nth 1 indmod))
            indmods)))

(defun d-modifiers-of-prefix (prefix &optional modlist keepindices)
  "Sort modifiers of PREFIX.
If MODLIST isprovided, it sorts against that instead of `d-modifiers-list'. If
KEEPINDICES is true, keep modifier indices."
  (let ((sorted (d-index-and-sort-modifiers (d-index-prefix-modifiers prefix modlist) t)))
    (if keepindices
        (if sorted sorted "") ; Let's return the empty string if there aren't any modifiers.
      (mapcar (lambda (indmod)
                (cdr indmod))
              (if sorted sorted "")))))

(defun d-string-together-modifiers (mods)
  "Concatenate the given list of MODS into a prefix."
  (cl-loop for mod in (reverse mods)
           concat (concat (char-to-string mod) "-")))

;;;;; Coordinates
(defun d--coords-from-binding (binding)
  "Retrieve coordinates associated with a BINDING if available.
Otherwise, return nil."
  (cond ((stringp (car binding))
         nil)
        ((and (consp (car binding)) (d-emacs-coords-p (cdar binding)))
         (cdar binding))
        ((d-emacs-coords-p (car binding)) (car binding))))

;;;;; Elaborate forms
(defun d--get-layout-matches-for-binding-string (str)
  "Match (indexed) layout entries against the last part of the string STR.
Return a cons of STR and the list of matching conses."
  (d-recursive-get-cons
   str
   (d-emacs-coords-coordinatize-layout
    (symbol-value (d-emacs-coords--dfk-or-xkb-layout)))
   (lambda (str compstr)
     (let ((case-fold-search nil))
       (string-match-p
        (rx-to-string

         ;; We have to put in a check to avoid partial matches of characters that don't actually match.
         `(: (or string-start
                 "-")
             ,(if (= 1 (length compstr))
                  compstr
                (car (last (split-string compstr "/"))))
             string-end))

        ;; If the tested string in the layout is longer than one character, split it along /'s. This is mostly for elements of d-emacs-xkb-layer-0. Let's just hope nobody ever defines a signal name containing /'s. But that seems unlikely.
        str)))
   t))

(defun d--get-unique-layout-match (str)
  "Obtain the correct match for STR from a list of potential layout matches.
Typically returns the longest match, excluding matches from layer 0 if others
are available."
  (let* ((matches (d--get-layout-matches-for-binding-string str))

         ;; Throw away 0-layer matches if another one exists.
         (redmatches (d-filter-by-predicate matches
                                            (lambda (match)
                                              (let* ((coords (car match))
                                                     (laycoord (car coords)))
                                                (not (= 0 laycoord))))))
         (matches (if redmatches redmatches matches)))

    (cond ((and (proper-list-p (cdr matches)) (cdr matches))
           (car (sort (cdr matches) (lambda (cons1 cons2)
                                      (if (> (length (cdr cons2))
                                             (length (cdr cons1)))
                                          cons2
                                        cons1)))))
          ((not (cdr matches))
           (prog2 (if d-mention-unmatched
                      (message "%s in %s is not matched by any signal in %s."
                               (car matches)
                               (current-buffer)
                               d-emacs-xkb-layout))
               (car matches)))
          (t (cdr matches)))))

(defun d--elaborate-on-bindstr (bindstr)
  "Transform a binding string BINDSTR into its elaborate form.
The binding is created by the position of the best match in the layout. If no
match is found, the suffix is converted into an elaborate binding."
  (let ((match (d--get-unique-layout-match bindstr)))
    (if match
        (let* ((matchstr (cdr match))
               (propermatchstr (car (last (string-split matchstr "/"))))
               (matchcoords (car match)))
          (cons (cons (d-modifiers-of-prefix
                       (string-remove-suffix propermatchstr bindstr)
                       nil t)
                      propermatchstr)
                matchcoords))
      (cons (cons (d-modifiers-of-prefix bindstr nil t) bindstr) nil))))

(defun d--elaborate-on-binding (binding)
  "Transform a d-emacs-xkb BINDING into its elaborate form.

If the binding is given by a binding string, it extracts the prefix, the suffix
and its corresponding coordinates from the string by matching the end of the
string against the symbols in the layout. If no matching suffix in the layout
given by d-emacs-xkb-layout is found, it tries to extract modifiers from the string
and returns the string along with the extracted modifiers and nil in place of
coordinates.

If the binding is given by a prefix and suffix, it adds coordinates
corresponding to the suffix.

Otherwise it adds empty strings so that the returned binding is always either of
the form

  (((PREFIX . SUFFIX) . COORDS) . VALUE)

or the original binding if its binding string could not be matched against any
symbol in the given layout."
  (let* ((value (cdr binding))

         (head (cond ((d--binding-suffix-form-p binding)
                      (let ((bindstr (car binding)))
                        (d--elaborate-on-bindstr bindstr)))

                     ;; Add coordinates corresponding to suffix if COORDSONLY is on.
                     ((d--binding-prefix-suffix-form-p binding)
                      (let* ((prefix (caar binding))
                             (prefixmods (d-modifiers-of-prefix prefix nil t))
                             (suffix (cdar binding))
                             (match (d--get-unique-layout-match suffix))
                             (coords (car match)))
                        (cons (cons prefixmods suffix) coords)))
                     ((d--binding-coords-form-p binding)
                      (cons (cons "" "") (car binding)))
                     ((d--binding-prefix-coords-form-p binding)
                      (cons (cons (d-modifiers-of-prefix (caar binding) nil t) "")
                            (cdar binding)))
                     ((d--binding-prefix-suffix-coords-form-p binding)
                      (cons (cons (d-modifiers-of-prefix (caaar binding) nil t)
                                  (cdaar binding))
                            (cdar binding)))
                     ((d--binding-elaborate-form-p binding) (car binding))
                     (t (error "%s in %s is an ill-formatted binding" binding (current-buffer)))))
         (elaborate-binding (cons head value)))
    elaborate-binding))

(defun d--reduce-binding (elbind &optional coordsonly)
  "Transform an elaborate binding ELBIND into its reduced form.
If COORDSONLY is given, use coordinates instead of suffixes whenever possible."
  (let* ((indmods (caaar elbind))
         (prefix (unless (not (caaar elbind))
                   (d-string-together-modifiers
                    (d-remove-indices indmods))))
         (suffix (cdaar elbind))
         (haspfx (and prefix (stringp prefix)
                      (not (string-empty-p prefix))))
         (hassfx (and suffix (stringp suffix)
                      (not (string-empty-p suffix))))
         (coords (cdar elbind))
         (value (cdr elbind))

         ;; Let's redefine COORDSONLY since we now know whether we have coordinates or not.
         (coordsonly (and coords coordsonly)))
    (cl-flet ((add-prefix-if-exists (arg)
                (if haspfx
                    (cons prefix arg)
                  arg)))
      (cons (if coordsonly (add-prefix-if-exists coords)
              (if hassfx (add-prefix-if-exists suffix)
                (if coords (add-prefix-if-exists coords)
                  (prog2 (message "Binding signal of %s in %s is empty." elbind
                                  (current-buffer))
                      nil))))
            value))))

;;;;; Comparison
(defun d--compare-standardized-modifier-lists (indmods1 indmods2)
  "Compare INDMODS1 and INDMODS2, two lists of standardized key modifiers.
 
Each list should be sorted and indexed by prefix as per
`d-sort-modifiers-by-prefix'. Return `(t)', `(nil)', or the string nil to
reflect how INDMODS1 compares to INDMODS2:

Return values and their meanings: - `(t)': INDMODS1 precedes INDMODS2 in the
sorted order. - `(nil)': INDMODS1 follows INDMODS2 in the sorted order. - nil:
INDMODS1 and INDMODS2 are equivalent.

The comparison is performed as follows: 1. Starting from the end, compare
corresponding pairs of modifiers in both lists. If the first differing pair is
found, return `(t)' if the modifier in INDMODS1 is less than that in INDMODS2,
or `(nil)' otherwise.

2. If both lists of modifiers are identical at this point, return nil.

3. If all modifiers in the shorter list match the corresponding elements in the
   longer list, but the lists are not equal in length, return `(t)' if INDMODS1
   is the shorter list (preceding by definition), or `(nil)' if INDMODS2 is
   shorter.

This function ensures consistent sorting of key modifiers lists by their
specificity and lexicographical order."
  ;; First compare modifiers over the length of the smaller.
  (let ((lengthcomp (cl-loop for indmod1 in (reverse indmods1)
                             for indmod2 in (reverse indmods2)
                             do (cond ((< (car indmod1) (car indmod2))
                                       (cl-return `(t)))
                                      ((> (car indmod1) (car indmod2))
                                       (cl-return `(nil)))))))
    (if lengthcomp
        lengthcomp

      ;; If the modifiers of the smaller are contained in those of the larger, test if they are equal.
      (unless (equal indmods1 indmods2)

        ;; If that's not the case, then the one with more modifiers should come last.
        (if (< (length indmods1) (length indmods2))
            `(t)
          `(nil))))))

(defun d--compare-coords (coords1 coords2)
  "This function compares two d-emacs-xkb-coordinates COORDS1 and COORDS2 lexically.
First it checks layer, then row, then place.
If it finds no difference between the coordinates it sends the string
    \"isequal\"."
  (cl-loop for coord1 in coords1
           and coord2 in coords2
           do (cond ((< coord1 coord2)
                     (cl-return `(t)))
                    ((> coord1 coord2)
                     (cl-return `(nil))))))

(defun d--compare-suffixes (suffix1 suffix2)
  "Compare SUFFIX1 to SUFFIX2.
If SUFFIX1 islonger than SUFFIX2, it signals `(nil)'. If SUFFIX2 is longer than
SUFFIX1, it signals `(t)'. If they have the same length, it comparison uses the
following rules: If one of them is capitalized and the other isn't, the one that
is capitalized comes last. Otherwise, it compares them according to their
constituent character codes."

  (d-compare-by-sequential-predicates
   suffix1 suffix2
   #'d-string-shorter-or-samep
   (lambda (sfx)
     (string= sfx (upcase sfx)))
   #'string<))

(defun d--compare-elaborate-bindings (elbind1 elbind2 &optional coordsonly)
  "Compare elaborate bindings ELBIND1 and ELBIND2.

If COORDSONLY is t, then this function doesn't consider suffixes in sorting.
Note that, since in this case the function has three arguments, it can't be
directly used as a :lessp function by `sort', but has to be surrounded by a
lambda to be used in a two-argument function.

The main use of this function is in `d--sort-and-format-bindlist', see there for
the sorting order."
  (let* ((srtmods1 (caaar elbind1))
         (srtmods2 (caaar elbind2))
         (suffix1 (cdaar elbind1))
         (suffix2 (cdaar elbind2))
         (hassfx1 (and suffix1 (stringp suffix1) (not (string-empty-p suffix1))))
         (hassfx2 (and suffix2 (stringp suffix2) (not (string-empty-p suffix2))))
         (coords1 (cdar elbind1))
         (coords2 (cdar elbind2))

         ;; Compare by modifiers.
         (compmods (d--compare-standardized-modifier-lists srtmods1
                                                           srtmods2)))

    ;; Look if suffixes exist in one case but not the other.
    (cl-flet ((true-and-not (val1 val2)
                (d-compare-if-decidable (lambda (vval1 vval2)
                                          (and vval1 (not vval2)))
                                        val1 val2)))

      (let* ((distinct-suffix-existence (true-and-not hassfx1 hassfx2))
             (distinct-coords-existence (true-and-not coords1 coords2)))

        (cond  ;; Unmatched bindings should come first.
         (distinct-coords-existence (not (car distinct-coords-existence)))

         (compmods (car compmods))

         ;; If they have the same prefixes and COORDSONLY isn't on, look if one of them has been given as a key combination string by comparing suffixes. A non-empty suffix indicates that that binding features a key combination string and should come before one than does not have a suffix.
         ((unless coordsonly distinct-suffix-existence)
          (car distinct-suffix-existence))

         ((and hassfx1 hassfx2 (not coordsonly))
          (let ((compsfx (d--compare-suffixes suffix1 suffix2)))
            (if compsfx (car compsfx)
              (message  "%s and %s have the same pre- and suffixes in %S."
                        elbind1 elbind2 (current-buffer)))))
         
         ((and coords1 coords2)
          (let ((compcoords (d--compare-coords coords1 coords2)))
            (if compcoords (car compcoords)
              (message "%s and %s have the same prefixes and coordinates in %S."
                       elbind1 elbind2 (current-buffer))))))))))

;;;;; Formatting
;;;;;; Lists
(defun d--sort-and-format-bindlist (blist &optional coordsonly prefun modlist)
  "Sort a d-emacs-xkb bindlist BLIST and format the result.

  Key combinations that are not matched by the layout in d-emacs-xkb-layout are put at
  the very top (because they are most likely errors or depreciated).

  Modifiers are ordered according to their order in MODLIST (d-modifiers-list by
  default). Sets of modifiers are ordered according to the modifier in them the
  furthest back in MODLIST and so are added below the modifier the furthest
  back.

  Key combinations given by a full combination string are listed before others
  and are ordered alphabetically (since they are supposed to be recalled
  phonetically or lexically, not positionally). Key combination strings with
  capital characters appear after those with downcased characters, and Greek
  letters appear after Latin ones. Combinations with symbols that are neither
  appear after either and are not otherwise sorted.

  Keys are ordered according to their layer, row and place in the row.

  With optional argument COORDSONLY, the function translates bindings that are
  given by a binding string into ones given by coordinates if that is possible,
  i.e. if the end of the binding string corresponds to a signal in the
  d-emacs-xkb-layout.

  PREFUN is a function that is applied to the bindlist after it is transformed
  into an elaborate bindlist. This is useful to apply functions that should act
  on an elaborate bindlist and whose results should be sorted, like coordinate
  transformations.

  To allow for using this function in `d--recursively-act-on-bindlist', it
  checks if the input is an atom or nil and, if so, it returns the input."
  (if (or (atom blist) (not blist))
      blist
    (let* ((modlist (if modlist modlist d-modifiers-list))
           (case-fold-search nil)

           ;; Bring bindings in elaborate form and sort contained bindlists.
           (elaborate-list (mapcar (lambda (elt)
                                     (cond ((d--binding-p elt)
                                            (d--elaborate-on-binding
                                             elt))
                                           ((not (atom elt))
                                            (d--sort-and-format-bindlist
                                             elt coordsonly prefun modlist))
                                           (t elt)))
                                   blist))

           ;; Do any function that should be applied before the sorting.
           (prefun-elaborate-list (if prefun (funcall prefun elaborate-list) elaborate-list))

           (sorted-list (sort prefun-elaborate-list :lessp
                              (lambda (elt1 elt2)
                                (cond ((atom elt1) t) ; Atoms should be at the beginning.
                                      ((atom elt2) nil)
                                      ((not (d--binding-p elt1)) t) ; Then contained lists.
                                      ((not (d--binding-p elt2)) nil)

                                      (t (d--compare-elaborate-bindings
                                          elt1 elt2 coordsonly))))))

           (formatted-list (d--format-sorted-bindlist sorted-list coordsonly))
           
           ;; We have to remove the prefixes of sorted elements because they are already in the suffix string.
           (formatted-sans-unmatched-prefixes-list
            (mapcar
             (lambda (potunmatch)
               (if (d--elaborate-unmatched-binding-p potunmatch)
                   (cons (cons (cons nil (cdaar potunmatch)) (cdar potunmatch))
                         (cdr potunmatch))
                 potunmatch))
             formatted-list))

           (final-list (mapcar (lambda (elt)
                                 (if (d--binding-p elt)
                                     (d--reduce-binding elt coordsonly)
                                   elt))
                               formatted-sans-unmatched-prefixes-list)))
      final-list)))

(defun d--format-sorted-bindlist (sblist &optional coordsonly)
  "Take a sorted bindlist SBLIST and format it.
That means inserting headings for unmatched elements, modifier combinations,
layers and rows.

If COORDSONLY is t, assume coordinates are prefered to suffixes when elaborate
bindings are reduced."
  (let (runlst)
    (cl-flet (;; Function to add paragraphs
              (hasstr (arg) (and arg (not (string-empty-p arg)))))

      (remq nil ; Remove if-clauses that are evaluated negatively.
            (cl-loop for n from 0 to (1- (length sblist))
                     do (let ((potbind (nth n sblist)))
                          (if (not (d--binding-p potbind))
                              (setq runlst (append runlst (list potbind)))

                            (if (or (= n 0)
                                    (not (d--binding-p (nth (1- n) sblist))))

                                (let* ((binding potbind)

                                       (indmods (caaar binding))
                                       (prefix (d-string-together-modifiers
                                                (d-remove-indices indmods)))
                                       (haspfx (hasstr prefix))
                                       
                                       (suffix (cdaar binding))
                                       (hassfx (hasstr suffix))
                                       
                                       (coords (cdar binding))
                                       (layer (car coords))
                                       (row (nth 1 coords)))
                                  
                                  (setq runlst
                                        (append runlst
                                                (list
                                                 (if (and coords (or coordsonly
                                                                     (not hassfx)))
                                                     "\n;;;;; Coordinates\n"
                                                   "\n;;;;; Strings\n")
                                                 
                                                 (if haspfx
                                                     (format ";;;;;; %s\n" prefix))

                                                 (if (and layer (or (not hassfx)
                                                                    coordsonly))
                                                     (format ";;;;;;; %s%s\n" prefix layer))
                                                 
                                                 (if (and row (or (not hassfx)
                                                                  coordsonly))
                                                     (format ";;;;;;;; %s%s-%s\n"
                                                             prefix
                                                             layer
                                                             row))

                                                 binding))))

                              (let* ((binding1 (nth (1- n) sblist))
                                     (binding2 (nth n sblist))
                                     (indmods1 (caaar binding1))
                                     (indmods2 (caaar binding2))
                                     ;; (prefix1 (d-string-together-modifiers
                                     ;;           (mapcar (lambda (indmod) (nth 1 indmod)) indmods2)))
                                     (prefix2 (d-string-together-modifiers
                                               (d-remove-indices indmods2)))
                                     (suffix1 (cdaar binding1))
                                     (suffix2 (cdaar binding2))
                                     (hassfx1 (hasstr suffix1))
                                     (hassfx2 (hasstr suffix2))
                                     (coords1 (cdar binding1))
                                     (coords2 (cdar binding2))
                                     (layer1 (nth 0 coords1))
                                     (layer2 (nth 0 coords2))
                                     (row1 (nth 1 coords1))
                                     (row2 (nth 1 coords2))

                                     (eqmatch (not (and (not coords1)
                                                        coords2)))
                                     (eqhssfx (or (and hassfx1 hassfx2)
                                                  (and (not hassfx1) (not hassfx2))))
                                     (eqpfx (or (and (not indmods1) (not indmods2))
                                                (and indmods1 indmods2
                                                     (not ; Nil means they are the same.
                                                      (d--compare-standardized-modifier-lists
                                                       indmods1
                                                       indmods2)))))
                                     (eqlay (or (and (not layer1) (not layer2))
                                                (and layer1 layer2 (= layer1 layer2))))
                                     (eqrow (or (and (not row1) (not row2))
                                                (and row1 row2 (= row1 row2)))))
                                
                                ;; The transition between strings and coordinates has to be placed differently depending on whether the suffixes are replaced by coordinates.
                                (if (or (and (not eqmatch) coordsonly)
                                        (and (not eqhssfx) (not coordsonly)))
                                    (setq runlst (append runlst
                                                         (list (d-generate-newlines 2)
                                                               (format
                                                                ";;;;; Coordinates")))))

                                (if (not eqpfx)
                                    (setq runlst (append
                                                  runlst
                                                  (if (d-string-exists-and-nonempty prefix2)
                                                      (list (d-generate-newlines
                                                             (if (or (and (not eqmatch)
                                                                          coordsonly)
                                                                     (and (not eqhssfx)
                                                                          (not coordsonly)))
                                                                 1
                                                               2))
                                                            (format
                                                             ";;;;;; %s"
                                                             prefix2))))))

                                (if (and (or (not eqpfx) (not eqlay))
                                         (or coordsonly
                                             (and (not hassfx1) (not hassfx2))
                                             (and (not coordsonly)
                                                  (not eqmatch))))
                                    
                                    (setq runlst (append runlst
                                                         (if layer2
                                                             (list (d-generate-newlines (if (and eqpfx
                                                                                                 eqmatch)
                                                                                            2
                                                                                          1))
                                                                   (format
                                                                    ";;;;;;; %s%s"
                                                                    prefix2
                                                                    layer2))))))

                                (if (and (not (and eqpfx eqlay eqrow))
                                         (or coordsonly
                                             (and (not hassfx1) (not hassfx2))))
                                    (setq runlst (append runlst
                                                         (if row2
                                                             (list (d-generate-newlines (if (and eqpfx
                                                                                                 eqlay)
                                                                                            2
                                                                                          1))
                                                                   (format
                                                                    ";;;;;;;; %s%s-%s"
                                                                    prefix2
                                                                    layer2
                                                                    row2))))))

                                (setq runlst (append runlst
                                                     (list (d-generate-newlines 1)
                                                           binding2)))))
                            ))
                     finally return runlst)))))

(defun d--sort-and-format-bindlists (&optional coordsonly prefun modlist directory)
  "Recurse through `d-emacs/pkg-configs' and format all bindlists within.
If PREFUN is specified, it denotes a function to run on each bindlist once its
bidings are in an elaborate form.

If COORDSONLY is t, prefer coordinates to suffixes when reducing elaborate
bindings.

 If MODLIST is given, use it to order modifiers instead of `d-modifiers-list'.

If `DIRECTORY' is given, it should be a subdirectory of `pkg-configs'. In that
case, recurse through `DIRECTORY'."
  (interactive)
  (d--act-on-pkg-files-by-type-and-maybe-kill
   `(((lambda (filename) (d--act-on-bindlists-in-file
                     filename
                     (lambda () (d--sort-and-format-marked-bindlist-string ,coordsonly ,prefun
                                                                      ,modlist))))
      .
      "bindlists"))
   (if directory directory)))

;;;;;; Strings
(defun d--sort-and-format-marked-bindlist-string (&optional coordsonly prefun modlist)
  "Sort and format a marked bindlist-string.
The function will read the contents of the selected region and process them
using `d--sort-and-format-bindlist' and
`d--format-bindlist-into-string-before-insertion', then replace the marked
region with the result.

COORDSONLY, PREFUN and MODLIST are passed forward to
`d--sort-and-format-bindlist'."
  (let* ((blist (d--extract-bindlist t))
         (formattedblist
          (d--sort-and-format-bindlist blist coordsonly prefun modlist))
         (formattedstring (d--format-bindlist-into-string-before-insertion formattedblist)))

    (d-replace-region-with-arg formattedstring))
  (unless (eobp)
    (forward-char)))

(defun d--format-bindlist-into-string-before-insertion (blist &optional headname)
  "Convert BLIST into a formatted string for reinsertion.
If HEADNAME is provided, use that as the head for the converted structure.
Otherwise the headname of the list or the name of the containing folder is
used."
  (let* ((print-level nil)
         (print-length nil)
         (initialstring (format "%S" blist))

         ;; Apply some finishing touches through string operations.
         (str-with-unquoted-newlines
          (replace-regexp-in-string
           (rx (: "\"" (group (one-or-more "\n")) "\""))
           "\\1"
           initialstring))

         (str-with-unquoted-comments
          (replace-regexp-in-string
           (rx (: "\"" (group (zero-or-more "\n") ";;;;" (one-or-more ";")
                              (one-or-more not-newline)
                              (zero-or-one "\n"))
                  "\""))
           "\\1"
           str-with-unquoted-newlines))

         (str-with-comments-at-line-beginnings
          (replace-regexp-in-string
           (rx (minimal-match line-start blank (group ";;;;" (one-or-more (not "\"")) line-end)))
           "\\1"
           str-with-unquoted-comments))

         (str-with-points-for-functions
          (replace-regexp-in-string " function " " . #'"
                                    str-with-comments-at-line-beginnings))

         (str-with-points-for-ifs
          (replace-regexp-in-string " \\(if .*\)\\)" " . \(\\1\)"
                                    str-with-points-for-functions))

         (str-with-points-for-lambdas
          (replace-regexp-in-string " \\(lambda .*\)\\)" " . \(\\1\)"
                                    str-with-points-for-ifs))

         (str-with-points-for-dynamic-rebinds
          (replace-regexp-in-string " \\(d-emacs-dynamic-binding .*\)\\)" " . \(\\1\)"
                                    str-with-points-for-lambdas))

         (str-with-points-and-brackets-around-coords
          (replace-regexp-in-string
           (rx blank (group (zero-or-one "-") num blank
                            (zero-or-one "-") num blank
                            (zero-or-one "-") num)
               "\)")
           " \. \(\\1\)\)"
           str-with-points-for-dynamic-rebinds))

         (head (if (string-match (rx-to-string '(: string-start "("
                                                   (minimal-match
                                                    (group (or (one-or-more (or "-" letter))
                                                               (: "\"" (one-or-more not-newline) "\"")))
                                                    (one-or-more space))
                                                   (group (or "\(" ";" "\n"))))
                                 str-with-points-and-brackets-around-coords)
                   (substring str-with-points-and-brackets-around-coords
                              (match-beginning 1) (match-end 1))))

         (str-with-line-breaks-after-head
          (if head (replace-regexp-in-string
                    (rx-to-string '(: string-start "("
                                      (minimal-match
                                       (group (or (one-or-more (or "-" letter))
                                                  (: "\"" (one-or-more not-newline) "\"")))
                                       (one-or-more space))
                                      (group (or "\(" ";"))))
                    "\(\\1\n\\2"
                    str-with-points-and-brackets-around-coords)
            str-with-points-and-brackets-around-coords))

         (finalstring (concat (format "\n;;;; %s\n`"
                                      (if headname headname
                                        (if head
                                            head
                                          (let ((filename (buffer-file-name)))
                                            (if filename
                                                (concat
                                                 (format "%s-mode-map"
                                                         (d-containing-directory-base-name
                                                          filename))))))))
                              str-with-line-breaks-after-head)))
    finalstring))

;;;;;; Buffers
(defun d--delete-duplicate-comment-lines ()
  "Delete duplicate comment lines separated by blank lines in current buffer."
  (save-excursion
    (goto-char (point-min))
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
        (forward-line)))))

;;;;; Extraction
(defun d--extract-binding-string (binding &optional translate csectoshft doublebind)
  "Return a binding string or list of strings from Daselt-binding.

BINDING is expected in a specific form compatible with Daselt. Unless DOUBLEBIND
is t, the return value is a string representing the binding, potentially
adjusted based on the optional parameters TRANSLATE, CSECTOSHFT.

- If TRANSLATE is t, translate the binding using translation alists
  `d-emacs-key-translations-alist' and `d-stump-emacs-key-translations-alist'.
  
- If CSECTOSHFT is t, and the binding corresponds to the second layer with
  either no modifiers or one including `C-', replace the binding suffix with its
  downcased form and add an \"S-\" modifier.

- If DOUBLEBIND is t, check if the suffix of the binding or the key from
  coordinates matches the car of a cons cell in `d-emacs-double-symbs-alist'. If
  matched, form a second binding using the corresponding cdr to form the
  returned string. If `d-stump' is t, also check if a string in
  `d-stump-emacs-key-translations-alist' matches the current binding string.
  This is necessary to be able to apply discrete modifiers to translated
  bindings. In either case, return a list of all binding strings.

Signal an error if the binding is invalid (neither a suffix nor has matching
coordinates)."
  (let* ((elbind (d--elaborate-on-binding binding))
         (coords (cdar elbind))
         (sfx (cdaar elbind))
         (mods (d-remove-indices (caaar elbind)))
         (pfx (if coords ; If the binding is unmatched, then it has already its modifiers in its suffix.
                  (d-string-together-modifiers mods)
                ""))
         (coordval (if coords (d-emacs-coords-binding coords)))
         (newsfx ;; Let's put an error check here.
          (let ((newsfx (if (d-string-exists-and-nonempty sfx)
                            sfx
                          (if coords coordval))))
            (if (d-string-exists-and-nonempty newsfx)
                newsfx
              (error (if coords (format "Coordinates %s in binding %s have no match in %s."
                                        coords binding (d-emacs-coords--dfk-or-xkb-layout))
                       (format "%s has neither coordinates nor a suffix." binding))))))
         (non-translated-string (concat pfx newsfx))
         shifted) ; To check later whether it was shifted.

    ;; If on the second layer and csectoshft is t, and if either C is a modifier or there either are no mods and the length of the retained sfx is 1, replace the obtained sfx with its downcased variant and add S-modifier.
    (if (and csectoshft
             coordval
             (not (cl-member coordval
                             d-emacs-no-shift-list
                             :test #'string=))
             (= 2 (nth 0 coords))
             (or (and (not mods) (> (length coordval) 1))
                 (member ?C mods)))

        (setq coordval (downcase coordval)
              pfx (concat "S-" pfx)
              non-translated-string (concat pfx coordval)
              shifted t))
    
    (let* ((doubleval (if (and doublebind
                               (= 1 (length newsfx)))
                          (alist-get (string-to-char newsfx)
                                     d-emacs-double-symbs-alist)))
           (doublestr (if doubleval (concat pfx (char-to-string doubleval))))

           ;; The key combinations translated by Stump need to be double-bound when discrete mods are applied.
           (stumpdoublebind
            (if (and doublebind
                     d-stump
                     (not shifted)) ; Don't doublebind shifted things.
                (let ((discmods (cl-intersection mods d-discrete-modifiers-list)))
                  (if discmods
                      (let ((match (alist-get non-translated-string
                                              d-stump-emacs-key-translations-alist
                                              nil
                                              nil
                                              (lambda (ntstr carstr)
                                                (string-match-p ntstr carstr)))))
                        (if match
                            (concat (d-string-together-modifiers discmods) match)))))))

           (transstr (if translate
                         (let* ((st-trans
                                 (if d-stump (alist-get non-translated-string
                                                        d-stump-emacs-key-translations-alist
                                                        non-translated-string
                                                        nil
                                                        #'string=)
                                   non-translated-string))
                                (em-trans (if d-emacs-translate-keys
                                              (alist-get st-trans
                                                         d-emacs-key-translations-alist
                                                         st-trans nil #'string=)
                                            st-trans)))
                           em-trans))))

      (if doublebind
          (remq nil (list (if translate
                              transstr
                            non-translated-string)

                          (if doubleval doublestr)

                          (if (d-string-exists-and-nonempty stumpdoublebind)
                              stumpdoublebind)))
        (if translate transstr non-translated-string)))))



;; Work in progress.
;; (defun d--extract-binding-vector (binding &optional translate csectoshft doublebind)
;;   "This function takes a binding in Daselt-compatible form and returns the corresponding binding This.

;; TRANSLATE and DOUBLEBIND work as for #'d--extract-binding-string."
;;   (let* ((pfx (caar binding))
;;          (modifiers (d-modifiers-of-prefix pfx))
;;          (suffix (cdar binding))
;;          (coords (cdar binding))
;;          (bindingfromcoords (if coords (d-emacs-coords-binding coords) nil))
;;          (value (cdr binding))
;;          (base-vector (vconcat modifiers (list (or suffix bindingfromcoords))))
;;          (vector-after-doublebind (if (and doublebind (or suffix
;;                                                           (and coords (length=1 bindingfromcoords))))
;;                                       (let ((doubleval (alist-get (string-to-char (or suffix bindingfromcoords))
;;                                                                   d-emacs-double-symbs-alist)))
;;                                         (if doubleval
;;                                             (vconcat modifiers (list doubleval))
;;                                           base-vector))
;;                                     base-vector))
;;          (final-vector (if (and csectoshft bindingfromcoords
;;                                 (not (member bindingfromcoords d-emacs-no-shift-list))
;;                                 (= 2 (nth 0 coords))
;;                                 (or (not modifiers) (member 'control modifiers)))
;;                            (vconcat (vector 'shift) (downcase vector-after-doublebind))
;;                          vector-after-doublebind)))

;;     (if translate
;;         (let ((translated (translate-vector final-vector))) ; assuming translate-vector function
;;           translated)
;;       final-vector)))
(defun d--extract-bindlist (&optional noconstruct)
  "Extract the bindlist of a marked region.
If NOCONSTRUCT is t, extract only bindlists that are not constructed.
Constructed bindlists are distinguished by the fact that it is necessary to
evaluate them twice."
  (let* ((evalregion (eval (d-read-region))))
    (if (d--bindlist-p evalregion)
        evalregion
      (unless noconstruct
        (eval evalregion)))))

(defun d--extract-constant-cons ()
  "Extract a constant in a daselt-constants-file by evaluating the region."
  (eval (d-read-region)))

(defalias 'd--extract-advicelist 'd--extract-constant-cons
  "Extract an advicelist in a daselt-advicelist-file by evaluating the region.")

;;;;; Generation
(defun d--generate-define-key-strings-from-marked-bindlist ()
  "Create a `define-key' string for each binding in the currently marked bindlist."
  (let* ((blist (eval (d-read-region)))
         (map (car blist))
         (body (cdr blist)))
    (mapcar (lambda (binding)
              (concat "(define-key " map " (kbd \""
                      (d--escape-chars-in-str (d--extract-binding-string binding))
                      "\"\) "
                      (let ((bindval (cdr binding)))
                        (if (stringp bindval)
                            (concat "\"" bindval "\"")
                          (if (symbolp (eval bindval))
                              (concat "'" (symbol-name (eval bindval))))))
                      "\)\n"))
            body)))



;;;;; Saving bindlists
(defun d--save-bindlist-as-variable (blist)
  "Save BLIST as a variable.
Works similarly to `d-emacs--with-eval-backup-and-apply-bindlist' but does not
include a call to `d-emacs--apply-binding'."
  (let* ((pkgname (d-containing-directory-base-name (buffer-file-name)))
         ;; (pkgsymb (intern pkgname))
         (mapsymbdefaultname (concat pkgname "-mode-map"))
         
         (pkgdirname (file-name-directory (buffer-file-name)))
         (pkgdirnameparts (split-string pkgdirname "/"))

         (pfx (cl-loop for n from 0 to (1- (length pkgdirnameparts))
                       do (if (string= "pkg-configs" (nth n pkgdirnameparts))
                              (cl-return (nth (1+ n) pkgdirnameparts)))))
         (pfx (if (string-empty-p pfx)
                  "d"
                pfx))
         (prefix (concat pfx "-")))

    (if (not (d-head-if-exists blist))
        (let* ((filepath (buffer-file-name))
               (filename (file-name-nondirectory filepath))
               (filenamebase (file-name-base filepath))
               (symbname (if (d--special-file-p filename)
                             (substring filenamebase 0 -1)
                           (concat prefix
                                   (if (d--user-defined-file-p filename)
                                       "user-defined-"
                                     "")
                                   mapsymbdefaultname
                                   "-bindlist"))))
          (set (intern symbname)
               blist))

      (cl-flet* ((head-over-body (bblist)
                   (and (d-head-if-exists bblist)
                        (or (stringp (car bblist))  ; Let's ensure the head is a symbol
                            (symbolp (car bblist))) ; or string.
                        (not (d-head-if-exists (cdr bblist)))))

                 (name-if-symbol (elt)
                   (if (symbolp elt)
                       (symbol-name elt)
                     elt)))

        (if (head-over-body blist)
            (let ((namecore (name-if-symbol (car blist))))
              (set (intern (concat prefix namecore "-bindlist"))
                   blist))
          
          (d-funcalls-recursively
           blist
           `(((lambda (bblist &optional heads)
                (let* ((namecore (name-if-symbol bblist)))
                  (set (intern (concat prefix namecore "-bindlist"))
                       bblist)))
              .
              (lambda (bblist &optional heads)
                (and (d-head-if-exists bblist)
                     (or (stringp (car bblist))  ; Let's ensure the head is a symbol
                         (symbolp (car bblist))) ; or string.
                     (not (d-head-if-exists (cdr bblist)))))))
           (lambda (idx lst &optional _heads)
             (let ((elt (nth idx lst)))
               (and (not (atom elt))
                    (not (d--binding-p elt)))))))))))

;;;; Coordinate changes
(defun d--exchange-coordinates (coordlistlist &optional modlist coordsonly directory)
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
d--sort-and-format-bindlists, see the documentation there for their function.

MODLIST, COORDSONLY and DIRECTORY are forwarded to
`d-sort-and-format-bindlists'."
  (d--sort-and-format-bindlists
   coordsonly
   (lambda (blist) (d--change-coords-in-bindlist blist coordlistlist))
   modlist
   directory))

(defun d--change-coords-in-bindlist (blist coordlistlist)
  "Change coordinates in BLIST according to COORDLISTLIST.
Return the modified bindlist."
  (mapcar (lambda (bind) (d--change-coords-in-binding bind coordlistlist)) blist))

(defun d--change-coords-in-binding (bind coordlistlist)
  "Change coordinates in BIND according to COORDLISTLIST.
Return the modified binding."
  (let* ((carcoordsp (d-emacs-coords-p (car bind)))
         (cdarcoordsp (unless carcoordsp (d-emacs-coords-p (cdar bind))))
         (origcoords (cond (carcoordsp (car bind))
                           (cdarcoordsp (cdar bind))))
         (newcoords (unless (not origcoords)
                      (d--change-coordlist origcoords coordlistlist)))
         (carrest (cond (carcoordsp nil)
                        (cdarcoordsp (caar bind))
                        (t (car bind))))
         (val (cdr bind)))
    (cond (carcoordsp (cons newcoords val))
          (cdarcoordsp (cons (cons carrest newcoords) val))
          (t (cons carrest val)))))

(defun d--change-coordlist (origcoords coordlistlist)
  "Change the coordinates in ORIGCOORDS based on the COORDLISTLIST.
ORIGCOORDS is a list of coordinates. COORDLISTLIST is a list of lists, each
inner list COORDLIST representing a set of coordinates.

Each coordinate in ORIGCOORDS is compared to the values in the COORDLIST in
COORDLISTLIST that has the same index. If a matching coordinate is found in
COORDLISTLIST, the function returns the next coordinate value from COORDLIST. If
no matching coordinate is found or the matching coordinate is the last entry in
COORDLIST, the function returns the original coordinate value from ORIGCOORDS."
  (mapcar (lambda (indcoord)
            (let ((coordlist (nth (car indcoord) coordlistlist)))
              (if coordlist
                  (cl-loop for n
                           from 0
                           to (- (length coordlist) 2)
                           do (if (= (nth n coordlist) (cdr indcoord))
                                  (cl-return (nth (1+ n)
                                                  coordlist)))
                           finally return (cdr indcoord))
                (cdr indcoord))))
          (d-add-list-indices origcoords)))

;;;; Drawing
(defun d-execute-in-maximized-maybe-temp-buffer (bufname fun)
  "Execute FUN in the buffer BUFNAME.
Maximize the created buffer window and ask whether to restore the previous
window configuration."
  (let ((display-buffer-alist '((".*" display-buffer-full-frame)))
        (windconf (current-window-configuration)))
    (with-current-buffer-window
        bufname
        nil
        (lambda (_a _b) (if (yes-or-no-p "Restore previous window configuration? ")
                       (set-window-configuration windconf)))
      (funcall fun))))



(defun d--placeval-from-elaborate-binding (elbind)
  "Return PLACEVAL whose car is coords of ELBIND and cdr is its cdr.
If ELBIND has no coordinates, return nil."
  (let ((coords (cdar elbind))
        (val (cdr elbind)))
    (if coords (cons coords val))))

(defun d--elbinds-matching-modifier-regexps (blist modrxs)
  "Return elaborate forms of bindings in BLIST matching MODS.
Filter bindings by modifier regexps MODRXS. A modifier regexp is a string
matched against all modifiers in a binding. If the regexp string starts with
`^', the binding is matched by the regexp if and only if no modifier in the
binding matches the string."
  (let* ((case-fold-search nil)
         (pblist (d-filter-by-predicate blist #'d--binding-p))
         (elblist (mapcar (lambda (bind)
                            (d--elaborate-on-binding bind))
                          pblist))
         (purelblist (d-filter-by-predicate elblist (lambda (bind)
                                                      (not (d--string-binding-p bind))))))

    (d-filter-by-predicate purelblist
                           (lambda (elbind)
                             (cl-flet* ((ispositive (modrx)
                                          (not (string-match-p (rx string-start "^")
                                                               modrx))))
                               (let* ((elbindmods (d-remove-indices
                                                   (caaar elbind)))
                                      (modstrs (mapcar #'char-to-string elbindmods)))
                                 (if (equal modrxs '(nil))
                                     t
                                   (cl-every
                                    (lambda (modrx)
                                      (if (ispositive modrx)
                                          (cl-member modrx modstrs :test #'string-match-p)
                                        (not (cl-member modrx modstrs
                                                        :test #'string-match-p))))
                                    modrxs))))))))



;;;; Minibuffer Interaction
(defun d--pick-pkg-file-by-type (type &optional subdir nodefault)
  "Select a file in pkg-configs by TYPE.
TYPE can be any expression that can act as a type specifier for
`d--act-on-pkg-files-by-type'. Restrict to files in SUBDIR if specified. Return
nil if no file is chosen. If NODEFAULT is nil, mention a default in the prompt."
  (let* ((filelist (d--act-on-pkg-files-by-type
                    `((identity . ,type))
                    subdir))
         (redfilelist (mapcar #'file-name-base filelist))
         (chosenfile (completing-read
                      (concat "Choose file " (unless nodefault "(default this file): "))
                      redfilelist))
         (match (unless (string-empty-p chosenfile)
                  (cl-loop for filepath in filelist
                           do (if (string-match-p chosenfile filepath)
                                  (cl-return filepath))))))
    match))

;;;; Bindlist conversion
(defun d--parse-for-keybindings (rx &optional mappos keypos valpos consespos mapdefaultfun)
  "Parse REGION for keybindings using RX.
Return lists of maps and bindlistpieces.

REGION is the active region, or the current buffer if no region is active.

MAPPOS is the position of the keymapname in the RX as a group number.

KEYPOS is the position of the key combination in the RX. VALPOS is the position
of the key combination in the RX.

CONSESPOS is the position of conses in bindings in `use-package' `:bind'
sections.

MAPDEFAULTFUN is a function that describes how to obtain a map symbol if none is
found in the RX match."
  (save-excursion
    (goto-char (point-min))
    (let (; (start (if (use-region-p)
                                        ; (region-beginning)
                                        ; (point-min)))
          (end (if (use-region-p)
                   (region-end)
                 (point-max)))
          maps
          blistpieces)
      (while (re-search-forward rx end t)
        (let ((map (let ((maprxstr (if mappos (match-string mappos))))
                     (if (d-string-exists-and-nonempty maprxstr)
                         (read maprxstr)
                       (if mapdefaultfun (funcall mapdefaultfun)))))
              (key (if keypos (d-remove-text-properties-from-string
                               (match-string keypos))))
              (val (if valpos (read (match-string valpos))))
              (conses (if consespos (read (concat "(" (match-string consespos) ")")))))

          (push map maps)
          (push (if (and keypos valpos)
                    (list (cons key val))
                  (if consespos
                      conses))
                blistpieces)))

      (cons maps blistpieces))))

(defun d--do-parse-for-define-key-bindings ()
  "Parse all `define-key'-bindings in REGION.
Return lists of maps and bindlistpieces. REGION is the active region, or the
current buffer if no region is active."
  (let ((drx (rx
              line-start
              (* (not (or ";" "\n")))
              (or "(define-key"
                  "(keymap-set")
              (+ blank)
              (group (+ (not space))) ;; MAP
              (+ space)
              "("
              "kbd"
              (+ space)
              "\""
              (group (+ (not (any "\"")))) ;; KEY
              "\""
              ")"
              (+ space)
              (group (+? anything)) ;; VAL
              ")")))
    (d--parse-for-keybindings drx 1 2 3)))

(defun d--do-parse-for-global-key-set-bindings ()
  "Parse all `global-set-key'-bindings in REGION.
Return lists of maps and bindlistpieces. REGION is the active region, or the
current buffer if no region is active."
  (let ((grx (rx (* (not ";"))
                 (or "(global-set-key" "(global-key-set")
                 (* blank)
                 "\""
                 "("
                 "kbd"
                 (+ space)
                 "\""
                 (group (+ (not (any "\"")))) ;; KEY
                 "\""
                 ")"
                 (group (+ (not blank)))          ;; VAL
                 ")")))
    (d--parse-for-keybindings grx nil 1 2)))

(defun d--do-parse-for-bind-key-bindings ()
  "Parse all `bind-key'-bindings in REGION.
Return lists of maps and bindlistpieces. REGION is the active region, or the
current buffer if no region is active."
  (let ((brx (rx line-start
                 (* (not ";"))
                 "(bind-key"
                 (zero-or-one "*")
                 (* blank)
                 "\""
                 (group (+ (not (any "\"")))) ;; KEY
                 "\""
                 (* blank)
                 (group (+ (not blank)))          ;; VAL
                 (optional (* blank) (group (+? (not blank))))  ;; MAP
                 ")")))
    (d--parse-for-keybindings brx 3 1 2)))

(defun d--do-parse-for-use-package-bindings ()
  "Parse all `:bind'-sections of `use-package' configurations in REGION.
Return lists of maps and bindlistpieces. REGION is the active region, or the
current buffer if no region is active."
  (let ((urx (rx line-start
                 (* (not (or ";" "\n")))
                 ":bind"
                 (group (zero-or-one "*")) ; If it's of the form `:bind*'.
                 (zero-or-one (* (or space "\n"))
                              "(:map"
                              (zero-or-more (or space "\n"))
                              (group (* (not (any "(" ")" space "\n"))))) ; MAP
                 (* (or (: line-start (* blank) ";" not-newline "\n")
                        (not "("))) ; Skip as many commented lines or lines without a bracket as you can.
                 (group (* (or "\n"
                               (: line-start (* blank) line-end)
                               (: line-start (* blank) ";" (* not-newline) "\n") ; CONSES
                               (* (* space)
                                  "("
                                  (* (or (not (any "(" ")"))
                                         "\(\""
                                         "\)\""))
                                  ")"
                                  (* space))))))))

    (d--parse-for-keybindings urx 2 nil nil 3 (lambda () 'global-map))))

;;;; Quick keys
(defun d--generate-quick-key-variables ()
  "Generate the quick key variables used in Daselt-configurations.
Uses `d-special-quick-keys-bindlist' as the basis for generation."
  (let* ((filepath
          (concat d-emacs-directory
                  "pkg-configs/d-special-quick-keys-bindlists.el"))

         (keylist (prog1 (cl-remove-duplicates
                          (flatten-list (d--act-on-bindlists-in-file
                                         filepath
                                         (lambda () (let ((blist (d--extract-bindlist)))
                                                 (remq nil (mapcar (lambda (bind)
                                                                     (let ((sig (d--extract-binding-string bind)))
                                                                       (if (= 1 (length sig))
                                                                           (string-to-char
                                                                            sig))))
                                                                   blist)))))))

                    (let ((filebuffer (get-file-buffer filepath))) ; `get-file-buffer' can get tripped up by symlinks.
                      (unless (or d-debug d-keep-read-buffers (not filebuffer))
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
Auto-generated using `d--generate-quick-key-variables.'")

    (defvar d-quick-key-string keystring
      "Quick key string for Daselt.
Auto-generated using `d--generate-quick-key-variables.'")

    (defvar d-quick-key-string-cons keystringpair
      "Quick key string pair for Daselt.
Auto-generated using `d--generate-quick-key-variables.'")))

(provide 'd-functions)
;;; d-functions.el ends here
