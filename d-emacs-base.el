;;; d-emacs-base.el --- Base functions and constants for d-emacs  -*- lexical-binding: t; -*-

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

;; Base functions for d-emacs. Mostly for other d-emacs packages.

;;; Code:

;;;; Customs
(defcustom d-debug
    nil
    "Enable debugging options in Daselt.

When non-nil, functions will print additional debugging messages."
    :type 'boolean
    :group 'Daselt)

(defcustom d-keep-read-buffers
                                                      nil
                                                      "Keep buffers open after d-emacs-functions read them.

If non-nil, previously read buffers will not be closed."
                                                      :type 'boolean
                                                      :group 'Daselt)

;;;; General purpose functions
;;;;; Numbers
(defun d-numbers-between (num1 num2 &optional exclude1 exclude2)
  "Generate a list of integers from num1 to num2, including both.
With optional arguments EXCLUDE1 and EXCLUDE2, don't include num1
repectively num2."
  (cl-loop for k from (funcall (if exclude1 #'1+ #'identity) num1)
           to (funcall (if exclude2 #'1- #'identity) num2)
           collect k))

(defun d-cardinal (n &optional fromone)
  "Generate a list of integers from 0 to N-1.
If optional argument FROMONE is non-nil, return a list starting from 1 to N
instead."
  (d-numbers-between 0 n fromone (not fromone)))

(defun d-add-list-indices (list &optional fromone)
                                                          "Cons each element of LIST with its position in LIST.
If optional argument FROMONE is non-nil, indices start from 1; otherwise, they
start from 0."
                                                          (cl-mapcar (lambda (index elt)
                                                                       (cons index elt))
             (d-cardinal (length list) fromone) list))


;;;;; Files
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

(defun d-emacs-goto-min ()
  "Go to `point-min'."
  (goto-char (point-min)))

(defun d-emacs-goto-max ()
  "Go to `point-max'."
  (goto-char (point-max)))

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

(defun d-emacs-preimage (lst fun obj &optional keepobj eqpred)
  "Return a list containing all elements of LST mapped to OBJ by FUN.

If KEEPOBJ is t, return instead a cons whose car is OBJ and whose
cdr are all elements mapped to it.

EQPRED is the predicate used to find out equality. By default it is
#'equal."
  (let* ((eqpred (or eqpred #'equal))
         (result (delq nil (mapcar (lambda (element)
                                     (when (funcall eqpred (funcall fun element) obj)
                                       element))
                                   lst))))
    (if keepobj
        (cons obj result)
      result)))

(defun d-emacs-image (lst fun &optional eqpred)
  "Return the list of all results of FUN applied to elements of LST.

EQPRED is used to compare results for equality. It is #'equal by
default."
  (let ((eqpred (or eqpred #'equal)))
    (cl-remove-duplicates (mapcar fun lst) :test eqpred)))

(defun d-emacs-fiber-by-property (lst propfun &optional keepprops eqpred)
  "Fiber the elements of LST according to PROP.

PROPFUN should be a function that can be applied to the elements of LST.

Return a list whose elements are lists consisting of the elements that have
the same output under PROP.

Use EQPRED to compare the outputs of PROP. By default, EQPRED is #'equal.

If KEEPPROPS is t, return a list of conses whose cdr are lists with the
same property and whose car is a representative of that property."
  (let* ((eqpred (or eqpred #'equal))
         (props (d-emacs-image lst propfun eqpred))
         (fibration (mapcar (lambda (prop)
                              (let ((fiber (d-emacs-preimage
                                            lst propfun prop keepprops eqpred)))
                                fiber))
                            props)))
    fibration))

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

(defun d-list-to-cons (lst)
  "Convert the list LST into a cons consisting of the first two elements of LST.."
  (cons (car lst) (nth 1 lst)))

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

(defun d-reverse-alist-get (key alist &optional default testfn)
  "Return the car of the first cons in ALIST whose cdr equals KEY.
If nothing is found, return DEFAULT.
If TESTFN is given, use it for testing, otherwise use `equal'."
  (catch 'found
    (dolist (item alist)
      (when (funcall (or testfn #'equal) (cdr item) key)
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


(defun d-froundout (num)
  "Round NUM to the nearest integer whose value is higher and return as a float.
The opposite of ftruncate, but unlike ftruncate accepts non-floating numbers."
  (let* ((num (float num))
         (tnum (ftruncate num)))
    (if (= tnum num)
        tnum
      (if (<= 0.0 num)
          (1+ tnum)
        (1- tnum)))))

(defun d-roundout (num)
  "Round NUM to the nearest integer whose value is higher and return as an integer.
The opposite of truncate, but unlike truncate accepts non-floating numbers."
  (let* ((num (float num))
         (tnum (truncate num)))
    (if (= tnum num)
                            tnum
                (if (<= 0.0 num)
                              (1+ tnum)
                  (1- tnum)))))

(defun d-emacs-namecore (sym pfx sfx)
  "Return the core of the symbol name of SYM.
This is the part between PFX and SFX."
  (let ((name (symbol-name sym)))
    (string-match (eval `(rx ,pfx (group (* not-newline)) ,sfx)) name)
    (match-string 1 name)))
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

(provide 'd-emacs-base)
;;; d-emacs-base.el ends here
