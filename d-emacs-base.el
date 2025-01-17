;;; d-emacs-base.el -- Base functions and constants for d-emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Alexander Prähauser

;; Author: Alexander Prähauser <ahprae@protonmail.com>
;; Keywords: tools

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

;; d-emacs-base is the foundational library for Daselt's Emacs configuration, offering
;; a comprehensive set of essential functions and constants that support and enhance
;; various d-emacs packages. This library provides customizable options for debugging
;; and buffer management, ensuring flexibility and ease of use.

;; Key features include:

;; - **Number Operations:** Utilities for generating and manipulating numerical ranges,
;;   including functions to create lists of integers with optional exclusions and
;;   indexing capabilities.

;; - **File Handling:** Functions to manage and manipulate file paths and directories,
;;   such as retrieving base directory names and filtering symbols in obarrays.

;; - **Region Operations:** Tools for reading and modifying buffer regions, enabling
;;   seamless interaction with selected text within buffers.

;; - **List Processing:** A variety of list manipulation functions, including filtering
;;   by predicates, removing elements by index, and performing set-theoretic operations
;;   like power lists and set equality checks.

;; - **String Manipulation:** Utilities for handling and transforming strings, such as
;;   escaping characters, removing surrounding brackets, and checking string properties
;;   like uppercase status.

;; - **Line Operations:** Functions to interact with and manipulate lines within buffers,
;;   including marking lines, reading current lines, and generating newline characters.

;; - **Logical and Set-Theoretic Operations:** Implements logical predicates and set
;;   operations to facilitate complex data manipulations and condition checks.

;; - **Comparison Utilities:** Provides functions for comparing strings and other data types
;;   based on customizable predicates, enabling flexible and accurate comparisons.

;; - **Insertion and Buffer Management:** Tools for capturing inserted text, executing
;;   functions within specific buffer contexts, and managing window configurations.

;; - **Recursive Processing:** Advanced functions to apply operations recursively on
;;   nested data structures, supporting deep processing and transformation of complex
;;   objects.

;; - **Drawing and Window Management:** Functions to control buffer display settings,
;;   such as maximizing buffer windows and restoring previous window configurations.

;; By offering these versatile and robust utilities, d-emacs-base ensures that other
;; d-emacs packages can operate efficiently and consistently, promoting modularity,
;; reusability, and maintainability within the d-emacs ecosystem.

;;; Code:

;;;; Customs
(defgroup d-emacs
                    nil
                    "Customization group for d-emacs."
                    :group 'Convenience
                    :group 'External
                    :prefix "d-")

(defcustom d-emacs-debug
                                            nil
                                            "Enable debugging options in Daselt.

When non-nil, functions will print additional debugging messages."
                                            :type 'boolean
                                            :group 'd-emacs)


;;;; Constants
(defconst d-emacs-definition-types-list
  '(defun defmacro defconst defcustom defun* defalias defgroup define-derived-mode defvar-keymap)
  "List of definition macros for which `d-emacs-beginning-of-docstring' works.")

;;;; Functions
;;;;; General
(defun d-emacs-definition-name ()
  "If point is within a definition, move to the beginning of the docstring.
Works for definition types in `d-emacs-definition-types-list'."
  (mark-defun) 
  (condition-case nil
      (let* ((defn (d-emacs-read-region))
             (first-symbol (nth 0 defn))
             (name (nth 1 defn)))
        (if (member first-symbol d-emacs-definition-types-list)
            name))
    (error (display-warning
            :warning
            (if (region-active-p)
                (format "Could not read %s"
                        (buffer-substring-no-properties
                         (region-beginning)
                         (region-end)))
              (format "Tried to read inactive region at %s" (point)))))))

;;;;; Numbers
(defun d-emacs-numbers-between (num1 num2 &optional exclude1 exclude2)
  "Generate a list of integers from num1 to num2, including both.
With optional arguments EXCLUDE1 and EXCLUDE2, don't include num1
repectively num2."
  (cl-loop for k from (funcall (if exclude1 #'1+ #'identity) num1)
           to (funcall (if exclude2 #'1- #'identity) num2)
           collect k))

(defun d-emacs-cardinal (n &optional fromone)
  "Generate a list of integers from 0 to N-1.
If optional argument FROMONE is non-nil, return a list starting from 1 to N
instead."
  (d-emacs-numbers-between 0 n fromone (not fromone)))

(defun d-emacs-index (list &optional fromone)
  "Cons each element of LIST with its position in LIST.
If optional argument FROMONE is non-nil, indices start from 1; otherwise, they
start from 0."
  (cl-mapcar (lambda (index elt)
                                                                   (cons index elt))
             (d-emacs-cardinal (length list) fromone) list))


;;;;; Files
(defun d-emacs-containing-directory-base-name (filepath)
                                      "Retrieve the base name of the containing directory of FILEPATH.
This function does not include the full path or trailing slashes in the result."
                                      (file-name-nondirectory (directory-file-name (file-name-parent-directory filepath))))

(defun d-emacs-filter-obarray (predicate)
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


(defun d-emacs-definition-names-in-file (fname)
                                                "Return the names of definitions in FNAME, listed according to definition type.
Works for definition types in `d-emacs-definition-types-list'."
                                                (let ((buf (current-buffer)))
    (set-buffer (find-file-noselect fname))
    (prog1 (remq nil (mapcar (lambda (deftype)
                                                                             (remq nil
                                     (save-excursion
                                                                                     (d-emacs-goto-min)
                                                                                     (let (rlist)
                                         (while (search-forward (symbol-name deftype) nil t)
                                           (save-excursion
                                                                                           (beginning-of-defun)
                                                                                           (push (d-emacs-definition-name) rlist)))
                                         rlist))))
                             d-emacs-definition-types-list))
      (set-buffer buf))))


;;;;; Region operations
(defun d-emacs-read-region (&optional properties)
  "Read and return the contents of the current region as a Lisp expression.
The region is defined by the currently selected text in the buffer. Unless
PROPERTIES is t, read without properties."
  (let ((beg (region-beginning))
        (end (region-end)))
    (read (if properties
                                                                                                                                  (buffer-substring beg end)
                                                                      (buffer-substring-no-properties beg end)))))

(defun d-emacs-replace-region (arg)
  "Replace the currently selected region with the content of ARG.
The text currently in the region is deleted, and ARG is inserted at the end of
the selection."
  (let* ((beg (region-beginning))
         (end (region-end)))
    (goto-char end)
    (insert arg)
    (delete-region beg end)))

;;;;; Lists
(defun d-emacs-remove-indices (indlst)
  "Remove indices of the elements of INDLST."
  (mapcar (lambda (indelt)
            (cdr indelt))
          indlst))

(defun d-emacs-filter-list (lst pred)
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

(defun d-emacs-global-sections (llist)
  "Given a LLIST of lists, return the global sections of LLIST.

That means, the return value is the list of all lists whose first entry is an
element of the first element of LLIST, whose second entry is an element of the
second element of LLIST and so on."
  (unless (proper-list-p llist)
    (error "Wrong-type input, expected a list"))
  (mapc (lambda (elt)
          (if (atom elt)
              (error "Wrong-type input, expected a list of lists")))
        llist)
  (mapcan (lambda (elt)
            (if (cdr llist)
                (mapcar (lambda (section)
                          (append (list elt) section))
                        (d-emacs-global-sections (cdr llist)))
              (list (list elt))))
          (car llist)))

(defun d-emacs-recursive-sections (obj)
  "If OBJ if not a list, put two brackets around it.

Otherwise, make each element of OBJ that is not a list into one and flatten each
element that is a list. Then apply `d-emacs-global-sections' around it."
  (if (proper-list-p obj)
      (let ((formattedlist (mapcar (lambda (elt)
                                     (if (and elt (proper-list-p elt))
                                         (flatten-list elt)
                                       (list elt)))
                                   obj)))
        (d-emacs-global-sections formattedlist))
    (list (list obj))))

(defun d-emacs-lisp-file-code (filename)
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

(defun d-emacs-cl-mapcar-or-only (fun &rest objs)
  "Invoke FUN with elements from the provided Lisp objects OBJS.
If all objects in OBJS are lists, apply FUN with their elements as arguments
using `cl-mapcar'. If some OBJS are not lists, they are converted into
one-element lists before the application."
  (apply #'cl-mapcar fun
         (mapcar (lambda (obj)
                   (d-emacs-make-list-if-not obj))
                 objs)))

(defun d-emacs-cons-to-list (cns)
  "Convert the cons cell CNS into a list containing its elements.
This creates a list where the first element is the car of CNS, and the second
element is its cdr."
  (list (car cns) (cdr cns)))

(defun d-emacs-list-to-cons (lst)
  "Convert the list LST into a cons consisting of the first two elements of LST.."
  (cons (car lst) (nth 1 lst)))

(defun d-emacs-flatten-until (lst cnd)
  "Flatten the list LST until the condition CND becomes true.
CND should be a function accepting one argument to check flattenings of LST."
  (let ((rlist lst))
    (cl-loop until (funcall cnd rlist)
             do (setq rlist (apply #'append rlist))
             finally return rlist)))

(defun d-emacs-flatten-n-times (lst &optional n)
  "Flatten LST N times, concatenating all lists within LST.
If N is not provided, the function flattens LST once."
  (let ((n (if n n 1))
        (runlst lst))
    (cl-loop for k from 1 to n
             do (setq runlst (apply #'append runlst))
             finally return runlst)))

(defun d-emacs-reverse-alist-get (key alist &optional default testfn)
  "Return the car of the first cons in ALIST whose cdr equals KEY.
If nothing is found, return DEFAULT.
If TESTFN is given, use it for testing, otherwise use `equal'."
  (catch 'found
    (dolist (item alist)
      (when (funcall (or testfn #'equal) (cdr item) key)
        (throw 'found (car item))))
    default))

(defun d-emacs-sexp-end-position (&optional beg)
  "Return the ending position of the sexp beginning at BEG.
If BEG is not given, it is set using `point'."
  (let ((beg (or beg (point))))
    (save-excursion
      (goto-char beg)
      (forward-sexp)
      (point))))

;;;;; Strings
(defun d-emacs--escape-chars-in-str (str)
  "Escape characters in STR that are defined in `d-emacs-bind-escape-kbd-regexps-list'.
This function modifies instances of the defined regex patterns."
  (let ((escaped-char-str
         (cl-loop for rx in d-emacs-bind-escape-kbd-regexps-list
                  if (string-match-p rx str)
                  return (replace-regexp-in-string rx
                                                   "\\\\\\1"
                                                   str
                                                   nil)
                  finally return str)))
    (if escaped-char-str escaped-char-str str)))

(defun d-emacs-remove-surrounding-brackets (str)
  "Remove the initial and final bracket in STR."
  (let ((inibrapos (string-search "\(" str))
        (finbrapos (- (string-search "\)" (reverse str)))))
    (substring str (1+ inibrapos) (1- finbrapos))))

(defun d-emacs-remove-text-properties-from-string (str)
  "Return a new string based on STR with all text properties removed."
  (let ((no-prop-str (copy-sequence str)))
    (set-text-properties 0 (length no-prop-str) nil no-prop-str)
    no-prop-str))

(defun d-emacs-string-exists-and-nonempty (str)
  "Return t if STR exists and is not empty."
  (and str (not (string-empty-p
                 str))))


;; Taken from s.el
(defun d-emacs-uppercase-p (str)
                        "Return t if all letters in STR are uppercase."
                        (declare (side-effect-free t))
                        (let ((case-fold-search nil))
    (not (string-match-p "[[:lower:]]" str))))

(defun d-emacs-concat-with-separators (separator &rest strs)
  "Concatenate STRS, inserting SEPARATOR between them."
  (mapconcat #'identity strs separator))
(defun d-emacs-intern-from-parts (&rest parts)
  "Concatenate strings PARTS, inserting an `-' separator between each.

Make the result a symbol."
  (intern (apply #'d-emacs-concat-with-separators "-" parts)))
;;;;; Filling
(defun d-emacs-fill-current-docstring ()
                                                                  "Fill the docstring of the current definition.
Works for definitions in `d-emacs-definition-types-list'.
Works for the definition point is in."
                                                                  (interactive)
                                                                  (cl-flet ((fill-rest (beg &optional end)
                                                                                    (progn (forward-char)
                     (let ((fillbeg (if (< (line-beginning-position) beg)
                                                                                                                                                                                    beg
                                                                                                            (beginning-of-line)
                                                                                                            (delete-horizontal-space)
                                                                                                            (point))))
                       (fill-region fillbeg (or end (progn (goto-char beg)
                                                           (forward-sexp)
                                                           (point))))))))
    (save-excursion
                                                                            (d-emacs-beginning-of-docstring)
                                                                            (d-emacs--fill-string-at-point-like-docstring))))

(defun d-emacs--fill-string-at-point-like-docstring ()
  "Fill the string at point like it's a docstring.

Return the filled string."
  (cl-flet ((fill-rest (beg &optional end)
              (progn (forward-char)
                     (let ((fillbeg (if (< (line-beginning-position) beg)
                                        beg
                                      (beginning-of-line)
                                      (delete-horizontal-space)
                                      (point))))
                       (fill-region fillbeg (or end (progn (goto-char beg)
                                                           (forward-sexp)
                                                           (point))))))))
    (let ((beg (point))
          (end (d-emacs-sexp-end-position)))
      (unless (and (<= end (line-end-position))
                   (<= (- end beg) 80)) ; If only one small line, leave alone.
        (forward-line)
        (fill-region beg (min end (line-end-position)))
        (let ((lineend (line-end-position))
              (end (save-excursion (goto-char beg)
                                   (forward-sexp)
                                   (point))))
          (goto-char beg)
          (cl-loop while (re-search-forward (rx (any ".!?"))
                                            lineend
                                            t) ; Look if it contains a potential sentence end.
                   do (if (looking-at "\n") ; Fill rest if already at line end.
                          (progn (forward-char)
                                 (cl-return (fill-rest beg end)))
                        (when (looking-at (rx space)) ; Check that it's a sentence end.
                          (delete-horizontal-space)
                          (unless (looking-at "\n")
                            (insert "\n"))
                          (cl-return (fill-rest beg))))

                   finally do ; Only called if no sentence end is found.
                   (fill-region beg end))
          (prog1 (progn (goto-char beg) ; Return the docstring.
                        (mark-sexp)
                        (buffer-substring (region-beginning)
                                          (region-end)))
            (end-of-defun)))))))

(defun d-emacs-fill-string-like-docstring (str)
  (with-temp-buffer
    (insert (concat "\"" str "\""))
    (d-emacs-goto-min)
    (d-emacs--fill-string-at-point-like-docstring)
    (buffer-substring (1+ (point-min)) (1- (point-max)))))

(defun d-emacs-fill-docstrings-in-buffer ()
                    "Fill the docstrings in the current buffer.
Works for functions, macros, constants, and customs. If the first line in the
region is a full sentence, insert a new line at the end and re-fill the rest."
                    (interactive)
                    (save-excursion
                            (goto-char (point-min))
                            (while (re-search-forward 
            (eval `(rx line-start
                       (* space)
                       "("
                       ,(append '(or) 
                                (mapcar #'symbol-name d-emacs-definition-types-list))
                       (+ space)
                       (1+ (not (any space)))))
            nil t)
      (d-emacs-fill-current-docstring)
      (end-of-defun))))

;;;;; Lines
;; Taken from min,sc-cmds.el (Icicle library).
(defun d-emacs-mark-line (&optional arg)
  "Put mark at end of line, point at beginning.
A numeric prefix ARG means move forward (backward if negative) that
many lines, thus marking a line other than the one point was
originally in."
  (interactive "P")
  (setq arg  (if arg (prefix-numeric-value arg) 0))
  (forward-line arg)
  (push-mark nil t t)
  (goto-char (line-end-position)))

(defun d-emacs-read-line ()
  "Read the current line."
  (buffer-substring-no-properties (line-beginning-position)
                                  (line-end-position)))

(defun d-emacs-generate-newlines (k)
  "Generate a string containing K newlines."
  (cl-loop for i from 1 to k
           concat "\n"))

(defun d-emacs-surround-by-newlines (k l str)
  "Prepend STR with K newlines and append it with L newlines."
  (concat (d-emacs-generate-newlines k) str (d-emacs-generate-newlines l)))

(defun d-emacs-prepend-newlines (k str)
  "Prepend K newlines before STR."
  (d-emacs-surround-by-newlines k 0 str))

(defun d-emacs-append-newlines (k str)
      "Append K newlines before STR."
      (d-emacs-surround-by-newlines 0 k str))


;;;;; Logical and set-theoretic operations
(defun d-emacs-forall-p (list predicate)
  "Return LIST if all elements satisfy PREDICATE; otherwise, return nil."
  (cl-loop for elt in list
           do (if (not (funcall predicate elt))
                                                                          (cl-return nil))
           finally return t))

(defun d-emacs-exists-p (list predicate)
  "Return LIST if at least one element satisfies PREDICATE.
Otherwise return nil."
  (cl-loop for elt in list
           do (if (funcall predicate elt) (cl-return t))))

(defun d-emacs-complement (list1 list2 &optional compfun)
  "Return a new list containing elements of LIST1 that are not in LIST2.
The operationis non-destructive, preserving the original lists. Use COMPFUN for
comparisons, defaulting to `equal'."
  (cl-remove-if (lambda (element)
                  (cl-member element list2 :test (or compfun #'equal)))
                list1))

(defun d-emacs-powerlist (list &optional elt)
            "Generate the power list of SET represented by LIST.
Returns a list of all sublists of LIST with elements ordered like in LIST. ELT
is used for recursion and should normally not be set by the user."
            (let ((powerlist (list nil))
        (cutlist list))
    (if elt (mapcar (lambda (sublist) (append (list elt) sublist))
                    (d-emacs-powerlist (cl-loop for index from 0 to (cl-position elt list)
                                          do (setq cutlist (remq (nth index list) cutlist))
                                          finally return cutlist)))
      (cl-loop for elt in list
               do (setq powerlist (append powerlist (d-emacs-powerlist list elt)))
               finally return powerlist))))

(defun d-emacs-setequal (list1 list2 &optional elttest)
  "Return t if LIST1 has the same elements as LIST2.

ELTTEST is the test used for element comparison. It defaults to `equal'."
  (and (cl-subsetp
        list1 list2 :test elttest)
       (cl-subsetp
        list2 list1 :test elttest)))

(defun d-emacs-remove-list-index (lst idx)
                          "Remove the element at index IDX from LST and return the resulting list.
The operation does not modify the original list."
                          (let (runlst)
    (cl-loop for runidx from 0 to (1- (length lst))
             do (unless (= runidx idx)
                  (setq runlst (append runlst (list (nth runidx lst)))))
             finally return runlst)))

;;;;; Comparison
(defun d-emacs-leq-p (seq1 seq2)
  "Return t if SEQ1 is shorter than or equal in length to SEQ2."
  (<= (length seq1)
      (length seq2)))

(defun d-emacs-geq-p (seq1 seq2)
  "Return t if SEQ1 is longer than or equal in length to SEQ2."
  (>= (length seq1)
      (length seq2)))

(defun d-emacs-compare-if-decidable (test arg1 arg2)
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

(defun d-emacs-compare-by-sequential-predicates (arg1 arg2 &rest predicates)
  "Compare ARG1 and ARG2 using the sequence of PREDICATES.
Return (t) if ARG1 fulfills the i-th predicate and ARG2 doesn't, return (nil) if
ARG2 fulfills the i-th predicate and ARG1 doesn't. If both or neither fulfill
the i-th predicate, compare using the next predicate. If both or neither fulfill
all predicates, return nil."
  (cl-loop for pred in predicates
           do (let ((compval (d-emacs-compare-if-decidable pred arg1 arg2)))
                (if compval (cl-return compval)))))

;;;;; Insertion
(defun d-emacs-capture-inserted-text (fn &rest args)
            "Capture the text inserted by FN called with ARGS and return it as a string."
            (with-temp-buffer
              (apply fn args)
              (buffer-string)))


(defun d-emacs-froundout (num)
  "Round NUM to the nearest integer whose value is higher and return as a float.
The opposite of ftruncate, but unlike ftruncate accepts non-floating numbers."
  (let* ((num (float num))
         (tnum (ftruncate num)))
    (if (= tnum num)
        tnum
      (if (<= 0.0 num)
          (1+ tnum)
        (1- tnum)))))

(defun d-emacs-roundout (num)
  "Round NUM to the nearest integer whose value is higher and return as an integer.
The opposite of truncate, but unlike truncate accepts non-floating numbers."
  (let* ((num (float num))
         (tnum (truncate num)))
    (if (= tnum num)
                            tnum
                (if (<= 0.0 num)
                              (1+ tnum)
                  (1- tnum)))))

(defun d-emacs-namecore (sym &optional pfx sfx)
  "Return the core of the symbol name of SYM.

This is the part between PFX and SFX.

If SYM does not have a PFX or SFX, ignore them."
  (let ((pfx (or pfx ""))
        (sfx (or sfx ""))
        (name (symbol-name sym)))
    (string-match (eval `(rx (? ,pfx) (group (* not-newline)) (? ,sfx))) name)
    (match-string 1 name)))

;;;;; Recursion
(defun d-emacs-funcalls-recursively (obj funtests &optional recursetest formatfun eltcolfun lstcolfun restargs restargfun contt debug)
  "Recursively apply functions to elements of OBJ based on condition tests.

This function processes OBJ using FORMATFUN to produce a list LST. If FORMATFUN
is not provided, it defaults to `identity'. The main operations of this function
involve iterating over LST's elements, applying tests and functions, and
potentially recursing into elements if they satisfy a recursion test.

FUNTESTS is a list of cons cells where each cell contains a function and a test
\(e.g., \(TEST . FUNCTION\)\). For each element ELT in LST, if its corresponding
TEST returns non-nil (using optional RESTARGS if provided), FUNCTION is applied
to ELT, and the result is collected into RUNLIST using ELTCOLFUN.

If RECURSETEST returns non-nil for an element ELT, `d-emacs-funcalls-recursively' is
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
                                     (d-emacs-funcalls-recursively
                                      elt funtests recursetest formatfun eltcolfun lstcolfun
                                      restargs restargfun contt debug))))
               
               finally return runlist))))

(defun d-emacs-funcall-recursively (obj fun test  &optional recursetest formatfun eltcolfun lstcolfun restargs restargfun contt debug)
                                "Recursively apply FUN to elements that satisfy TEST.
This wraps the contouring of arguments and collections found in
`d-emacs-funcalls-recursively'.
See there for further explanation."
                                (d-emacs-funcalls-recursively obj `((,fun . ,test)) recursetest formatfun eltcolfun lstcolfun restargs restargfun contt debug))

;;;;; Conses
(defun d-recursively-act-on-proper-conses (list fun &optional lstcolfun)
    "Recursively apply FUN to all non-list cons cells in LIST.

This function traverses through LIST and applies the function FUN to each cons
cell that is not considered a proper list. The goal is to process individual
cons cells while ignoring proper lists composed of them.

Parameters: - LIST: The structure containing cons cells and lists. - FUN: The
function to be applied to each non-list cons cell. - LSTCOLFUN: An optional
function to collect results; defaults to `append'.

The function uses `d-emacs-funcall-recursively' to handle traversal: - It identifies
and applies FUN to cons cells that are not proper lists. - Recursion occurs into
elements that are proper lists.

Results are collected using the specified LSTCOLFUN function, with a default
behavior of concatenating results via `append', which should return them in a
flat list."
    (let ((lstcolfun (or lstcolfun #'append)))
    (d-emacs-funcall-recursively list
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

;;;;; Drawing
(defun d-emacs-bind-with-max-buffer-maybe-return (bufname fun)
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

;;;;; Macros
;;;;;; Function-generation
(defmacro d-emacs-def-by-forms (templates &rest mappings)
  "Define families of functions based on TEMPLATES.

TEMPLATES should be a list of function-definition-templates starting with
backquotes that evaluate to function definitions when all placeholders are
bound.

Each element of MAPPINGS should be a cons cell where the car is a placeholder
symbol used in the TEMPLATES and the cdr is a list of values to substitute.

During macro expansion, each template is duplicated for each set of
substitutions from MAPPINGS with each placeholder replaced with the
corresponding value.

If a tuple of nth elements of MAPPINGS contains lists of elements, then
a copy of each template is generated for each combination of atomic elements
of these lists. For instance, if there are two TEMPLATES

`(obj1 a b)'
`(obj2 c (d (e f)))'

then template insertions are generated with
obj1 → a, obj → c
obj1 → b, obj → d
obj1 → b, obj → e
obj1 → b, obj → f."
  (let* ((substitutions (mapcar #'car mappings))
         (substitution-cardinal (d-emacs-cardinal (length substitutions)))
         (value-lists (mapcar #'cdr mappings))
         (lengths-set (let ((unique-lengths (delete-dups (mapcar #'length
                                                                 value-lists))))
                        (if (= (length unique-lengths) 1)
                            (car unique-lengths)
                          (error "All substitution lists must have the same length"))))
         (defun-list
          (cl-remove-duplicates
           (mapcan
            (lambda (template)
              (let* ((template-list
                      (mapcan
                       (lambda (defun-num)
                         (let* ((nth-values (mapcar
                                             (lambda (subst-num)
                                               (nth defun-num
                                                    (nth subst-num
                                                         value-lists)))
                                             substitution-cardinal))
                                (extended-values (d-emacs-recursive-sections nth-values))
                                (contexts (mapcar
                                           (lambda (extended-value)
                                             (mapcar (lambda (subst-num)
                                                       (cons (nth subst-num
                                                                  substitutions)
                                                             (nth subst-num
                                                                  extended-value)))
                                                     substitution-cardinal))
                                           extended-values)))
                           (mapcar (lambda (context)
                                     (eval template context))
                                   contexts)))
                       (d-emacs-cardinal lengths-set))))
                template-list))
            templates)
           :test #'equal)))
    (append '(progn) defun-list)))

(defmacro d-emacs-def-by-forms-by-variables (templates &rest mappings)
    "Like `d-emacs-def-by-forms' but each cdr of mappings should be a variable name or
otherwise evaluate to a list.

See `d-emacs-def-by-forms' for more documentation."
    (let ((newmappings (mapcar (lambda (mapping)
                                 (cons (car mapping) (eval (cdr mapping))))
                             mappings)))
    `(d-emacs-def-by-forms ,templates ,@newmappings)))

;;;;; Commands
(defun d-emacs-trim-lines ()
  "Remove whitespace at the end of each line in the current buffer."
  (interactive)
  (save-excursion (d-emacs-goto-min)
                  (while (re-search-forward (rx (+ blank) line-end) nil t)
                    (replace-match ""))))

(defun d-emacs-search-at-line-start (str &optional withcomments)
          "Search for an occurrence of STR at the start of a line.
Unlike normal `search-forward', `d-emacs-search-at-line-start' returns nil if no
match is found and does not cause an error.

If WITHCOMMENTS is t, also include occurences that are commented out."
          (interactive "MSearch for string: ")
          (re-search-forward (rx-to-string
                      (remq nil `(: line-start
                                    ,(if withcomments '(* (syntax comment-delimiter)))
                                    ,(if withcomments '(* blank))
                                    (literal ,str))))
                     (if (region-active-p) (region-end) nil)
                     t))
;;;; Provide
(provide 'd-emacs-base)
;;; d-emacs-base.el ends here
