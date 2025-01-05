;;; d-emacs.el --- Daselt's Emacs module              -*- lexical-binding: t; -*-

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

;;  d-emacs commands.

;;; Code:

;;;; Preamble
(require 'cl-lib)
(require 'outline) 

;; Declarations for functions from other files
(declare-function d--pick-pkg-file-by-type "external-file")
(declare-function d--act-on-bindlists-in-file "external-file")
(declare-function d--act-on-constants-in-file "external-file")
(declare-function d--act-on-advicelists-in-file "external-file")
(declare-function d--extract-bindlist "external-file")
(declare-function d--extract-constant-cons "external-file")
(declare-function d-emacs--with-eval-add-marked-advicelist "external-file")
(declare-function d-emacs--with-eval-remove-marked-advicelist "external-file")

;;;; Commands for `d-emacs-mode'
(defun d-emacs--with-eval-backup-and-apply-bindlists-in-file (&optional blistfile)
                  "Backup and bind all the bindlists in the file BLISTFILE.
BLISTFILE can be selected interactively from available bindlists in `d-emacs-directory/pkg-configs/d-emacs/'. If BLISTFILE is nil, defaults to the current buffer's file name."
                  (interactive  (list (d--pick-pkg-file-by-type '("bindlists" "regular")
                                                "d-emacs/")))

                  (let ((blistfile (or blistfile (buffer-file-name))))
    (d--act-on-bindlists-in-file
     blistfile
     (lambda () (d-emacs--with-eval-backup-and-apply-bindlist
            (d--extract-bindlist))))))


(defun d-emacs--backup-and-set-constants-in-file (&optional constfile)
  "Set all constants defined in CONSTFILE located in d-emacs constants files. Utilizes the buffer's file when CONSTFILE is nil. Parameters are set using `setopt', ensuring customs are set as appropriate."
  (interactive  (list (d--pick-pkg-file-by-type "constants")))

  (let ((constfile (or constfile (buffer-file-name))))
    (d--act-on-constants-in-file
     constfile
     (lambda () (let* ((constcons (d--extract-constant-cons))
                  (constsymb (car constcons))
                  (constname (symbol-name constsymb))
                  (constval (cdr constcons))
                  (backupname (concat "d-emacs-" constname "-backup"))
                  (backupsymb (intern backupname)))

             (unless (or (boundp backupsymb) ; Don't overwrite an existing backup.
                         (not (boundp constsymb)))
               (set backupsymb (symbol-value constsymb)))
             
             (set constsymb constval))))))

;;;; Bound to keys
;;;;; Modifiers
(defun d-emacs-MetaSuper-next-cmd ()
        "Apply Meta and Super modifiers to the next event."
        (interactive)
        (execute-kbd-macro
   (vector
    (event-apply-modifier
     (event-apply-modifier
      (read-event) 'super 23 "s-")
     'meta 27 "M-"))))

;;;;; General purpose
(defun d-emacs-rename-file-and-buffer (new-name)
      "Rename both the current buffer and its visiting file to NEW-NAME. Displays a message if the buffer isn't visiting a file or if a buffer with NEW-NAME already exists."
      (interactive "sNew name: ")
      (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
                    (message "Buffer '%s' is not visiting a file!" name)
            (if (get-buffer new-name)
                      (message "A buffer named '%s' already exists!" new-name)
              (progn
                (rename-file filename new-name 1)
                (rename-buffer new-name)
                (set-visited-file-name new-name)
                (set-buffer-modified-p nil))))))

(defun d-emacs-set-test-fun ()
  (interactive)
  "Evaluate the current defun and define `d-emacs-test' as an alias to it."
  (let ((fun (call-interactively #'eval-defun)))
    (defalias 'd-emacs-test fun)))

;;;;; Navigation
(defun d-emacs-scroll-chunk-up ()
        "Scroll up by a fixed chunk."
        (interactive)
        (scroll-up-command 10))

(defun d-emacs-scroll-chunk-down ()
        "Scroll down by a fixed chunk."
        (interactive)
        (scroll-down-command 10))

(defun d-emacs-outline-forward-up-heading (arg &optional invisible-ok)
          "Move up to a visible heading line that includes the current line as a subheading. Move up ARG levels. If INVISIBLE-OK is non-nil, consider invisible lines."
          (interactive "p")
          (and (eq this-command 'outline-up-heading)
       (or (eq last-command 'outline-up-heading) (push-mark)))
          (outline-back-to-heading invisible-ok)
          (let ((start-level (funcall outline-level)))
    (when (<= start-level 1)
      (error "Already at top level of the outline"))
    (while (and (> start-level 1) (> arg 0) (not (bobp)))
      (let ((level start-level))
        (while (not (or (< level start-level) (bobp)))
	  (if invisible-ok
	                      (outline-next-heading)
	            (outline-next-visible-heading 1))
	  (setq level (funcall outline-level)))
        (setq start-level level))
      (setq arg (- arg 1))))
          (if outline-search-function
                      (funcall outline-search-function nil nil t t)
            (looking-at outline-regexp)))

;;;;; Context-Specific behavior
(defun d-emacs-C-1-0-2 ()
  "Contextual command on `C-(1 0 2)'. Mode-specific contexts can be edited by changing `d-emacs-C-1-0-2-contexts-list`. Default behavior is to move cursor one character forward."
  (interactive)
  (d-emacs--first-mode-match d-emacs-C-1-0-2-contexts-list #'forward-char))

(defun d-emacs-C-1-0--2 ()
  "Contextual command on `C-(1 0 -2)'. Mode-specific contexts can be edited by changing `d-emacs-C-1-0--2-contexts-list`. Default behavior is to move cursor one character backward."
  (interactive)
  (d-emacs--first-mode-match d-emacs-C-1-0--2-contexts-list #'backward-char))

(defun d-emacs-C-1-0--3 ()
  "Contextual command on `C-(1 0 -3)'. Mode-specific contexts can be edited by changing `d-emacs-C-1-0--3-contexts-list`. Default behavior is to move cursor one line up."
  (interactive)
  (d-emacs--first-mode-match d-emacs-C-1-0--3-contexts-list #'previous-line))

(defun d-emacs-C-1-0-3 ()
  "Contextual command on `C-(1 0 3)'. Mode-specific contexts can be edited by changing `d-emacs-C-1-0-3-contexts-list`. Default behavior is to move cursor line down."
  (interactive)
  (d-emacs--first-mode-match d-emacs-C-1-0-3-contexts-list #'next-line))

(defun d-emacs-C-1-0--1 ()
  "Contextual command on `C-(1 0 -1)'. Mode-specific contexts can be edited by changing `d-emacs-C-1-0--1-contexts-list`. Default behavior is to move one page up."
  (interactive)
  (d-emacs--funcall-first-mode-match d-emacs-C-1-0--1-contexts-list #'scroll-down-command))

(defun d-emacs-C-1-0-1 ()
    "Contextual command on `C-(1 0 1)'. Mode-specific contexts can be edited by changing `d-emacs-C-1-0-1-contexts-list`. Default behavior is to move one page down."
    (interactive)
    (d-emacs--funcall-first-mode-match d-emacs-C-1-0-1-contexts-list #'scroll-up-command))

(defun* d-emacs-C-3-0-2 ()
  "Expand Yasnippet, Cdlatex item, abbrev, Corfu expansion if available. Also, go to the next item or environment, open a footnote, or open something as needed."
  (interactive)
  (cl-flet ((yas-maybe-expand ()
              (if (yas-maybe-expand-abbrev-key-filter t)
                  (progn (yas-expand)
                         (cl-return-from d-emacs-C-3-0-2))))

            (cdlatex-maybe-expand ()
              (let ((pos (point)) expr math-mode)
                (backward-word 1)
                (while (eq (following-char) ?$) (forward-char 1))
                (setq expr (buffer-substring-no-properties (point) pos))
                (setq expr (assoc expr cdlatex-command-alist-comb))
                (if expr (progn (setq math-mode (cdlatex--texmathp))
                                (when (or (and (not math-mode) (nth 5 expr))
                                          (and math-mode (nth 6 expr)))
                                  (delete-char (- pos (point)))
                                  (insert (nth 2 expr))
                                  (and (nth 3 expr)
                                       (if (nth 4 expr)
                                           (apply (nth 3 expr) (nth 4 expr))
                                         (funcall (nth 3 expr)))))
                                (cl-return-from d-emacs-C-3-0-2))
                  (goto-char pos))))

            (maybe-expand-abbrev ()
              (let ((pos (point)))
                (expand-abbrev)
                (unless (= pos (point))
                  (cl-return-from d-emacs-C-3-0-2)))))

    (if d-emacs-yasnippet (yas-maybe-expand))
    (if d-emacs-cdlatex (cdlatex-maybe-expand))
    (maybe-expand-abbrev)

    (d-emacs--funcall-first-mode-match d-emacs-C-3-0-2-contexts-list #'org-open-at-point-global)))


;;;;;; Minibuffer
(defun d-emacs-move-right-or-exit ()
  "Move a character right unless at the end of the minibuffer, where it exits."
  (interactive)
  (if (eobp)
      (exit-minibuffer)
    (forward-char)))

;;;;; Yanking 
(defun d-emacs-yank-or-org-roam-node-insert ()
  "Yank text when mark is inactive; otherwise call `org-roam-node-insert`."
  (interactive)
  (cond ((derived-mode-p 'vterm-mode)
         (vterm-yank))
        ((use-region-p)
         (call-interactively #'org-roam-node-insert))
        (t (yank))))

(defun d-emacs-kill-append ()
  (interactive)
  (append-next-kill)
  (kill-region (mark) (point)))

;;;;; Killing
(defun d-emacs-kill-defun (&optional arg)
  "Kill the current defun. Marks the active region and kills it. Uses ARG to determine the scope."
  (interactive "p")
  (mark-defun arg)
  (kill-region (region-beginning) (region-end)))


;;;;; Insertion
(defun d-emacs-insert-and-return (str)
  "Insert STR and return to the position before insertion."
  (interactive "String: %s")
  (let ((pos (point)))
    (insert str)
    (goto-char pos)))

(defun d-emacs-insert-space-forward ()
  "Insert a space and return to the original position."
  (interactive)
  (d-emacs-insert-and-return " "))

(defun d-emacs-insert-newline-forward ()
  "Insert a newline and return to the original position."
  (interactive)
  (d-emacs-insert-and-return "\n"))

(defun d-emacs-insert-lambda-string (&optional arg)
              "Insert string 'lambda'. With ARG, insert 'λ' symbol."
              (interactive "P")
              (if arg
                              (insert "λ")
                (insert "lambda")))


;;;;; Filling
;; (defun d-emacs-fill-docstrings-in-buffer ()
;;   "Fill the docstrings of functions, macros, constants, and customs in the current buffer."
;;   (interactive )
;;   (save-excursion
;;     (goto-char (point-min))
;;     (while (re-search-forward (rx line-start
;;                                   (* space)
;;                                   "("
;;                                   (or "defun" "defmacro" "defconst" "defcustom" "defun*")
;;                                   (+ space)
;;                                   (1+ (not (any space))))
;;                               nil t)
;;       (d-emacs-mark-docstring)
;;       (fill-region (region-beginning) (region-end)))))

(defun d-emacs-fill-current-docstring ()
  "Fill the docstring of the current definition.
Works for definitions in `d-emacs-docstring-functions-list'.
Works for the definition point is in."
  (interactive)
  (cl-flet ((fill-rest (beg &optional end)
              (progn (forward-char)
                     (fill-region (point)
                                  (or end (progn (goto-char beg)
                                                 (forward-sexp)
                                                 (point)))))))
    (save-excursion
      (d-emacs-beginning-of-docstring)
      (let ((beg (point))
            (end (d-sexp-end-position)))
        (unless (<= end (line-end-position)) ; If only one line, leave alone.
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
            (end-of-defun)))))))

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
                                (mapcar #'symbol-name d-emacs-docstring-functions-list))
                       (+ space)
                       (1+ (not (any space)))))
            nil t)
      (d-emacs-fill-current-docstring)
      (end-of-defun))))


;;;;; Declaring
(defun d-emacs-generate-declare-function ()
  "Generate a `declare-function' statement for the function at point."
  (interactive)
  ;; Check if the thing at point is a symbol
  (let ((sym (symbol-at-point)))
    (if (not (and sym (fboundp sym)))
        (message "No function at point.")
      ;; If it is a function, find where it is defined
      (let* ((function-location (find-lisp-object-file-name sym (symbol-function sym)))
             (args (help-function-arglist sym t)))
        (if (not function-location)
            (message "Cannot find function location."))
        ;; Prepare the declare-function expression
        (let ((declare-statement 
               (format "(declare-function %s \"%s\" %s)"
                       sym
                       (if function-location (file-name-sans-extension
                                              (file-name-nondirectory function-location)))
                       (if args
                           (format "%s" (mapcar (lambda (arg) (if (symbolp arg) arg (car arg))) args))
                         "nil"))))
          (save-excursion (if (progn (goto-char (point-min))
                                     (d-search-at-line-start "(declare-function"))
                              (progn (beginning-of-line)
                                     (insert (d-append-newlines 1 declare-statement)))
                            (if (re-search-forward (rx line-start "(require"))
                                (progn (while  (d-search-at-line-start "(require")
                                         (end-of-line))
                                       (insert (d-prepend-newlines 2 declare-statement)))
                              (if (d-search-at-line-start ";;;; Preamble")
                                  (progn (end-of-line)
                                         (insert (d-append-newlines 1 declare-statement)))
                                (if (d-search-at-line-start ";;; Code")
                                    (progn (end-of-line)
                                           (insert (format "\n;;;; Preamble\n%s"
                                                           declare-statement)))))))))))))

(defun d-emacs-generate-variable-definition ()
  "Generate an unvalued `defvar'-statement for the variable under point.
The statement is put into the preamble of the current file."
                                                                                          (interactive)
                                                                                          (let ((sym (symbol-at-point)))
    (if (not sym)
                                                    (message "No symbol under point.")
                            (let ((defvar-statement (concat "(define-variable " (symbol-name sym))))
        (save-excursion (if (progn (goto-char (point-min))
                                   (d-search-at-line-start "(define-variable"))
                                                                        (progn (beginning-of-line)
                                   (insert (d-append-newlines 1 defvar-statement)))
                          (if (d-sea)
                                                                          (if (d-search-at-line-start "(declare-function")
                                                                              (progn (while  (d-search-at-line-start "(declare-function")
                                           (end-of-line))
                                         (insert (d-prepend-newlines 2 defvar-statement)))
                                (if (d-search-at-line-start ";;;; Preamble")
                                                                                (progn (end-of-line)
                                           (insert (d-append-newlines 1 defvar-statement)))
                                  (if (d-search-at-line-start ";;; Code")
                                                                                  (progn (end-of-line)
                                             (insert (format "\n;;;; Preamble\n%s"
                                                             defvar-statement)))))))))))))

;;;;; Recentering
(defun d-emacs-recenter-top ()
  "Recenter the screen with the current line at the top."
  (interactive)
  (recenter 1))

(defun d-emacs-recenter-bottom ()
  "Recenter the screen with the current line at the bottom."
  (interactive)
  (recenter -1))

;;;;; Toggling
(defun d-emacs-toggle-mode-line ()
  "Toggle the mode-line."
  (interactive)
  (if (equal mode-line-format nil)
      (setq mode-line-format
            '("%e" mode-line-front-space
              (:propertize
               ("" mode-line-mule-info mode-line-client mode-line-modified
                mode-line-remote)
               display (min-width (5.0)))
              mode-line-frame-identification mode-line-buffer-identification "   "
              mode-line-position (vc-mode vc-mode) "  " mode-line-modes
              mode-line-misc-info mode-line-end-spaces))
    (setq mode-line-format nil)))


(defun d-emacs-toggle-variable (var)
            "Toggle the variable VAR on or off."
            (interactive "vToggle variable: ")
            (if (symbol-value var)
                          (set var nil)
              (set var t)))

(defun d-emacs-trim-line-ends ()
  "Remove whitespace at the end of each line in the current buffer."
  (interactive)
  (save-excursion (d-emacs-goto-min)
                  (while (re-search-forward (rx (+ blank) line-end) nil t)
                    (replace-match ""))))

;;;;; Search
(defun d-emacs-do-not-search-invisible ()
            "Set the search to ignore invisible portions of the text."
            (interactive)
            (setq search-invisible nil))

(defun d-emacs-search-invisible ()
      "Set the search to include invisible portions of the text."
      (interactive)
      (setq search-invisible t))

(defun d-emacs-isearch-backward-thing-at-point ()
  "Do incremental search forward for the \"thing\" found near point.
Like ordinary incremental search except that the \"thing\" found at point
is added to the search string initially.  The \"thing\" is defined by
`bounds-of-thing-at-point'.  You can customize the variable
`isearch-forward-thing-at-point' to define a list of symbols to try
to find a \"thing\" at point.  For example, when the list contains
the symbol `region' and the region is active, then text from the
active region is added to the search string."
  (interactive)
  (isearch-forward nil 1)
  (let ((bounds (seq-some (lambda (thing)
                            (bounds-of-thing-at-point thing))
                          isearch-forward-thing-at-point)))
    (cond
     (bounds
      (when (use-region-p)
        (deactivate-mark))
      (when (< (car bounds) (point))
	(goto-char (car bounds)))
      (isearch-yank-string
       (buffer-substring-no-properties (car bounds) (cdr bounds))))
     (t
      (setq isearch-error "No thing at point")
      (isearch-push-state)
      (isearch-update)))))


(defun isearch-backward-symbol (&optional _not-symbol no-recursive-edit)
  "Do incremental search backward for a symbol.
The prefix argument is currently unused.
Like ordinary incremental search except that your input is treated
as a symbol surrounded by symbol boundary constructs \\_< and \\_>.
See the command `isearch-backward' for more information.
This command does not support character folding, and lax space matching
has no effect on it."
  (interactive "P\np")
  (isearch-mode nil nil nil (not no-recursive-edit) 'isearch-symbol-regexp))

(defun isearch-backward-word (&optional not-word no-recursive-edit)
  "Do incremental search backward for a sequence of words.
With a prefix argument, do a regular string search instead.
Like ordinary incremental search except that your input is treated
as a sequence of words without regard to how the words are separated.
See the command `isearch-backward' for more information.
This command does not support character folding, and lax space matching
has no effect on it."
  (interactive "P\np")
  (isearch-mode nil nil nil (not no-recursive-edit) (null not-word)))

(defun d-emacs-isearch-backward-symbol-at-point (&optional arg)
  "Do incremental search backward for a symbol found near point.
Like ordinary incremental search except that the symbol found at point
is added to the search string initially as a regexp surrounded
by symbol boundary constructs \\_< and \\_>.
See the command `d-emacs-isearch-backward-symbol' for more information.
With a prefix argument, search for ARGth symbol backward if ARG is
positive, or search for ARGth symbol backward if ARG is negative."
  (interactive "P")
  (d-emacs-isearch-backward-symbol nil 1)
  (let ((bounds (find-tag-default-bounds))
        (count (and arg (prefix-numeric-value arg))))
    (cond
     (bounds
      (when (< (car bounds) (point))
	(goto-char (car bounds)))
      (isearch-yank-string
       (buffer-substring-no-properties (car bounds) (cdr bounds)))
      (when count
        (isearch-repeat-backward count)))
     (t
      (setq isearch-error "No symbol at point")
      (isearch-push-state)
      (isearch-update)))))

;;;;; Replace
(defun d-emacs-ireplace-listwise (&rest strings) 
  "Interactively replace strings listwise within a region or buffer. Operates on each instance of STRINGS, mapping the nth string to the (n-1)th. For the final string in STRINGS, if it has not appeared previously, it is removed; otherwise, it cycles the replacements. A prefix argument reverses the mappings.

For example, with input strings A, B, C, ...: Without prefix: A -> B, B -> C, ..., Y -> Z, Z -> (removed) With prefix: A <- B, B <- C, ..., Y <- Z, Z <- (removed)

Special case: With A, B, A, instances of A are replaced with B, and vice versa. The user is prompted to input the strings, which are collected into STRINGS.

This function uses a placeholder based on 'πλαχεηολδερ' to handle replacements. This word should not occur in the text being processed. If it does, change the placeholder.

Operates on the active region if present; otherwise, it operates on the entire buffer.

Interactively, STRINGS are collected by prompting with a minibuffer until an empty string is entered. The function then sequentially replaces occurrences of each string with a unique placeholder, followed by a second pass replacing placeholders with the mapped string values, ensuring reliable in-place text replacement."
  (interactive (let (input inputs)
                 (prog2 (while (not (equal (setq input
                                                 (completing-read "Enter strings to cycle (enter empty string to exit): " nil))
                                           ""))
                          (push input inputs))
                     inputs)))
  (let* ((beg (if (use-region-p) (region-beginning) (point-min)))
         (end (if (use-region-p) (region-end) (point-max)))
         ;; Remove brackets if you get arguments as a list.
         (strings (if (listp (car strings)) (car strings) strings))
         ;; Reverse string if you get a prefix arg.
         (strings (if (and (called-interactively-p) (not current-prefix-arg))
                      (reverse strings)
                    strings)))
    (progn (cl-loop for n from 0 to (1- (length strings))
                    for oldstring = (nth n strings)
                    do (while (search-forward oldstring nil t)
                         (replace-match (format "πλαχεηολδερ%s" n) case-replace t))
                    do (goto-char beg))
           (cl-loop for n from 0 to (1- (1- (length strings)))
                    for newstring = (nth (1+ n) strings)
                    do (while (search-forward (format "πλαχεηολδερ%s" n) nil t)
                         (replace-match newstring case-replace t))
                    do (goto-char beg))
           (while (re-search-forward "πλαχεηολδερ." nil t)
             (replace-match "" nil t)))))

(defun d-emacs-exchange (string1 string2)
  "Interactively exchange STRING1 with STRING2 in the current buffer or active region. Calls `d-emacs-ireplace-listwise` to perform the exchange."
  (interactive "sEnter first string to exchange: \nsEnter second string to exchange: ")
  (d-emacs-ireplace-listwise string1 string2 string1))

(defun d-emacs-replace-string-backward (from-string to-string &optional delimited start end region-noncontiguous-p)
  "Perform backward string replacement from FROM-STRING to TO-STRING. DELIMITED, START, END, and REGION-NONCONTIGUOUS-P are as in `replace-string`."
  (declare (interactive-only
	    "use `search-forward' and `replace-match' instead.")
	   (interactive-args
	    (start (use-region-beginning))
	    (end (use-region-end))
	    (region-noncontiguous-p (use-region-noncontiguous-p))))
  (interactive
   (let ((common
	  (query-replace-read-args
	   (concat "Replace"
		   " backward"
		   " string"
		   (if (use-region-p) " in region" ""))
	   nil)))
     (list (nth 0 common) (nth 1 common) (nth 2 common)
	   (use-region-beginning) (use-region-end)
	   (use-region-noncontiguous-p))))
  (perform-replace from-string to-string nil nil delimited nil nil start end t region-noncontiguous-p))

(defun d-emacs-replace-regexp-backward (regexp to-string &optional delimited start end region-noncontiguous-p)
  "Perform backward regexp replacement from REGEXP to TO-STRING. DELIMITED, START, END, and REGION-NONCONTIGUOUS-P are as in `replace-regexp`."
  (declare (interactive-only
	    "use `re-search-forward' and `replace-match' instead.")
	   (interactive-args
	    (start (use-region-beginning))
	    (end (use-region-end))
	    (region-noncontiguous-p (use-region-noncontiguous-p))))
  (interactive
   (let ((common
	  (query-replace-read-args
	   (concat "Replace"
		   " backward"
		   " regexp"
		   (if (use-region-p) " in region" ""))
	   t)))
     (list (nth 0 common) (nth 1 common) (nth 2 common)
	   (use-region-beginning) (use-region-end)
	   (use-region-noncontiguous-p))))
  (perform-replace regexp to-string nil t delimited nil nil start end t region-noncontiguous-p))

(defun d-emacs-query-replace-string-backward (from-string to-string &optional delimited start end region-noncontiguous-p)
  "Perform backward interactive query replace from FROM-STRING to TO-STRING. DELIMITED, START, END, and REGION-NONCONTIGUOUS-P are as in `query-replace-string`."
  (declare (interactive-only
	    "use `search-forward' and `replace-match' instead.")
	   (interactive-args
	    (start (use-region-beginning))
	    (end (use-region-end))
	    (region-noncontiguous-p (use-region-noncontiguous-p))))

  (interactive
   (let ((common
	  (query-replace-read-args
	   (concat "Replace"
		   " backward"
		   " string"
		   (if (use-region-p) " in region" ""))
	   nil)))
     (list (nth 0 common) (nth 1 common) (nth 2 common)
	   (use-region-beginning) (use-region-end)
	   (use-region-noncontiguous-p))))
  (perform-replace from-string to-string t nil delimited nil nil start end t region-noncontiguous-p))

(defun d-emacs-query-replace-regexp-backward (regexp to-string &optional delimited start end region-noncontiguous-p)
  "Perform backward interactive query replace from REGEXP to TO-STRING. DELIMITED, START, END, and REGION-NONCONTIGUOUS-P are as in `query-replace-regexp`."
  (declare (interactive-only
	    "use `re-search-forward' and `replace-match' instead.")
	   (interactive-args
	    (start (use-region-beginning))
	    (end (use-region-end))
	    (region-noncontiguous-p (use-region-noncontiguous-p))))
  (interactive
   (let ((common
	  (query-replace-read-args
	   (concat "Replace"
		   " backward"
		   " regexp"
		   (if (use-region-p) " in region" ""))
	   t)))
     (list (nth 0 common) (nth 1 common) (nth 2 common)
	   (use-region-beginning) (use-region-end)
	   (use-region-noncontiguous-p))))
  (perform-replace regexp to-string t t delimited nil nil start end t region-noncontiguous-p))

(defun d-emacs-replace-string-throughout-buffer (from-string to-string)
  "Replace all instances of FROM-STRING with TO-STRING throughout the entire buffer."
  (interactive
   (let ((common
	  (query-replace-read-args (concat "Replace string in buffer: ")
                                   nil)))
     (list (nth 0 common) (nth 1 common))))
  (save-excursion (d-emacs-goto-min)
                  (while (search-forward from-string nil t)
                    (replace-match to-string))))

(defun d-emacs-replace-regexp-throughout-buffer (from-regexp to-regexp)
                                  "Replace all instances of FROM-REGEXP with TO-REGEXP throughout the entire buffer."
                                  (interactive
   (let ((common
	  (query-replace-read-args
	   (concat "Replace regexp in buffer."
		   (if (use-region-p) " in region" "")))))
     (list (nth 0 common) (nth 1 common))))
                                  (let ((pos (point))
        (perform-replace from-regexp to-regexp nil regexp nil nil nil))))


(defun d-emacs-replace-string-throughout-directory (from-string to-string dir &optional filetest dirtest allfiles)
  "Replace FROM-STRING with TO-STRING in files in DIR.

FILETEST can be either a regexp or a function. If it is a regexp, replacement is
only done in a file if the regexp is matched in the filename. If it is a
function, that function is called with the filename as an argument and
replacement is only done if the output is non-nil.

DIRTEST is a test for whether a directory should be recursed into. It can be
either a regexp or a function and works similar to FILETEST.

If ALLFILES is t, all files and directories are considered, otherwise hidden
files and directories (starting with a period) are left out."
  (interactive "sString: \nsReplacement: \nDDirectory: ")
  (let* ((buf (current-buffer))
         (filetest (if filetest
                       (if (stringp filetest)
                           (lambda (fname) (string-match-p filetest fname))
                         filetest)
                     (lambda (file) t)))
         (dirtest (if (stringp dirtest)
                      (lambda (fname) (re-search-forward dirtest fname))
                    dirtest)))
    (save-excursion
      (d-recurse-through-directory
       dir
       `(((lambda (fname) (set-buffer (find-file-noselect fname))
            (d-emacs-replace-string-throughout-buffer ,from-string ,to-string))
          . (lambda (idx flist)
              (let ((file (nth idx flist)))
                (funcall ,filetest file)))))
       dirtest
       nil
       allfiles)
      (set-buffer buf))))

;;;;; Open Emacs
(defun d-emacs-open-file-in-new-emacs (&optional filename)
  "Open FILENAME in a new Emacs instance with `--debug-init` and `--no-desktop`. Defaults to the current buffer's file if FILENAME isn't specified."
  (interactive)
  (let ((filename (if filename filename (buffer-file-name))))
    (shell-command (format "emacs --no-desktop --debug-init %s &" filename))))


(defun d-emacs-open-file-in-new-emacs-no-init (&optional filename)
  "Open FILENAME in a new Emacs instance without loading the init file. Defaults to the current buffer's file if FILENAME isn't specified."
  (interactive)
  (let ((filename (if filename filename (buffer-file-name))))
    (shell-command (format "emacs -q %s &" filename))))


;;;; Keys
(defun d-emacs-minor-mode-key-binding (key)
  "Retrieve the list of minor mode keybindings for KEY, ignoring the override map."
  (interactive "sKey: ")
  (let ((active-maps nil))
    (mapc (lambda (x)
	    (when (and (symbolp (car x)) (symbol-value (car x)))
	      (add-to-list 'active-maps  (lookup-key (cdr x) (kbd key)))))
	  minor-mode-map-alist )
    (make-composed-keymap active-maps)))

;;;; Macros to create further `d-emacs-mode' commands
(cl-loop for str in '("top" "bottom")
         do (eval `(defun ,(intern (concat "d-emacs-move-to-" str)) ()
                     "Move to the top of the current window."
                     (interactive)
                     (let ((this-scroll-margin (min (max 0 scroll-margin) (truncate (/ (window-body-height) 4.0)))))
                       (move-to-window-line ,(if (string= str "top")
                                                                         'this-scroll-margin
                                                           '(- -1 this-scroll-margin)))))))
;; Make add and remove variants of advice-read-in-file commands.
(cl-loop for str in '("add" "remove")
         do (eval `(defun ,(intern (concat "d-emacs--with-eval-" str "-advicelists-in-file")) (&optional adfile)
                     ,(format "This command %s all advices in the d-emacs-advices-file
                     whose name is ADFILE. It uses the same mechanism to read in
                     ADFILE as #'d-emacs--with-eval-backup-and-apply-bindlists-in-file,
                     so in particular, it treats the adfile of the current buffer as a
                     default." (concat str "s"))
                     (interactive (list (d--pick-pkg-file-by-type "advicelists")))

                     (let ((adfile (or adfile (buffer-file-name))))
                       (d--act-on-advicelists-in-file
                        adfile
                        #',(intern (concat "d-emacs--with-eval-" str "-marked-advicelist")))))))
(require 'd-emacs-constants)

;; Create backward-commands from d-emacs-backward-commands-list
(mapcar (lambda (cmd)
          (eval `(defun ,(intern (concat "d-emacs-backward-"
                                         (replace-regexp-in-string
                                          "d-emacs-" "" (symbol-name cmd))))
                     (&optional arg)
                   "A version of ,cmd with a negative argument so it acts backwards.
Generated automatically using `d-emacs-backward-command-list'."
                   (interactive "p")
                   (,cmd (- arg)))))
        d-emacs-backward-command-list)

(mapcar (lambda (cmd)
          (eval `(defun ,(intern (concat "d-emacs-in-new-buffer-"
                                         (replace-regexp-in-string
                                          "d-emacs-" "" (symbol-name cmd))))
                     (&optional arg)
                   "A version of ,cmd with an argument so it uses a new buffer.
Generated automatically using `d-emacs-new-buffer-command-list'."
                   (interactive "p")
                   (,cmd (- arg)))))
        d-emacs-new-buffer-command-list)

;; Create subsentence-analogs of sentence-commands in d-emacs-subsentence-command-list.
(mapcar (lambda (cmd)
          (eval `(defun ,(intern (concat "d-emacs-"
                                         (replace-regexp-in-string
                                          "d-emacs-" ""
                                          (replace-regexp-in-string
                                           "sentence" "subsentence" (symbol-name cmd)))))
                                                                 (&optional arg)
                                       "Execute ,(symbol-name cmd) with subsentence-signs as delimiters."
                                         (interactive "p")
                                         (let ((sentence-end-base "[.?!…‽;:,][]\"'”’)}»›]*"))
                     (,cmd)))))
        d-emacs-subsentence-command-list)

;;;; Provide
(provide 'd-emacs-commands)
;;; d-emacs-commands ends here
