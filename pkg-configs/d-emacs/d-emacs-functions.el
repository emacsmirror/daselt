;;; d-emacs-functions.el --- Functions for Daselt's Emacs module  -*- lexical-binding: t; -*-

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

;;

;;; Code:
;;;; Preamble
(require 'cl-macs)

(declare-function d-emacs-read-region "d-functions" (&optional properties))
(declare-function checkdoc-recursive-edit "checkdoc" (msg))
(declare-function checkdoc-error-unfixable "checkdoc" (cl-x))
(declare-function checkdoc-error-text "checkdoc" (cl-x))
(declare-function checkdoc-error-end "checkdoc" (cl-x))
(declare-function checkdoc-error-start "checkdoc" (cl-x))
(declare-function d-emacs-coords-binding "d-emacs-xkb-functions" (coords &optional extlayout))
(declare-function d-emacs-minor-mode-key-binding "d-emacs-commands" (key))
(declare-function d-emacs-reverse-alist-get "d-functions" (key alist &optional default))
(declare-function d-recurse-through-directory "d-functions" (dir funtests &optional dirtest lstcolfun allfiles sortfun contt))
(declare-function d--act-on-pkg-files-by-type-and-maybe-kill "d-functions" (funtypes &optional subdir customt))
(declare-function d-emacs-exists-p "d-functions" (list predicate))
(declare-function d--extract-binding-string "d-functions" (binding &optional translate csectoshft doublebind))
(declare-function d--recursively-act-on-bindings "d-functions" (blist fun &optional nooutput))
(declare-function d-emacs-containing-directory-base-name "d-functions" (filepath))

(defvar d-emacs-docstring-functions-list)
(defvar checkdoc--help-buffer)
(defvar d-emacs-special-read-answer-bindlist)
(defvar d-emacs-directory)
(defvar d-emacs-coords-bad-combinations-list)
(defvar d-emacs-replace-binding-strings-alist)
(defvar d-emacs-pkgs-list)

;;;; Functions to initialize d-emacs-mode
;;;;; Generate pkg-customization-options
(defun d-emacs--create-pkg-customization-options ()
  "Create Boolean customization options from `d-emacs-pkgs-list`."
  (mapcar (lambda (pkg)
            (eval `(defcustom ,(intern (concat "d-emacs-" (symbol-name pkg)))
                     ,(if (package-installed-p pkg) t nil)
                     ,(format "This customization option allows users to use Daselt's %s configuration. If it is nil, Daselt prefers other commands while, if it is t, Daselt requires %s and prefers commands from %s." pkg pkg pkg))))
          d-emacs-pkgs-list))

;;;;; Rebind keys
(defun d-emacs--with-eval-backup-and-apply-bindlist (blist)
  "Rebind keys in a given keymap after evaluating an
associated condition. The rebinding is specified by the bindlist BLIST, which
has structurally two forms:

1. A single keymap with EVAL: ([MAP] BIND1 BIND2 ...)

2. Multiple keymaps with respective bindings: (EVAL (MAP1 BIND11 BIND12 ...)
  (MAP2 BIND21 BIND22 ...))

In both forms: - EVAL is an expression to be evaluated within
`with-eval-after-load'. If the EVAL entry is ommitted, it defaults to the
feature whose name is the same as directory name containing the current buffer's
file. - MAP is a symbol referring to the keymap to modify. If the MAP entry is
omitted, it will default to the mode map corresponding to the containing
directory name.

For each MAP, the current keymap is backed up as `d-emacs-MAP-backup' before
rebindings are applied. If `d-emacs-MAP-backup' is already bound to a keymap, no
backup is made, indicating that a prior backup exists.

The keymap's symbol (MAP) can only be evaluated within `with-eval-after-load',
as bindings should apply after the relevant features are loaded."
  (let* ((pkgname (d-emacs-containing-directory-base-name (buffer-file-name)))
         (pkgsymb (intern pkgname))
         (mapsymbdefault (intern (concat pkgname "-mode-map"))))

    (d--recursively-act-on-bindings
     blist
     (lambda (bind &optional heads)
         (let* ((headpairt (= (length heads) 2))
              (evalcnd (if headpairt
                               (car heads)
                           pkgsymb))
              (mapsymb (if headpairt
                               (car (last heads))
                           (if heads
                                 (car heads)
                             mapsymbdefault)))
              (backupsymb (intern (concat "d-emacs-" (symbol-name mapsymb) "-backup"))))

         (with-eval-after-load evalcnd
           (let ((map (symbol-value mapsymb))) ; Mapsymb has to be evaluated only within the with-eval-after-load expression.

             (progn (unless (and (boundp backupsymb) ; Don't overwrite an already existing backup.
                                 (keymapp (symbol-value backupsymb)))
                      (set backupsymb map))

                    (d-emacs--apply-binding bind map))))))
     t)))

(defun d-emacs--apply-binding (binding map)
  "Apply the key BINDING in MAP.
The binding value is evaluated and assigned to the corresponding keys."
  (let* ((orig-binding-strings (d--extract-binding-string binding t t t))
         (binding-strings (mapcar (lambda (bstr)
                                      (alist-get bstr
                                               d-emacs-replace-binding-strings-alist
                                               bstr))
                                  orig-binding-strings))
         (value (cdr binding)))
    (mapcar (lambda (bstr)
                (define-key map (kbd bstr) (eval value))
                (if (d-emacs-exists-p d-emacs-coords-bad-combinations-list
                              (lambda (combination)
                                  (string= (d--extract-binding-string (cons combination nil))
                                         bstr)))
                      (define-key map
                              (kbd (string-replace
                                    "H-" "s-M-"
                                    (string-replace "C-" "A-" bstr)))
                              (eval value))))
            binding-strings)))

;;;;; Add and remove advices
;; Make add and remove advice commands.
(cl-loop for str in '("add" "remove")
         do (let ((addp (string= str "add")))
              (eval `(defun ,(intern (concat "d-emacs--with-eval-" str "-marked-advicelist")) ()
                       "For all all advice combinations in an advicelist,
,str the advice to the corresponding function after EVALCOND is fulfilled.
EVALCOND is given either as the head of the list if its car is a head
(meaninig not a cons) or by the name of the containing directory."
                       (let* ((adlist (d--extract-advicelist))
                              (head (unless (consp (car adlist)) (car adlist)))
                              (pkgname (d-emacs-containing-directory-base-name (buffer-file-name)))
                              (pkgsymb (intern pkgname))
                              (evalcond (if head head pkgsymb))
                              (radlist (if head (cdr adlist) adlist)))
                         (with-eval-after-load evalcond
                           (mapcar (lambda (adv)
                                       (let ((symbs (car adv))
                                           (how (cadr adv))
                                           (funs (caddr adv))
                                           (props (cadddr adv)))
                                       (mapcar (lambda (symb)
                                                   (mapcar (lambda (fun)
                                                             (apply
                                                            (remq nil
                                                                  (list #',(intern (concat "advice-" str))
                                                                        symb ,(if addp `how) fun ,(if addp `props)))))
                                                         funs))
                                               symbs)))
                                   radlist)))))))

;;;;; Restore backups
(defun d-emacs--reset-backed-up-variables ()
  "Restore each variable named `d-emacs-STRING-backup' to its original value.
Restores the variable whose name is STRING using the value stored in
`d-emacs-STRING-backup'. Unbinds the backup-symbols."
  (mapcar (lambda (symb)
            (let* ((symbname (symbol-name symb))
                   (pfxend (progn (string-match "d-emacs-" symbname)
                                  (match-end 0)))
                   (sfxbgn (progn (string-match "-backup" symbname)
                                  (match-beginning 0)))
                   (origsymbname (substring symbname pfxend sfxbgn))
                   (origsymb (intern origsymbname)))
              (if (boundp symb)
                  (progn (set origsymb (symbol-value symb))
                         (makunbound symb)))))
          (apropos-internal "d-emacs-.*-backup")))

;;;; Functions for key commands
(defun d-emacs--funcall-first-mode-match (alist &optional default)
  "Call the cdr of the first cons in ALIST whose car is either an active mode.
This means either a minor mode set to t or the current major mode.
If no match is found, call DEFAULT."
  (let ((cmd (cl-loop for mcons in alist
                      for mmode = (car mcons)
                      for mcmd = (cdr mcons)
                      do (if (or (and (boundp mmode) (symbol-value mmode))
                                 (derived-mode-p mmode))
                             (cl-return mcmd))
                      finally return default)))
    (funcall-interactively cmd)))

;;;; Advice functions
;;;;; Read functions
(defun d-emacs--translate-read-multiple-choice (fun prompt choices &optional help-string show-help long-form)
  "Replace short-choices in read-multiple-choice-input.
Replacemets are specified in `d-emacs-special-read-answer-bindlist'. To be
wrapped around `read-multiple-choice'."
  (let* ((keylst (mapcar (lambda (bind)
                             (cons (cdr bind)
                                 (string-to-char
                                  (d--extract-binding-string bind))))
                         d-emacs-special-read-answer-bindlist))

         (newchoices (mapcar (lambda (choice) ; We translate forward…
                                 (let* ((choicekey (car choice))

                                      (newkey (alist-get choicekey keylst choicekey))
                                      (newchoice (cons newkey
                                                       (cdr choice))))
                                 newchoice))
                             choices))

         (choicereturn (funcall fun prompt newchoices help-string show-help long-form))

         (carret (car choicereturn)) ; And back.
         (transcarret (char-to-string
                       (d-emacs-reverse-alist-get
                        carret keylst carret)))
         (transret (cons transcarret (cdr choicereturn))))
    transret))

(defun d-emacs--translate-read-answer (fun question answers)
  "Replace short answers in `read-answer'-input with Daselt values.
These are specified in `d-emacs-special-read-answer-bindlist'. Wraps around
`read-answer'."
  (let ((newans (mapcar (lambda (answer)
                          (let* ((keylst (mapcar (lambda (bind)
                                                   (cons (cdr bind)
                                                         (string-to-char
                                                          (d--extract-binding-string bind))))
                                                 d-emacs-special-read-answer-bindlist))
                                 (repl (alist-get (cadr answer) keylst))
                                 (ansargs (list (car answer)
                                                (if repl repl (cadr answer))
                                                (caddr answer))))
                            ansargs))
                        answers)))
    (funcall fun question newans)))

(defun d-emacs--translate-read-char-choice (fun prompt chars &optional inhibit-keyboard-quit)
  "Replace characters in `read-char-choice' with Daselt values.
These are specified in `d-emacs-special-read-answer-bindlist'. Wraps around
`read-char-choice'."
  (let* ((keylst (mapcar (lambda (bind)
                             (cons (cdr bind)
                                 (string-to-char
                                  (d--extract-binding-string bind))))
                         d-emacs-special-read-answer-bindlist))
         (trans (mapcar (lambda (char)
                            (let ((ret (alist-get char keylst char nil #'=)))
                            ret))
                        chars))
         (answer (funcall fun prompt trans inhibit-keyboard-quit))
         (retrans (d-emacs-reverse-alist-get answer keylst nil #'=)))
    retrans))

;;;; Rebinder functions, taken from Abdulla Bubshait's rebinder package: https://github.com/darkstego/rebinder.el
(defun d-emacs-dynamic-binding (key &optional toggle)
  "Act as KEY definition in the current context.
This uses an extended menu item's capability of dynamically computing a
definition. This idea came from general.el. TOGGLE changes keymaps associated
from Ctrl to regular key and vice versa"
  `(menu-item
    ,""
    nil
    :filter
    (lambda (&optional _)
      ,`(d-emacs-key-binding ,key ,toggle))))

;; might need to do keymap inheretence to perserve priority
(defun d-emacs-key-binding (key &optional toggle)
  "Get the keymap of associated KEY.
If TOGGLE is non-nil then the Ctrl status of all bindings in the returned keymap
will be changed."
  (let ((map (make-composed-keymap (list (d-emacs-minor-mode-key-binding key) (local-key-binding (kbd key)) (global-key-binding (kbd key))))))
    (if toggle
	(mapcar 'd-emacs-toggle-ctrl map)
      map)))

(defun d-emacs-toggle-ctrl (item)
  "Return ITEM key with all Ctrl status of binding toggled."
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
  "This function takes a list of conses whose car is a prefix key combination
and whose cdr is the key combination that should act as the prefix key from now
on."
  (mapcar (lambda (i) (define-key global-map (kbd (car i)) (d-emacs-dynamic-binding (nth 1 i))))
          keylist))

;;;; Drawing
(defun d-emacs-keymap-to-cons-list (keymap)
  "Convert a KEYMAP of key bindings into a list of conses.
Each cons has a string (for `kbd`) as its car and the binding value as its cdr."
  (let (result)
    (map-keymap
     (lambda (key binding)
       (when binding
         (let ((key-string (if (vectorp key)
                               (key-description key)
                             (format "%s" key))))
           (push (cons key-string binding) result))))
     keymap)
    (reverse result)))

;;;; Replace hardcoded keybinds
(defun d-emacs-checkdoc-interactive-loop (start-here showstatus findfunc)
  "`checkdoc-interactive-loop`
with Daselt shortcuts. See the main command for further documentation."
  ;; Determine where to start the test
  (let* ((begin (prog1 (point)
		  (if (not start-here) (goto-char (point-min)))))
	 ;; Assign a flag to spellcheck flag
	 (checkdoc-spellcheck-documentation-flag
	  (car (memq checkdoc-spellcheck-documentation-flag
                     '(buffer interactive t))))
	 ;; Fetch the error list
	 (err-list (list (funcall findfunc nil)))
	 (cdo nil)
	 (returnme nil)
	 c
         (nextchar (string-to-char (d-emacs-coords-binding '(1 0 3))))
         (prevchar (string-to-char (d-emacs-coords-binding '(1 0 -3))))
         (quitchar (string-to-char (d-emacs-coords-binding '(1 1 -2))))
         (fixchar (string-to-char (d-emacs-coords-binding '(1 0 2))))
         (inschar (string-to-char (d-emacs-coords-binding '(1 0 -2)))))
    (save-window-excursion
      (if (not (car err-list)) (setq err-list nil))
      ;; Include whatever function point is in for good measure.
      (beginning-of-defun)
      (while err-list
	(goto-char (cdr (car err-list)))
	;; The cursor should be just in front of the offending doc string
	(setq cdo (if (stringp (car (car err-list)))
	              (save-excursion (make-overlay
				       (point) (progn (forward-sexp 1)
						      (point))))
                    (make-overlay
		     (checkdoc-error-start (car (car err-list)))
		     (checkdoc-error-end (car (car err-list))))))
	(unwind-protect
	    (progn
	      (overlay-put cdo 'face 'highlight)
	      ;; Make sure the whole doc string is visible if possible.
	      (sit-for 0)
	      (if (and (= (following-char) ?\")
		       (not (pos-visible-in-window-p
			     (save-excursion (forward-sexp 1) (point))
			     (selected-window))))
		  (let ((l (count-lines (point)
					(save-excursion
					  (forward-sexp 1) (point)))))
		    (if (> l (window-height))
			(recenter 1)
		      (recenter (/ (- (window-height) l) 2))))
		(recenter))
	      (message "%s (C-h,%s%s)" (checkdoc-error-text
                                        (car (car err-list)))
		       (if (checkdoc-error-unfixable (car (car err-list)))
			   "" (concat (char-to-string fixchar) ","))
                       (mapconcat #'char-to-string
                                  (list nextchar prevchar quitchar inschar)
                                  ","))
	      (save-excursion
		(goto-char (checkdoc-error-start (car (car err-list))))
		(if (not (pos-visible-in-window-p))
		    (recenter (- (window-height) 2)))
		(setq c (read-event)))
	      (if (not (integerp c)) (setq c ??))
	      (cond
	       ;; Exit condition
	       ((eq c ?\C-g) (signal 'quit nil))
	       ;; Request an auto-fix
	       ((eq c fixchar)
		(delete-overlay cdo)
		(setq cdo nil)
		(goto-char (cdr (car err-list)))
		;; `automatic-then-never' tells the autofix function
		;; to only allow one fix to be automatic.  The autofix
		;; function will then set the flag to `never', allowing
		;; the checker to return a different error.
		(let ((checkdoc-autofix-flag 'automatic-then-never)
		      (fixed nil))
		  (funcall findfunc t)
		  (setq fixed (not (eq checkdoc-autofix-flag
				       'automatic-then-never)))
		  (if (not fixed)
		      (progn
			(message "A Fix was not available.")
			(sit-for 2))
		    (setq err-list (cdr err-list))))
		(beginning-of-defun)
		(let ((ne (funcall findfunc nil)))
		  (if ne
		      (setq err-list (cons ne err-list))
		    (cond ((not err-list)
			   (message "No More Stylistic Errors.")
			   (sit-for 2))
			  (t
			   (message
			    "No Additional style errors.  Continuing...")
			   (sit-for 2))))))
	       ;; Move to the next error (if available)
	       ((eq c nextchar)
		(let ((ne (funcall findfunc nil)))
		  (if (not ne)
		      (if showstatus
			  (setq returnme err-list
				err-list nil)
			(if (not err-list)
			    (message "No More Stylistic Errors.")
			  (message "No Additional style errors.  Continuing..."))
			(sit-for 2))
		    (setq err-list (cons ne err-list)))))
	       ;; Go backwards in the list of errors
	       ((eq c prevchar)
		(if (/= (length err-list) 1)
		    (progn
		      (setq err-list (cdr err-list))
		      (goto-char (cdr (car err-list)))
		      (beginning-of-defun))
		  (message "No Previous Errors.")
		  (sit-for 2)))
	       ;; Edit the buffer recursively.
	       ((eq c inschar)
		(checkdoc-recursive-edit
		 (checkdoc-error-text (car (car err-list))))
		(delete-overlay cdo)
		(setq err-list (cdr err-list)) ;back up the error found.
		(beginning-of-defun)
		(let ((ne (funcall findfunc nil)))
		  (if (not ne)
		      (if showstatus
			  (setq returnme err-list
				err-list nil)
			(message "No More Stylistic Errors.")
			(sit-for 2))
		    (setq err-list (cons ne err-list)))))
	       ;; Quit checkdoc
	       ((eq c quitchar)
		(setq returnme err-list
		      err-list nil
		      begin (point)))
	       ;; Goofy stuff
	       (t
                (if (get-buffer-window checkdoc--help-buffer)
		    (progn
                      (delete-window (get-buffer-window checkdoc--help-buffer))
                      (kill-buffer checkdoc--help-buffer))
                  (with-output-to-temp-buffer checkdoc--help-buffer
                    (with-current-buffer standard-output
                      (insert
                       "Checkdoc Keyboard Summary:\n"
                       (if (checkdoc-error-unfixable (car (car err-list)))
                           ""
                         (concat
                          (format "%s    - auto Fix this warning without asking" fixchar)
                          " (if available.)\n"
                          "         Very complex operations will still query.\n"))
                       (format "%s      - Enter recursive Edit.  Press C-M-c to exit.\n" inschar)
                       (format "%s - skip to the Next error.\n" nextchar)
                       (format "%s - skip to the Previous error.\n" prevchar)
                       (format "%s      - Quit checkdoc.\n" quitchar)
                       "C-h    - Toggle this help buffer.")))
		  (shrink-window-if-larger-than-buffer
                   (get-buffer-window checkdoc--help-buffer))))))
	  (if cdo (delete-overlay cdo)))))
    (goto-char begin)
    (if (get-buffer checkdoc--help-buffer) (kill-buffer checkdoc--help-buffer))
    (message "Checkdoc: Done.")
    returnme))

;;;; Functions for bound commands
(defun d-emacs-beginning-of-docstring ()
  "If point is within a definition, move to the beginning of the docstring.
Works for `defun', `defmacro', `defconst', `defcustom', `defalias' and `defun*'."
  (beginning-of-defun)
  (forward-char)
  (let* ((first-symbol (progn (mark-sexp)
                              (prog1 (d-emacs-read-region)
                                (deactivate-mark))))
         (commentfourthgroup d-emacs-docstring-functions-list)
         (types (append commentfourthgroup)))
    (when (member first-symbol types)
      (let ((place (if (member first-symbol commentfourthgroup)
                           3)))
        (forward-sexp place)
        (search-forward "\"")
        (backward-char)))))

;;;; Symbol renaming
(defun d-emacs-definition-name ()
  "If point is within a definition, move to the beginning of the docstring.
Works for definition types in `d-emacs-docstring-functions-list'."
  (mark-defun)
  (let* ((defn (d-emacs-read-region))
         (first-symbol (nth 0 defn))
         (name (nth 1 defn)))
    (if (member first-symbol d-emacs-docstring-functions-list)
            name)))

(defun d-emacs-definition-names-in-file (fname)
  "Return the names of definitions in FNAME, listed according to definition type.
Works for definition types in `d-emacs-docstring-functions-list'."
  (let ((buf (current-buffer)))
    (set-buffer (find-file-noselect fname))
    (prog1 (remq nil (mapcar (lambda (deftype)
                                                                                 (save-excursion
                                                                                   (goto-char (point-min))
                                                                                   (let (rlist)
                                   (while (search-forward (symbol-name deftype) nil t)
                                     (save-excursion
                                                                                         (beginning-of-defun)
                                                                                         (push (d-emacs-definition-name) rlist)))
                                   rlist)))
                             d-emacs-docstring-functions-list))
      (set-buffer buf))))

;;;; Replacements
(defun d-emacs-replace-list-throughout-directory (lst dir fun &optional filetest dirtest allfiles)
  "Replace occurrences of strings or symbols in files throughout a specified directory.

This function takes a list `lst' of strings or symbols, and for each element,
it replaces occurrences of the corresponding string in the files found in the
specified directory `dir'. The replacement is performed using the provided
function `fun' to determine the replacement string for non-string elements. 
The function can also utilize optional predicates for filtering files and directories.

Parameters:
- LST: A list of strings or symbols. If an element is a cons cell (pair), the
car is treated as a string to be replaced, and the cdr will be used as the
replacement directly. For symbols, they are first converted to strings.
- DIR: The directory in which to perform the replacements.
- FUN: A function that takes an element from `lst' and returns the replacement
string if the element is not a cons cell.
- FILETEST (optional): A predicate function to determine if a file should be
processed. Should return non-nil for files that match the criteria.
- DIRTEST (optional): A predicate function to determine if a directory should be
processed. Should return non-nil for directories that match the criteria.
- ALLFILES (optional): If non-nil, indicates that all files should be included
in the search, regardless of the `filetest' predicate.

Returns:
A list of the replacement strings corresponding to each element in `lst'.

Example Usage:
(d-emacs-replace-list-throughout-directory
 '(old_string1 old_string2 ((old_string3 . new_string3)))
 /path/to/directory
 'some-replacement-function
 'file-filter-predicate
 'dir-filter-predicate
 t) ; where t indicates to include all files."
  (cl-flet ((obj-or-name (obj) (if (symbolp obj)
                                   (symbol-name obj)
                                 obj)))
    (mapcar (lambda (obj)
              (let* ((ocnsp (consp obj))
                     (str (if ocnsp (obj-or-name (car obj))
                            (obj-or-name obj)))
                     (repl (if ocnsp
                               (cdr obj)
                             (funcall fun obj))))
                (d-emacs-replace-string-throughout-directory str repl dir filetest dirtest allfiles)
                repl))
            lst)))

;;;; Provide
(provide 'd-emacs-functions)
;;; d-emacs-functions.el ends here
