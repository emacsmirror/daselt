;;; d-commands.el --- General commands for Daselt.   -*- lexical-binding: t; -*-

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

;; This file houses the general commands for Daselt.

;;; Code:

;; (eval-when-compile (require 'd-constants))
;; (eval-when-compile (require 'd-functions))
;; (eval-when-compile (require 'd-emacs-xkb-constants))
;; (eval-when-compile (require 'd-emacs-xkb-functions))
;; (eval-when-compile (require 'd-emacs-xkb-customs))

(declare-function d--pick-pkg-file-by-type "../d-functions.el"
                  (type &optional subdir nodefault))
(declare-function d--act-on-bindlists-in-file "../d-functions.el"
                  (filepath function &optional untangle))
(declare-function d--act-on-bindlists-in-file "../d-functions.el"
                  (filepath function &optional untangle))
(declare-function d--sort-and-format-marked-bindlist-string "../d-functions.el"
                  (&optional coordsonly prefun modlist))
(declare-function d--delete-duplicate-comment-lines "../d-functions.el"
                  ())
(declare-function d-filter-obarray-by-predicate "d-functions.el"
                  (predicate))
(declare-function d-filter-by-predicate "d-functions.el"
                  (lst predicate))
(declare-function d--pick-pkg-file-by-type "d-functions.el"
                  (type &optional subdir nodefault))
(declare-function d-powerlist "d-functions.el"
                  (list &optional elt))
(declare-function d-complement "d-functions.el"
                  (list1 list2 &optional compfun))
(declare-function d-emacs-coords-placevals-matching-indexed-rx "d-emacs-xkb/d-emacs-xkb-functions.el"
                  (placevals idx coordrx))
(declare-function d-emacs-coords-draw-placevals "d-functions.el"
                  (placevals &optional drawfull runcoords org))
(declare-function d-execute-in-maximized-maybe-temp-buffer "d-functions.el"
                  (buffername function))
(declare-function d--bindlist-symb-p "d-functions.el"
                  (symb))
(declare-function d-flatten-until "d-functions.el"
                  (lst cnd))
(declare-function d-emacs-coords-p "d-emacs-xkb/d-emacs-xkb-functions.el"
                  (list))
(declare-function d-string-together-modifiers "d-functions.el"
                  (modifiers))
(declare-function d-string-exists-and-nonempty "d-functions.el"
                  (str))
(declare-function d-filter-by-predicate "d-functions.el"
                  (lst predicate))
(declare-function d-emacs-coords-extract-value-string "d-emacs-xkb/d-emacs-xkb-functions.el"
                  (val))
(declare-function d-emacs-coords-placevals-matching-coordrx "d-emacs-xkb/d-emacs-xkb-functions.el"
                  (placevals rx))
(declare-function d-emacs-coords-binding "d-emacs-xkb/d-emacs-xkb-functions.el"
                  (coords))
(declare-function d--bindlist-p "d-functions.el"
                  (list))
(declare-function d--format-bindlist-into-string-before-insertion "d-functions.el"
                  (bindlist &optional coordsonly))
(declare-function d--extract-bindlist "d-functions.el"
                  (&optional noconstruct))
(declare-function d--save-bindlist-as-variable "d-functions.el"
                  (bindlist))
(declare-function d-read-region "d-functions.el"
                  ())

;;;; Bindlists-files
(defun d--sort-and-format-bindlists-in-file (&optional blistfile coordsonly prefun modifierlist)
  "Sort and format bindlists in specified FILE.
If BLISTFILE is nil, the file corresponding to the current buffer is used. If
PREFUN is provided, it will be applied to each bindlist after elaboration but
before sorting. MODIFIERLIST is a list of modifiers for formatting and ordering
prefix strings. Its default is `d-modifiers-list'. If COORDSONLY is non-nil,
replace matching binding strings with prefix-coords pairs."
  (interactive (list (d--pick-pkg-file-by-type "bindlists")
                     (yes-or-no-p "Replace suffixes by coordinates? ")))

  (let ((blistfile (or blistfile (buffer-file-name))))
    
    (d--act-on-bindlists-in-file
     blistfile
     (lambda () (d--sort-and-format-marked-bindlist-string coordsonly prefun
                                                      modifierlist)))

    ;; Do some buffer formatting
    (let ((buffer (current-buffer))
          (pos (point)))
      (find-file blistfile)
      (d--delete-duplicate-comment-lines)

      ;; Remove unnecessary empty lines
      (goto-char (point-min))
      (while (re-search-forward (rx "\n" (one-or-more "\n")) nil t)
        (replace-match "\n\n"))

      (set-buffer buffer)
      (goto-char pos))))

(defun d--save-bindlists-in-file-as-variables (&optional blistfile)
                          "Save all bindlists in BLISTFILE as variables.
If BLISTFILE is nil, uses the current buffer's filename."
                          (interactive  (list (d--pick-pkg-file-by-type "bindlists")))
                          (let ((blistfile (or blistfile (buffer-file-name))))
    (d--act-on-bindlists-in-file
     blistfile
     (lambda () (d--save-bindlist-as-variable (d--extract-bindlist))))))

;;;; Help
(defun d-draw-bindlist-layer (blistsymb laycoord &rest mods)
  "Draw a layer of the bindlist identified by BLISTSYMB.
Use a maximized window.
LAYCOORD specifies the layer to draw, and MODS the modifiers of the layer."
  (interactive (append (list (intern (completing-read "Bindlist: " obarray
                                                      (lambda (symb)
                                                        (and (boundp symb)
                                                             (d--bindlist-symb-p symb)))
                                                      t nil
                                                      'variable-name-history
                                                      "d-emacs-d-emacs-mode-map-bindlist"))
                             (completing-read
                              "Layer: "
                              (append '(0)
                                      (d-cardinal (length d-emacs-coords-layer-numbers-list) t))
                              t nil nil nil
                              "1"))
                       (cl-loop for repl = (completing-read "Modifier (empty to exit): "
                                                            (mapcar (lambda (mod)
                                                                      (char-to-string mod))
                                                                    d-modifiers-list))
                                while (not (string-empty-p repl))
                                collect repl)))

  (let ((placevals (d-emacs-coords-placevals-matching-indexed-rx
                    (remq nil ; Filters out bindings without coordinate matches.
                          (mapcar #'d--placeval-from-elaborate-binding
                                  (d--elbinds-matching-modifier-regexps
                                   (symbol-value blistsymb) mods)))
                    0
                    laycoord)))
    
    (funcall
     (if (called-interactively-p 'any)
         #'d-emacs-coords-draw-placevals-in-temp-buffer
       #'d-emacs-coords-draw-placevals)
     placevals
     nil
     nil
     current-prefix-arg)))

(defun d-draw-bindings-from-regexps (blistrx valrx coordrx &rest modrxs)
  "Draw the bindings matching BLISTRX, VALRX, COORDRX and MODRXS.
This is the most powerful of the Daselt-helper functions.

- BLISTRX matches against all bindlist-names. For example, if you want to draw
  all layers of all bindlists for `d-stump', provide `d-stump' for BLISTRX and
  have the other regexps be empty strings.

- VALRX matches against the values of all bindings in matched bindlists. For
  example, if you want to draw all bindings to `projectile'-functions in all
  bindlists, provide `projectile' for VALRX and leave the other regexps empty.

- COORDRX matches against all coordinates. So if you want to draw the first row
  of all layers of all bindlists with any modifiers, provide `. -1 [-]?.' for
  COORDRX and leave the other regexps empty.

- MODRXS are regexps that match against the modifiers of bindings. The syntax is
  adapted to make the matching intuitive: if the regexp starts with `^', a
  binding is matched if and only if the regexp does not match any modifiers in
  it. So for instance, if you want to draw all bindings with a C-modifier and no
  s-modifier, with or without any other modifiers, provide two regexps for
  MODRXS, `C' `^s'."
  (interactive (append (list (read-string "Bindlist regexp (leave empty to match all): ")
                             (read-string "Value regexp (leave empty to match all): ")
                             (read-string "Coordinate regexp (leave empty to match all): "))
                       (cl-loop for repl = (completing-read "Modifier (empty to exit): "
                                                            (mapcar (lambda (mod)
                                                                      (char-to-string mod))
                                                                    d-modifiers-list))
                                while (not (string-empty-p repl))
                                collect repl)))

  (let* ((blistsymbs (d-filter-obarray-by-predicate #'d--bindlist-symb-p))
         (matchedblsymbs (if (string-empty-p blistrx)
                             blistsymbs
                           (d-filter-by-predicate blistsymbs (lambda (blistsymb)
                                                               (string-match-p
                                                                blistrx
                                                                (symbol-name blistsymb)))))))

    (d-execute-in-maximized-maybe-temp-buffer
     "*daselt-layout*"
     (lambda ()
       (cl-loop for blistsymb in matchedblsymbs
                for blist = (symbol-value blistsymb)
                do (insert (concat "\n" (symbol-name blistsymb) "\n"))
                do (let* ((modmatchedbinds
                           (if modrxs
                               (d--elbinds-matching-modifier-regexps
                                blist modrxs)
                             blist)))

                     ;; Isolate the matched bindings for each modifier combination.
                     (cl-loop for mods in (d-powerlist d-modifiers-list)
                              do (let* ((specificmodrxs
                                         (append (mapcar #'char-to-string mods)
                                                 (mapcar (lambda (mod)
                                                           (concat
                                                            "^" (char-to-string mod)))
                                                         (d-complement d-modifiers-list
                                                                       mods))))

                                        (specificmodmatchedbinds (d--elbinds-matching-modifier-regexps modmatchedbinds specificmodrxs))

                                        (modmatchedplacevals
                                         (remq nil (mapcar #'d--placeval-from-elaborate-binding
                                                           specificmodmatchedbinds)))

                                        ;; If C-g is not translated by `d-stump' or `d-emacs-translate-C-1-1--2-C-g' and the modifier is `C', check all placevals if they are bound to "g", and, if so, put the value of that placeval on `C-1-1--2'.
                                        (modmatchedplacevalsw-C-g-remapped
                                         (if (and (equal mods '(C))
                                                  (not (or d-stump
                                                           d-emacs-translate-C-1-1--2-C-g)))
                                             (mapcar
                                              (lambda (placeval)
                                                (let* ((coords (car placeval))
                                                       (val (cdr placeval)))
                                                  (if (string= "g"
                                                               (d-emacs-coords-binding
                                                                coords))
                                                      (cons '(1 1 -2)
                                                            val)
                                                    placeval)))
                                              modmatchedplacevals)
                                           modmatchedplacevals))
                                        
                                        (coordmatchedplacevals
                                         (if (d-string-exists-and-nonempty coordrx)
                                             (d-emacs-coords-placevals-matching-coordrx
                                              modmatchedplacevals coordrx)
                                           modmatchedplacevalsw-C-g-remapped))

                                        (valmatchedplacevals
                                         (if (d-string-exists-and-nonempty valrx)
                                             (d-filter-by-predicate
                                              coordmatchedplacevals
                                              (lambda (placeval)
                                                (string-match-p
                                                 valrx (d-emacs-coords-extract-value-string
                                                        (cdr placeval)))))
                                           coordmatchedplacevals)))

                                   (if valmatchedplacevals
                                       (progn (insert (format "\n%s\n"
                                                              (d-string-together-modifiers
                                                               mods)))
                                              (d-emacs-coords-draw-placevals valmatchedplacevals t))))))
                
                do (insert "\n"))))))

(defun d-draw-free-places-from-regexps (blistrx coordrx &rest modrxs)
  "Draw free bindings that match BLISTRX, COORDRX and MODRXS.
The arguments work as for `d-draw-bindings-from-regexps', see the documentation
there."
  (interactive (append (list (read-string "Bindlist regexp (leave empty to match all): ")
                             (read-string "Coordinate regexp (leave empty to match all): "))
                       (cl-loop for repl = (completing-read "Modifier (empty to exit): "
                                                            (mapcar (lambda (mod)
                                                                      (char-to-string mod))
                                                                    d-modifiers-list))
                                while (not (string-empty-p repl))
                                collect repl)))

  (let* ((blistsymbs (d-filter-obarray-by-predicate #'d--bindlist-symb-p))
         (matchedblsymbs (if (string-empty-p blistrx)
                             blistsymbs
                           (d-filter-by-predicate blistsymbs (lambda (blistsymb)
                                                               (string-match-p
                                                                blistrx
                                                                (symbol-name blistsymb))))))
         
         (allcoords (d-flatten-until d-emacs-xkb-coordinates
                                     (lambda (lst)
                                       (d-emacs-coords-p
                                        (car lst)))))

         (allmodifiercoordscombinations (apply #'append (mapcar (lambda (coords)
                                                                  (mapcar
                                                                   (lambda (mods)
                                                                     (cons (d-string-together-modifiers mods)
                                                                           coords))
                                                                   (d-powerlist d-modifiers-list)))
                                                                allcoords)))
         
         (allbinds (mapcar (lambda (modcoords)
                             (cons modcoords "free"))
                           allmodifiercoordscombinations))

         (allmodmatchedbinds (d--elbinds-matching-modifier-regexps
                              allbinds modrxs))
         
         (usedmodmatchedbinds (cl-loop for blistsymb in matchedblsymbs
                                       for blist = (symbol-value blistsymb)
                                       append
                                       (if modrxs
                                           (d--elbinds-matching-modifier-regexps
                                            blist modrxs)
                                         blist)))
         
         (freemodmatchedbinds (d-complement allmodmatchedbinds usedmodmatchedbinds
                                            (lambda (allbind usedbind)
                                              (let ((allcoords (cdar allbind))
                                                    (usedcoords (cdar usedbind))
                                                    (allindpfxs (caaar allbind))
                                                    (usedindpfxs (caaar usedbind)))
                                                (and (equal allcoords usedcoords)
                                                     (equal allindpfxs usedindpfxs)))))))
    
    (d-execute-in-maximized-maybe-temp-buffer
     "*daselt-layout*"
     ;; Isolate the matched bindings for each modifier combination.
     (lambda ()
       (insert (format "Matched layouts: %s\n" matchedblsymbs))
       (cl-loop for mods in (d-powerlist d-modifiers-list)
                do (let* ((specificmodrxs
                           (append (mapcar #'char-to-string mods)
                                   (mapcar (lambda (mod)
                                             (concat
                                              "^" (char-to-string mod)))
                                           (d-complement
                                            d-modifiers-list mods))))
                          
                          (specificmodmatchedfreebinds (d--elbinds-matching-modifier-regexps freemodmatchedbinds specificmodrxs))

                          (modmatchedfreeplacevals
                           (mapcar #'d--placeval-from-elaborate-binding
                                   specificmodmatchedfreebinds))

                          (coordmatchedfreeplacevals
                           (if (string-empty-p coordrx)
                               modmatchedfreeplacevals
                             (d-emacs-coords-placevals-matching-coordrx
                              modmatchedfreeplacevals coordrx))))

                     (if coordmatchedfreeplacevals
                         (progn (insert (format "\n%s\n"
                                                (d-string-together-modifiers
                                                 mods)))
                                (d-emacs-coords-draw-placevals coordmatchedfreeplacevals t))))
                do (insert "\n"))))))

;;;; Import
(defun d--convert-bindings-to-bindlist (&optional coordsonly)
  "Convert the marked key bindings into a Daselt-bindlist.
If COORDSONLY is t, replace suffixes by coordinates whenever possible.

Four formats are accepted:

- Bindings of the form `(define-key MAP (kbd KEY) VAL)' or `(keymap-set MAP
\(kbd KEY) VAL)'.

- Bindings of the form `(bind-key KEY VAL &optional MAP)'.

- Bindings of the form `(global-set-key (kbd KEY) VAL)' or
`(keymap-global-set (kbd KEY) VAL)'.

- Sections of `use-package' configurations of the form `:bind (CONSES)' or
`:bind (:map MAP CONSES)'."
  (interactive (list (yes-or-no-p  "Convert prefixes to coordinates? ")))
  (let* ((parsefuns (d-filter-obarray-by-predicate
                     (lambda (symb)
                       (and (fboundp symb)
                            (string-match-p (rx string-start
                                                "d--do-parse-")
                                            (symbol-name symb))))))
         (mapsblistpieces (mapcar #'funcall parsefuns))
         (maps (apply #'append (mapcar #'car mapsblistpieces)))
         (blistpieces (apply #'append (mapcar #'cdr mapsblistpieces)))
         (redmaps (cl-remove-duplicates maps)))

    (message "Redmaps %s\nBlistpieces %s" redmaps blistpieces)
    (pop-to-buffer "*daselt-imported-bindlists*")
    (cl-loop for redmap in redmaps
             do (let* ((blist redmap))
                  (cl-loop for idx from 0 to (1- (length blistpieces))
                           do (let* ((map (nth idx maps))
                                     (blistpiece (nth idx blistpieces)))
                                (if (eq map redmap)
                                    (setq blist (cons blist blistpiece)))))

                  (goto-char (point-max))
                  (if (d--bindlist-p blist)
                      (insert (concat
                               (d--format-bindlist-into-string-before-insertion
                                (d--sort-and-format-bindlist blist coordsonly)
                                coordsonly)
                               "\n")))))))

;;;; Navigation
(defun d-find-pkg-file-by-type (type &optional typemodifiers)
  "Pick a file of a Daselt-type TYPE with modifiers TYPEMODIFIERS and visit it.
The only difference to `find-file' is in the interactive completion, which asks
for a filetype in `d-pkg-file-types-list' and some type modifiers in
`d-pkg-type-modifiers-list', then displays all files of that type with those
modifiers."
  (interactive (let* ((type (completing-read "Main type: "
                                             d-pkg-file-types-list))
                      (typemodifiers (cl-loop for repl = (completing-read "Type modifier (empty to exit): "
                                                                          d-pkg-type-modifiers-list)
                                              while (not (string-empty-p repl))
                                              collect repl)))
                 (list type typemodifiers)))
  (let ((filepath (d--pick-pkg-file-by-type (cons type typemodifiers))))
    (find-file filepath)))

(defun d-find-bindlists-file ()
                                                      "Visit a bindlists file.
With a prefix argument, only regular bindlists files are considered."
                                                      (interactive)
                                                      (if current-prefix-arg
                                                                                                              (d-find-pkg-file-by-type "bindlists" '("regular"))
                                                        (d-find-pkg-file-by-type "bindlists" nil)))

;;;; Cleaning
(defun d-recursively-remove-nonstandard-files (&optional dir)
  "Remove files in subdirectories of DIR that are non-standard.
That means they are not directory and don't fulfill
`d--standard-file-p'.
The default for DIR is `d-emacs-pkg-configs-dir'."
  (interactive "DDirectory: ")
  (let ((dir (or dir d-emacs-pkg-configs-dir)))
    (d-recurse-through-directory dir
                                 `(((lambda (fn)
                                      (delete-file fn))
                                    . (lambda (idx lst) (let ((fn (nth idx lst)))
                                                     (string-match-p "newdoc" fn)))))
                                 nil
                                 nil
                                 t)))
;;;; Tutorial
(defun d-generate-tutorial ()
                            "Generate the Daselt-tutorial."
                            (interactive)
                            (let ((tutfile (concat d-emacs-directory "pkg-configs/d-tutorial.el"))
        (display-buffer-alist '((".*" display-buffer-full-frame))))
    (find-file tutfile)
    (goto-char (point-min))
    (search-forward ";;; Code:")
    (search-forward "\(")
    (backward-char)
    (mark-sexp)
    (let ((tuttext (eval (d-read-region))))
      (pop-to-buffer "*daselt-tutorial*")
      (delete-minibuffer-contents)
      (org-mode)
      (setq visual-line-mode t)
      (insert tuttext)
      (goto-char (point-min)))))


;;;; Provide
(provide 'd-commands)
;;; d-commands.el ends here
