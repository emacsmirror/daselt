;;; d-stump-functions.el --- d-stump functions       -*- lexical-binding: t; -*-

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

(defun d-stump--generate-init ()
  "Generate a d-stump initialization file from the files in
  `D-STUMP-DIRECTORY/pkg-configs/d-stump'.

This function copies `d-stump.lisp' to a backup file before creating a new
`d-stump.lisp' file. It then deletes the contents of this file and replaces them
with a newly generated init. "
  (interactive)
  (dired-copy-file (concat d-directory "d-stump/d-stump.lisp")
                   (concat d-directory "d-stump/d-stump.lisp.backup")
                   t)
  (find-file (concat d-directory "d-stump/d-stump.lisp"))
  (delete-region (point-min) (point-max))
  (insert ";;;; d-stump-init.lisp\n\n")
  (insert "(stumpwm:set-prefix-key (stumpwm:kbd \"F11\"))\n")
  (let* ((print-level nil)
         (print-length nil)
         (d-stump-config-dir (concat d-emacs-directory "pkg-configs/d-stump/"))
         (buffer (current-buffer))

         ;; Remove initial and final bracket.
         (bindingstring
          (d-remove-surrounding-brackets
           (format
            "%s" 
            
            (flatten-list
             (d--act-on-pkg-files-by-type-and-maybe-kill
              `(((lambda (filename)
                   (progn
                     (set-buffer (find-file-noselect filename))
                     (goto-char (point-min))
                     (d--act-on-bindlists-in-file
                      filename
                      #'d--generate-define-key-strings-from-marked-bindlist
                      t)))
                 .
                 ("bindlists" "regular")))
              "d-stump/")))))

         ;; Get all lisp-code from init files. Again we have to remove initial and final brackets.
         (otherstring (d-remove-surrounding-brackets
                       (format "%s"
                               (remq nil
                                     (flatten-list
                                      (d--act-on-pkg-files-by-type-and-maybe-kill
                                       `((d-lisp-file-code . (nil "init")))
                                       "d-stump/"))))))

         (remappedstring (format "%s" (list 'define-remapped-keys ))))
    (set-buffer buffer)

    (d-stump--generate-module-code)
    (d-stump--generate-keymaps-code)
    (insert bindingstring)
    (insert otherstring)
    (insert (d-stump--generate-iresize-map-code))
    (if d-stump-binwarp (insert (d-stump--generate-binwarp-mode-code)))
    (insert (d-stump--generate-remap-list-code))))

(defun d--init-file-p (filepath)
  "Check if FILEPATH is an init file.

An init file is defined as a lisp file whose base name contains 'init' and that
does not contain '#' or '~'."
  (and (d--standard-file-p filepath)
       (string-match-p "init" (file-name-base filepath))
       (let ((ext (file-name-extension filepath)))
         (if ext (string-match-p "lisp" (file-name-extension filepath))))))

(defun d-stump--generate-module-code ()
  "Generate code to load each module in `D-STUMP-MODULES' if its corresponding
custom is set to t."
  (mapcar (lambda (module)
            (if (intern (concat "d-stump-" module))
                (insert (format "(load-module \"%s\")\n" module))))
          d-stump-modules))

(defun d-stump--generate-keymaps-code ()
  "For each keymap in D-STUMP-KEYMAPS, generate code to initialize that keymap
in the d-stump-init."
  (mapcar (lambda (map)
            (insert (format "(defparameter %s (make-sparse-keymap))\n" map)))
          d-stump-keymaps))

(defun d-stump--generate-iresize-map-code ()
  "This function generates the code for the iresize map."
  (let* ((blist (car (d--act-on-bindlists-in-file
                      (concat d-emacs-directory "pkg-configs/d-stump/stumpwm/d-stump-iresize-special-bindlists.el")
                      (lambda () (d--extract-bindlist)))))
         (head (d-head-if-exists blist))
         (body (cdr blist)))
    (cl-flet* ((kbd-car (bind) `(kbd ,(d--extract-binding-string bind)))
               (kbd-conss (bblist) (mapcar (lambda (bind)
                                             (cons (kbd-car bind)
                                                   (cdr bind)))
                                           bblist)))
      (format "%S\n\n" (append `(define-interactive-keymap)
                               `((iresize tile-group))
                               `((:on-enter #'setup-iresize
                                            :on-exit #'resize-unhide
                                            :abort-if #'abort-resize-p
                                            :exit-on ,(mapcar (lambda (bind) (kbd-car bind)) head)))
                               (kbd-conss body))))))


(defun d-stump--generate-binwarp-mode-code ()
  "Generate the code for the binwarp mode."
  (let* ((blist (car (d--act-on-bindlists-in-file
                      (concat d-emacs-directory "pkg-configs/d-stump/binwarp/d-stump-binwarp-special-bindlists.el")
                      (lambda () (d--extract-bindlist)))))
         (head (d-head-if-exists blist))
         (headhead (d-head-if-exists head))
         (headbody (cdr head))
         (body (cdr blist)))
    (cl-flet* ((kbd-car (bind) `(kbd ,(d--extract-binding-string bind)))
               (kbd-lists (bblist) (mapcar (lambda (bind)
                                             (list (kbd-car bind)
                                                   (cdr bind)))
                                           bblist)))
      (format "%S\n\n" (append `(binwarp:define-binwarp-mode binwarp-mode ,headhead)
                               `((:map *top-map*
                                       :redefine-bindings t
                                       :exit-keys ,(mapcar (lambda (bind) (kbd-car bind)) headbody)))
                               (kbd-lists body))))))

;;;; Remapped keys functions
(defun d-stump--exceptional-bindings ()
  "Return exceptional bindings from the marked bindlist, including head escape
bindings."
  (let* ((blist (d--extract-bindlist))
         (head (d-head-if-exists blist))
         (headhead (d-head-if-exists head))
         (headbinds (if head
                        (if headhead
                            (cdr head)
                          head))))
    (if head (append headbinds (cdr blist))
      blist)))

(defun d-stump--non-exceptional-bindings (excp-bindings)
  "Given a list of exceptional bindings EXCP-BINDINGS, return the
non-exceptional-bindings in a marked bindlist. These are those that are not in
EXCP-BINDINGS."
  (let* ((blist (d--extract-bindlist))
         (head (d-head-if-exists blist))
         (body (if head (cdr blist) blist))
         (non-excp-binds (remq nil (mapcar
                                    (lambda (bind)
                                      (unless (cl-member
                                               bind excp-bindings
                                               :test (lambda (bind excpbind)
                                                       (string=
                                                        (d--extract-binding-string
                                                         bind)
                                                        (d--extract-binding-string
                                                         excpbind))))
                                        (cons (d--extract-binding-string bind)
                                              (cdr bind))))
                                    body))))
    non-excp-binds))

(defun d-stump--excp-bindings (excps)
  "Return exceptional bindings from a list of exceptions EXCPS."
  (apply #'append ; Append bindings of all exceptions.
         (mapcar
          (lambda (excp)
            (d-flatten-until
             (remq nil
                   (d--act-on-pkg-files-by-type-and-maybe-kill
                    `(((lambda (file)
                         (d--act-on-bindlists-in-file
                          file
                          #'d-stump--exceptional-bindings))
                       . "bindlists"))
                    (concat "d-stump/" (car excp))))
             (lambda (lst)
               (d--binding-p (car lst)))))
          excps)))

(defun d-stump--format-remap-list-code (lst)
  "Format a list of remapped keys LST into a string suitable for Lisp."
  ;; (replace-regexp-in-string
  ;;  (rx (one-or-more (and (zero-or-more space) "\n" (zero-or-more space)))) "\n"
  (string-replace
   "\"\n\"" "\n"
   (string-replace
    "\\\"" "\""
    (string-replace
     "\\\"" "\""
     (string-replace
      ")\"" ")"
      (string-replace
       "\"(" "("
       (string-replace
        "\n\"" "\n"
        (format "%S" lst))))))))


(defun d-stump--format-remap-bindlist-code (binds modes &optional head)
  "Format bindings BINDS for use in a remap list, including optional HEAD list.

MODES are included for conditional mappings."
  (concat (string-replace "\",\" " ","
                          (format
                           "%S"
                           (cons ","
                                 (cons
                                  (cons `lambda
                                        (cons
                                         `(win)
                                         (list
                                          (if head `(d-stump-test-for-window-name-and-modes
                                                     win
                                                     (list ,head)
                                                     ,(if modes (append (list 'list)
                                                                        modes)))
                                            `(d-stump-test-for-modes
                                              ,(if modes (append (list 'list)
                                                                 modes)))))))
                                  binds))))))

(defun d-stump--generate-remap-list-code ()
  "Generate the list of key translations for d-stump's init file.

It processes each bindlist in `D-STUMP-REMAPPED-KEYS-SPECIAL-BINDLISTS` and
combines them with the exceptions defined in `d-stump-remap-exceptions-alist`,
producing remapped keys that meet the specified conditions."
  (let* ((base-file-path
          "pkg-configs/d-stump/stumpwm/d-stump-remapped-keys-special-bindlists.el")
         (user-file-path "pkg-configs/d-stump/stumpwm/d-stump-remapped-keys-user-defined-special-bindlists.el")
         (file-path (if (file-exists-p (concat d-emacs-directory user-file-path))
                        user-file-path
                      base-file-path))
         
         (remapped-keys-list
          (apply
           #'append ; Append listlists for all exceptions.
           (mapcar ; For all exceptions
            (lambda (excps)
              (let ((modes (mapcar (lambda (excp)
                                     (cdr excp))
                                   excps))
                    (excp-bindings (d-stump--excp-bindings excps)))
                
                (d--act-on-bindlists-in-file
                 (concat d-emacs-directory file-path)
                 (lambda () (let* ((blist (d--extract-bindlist))
                              (head (d-head-if-exists blist))
                              (non-excp-binds (d-stump--non-exceptional-bindings
                                               excp-bindings)))
                         
                         (concat (d-stump--format-remap-bindlist-code
                                  non-excp-binds modes head)
                                 "\n"))))))
            (reverse (d-powerlist d-stump-remap-exceptions-alist)))))
         
         (remapped-keys-str (d-stump--format-remap-list-code remapped-keys-list))
         (overallstr (concat "(define-remapped-keys `"
                             remapped-keys-str
                             ")")))
    overallstr))

(provide 'd-stump-functions)
;;; d-stump-functions.el ends here
