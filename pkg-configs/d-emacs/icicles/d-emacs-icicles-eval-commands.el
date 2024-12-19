;;; d-emacs-icicles-eval-commands.el --- Daselt's Emacs module              -*- lexical-binding: t; -*-

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

;;  d-emacs-icicles commands.

;;; Code:

(defun d-emacs-icicle-previous-TAB-completion-method (temporary-p) ; Bound to a key in minibuffer.
      "Cycle to the previous `TAB' completion method.
Option `icicle-TAB-completion-methods' determines the TAB completion
methods that are available.

With a prefix argument, the newly chosen method is used only for the
current command.  More precisely, the previously active method is
restored as soon as you return to the top level."
      (interactive "P")
      (unless icicle-current-TAB-method     ; nil means the same as the default.
    (setq icicle-current-TAB-method  (car icicle-TAB-completion-methods)))
      (if temporary-p
              (unless (get 'icicle-last-top-level-command 'icicle-current-TAB-method)
        (put 'icicle-last-top-level-command 'icicle-current-TAB-method icicle-current-TAB-method))
    (put 'icicle-last-top-level-command 'icicle-current-TAB-method nil))

      (let ((now  (memq icicle-current-TAB-method icicle-TAB-completion-methods))
        preceding)
    (if (eq now (car icicle-TAB-completion-methods))
                (setq preceding (last icicle-TAB-completion-methods))
          (setq preceding (last icicle-TAB-completion-methods 2)))
    (setq icicle-current-TAB-method  (if (eq now (car icicle-TAB-completion-methods))
                                                 (car (last icicle-TAB-completion-methods))
                                           (car (last icicle-TAB-completion-methods (1+ (length (memq (car now) icicle-TAB-completion-methods))))))
          preceding                  (or (car (last (memq icicle-current-TAB-method
                                                          icicle-TAB-completion-methods) 2))
                                         (car (last icicle-TAB-completion-methods))))
    ;; Skip any method that is not currently supported.
    (while (or (and (eq icicle-current-TAB-method 'fuzzy)       (not (featurep 'fuzzy-match)))
               (and (eq icicle-current-TAB-method 'vanilla)     (not (boundp 'completion-styles)))
               (and (eq icicle-current-TAB-method 'swank)       (not (featurep 'el-swank-fuzzy))))
      (if (eq (car icicle-TAB-completion-methods) icicle-current-TAB-method)
                  (setq preceding (last icicle-TAB-completion-methods))
            (setq preceding (last icicle-TAB-completion-methods 2)))
      (setq icicle-current-TAB-method  (if (eq now (car icicle-TAB-completion-methods))
                                                   (car (last icicle-TAB-completion-methods))
                                             (car (last icicle-TAB-completion-methods (1+ (length (memq (car now) icicle-TAB-completion-methods))))))))
    (icicle-msg-maybe-in-minibuffer
     "TAB completion is %s %s  Previous: %s"
     (icicle-propertize (icicle-upcase (symbol-name icicle-current-TAB-method))
                        'face 'icicle-msg-emphasis)
     (if temporary-p (concat "for " (icicle-propertize "this command" 'face 'icicle-msg-emphasis)) "now.")
     (if temporary-p "" (icicle-upcase (symbol-name preceding)))))
      ;; Update keymap bindings if necessary, similar to the original function, adjusted for context if needed
      )

(defun d-emacs-icicle-previous-S-TAB-completion-method (temporary-p) ; Bound to a key in minibuffer.
  "Cycle to the previous `S-TAB' completion method.
Option `icicle-S-TAB-completion-methods-alist' customizes the
available `S-TAB' completion methods.

With a prefix argument, the newly chosen method is used only for the
current command.  More precisely, the previously active method is
restored as soon as you return to the top level."
  (interactive "P")
  (if temporary-p
      (unless (get 'icicle-last-top-level-command 'icicle-apropos-complete-match-fn)
        (put 'icicle-last-top-level-command 'icicle-apropos-complete-match-fn
             icicle-apropos-complete-match-fn))
    (put 'icicle-last-top-level-command 'icicle-apropos-complete-match-fn nil))
  (let ((entry (rassq icicle-apropos-complete-match-fn icicle-S-TAB-completion-methods-alist))
        preceding)
    (setq icicle-apropos-complete-match-fn       (or (cdadr (let ((pos (cl-position entry icicle-S-TAB-completion-methods-alist
                                                                                    :test #'equal :key #'car)))
                                                              (if pos
                                                                  (nthcdr (1- pos) icicle-S-TAB-completion-methods-alist)
                                                                (last icicle-S-TAB-completion-methods-alist))))
                                                     (cdar (last icicle-S-TAB-completion-methods-alist)))
          preceding                              (or (caadr (let ((pos (cl-position entry icicle-S-TAB-completion-methods-alist
                                                                                    :test #'equal :key #'car)))
                                                              (if pos
                                                                  (nthcdr (1- pos) icicle-S-TAB-completion-methods-alist)
                                                                (last icicle-S-TAB-completion-methods-alist))))
                                                     (caar (last icicle-S-TAB-completion-methods-alist)))
          icicle-last-apropos-complete-match-fn  icicle-apropos-complete-match-fn) ; Backup copy.
    (icicle-msg-maybe-in-minibuffer
     "S-TAB completion is %s%s %s  Previous: %s"
     (icicle-propertize (icicle-upcase (car (rassq icicle-apropos-complete-match-fn
                                                   icicle-S-TAB-completion-methods-alist)))
                        'face 'icicle-msg-emphasis)
     (if (memq icicle-apropos-complete-match-fn
               '(icicle-levenshtein-match icicle-levenshtein-strict-match))
         (icicle-propertize (format " (%d)" icicle-levenshtein-distance) 'face 'icicle-msg-emphasis)
       "")
     (if temporary-p (concat "for " (icicle-propertize "this command" 'face 'icicle-msg-emphasis)) "now.")
     (if temporary-p "" (icicle-upcase (car preceding))))))

(defun icicle-next-completion-style-set () ; Bound to `C-M-(' in minibuffer.
  "Cycle to the next set (list) of completion styles, for this command only.
Has no effect if the current TAB completion method is not `vanilla'.
Bound to \\<minibuffer-local-completion-map>`\\[icicle-next-completion-style-set]' in the minibuffer.
Option `icicle-completion-style-sets' defines the available sets.
The newly chosen style set is used only for the current command.

To change the value of option `completion-styles' (and optionally save
the new value), use `\\[icicle-choose-completion-style-set]'.  Unlike
`customize-option', this lets you use completion against the
`icicle-completion-style-sets' you have defined."
  (interactive)
  (if (not (boundp 'completion-styles))
      (icicle-msg-maybe-in-minibuffer "`completion-styles' not available for this Emacs version")
    (if (not (eq 'vanilla icicle-current-TAB-method))
        (icicle-msg-maybe-in-minibuffer
         (substitute-command-keys "You must first set TAB completion to `vanilla' using \
\\<minibuffer-local-completion-map>`\\[icicle-next-TAB-completion-method]'"))
      (unless icicle-completion-style-set ; nil means the same as the default (first).
        (setq icicle-completion-style-set  (car icicle-completion-style-sets)))
      (unless (get 'icicle-last-top-level-command 'icicle-completion-style-set)
        (put 'icicle-last-top-level-command 'icicle-completion-style-set icicle-completion-style-set))
      (let ((now  (memq icicle-completion-style-set icicle-completion-style-sets))
            following)
        (setq icicle-completion-style-set  (or (cadr now)  (car icicle-completion-style-sets))
              following                    (or (cadr (memq icicle-completion-style-set
                                                           icicle-completion-style-sets))
                                               (car icicle-completion-style-sets)))
        (icicle-msg-maybe-in-minibuffer
         "Completion style is %s %s  Next: %s"
         (icicle-propertize (format "%s" icicle-completion-style-set) 'face 'icicle-msg-emphasis)
         (concat "for " (icicle-propertize "this command" 'face 'icicle-msg-emphasis))
         (format "%s" following))))))

(defun d-emacs-icicle-previous-completion-style-set () ; Custom binding suggested: `C-M-)'
  "Cycle to the previous set (list) of completion styles, for this command only.
Has no effect if the current TAB completion method is not `vanilla'.
Intended for binding in the minibuffer.
Option `icicle-completion-style-sets' defines the available sets.
The newly chosen style set is used only for the current command.

To change the value of option `completion-styles' (and optionally save
the new value), use `icicle-choose-completion-style-set'. Unlike
`customize-option', this lets you use completion against the
`icicle-completion-style-sets' you have defined."
  (interactive)
  (if (not (boundp 'completion-styles))
      (icicle-msg-maybe-in-minibuffer "`completion-styles' not available for this Emacs version")
    (if (not (eq 'vanilla icicle-current-TAB-method))
        (icicle-msg-maybe-in-minibuffer
         (substitute-command-keys "You must first set TAB completion to `vanilla' using \
`\\[icicle-next-TAB-completion-method]'"))
      (unless icicle-completion-style-set ; nil means the same as the default (first).
        (setq icicle-completion-style-set  (car icicle-completion-style-sets)))
      (unless (get 'icicle-last-top-level-command 'icicle-completion-style-set)
        (put 'icicle-last-top-level-command 'icicle-completion-style-set icicle-completion-style-set))
      (let ((now  (memq icicle-completion-style-set icicle-completion-style-sets))
            preceding)
        (setq icicle-completion-style-set  (if (eq now (car icicle-completion-style-sets))
                                               (car (last icicle-completion-style-sets))
                                             (nth (- (length (memq (car now) icicle-completion-style-sets)) 2)
                                                  icicle-completion-style-sets))
              preceding                    (if (eq now (car icicle-completion-style-sets))
                                               (car (last icicle-completion-style-sets))
                                             (nth (- (length (memq (car now) icicle-completion-style-sets)) 2)
                                                  icicle-completion-style-sets)))
        (icicle-msg-maybe-in-minibuffer
         "Completion style is %s %s  Previous: %s"
         (icicle-propertize (format "%s" icicle-completion-style-set) 'face 'icicle-msg-emphasis)
         (concat "for " (icicle-propertize "this command" 'face 'icicle-msg-emphasis))
         (format "%s" preceding))))))

(defun d-emacs-icicle-previous-sort-order (&optional arg alternativep) ; Suggested binding: `C-M-,'
  "Choose a sort order by cycling to the previous one or using completion.
With a numeric prefix arg, just reverse the current sort order.

Option `icicle-change-sort-order-completion' determines the approach:

 - nil means cycle to the previous order; do not use completion.
 - An integer means use completion if there are more orders than the number.
 - Any other non-nil value means use completion.

A plain prefix argument (`C-u') flips behavior between cycling and completion.

This command updates `icicle-sort-comparer'. Non-interactively,
ALTERNATIVEP means change the current alternative sort order, updating
`icicle-alternative-sort-comparer'.

NOTE: Automatic sorting suppression due to high candidate counts does not
get overridden. The new sort order is applied once sorting is enabled."
  (interactive "P")
  (let ((orders (delq nil icicle-sort-orders-alist))) ; Remove nil entries.
    (if (and (interactive-p) icicle-inhibit-sort-p)
        (icicle-msg-maybe-in-minibuffer "Cannot sort candidates now")
      (if (and arg (not (consp arg)))
          (icicle-reverse-sort-order)
        (let* ((use-completion-p (if (integerp icicle-change-sort-order-completion)
                                     (> (length (icicle-current-sort-functions orders))
                                        icicle-change-sort-order-completion)
                                   icicle-change-sort-order-completion))
               (use-completion-p (or (and (not arg) use-completion-p) ; Resolve completion mode.
                                     (and arg (not use-completion-p))))
               preceding-order next-order)
          (cond (use-completion-p
                 ;; Use completion to select sort order
                 (setq next-order (let ((icicle-whole-candidate-as-text-prop-p nil)
                                        (icicle-must-pass-after-match-predicate nil)
                                        (icicle-show-Completions-initially-flag t)
                                        (enable-recursive-minibuffers t)
                                        (icicle-default-value t) ; Show current in prompt.
                                        (icicle-sort-comparer 'icicle-case-string-less-p)
                                        (icicle-sort-orders-alist nil) ; No sorting choices here.
                                        (icicle-default-in-prompt-format-function
                                         (lambda (def) (format " (default: %s)" def))))
                                    (save-selected-window
                                      (completing-read
                                       (format "New %ssort order: " (if alternativep
                                                                        "alternative "
                                                                      ""))
                                       (icicle-current-sort-functions orders)
                                       nil t nil nil
                                       (car (rassoc (if alternativep
                                                        icicle-alternative-sort-comparer
                                                      icicle-sort-comparer)
                                                    orders))))))
                 ;; Apply chosen sort order.
                 (when (if alternativep icicle-alternative-sort-comparer icicle-sort-comparer)
                   (setq icicle-last-sort-comparer (if alternativep icicle-alternative-sort-comparer
                                                     icicle-sort-comparer))) ; Save current.
                 (set (if alternativep 'icicle-alternative-sort-comparer 'icicle-sort-comparer)
                      (cdr (assoc next-order orders))))
                (t
                 ;; Cycle to previous sort order.
                 (let* ((order-names (mapcar #'car (icicle-current-sort-functions orders)))
                        (current-order-index (cl-position (icicle-current-sort-order alternativep)
                                                          order-names :test #'equal))
                        (previous-order-index (if current-order-index
                                                  (mod (- current-order-index 1) (length order-names))
                                                (1- (length order-names))))
                        (next-order (nth previous-order-index order-names))
                        (preceding-order (if (> (length order-names) 1)
                                             (nth (mod (- previous-order-index 1) (length order-names))
                                                  order-names)
                                           nil)))
                   (when (if alternativep icicle-alternative-sort-comparer icicle-sort-comparer)
                     (setq icicle-last-sort-comparer (if alternativep icicle-alternative-sort-comparer
                                                       icicle-sort-comparer))) ; Save current.
                   (set (if alternativep 'icicle-alternative-sort-comparer 'icicle-sort-comparer)
                        (cdr (assoc next-order icicle-sort-orders-alist)))
                   (icicle-complete-again-update)
                   (icicle-msg-maybe-in-minibuffer
                    "%sorting is now %s.  Reverse: `C-9 C-,'%s"
                    (if alternativep "Alternative s" "S")
                    (icicle-propertize (concat next-order (and icicle-reverse-sort-p ", REVERSED"))
                                       'face 'icicle-msg-emphasis)
                    (if preceding-order (format ".  Previous: %s" preceding-order) "")))))
          (icicle-complete-again-update)
          (icicle-msg-maybe-in-minibuffer
           "%sorting is now %s.  Reverse: `C-9 C-,'%s"
           (if alternativep "Alternative s" "S")
           (icicle-propertize (concat next-order (and icicle-reverse-sort-p  ", REVERSED"))
                              'face 'icicle-msg-emphasis)
           (if preceding-order (format ".  Previous: %s" preceding-order) "")))
        (when (fboundp 'completion--flush-all-sorted-completions)
          (completion--flush-all-sorted-completions))))))

(defun d-emacs-icicle-backward-cycle-incremental-completion () ; Consider binding to a suitable key in minibuffer.
      "Cycle the value of option `icicle-incremental-completion' in reverse order.
If the current value is nil      then it is set to `always'.
If the current value is `always' then it is set to t.
If the current value is t        then it is set to nil.

Consider binding this function in the minibuffer for ease of use."
      (interactive)
      (setq icicle-incremental-completion    (case icicle-incremental-completion
                                           ((nil)      'always)
                                           ((always)   t)
                                           (otherwise  nil))
        icicle-incremental-completion-p  icicle-incremental-completion) ; Ensure both are synced.
      (icicle-msg-maybe-in-minibuffer
   "Incremental completion is now %s"
   (icicle-propertize (case icicle-incremental-completion
                        ((nil)      "OFF")
                        ((t)        "ON")
                        (otherwise  "EAGER"))
                      'face 'icicle-msg-emphasis)))

(defun d-emacs-icicle-backward-cycle-expand-to-common-match () ; Consider for binding to a key in the minibuffer.
  "Cycle the value of option `icicle-expand-input-to-common-match' in reverse.
This cycles among all possible values of the option in the reverse order."

  (interactive)
  (setq icicle-expand-input-to-common-match  (mod (1- icicle-expand-input-to-common-match + 5) 5))
  ;; Note: The addition of 5 ensures the argument to `mod` is always positive.

  (icicle-msg-maybe-in-minibuffer
   "Expanding input to common match is now %s"
   (icicle-propertize (case icicle-expand-input-to-common-match
                        (0  "0 - NEVER")
                        (1  "1 - `TAB', `S-TAB' ONLY")
                        (2  "2 - SOLE MATCH")
                        (3  "3 - PREFIX OR SOLE MATCH")
                        (t  "4 - ALWAYS"))
                      'face 'icicle-msg-emphasis)))

(defun d-emacs-icicle-insert-anychar-regexp ()
                          "Insert icicle-anychar-regexp."
                          (interactive)
                          (icicle-call-then-update-Completions (lambda () (insert (icicle-anychar-regexp)))))

(defun d-emacs-icicle-avy-jump ()
  "Select a candidate in the completions buffer using avy."
  (interactive)
  (let ((buffer (get-buffer "*Completions*")))
    (if (not buffer)
        (message "No completions buffer found.")
      (pop-to-buffer buffer)
      (let ((avy-all-windows nil)
            (avy-goto-word-0-regexp "[^ \r\n\t]+"))
        (call-interactively #'avy-goto-word-0)
        (icicle-choose-completion)))))

;; (defun d-emacs-icicle-avy-insert ()
;;   "Select a candidate in the completions buffer using avy."
;;   (interactive)
;;   (let ((buffer (get-buffer "*Completions*")))
;;     (if (not buffer)
;;         (message "No completions buffer found.")
;;       (pop-to-buffer buffer)
;;       (let ((avy-all-windows nil)
;;             (avy-goto-word-0-regexp "[^ \r\n\t]+"))
;;         (call-interactively #'avy-goto-word-0)
;;         (icicle-insert-candidate-)))))

(provide 'd-emacs-icicles-eval-commands)
;;; d-emacs-icicles-eval-commands.el ends here
