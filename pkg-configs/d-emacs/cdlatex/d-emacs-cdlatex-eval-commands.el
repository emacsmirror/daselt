;;; d-emacs-cdlatex-eval-commands.el --- d-emacs-commands for cdlatex  -*- lexical-binding: t; -*-

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

(defun d-cdlatex-math-symbol ()
  "This is a modified version of cdlatex-math-symbol that ensures no math delimiters are
inserted when it is used in the minibuffer and during searches."
  (interactive)
  (let* ((cell (cdlatex-read-char-with-help
                cdlatex-math-symbol-alist-comb
                1 cdlatex-math-symbol-no-of-levels
                "Math symbol level %d of %d: "
                "AVAILABLE MATH SYMBOLS.  [%c]=next level "
                cdlatex-math-symbol-prefix
                (get 'cdlatex-math-symbol-alist-comb 'cdlatex-bindings)))
         (char (car cell))
         (level (cdr cell))
         (entry (assoc char cdlatex-math-symbol-alist-comb))
         (symbol (nth level entry)))

    (if (or (not symbol)
            (not (stringp symbol))
            (equal symbol ""))
        (error "No such math symbol %c on level %d" char level))

    (unless (or (derived-mode-p 'minibuffer-mode)
                (derived-mode-p 'isearch-mode))
      (cdlatex-ensure-math))

    (insert symbol)
    (when (string-match "\\?" symbol)
      (cdlatex-position-cursor))))

(defun d-cdlatex-math-modify (&optional ARG)
  "This is a modification of cdlatex-math-modify which uses a prefix argument to switch
between math- and non-math bindings in the minibuffer and during searches, since
math-expressions in the main document are often sub-expressions and so can't be surrounded
by delimiters in searches without using regular expressions. Without ARG, it uses
text-expressions, with ARG, it uses math expressions."
  (interactive)
  (catch 'exit

    (let ((inline-math-or-arg (if (or (derived-mode-p 'minibuffer-mode)
                                      (derived-mode-p 'isearch-mode))
                                  ARG
                                (cdlatex--texmathp)))
          (win (selected-window))
          (savedpos (make-marker))
          char (help-is-on nil) ass acc rmdot it cmd extrabrac
          before after)
      (catch 'exit1
        (save-window-excursion
          (while t
            (if help-is-on
                (progn
                  (cdlatex-turn-on-help
                   "AVAILABLE MODIFIERS. (?=SCROLL)"
                   (if inline-math-or-arg 1 2)
                   cdlatex-math-modify-alist-comb help-is-on t)
                  (message "Math modify: "))
              (message "Math modify: (?=HELP)"))

            (if (and (not help-is-on)
                     (sit-for cdlatex-auto-help-delay))
                (setq char ?\?)
              (setq char (read-char)))

            (cond
             ((= char ?\C-g)
              (keyboard-quit))
             ((= char ?\?)
              (if help-is-on
                  (progn
                    (setq help-is-on (+ help-is-on (- (window-height) 1)))
                    (if (> help-is-on (count-lines (point-min) (point-max)))
                        (setq help-is-on 1)))
                (setq help-is-on 1)))
             ((equal char cdlatex-math-modify-prefix)
              (select-window win)
              (insert cdlatex-math-modify-prefix)
              (message "")
              (throw 'exit t))
             (t (throw 'exit1 t))))))
      (message "")
      (setq ass (assoc char cdlatex-math-modify-alist-comb))
      (if (not ass)
          (progn
            (insert cdlatex-math-modify-prefix char)
            (throw 'exit t)))
      (setq ass    (cdr ass))
      (setq cmd    (nth (if inline-math-or-arg 0 1) ass))
      (setq acc    (nth 2 ass))
      (setq rmdot  (nth 3 ass))
      (setq it     (nth 4 ass))
      (if (not cmd)
          (progn
            (message "No such modifier `%c' %s math mode" char
                     (if inline-math-or-arg "inside" "outside"))
            (insert cdlatex-math-modify-prefix char)
            (throw 'exit t)))
      (if (string-match "\\(.*\\)\\?\\(.*\\)" cmd)
          (setq before (match-string 1 cmd) after (match-string 2 cmd)))
      (cond
       ((cdlatex-region-active-p)
        (let ((beg (min (region-beginning) (region-end)))
              (end (max (region-beginning) (region-end))))
          (goto-char end)
          (move-marker savedpos (point))
          (goto-char beg)
          (if before
              (insert before)
            (insert "{")
            (if acc (forward-char -1))
            (insert cmd)
            (if (not acc) (insert " ")))
          (goto-char savedpos)
          (if after
              (insert after)
            (insert "}"))))
       (arg
        (move-marker savedpos (point))
        (backward-word arg)
        (if before
            (insert before)
          (insert "{")
          (if acc (forward-char -1))
          (insert cmd)
          (if (not acc) (insert " ")))
        (goto-char savedpos)
        (if after
            (insert after)
          (insert "}")))
       ((or (bolp)
            (not cdlatex-modify-backwards)
            (memq (preceding-char) '(?\  ?$ ?- ?{ ?\( )))
        ;; Just insert empty form and position cursor
        (if (string-match "\\?" cmd)
            (insert cmd)
          (if acc
              (insert cmd "{?")
            (insert "{" cmd " ?"))
          (if it (insert "\\/"))
          (insert "}"))
        (search-backward "?")
        (delete-char 1))
       (t
        ;; Modify preceding character or word
        (move-marker savedpos (point))
        (if (= (preceding-char) ?\})
            ;; its a group
            (progn (setq extrabrac nil)
                   (backward-list 1)
                   (if (not acc) (forward-char 1)))
          ;; not a group
          (forward-char -1)
          (if (looking-at "[a-zA-Z]")
              ;; a character: look if word or macro
              (progn
                (setq extrabrac t)
                (re-search-backward "[^a-zA-Z]")
                (cond
                 ((= (following-char) ?\\))
                 ((not inline-math-or-arg) (forward-char 1))
                 (t (goto-char savedpos)
                    (forward-char -1)
                    (if (and rmdot (let (case-fold-search) (looking-at "[ij]")))
                        (progn (insert "\\")
                               (forward-char 1)
                               (insert "math")
                               (move-marker savedpos (point))
                               (forward-char -6))))))
            (setq extrabrac t)))
        (if extrabrac (progn (insert "{")
                             (if acc (forward-char -1))))
        (insert cmd)
        (if (not acc) (insert " "))
        (goto-char savedpos)
        (if extrabrac (insert "}")))))))

(defun d-cdlatex-math-modify-modeline-math ()
  "This is a wrapper around d-cdlatex-math-modify-modeline that adds an argument, so
d-cdlatex-math-modify-modeline is called for math-symbols."
  (interactive)
  (d-cdlatex-math-modify t))



(provide 'd-emacs-cdlatex-eval-commands)
;;; d-emacs-cdlatex-eval-commands.el ends here
