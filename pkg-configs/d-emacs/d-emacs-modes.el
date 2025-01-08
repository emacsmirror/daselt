;;; d-emacs-modes.el --- Modes for d-emacs           -*- lexical-binding: t; -*-

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

(define-minor-mode d-emacs-mode
  "Daselt's minor mode."
  :init-value nil
  :global t
  :interactive t
  :lighter "Daselt"
  (if d-emacs-mode
      (progn (if d-emacs-translate-keys
                 ;; Add the key translations for C-g and ("C-" . (1 1 -2)) if they aren't there yet.
                 (progn (if d-emacs-translate-C-1-1--2-C-g
                            (let ((transcons
                                   `(,(d--extract-binding-string `(("C-" . (1 1 -2)))) . "C-g"))
                                  (revtranscons
                                   `("C-g" . ,(d--extract-binding-string `(("C-" . (1 1 -2)))))))
                              (add-to-list 'd-emacs-key-translations-alist transcons)
                              (add-to-list 'd-emacs-key-translations-alist revtranscons)))
                        (mapcar
                         (lambda (cns) (key-translate (car cns) (cdr cns)))
                         d-emacs-key-translations-alist)))

             ;; Refresh the d-emacs-xkb-layouts in case someone has changed bindings.
             (d-emacs-xkb--generate-layouts)

             ;; Generate d-emacs-dfk-layout from the current d-emacs-xkb-layout.
             (d-emacs-dfk-import-current-layout)

             ;; Add all files in pkg-configs to the load-path.
             (let ((default-directory  (concat d-emacs-directory "pkg-configs/")))
               (normal-top-level-add-to-load-path '("."))
               (normal-top-level-add-subdirs-to-load-path))
             
             (d--generate-quick-key-variables)
             
             ;; Has to be defined here so later `emulation'-maps are put on top.
             (defvar-keymap d-emacs-mode-map)
             (if d-emacs-put-d-emacs-mode-map-into-emulation
                 (add-to-list 'emulation-mode-map-alists
                              `((d-emacs-mode . ,d-emacs-mode-map))))

             (let ((undo-tree-auto-save-history nil) ; Saving undo-state of opened files is useless here and slows down startup.
                   )  
               (d--act-on-pkg-files-by-type-and-maybe-kill
                `((d-require-file . (nil "eval"))
                  (d-emacs--with-eval-backup-and-apply-bindlists-in-file
                   . (lambda (filepath) ; We have to check only bindlists for Emacs are rebound.
                       (and (d--bindlists-p filepath)
                            (d--regular-file-p filepath)
                            (string-match-p "pkg-configs/d-emacs/" filepath)))
                   )                
                  ;; Do rebinding before other operations, that way if something goes wrong, at least the layout is defined
                  (d--save-bindlists-in-file-as-variables . "bindlists")
                  (d-emacs--backup-and-set-constants-in-file . ("constants" "regular"))
                  (d-emacs--with-eval-add-advicelists-in-file . ("advicelists" "regular")))))

             (if d-emacs-globalize-d-emacs-mode-map
                 (progn (unless (boundp d-emacs-global-map-backup)
                          (setq d-emacs-global-map-backup global-map))
                        (setq global-map d-emacs-mode-map)))

             (if d-show-tutorial (d-generate-tutorial)))

    ;; Reset variables and remove advice
    (d-emacs--reset-backed-up-variables)
    (d--act-on-pkg-files-by-type-and-maybe-kill
     `((d-emacs--with-eval-remove-advicelists-in-file . ("advicelists" "regular"))))))

(provide 'd-emacs-modes)
;;; d-emacs-modes.el ends here
