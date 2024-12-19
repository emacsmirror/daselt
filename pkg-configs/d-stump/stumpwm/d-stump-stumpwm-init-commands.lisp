 ;;; d-stump-stumpwm-init-commands.lisp --- Commands for d-stump's init.              -*- lexical-binding: t; -*-

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

;;  d-stump commands.

;;; Code:

(defcommand redaselt () ()
            (run-shell-command "redaselt"))

(defcommand batterystate () ()
            (run-shell-command "acpi -b" T))

(defcommand showtemperature () ()
            (run-shell-command "sensors | grep Tctl" T))

(defcommand suspend () ()
            (run-shell-command "systemctl suspend -i"))

(defcommand d-stump-lock-layer (num)
  ((:string "Lock layer number: "))
  (let ((lockcmd (format nil "d-toggle-layer-lock ~D" num)))
    (run-shell-command lockcmd)))

(defcommand hsplit-by-letter (str)
          ((:string "Letter: "))
          (hsplit-equally (d-stump--letter-to-number str)))

(defcommand vsplit-by-letter (letter)
  ((:string "Letter: "))
  (vsplit-equally (d-stump--letter-to-number str)))

(defcommand redshift-by-level (level)
  ((:string "Level: "))
  (let ((num (d-stump--letter-to-number level)))
    (cond ((= num 0)
           (stumpwm:run-shell-command "redshift -x"))
          ((= num 1)
           (progn (stumpwm:run-shell-command "redshift -x")
                  (stumpwm:run-shell-command "redshift -O 1000")))
          ((= num 2)
           (progn (stumpwm:run-shell-command "redshift -x")
                  (stumpwm:run-shell-command "redshift -O 1500")))
          ((= num 3)
           (progn (stumpwm:run-shell-command "redshift -x")
                  (stumpwm:run-shell-command "redshift -O 2000")))
          ((= num 4)
           (progn (stumpwm:run-shell-command "redshift -x")
                  (stumpwm:run-shell-command "redshift -O 2500")))
          ((= num 5)
           (progn (stumpwm:run-shell-command "redshift -x")
                  (stumpwm:run-shell-command "redshift -O 3000")))
          ((= num 6)
           (progn (stumpwm:run-shell-command "redshift -x")
                  (stumpwm:run-shell-command "redshift -O 3500")))
          ((= num 7)
           (progn (stumpwm:run-shell-command "redshift -x")
                  (stumpwm:run-shell-command "redshift -O 4000")))
          ((= num 8)
           (progn (stumpwm:run-shell-command "redshift -x")
                  (stumpwm:run-shell-command "redshift -O 5000")))
          ((= num 9)
           (progn (stumpwm:run-shell-command "redshift -x")
                  (stumpwm:run-shell-command "redshift -O 5500"))))))



;; ;; Commands to move programs between groups.
;; (defcommand coord-move-left      () () (coord-move-group-change -1  0  0))
;; (defcommand coord-move-right     () () (coord-move-group-change  1  0  0))
;; (defcommand coord-move-up        () () (coord-move-group-change  0  1  0))
;; (defcommand coord-move-down      () () (coord-move-group-change  0 -1  0))
;; (defcommand coord-move-taskleft  () () (coord-move-group-change  0  0 -1))
;; (defcommand coord-move-taskright () () (coord-move-group-change  0  0  1))

;; (defun coord-move-group-change (xo yo zo)
;;   "Move current window to another group by coordinate offset"
;;   (let* ((current-coords
;;           (mapcar #'parse-integer
;;                   (split-string (group-name (current-group)) ",")))
;;          (new-coords (mapcar #'+ current-coords (list xo yo zo)))
;;          (new-group-name (format nil "~{~a~^,~}" new-coords))
;;          (new-group (or (spatial-groups:spatial-find-group (current-screen) new-group-name)
;;                         (gnew new-group-name))))
;;     ;; Move the current window to the new group, then move our view to that group
;;     (gmove new-group)
;;     (coord-group-change xo yo zo)))

;; Command to toggle fullscreen-in-frame-p
(setq fullscreen-in-frame-p nil)
(defcommand fullscreen-in-frame () ()
            (setq fullscreen-in-frame-p t)
            (fullscreen)
            (setq fullscreen-in-frame-p nil))

(define-fullscreen-in-frame-rule fullscreen-in-frame-p-rule (win)
  fullscreen-in-frame-p)

;;; d-stump-stumpwm-init-commands.lisp ends here
