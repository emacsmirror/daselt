;;; d-stump-stumpwm-bindlists.el --- bindlists for the d-stump init.  -*- lexical-binding: t; -*-

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

;; This file contains the keylists for StumpWM maps of d-stump. If daselt-d-stump is t, it is parsed automatically when daselt-mode is started. Each element in this file should be either

;; 1. A LIST consisting of conses of whose car is a binding-location (see d--binding-location-p) binding-cars and values. This map list corresponds to  and should generally be at be at the top if it exists, though sometimes it makes sense to put it lower down if it references maps that are previously defined. When d-emacs-mode is activated and  is evaluated,  is backed up at d-emacs--mode-backup and the key combinations in this list are rebound to their corresponding values.

;; 2. A cons CONS1 whose car is a symbol SYMB whose cdr is a list as in 1. If SYMB is already bound when  is evaluated and d-emacs-mode is activated, then its value should be a keymap.

;; Each of these options should be given using a backquote.

;;; Code:

;;;; "*top-map*"
`("*top-map*" 
;;;;; Strings
  ("F12" . "redaselt") 
  ("KP_0" . "move-focus left") 
  ("KP_2" . "move-focus up") 
  ("KP_4" . "coord-left") 
  ("KP_5" . "coord-down") 
  ("KP_6" . "coord-right") 
  ("KP_7" . "coord-taskleft") 
  ("KP_8" . "coord-up") 
  ("KP_9" . "coord-taskright") 
  ("Print" . "screenshot") 
  ("KP_Enter" . "move-focus right") 
  ("KP_Separator" . "move-focus down") 
  ("XF86AudioMute" . "pamixer-toggle-mute") 
  ("XF86MonBrightnessUp" . "backlight-up") 
  ("XF86AudioLowerVolume" . "pamixer-volume-down") 
  ("XF86AudioRaiseVolume" . "pamixer-volume-up") 
  ("XF86MonBrightnessDown" . "backlight-down") 

;;;;;; C- 
  ("C-Print" . "screenshot-area") 

;;;;;; H- 
  ("H-Up" . "move-focus up") 
  ("H-TAB" . "next-in-frame") 
  ("H-Down" . "move-focus down") 
  ("H-Left" . "move-focus left") 
  ("H-Print" . "screenshot-window") 
  ("H-Right" . "move-focus right") 
  ("H-ISO_Left_Tab" . "prev-in-frame") 
  ("H-XF86AudioLowerVolume" . "backlight-down") 
  ("H-XF86AudioRaiseVolume" . "backlight-up") 

;;;;;; M- 
  ("M-Up" . "coord-up") 
  ("M-Down" . "coord-down") 
  ("M-Left" . "coord-left") 
  ("M-Right" . "coord-right") 

;;;;;; S- 
  ("S-KP_7" . "coord-taskleft") 
  ("S-KP_9" . "coord-taskright") 

;;;;; Coordinates 
;;;;;; H- 
;;;;;;; H-1 
  (("H-" . (1 -1 -5)) . "fullscreen-in-frame") 
  (("H-" . (1 -1 -4)) . "coord-taskleft") 
  (("H-" . (1 -1 -3)) . "coord-up") 
  (("H-" . (1 -1 -2)) . "coord-left") 
  (("H-" . (1 -1 -1)) . "pamixer-volume-down") 
  (("H-" . (1 -1 1)) . "pamixer-volume-up") 
  (("H-" . (1 -1 2)) . "coord-right") 
  (("H-" . (1 -1 3)) . "coord-down") 
  (("H-" . (1 -1 4)) . "coord-taskright") 
  (("H-" . (1 -1 5)) . "fullscreen") 

;;;;;;;; H-1-0 
  (("H-" . (1 0 -5)) . "only") 
  (("H-" . (1 0 -4)) . "vsplit") 
  (("H-" . (1 0 -3)) . "move-focus up") 
  (("H-" . (1 0 -2)) . "move-focus left") 
  (("H-" . (1 0 -1)) . "winner-undo") 
  (("H-" . (1 0 1)) . "winner-redo") 
  (("H-" . (1 0 2)) . "move-focus right") 
  (("H-" . (1 0 3)) . "move-focus down") 
  (("H-" . (1 0 4)) . "hsplit") 
  (("H-" . (1 0 5)) . "remove-split") 

;;;;;;;; H-1-1 
  (("H-" . (1 1 -6)) . "sibling") 
  (("H-" . (1 1 -3)) . "show-clipboard-history") 
  (("H-" . (1 1 -2)) . "backlight-down") 
  (("H-" . (1 1 0)) . "send-raw-key") 
  (("H-" . (1 1 2)) . "backlight-up") 

;;;;;; H-C- 
;;;;;;; H-C-1 
;;;;;;;; H-C-1--1 
  (("H-C-" . (1 -1 -4)) . "coord-move-taskleft") 
  (("H-C-" . (1 -1 -3)) . "coord-move-up") 
  (("H-C-" . (1 -1 -2)) . "coord-move-left") 
  (("H-C-" . (1 -1 -1)) . "pamixer-source-volume-down") 
  (("H-C-" . (1 -1 1)) . "pamixer-source-volume-up") 
  (("H-C-" . (1 -1 2)) . "coord-move-right") 
  (("H-C-" . (1 -1 3)) . "coord-move-down") 
  (("H-C-" . (1 -1 4)) . "coord-move-taskright") 

;;;;;;;; H-C-1-0 
  (("H-C-" . (1 0 -4)) . "vsplit-by-letter") 
  (("H-C-" . (1 0 -3)) . "move-window up") 
  (("H-C-" . (1 0 -2)) . "move-window left") 
  (("H-C-" . (1 0 2)) . "move-window right") 
  (("H-C-" . (1 0 3)) . "move-window down") 
  (("H-C-" . (1 0 4)) . "hsplit-by-letter") 

;;;;;;;; H-C-1-1 
  (("H-C-" . (1 1 -5)) . "kill-window") 

;;;;;;; H-C-2 
;;;;;;;; H-C-2-0 
  (("H-C-" . (2 0 -3)) . "exchange-direction up") 
  (("H-C-" . (2 0 -2)) . "exchange-direction left") 
  (("H-C-" . (2 0 2)) . "exchange-direction right") 
  (("H-C-" . (2 0 3)) . "exchange-direction down")) 

;;;;;;;; H-C-1-1 

;;;; "*root-map*"
`("*root-map*" 
;;;;; Coordinates
;;;;;;; 1
;;;;;;;; 1--1
  ((1 -1 -4) . "loadrc") 
  ((1 -1 -2) . "batterystate") 
  ((1 -1 -1) . "pamixer-mute") 
  ((1 -1 1) . "pamixer-unmute") 
  ((1 -1 2) . "showtemperature") 

;;;;;;;; 1-0 
  ((1 0 -5) quote *help-map*) 
  ((1 0 -4) . "redshift-by-level") 
  ((1 0 -3) quote *emacs-map*) 
  ((1 0 -1) quote *run-app-map*) 
  ((1 0 1) quote *quit-map*) 
  ((1 0 2) . "mode-line") 
  ((1 0 3) . "iresize") 

;;;;;;;; 1-1 
  ((1 1 -2) . "backlight-set 0") 
  ((1 1 0) . "groups") 
  ((1 1 2) . "backlight-set 100") 

;;;;;; H- 
;;;;;;; H-1 
;;;;;;;; H-1--1 
  (("H-" . (1 -1 -2)) . "pamixer-source-mute") 
  (("H-" . (1 -1 1)) . "pamixer-source-unmute") 

;;;;;;;; H-1-0 
  (("H-" . (1 0 -5)) . "restore-from-file") 
  (("H-" . (1 0 5)) . "dump-desktop-to-file") 

;;;;;;;; H-1-1 
  (("H-" . (1 1 0)) . "coord-taskorigin"))

;;;; "*run-app-map*"
`("*run-app-map*" 

;;;;; Coordinates
;;;;;;; 1
;;;;;;;; 1--1
  ((1 -1 5) . "rr-firefox"))

;;;; "*quit-map*"
`("*quit-map*" 

;;;;; Coordinates
;;;;;;; 1
;;;;;;;; 1--1
  ((1 -1 -2) . "exec /usr/bin/sudo pm-hibernate") 
  ((1 -1 2) . "suspend") 
  ((1 -1 5) . "quit") 

;;;;;;;; 1-0 
  ((1 0 -5) . "restart-hard") 
  ((1 0 5) . "restart-soft")) 

;;;;;; C- 
;;;;;;; C-1 
;;;;;;;; C-1-0

;;;; "*emacs-map*"
`("*emacs-map*" 

;;;;; Coordinates
;;;;;;; 1
;;;;;;;; 1--1
  ((1 -1 -2) . "emacs-daemon-start") 
  ((1 -1 2) . "emacs-daemon-stop") 
  ((1 -1 3) . "emacs-daemon-kill-force") 

;;;;;;;; 1-0 
  ((1 0 -3) . "swm-emacs") 
  ((1 0 3) . "emacs"))

;;   ('d-stump-emacs-names 'd-stump-emacs-translate-list))

;;; d-stump-stumpwm-bindlists.el ends here
