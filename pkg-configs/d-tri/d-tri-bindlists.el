;;; d-tri-bindlists.el --- d-emacs-maps-lists for d-tri  -*- lexical-binding: t; -*-

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

;; This file contains the keylists for maps of d-tri. It is basically the raw elisp-information from which the d-tri file is formed through d-tri--generate-config.

;;; Code:

;;;; d-tri-mode-map
`(
;;;;; Coordinates
;;;;;;; 1
;;;;;;;; 1--1
((1 -1 -5) . "pin") 
((1 -1 -3) . "hint -h") 
((1 -1 3) . "hint") 
((1 -1 4) . "fillcmdline tab") 

;;;;;;;; 1-0 
((1 -1 -5) . "help") 
((1 0 -3) . "scrollline -15") 
((1 0 -2) . "jumpprev") 
((1 0 -1) . "scrollpage -1") 
((1 0 1) . "scrollpage 1")
((1 0 2) . "jumpnext") 
((1 0 3) . "scrollline 15") 
((1 0 4) . "reader") 
((1 0 5) . "fillcmdline open") 

;;;;;;;; 1-1 
((1 1 -4) . "mute") 
((1 1 -3) . "clipboard yank") 
((1 1 -2) . "reload 1") 
((1 1 2) . "fillcmdline") 
((1 1 3) . "clipboard open")
((1 1 4) . "mute unmute") 

;;;;;;; 2
((2 -1 -2) . "bmark") 
(("C-" . (2 -1 -2)) . "gobble 1 quickmark") 
((2 -1 2) . "fillcmdline bmarks") 
((2 -1 4) . "fillcmdline taball") 

;;;;;;;; 2-0 
((2 0 -2) . "back") 
((2 0 2) . "forward") 

;;;;;;;; 2-1 
((2 1 -3) . "clipboard yankshort") 
((2 1 3) . "clipboard tabopen") 

;;;;;;; 3
;;;;;;;; 3--1 
((3 -1 -2) . "followpage prev") 
((3 -1 1) . "focusinput -b") 
((3 -1 2) . "followpage next")

;;;;;;;; 3-0 

;;;;;;;; 3-1 
((3 1 -5) . "changelistjump -1") 
((3 1 -4) . "focusinput -p") 
((3 1 -3) . "clipboard yanktitle") 
((3 1 -2) . "focusinput -N") 
((3 1 0) . "focusinput -l") 
((3 1 2) . "focusinput -n") 
((3 1 4) . "focusinput -b") 

;;;;;;; 4 
;;;;;;;; 4--1 
((4 -1 -4) . "reloadhard") 
((4 -1 -2) . "fillcmdline markaddglobal ") 
((4 -1 4) . "fillcmdline taball") 

;;;;;;;; 4-0 
((4 0 -1) . "scrollto 0") 
((4 0 1) . "scrollto 100")

;;;;;;;; 4-1 
((4 1 -3) . "clipboard yankorg") 

;;;;;;; 5 
;;;;;;;; 5--1 
((5 -1 -2) . "gobble 1 markadd") 
((5 -1 2) . "gobble 1 markjump") 

;;;;;;;; 5-0 
((5 0 -3) . "urlparent") 
((5 0 -1) . "urlroot") 
((5 0 4) . "viewsource") 

;;;;;;;; 5-1 
((5 1 3) . "yankimage") 

;;;;;;; 6 
;;;;;;;; 6--1 
((6 -1 -5) . "hint -I") 
((6 -1 -4) . "hint -b") 
((6 -1 -3) . "hint -K") 
((6 -1 -1) . "hint -a") 
((6 -1 1) . "hint -z") 
((6 -1 2) . "hint -t") 
((6 -1 3) . "hint -k") 
((6 -1 4) . "hint -r") 
((6 -1 5) . "hint -wp") 

;;;;;;;; 6-0 
((6 0 -5) . "hint -;") 
((6 0 -4) . "fillcmdline hint -f") 
((6 0 -2) . "hint -i") 
((6 0 2) . "hint -s") 
((6 0 4) . "fillcmdline hint -fr") 
((6 0 5) . "hint -#") 

;;;;;;;; 6-1 
((6 1 -3) . "hint -P") 
((6 1 -2) . "hint -w") 
((6 1 2) . "hint -y") 
((6 1 4) . "hint -p") 

;;;;;;; 7 
((7 -1 -4) . "undo window") 
((7 -1 -3) . "fillcmdline winopen")
((7 -1 -3) . "winopen") 
((7 -1 2) . "winmerge")
((7 -1 3) . "winclose") 

;;;;;;;; 7-0 
((7 0 -5) . "tabduplicate") 
((7 0 -4) . "undo tab") 
((7 0 -3) . "fillcmdline tabopen") 
((7 0 -2) . "tabprev") 
((7 0 -1) . "tabfirst") 
((7 0 1) . "tablast") 
((7 0 2) . "tabnext") 
((7 0 3) . "tabclose") 
((7 0 4) . "tabaudio") 
((7 0 5) . "tabpush") 

;;;;;;;; 7-1 
((7 1 0) . "rot13")   

;;;;;; C- 
;;;;;;; C-1 
;;;;;;;; C-1--1 

;;;;;;; C-3 
;;;;;;;; C-3-0 
(("C-" . (3 -1 -4)) . "source --clipboard") 
(("C-" . (2 -1 2)) . "fillcmdline bmark . ") 

;;;;;;; C-4 
;;;;;;;; C-4-1 
(("C-" . (4 1 -3)) . "clipboard yankmd") 

;;;;;;; C-6 
;;;;;;;; C-6--1 
(("C-" . (6 -1 -3)) . "hint -J*b") 
(("C-" . (6 -1 -1)) . "hint -J*a") 
(("C-" . (6 -1 1)) . "hint -J*z") 
(("C-" . (6 -1 2)) . "hint -J*t") 
(("C-" . (6 -1 3)) . "hint -J*r") 
(("C-" . (6 -1 3)) . "hint -J*wp") 
(("C-" . (6 -1 4)) . "hint -J*I") 

;;;;;;;; C-6-0 
(("C-" . (6 0 -4)) . "hint -J*;") 
(("C-" . (6 0 -2)) . "hint -J*i") 
(("C-" . (6 0 2)) . "hint -J*s") 
(("C-" . (6 0 4)) . "hint -J*#") 

;;;;;;;; C-6-1 
(("C-" . (6 1 -3)) . "hint -J*P") 
(("C-" . (6 1 -2)) . "hint -J*w") 
(("C-" . (6 1 3)) . "hint -J*y") 
(("C-" . (6 1 4)) . "hint -J*p") 

;;;;;;; C-7 
;;;;;;;; C-7--1 
(("C-" . (7 0 -4)) . "tabclosealltoleft") 
(("C-" . (7 0 3)) . "tabonly") 
(("C-" . (7 0 4)) . "tabclosealltoright") 

;;;;;;;; 7-0 
(("C-" . (7 0 -2)) . "tabmove -1") 
(("C-" . (7 0 2)) . "tabmove +1") 

;;;;;;;; C-7--1 
(("C-" . (7 -1 -3)) . "fillcmdline winopen -private") 
(("C-" . (7 1 0)) . "jumble") 
(("C-" . (7 -1 2)) . "winmerge") 

;;;;;; H- 
;;;;;;; H-6 
;;;;;;;; H-6--1 
(("H-" . (6 -1 -3)) . "hint -Vb") 
(("H-" . (6 -1 -1)) . "hint -Va") 
(("H-" . (6 -1 1)) . "hint -Vz") 
(("H-" . (6 -1 2)) . "hint -Vt") 
(("H-" . (6 -1 3)) . "hint -Vr") 
(("H-" . (6 -1 3)) . "hint -Vwp") 
(("H-" . (6 -1 4)) . "hint -VI") 

;;;;;;;; H-6-0 
(("H-" . (6 0 -4)) . "hint -V;") 
(("H-" . (6 0 -2)) . "hint -Vi") 
(("H-" . (6 0 2)) . "hint -Vs") 
(("H-" . (6 0 4)) . "hint -V#") 

;;;;;;;; H-6-1 
(("H-" . (6 1 -3)) . "hint -VP") 
(("H-" . (6 1 -2)) . "hint -Vw") 
(("H-" . (6 1 3)) . "hint -Vy") 
(("H-" . (6 1 4)) . "hint -Vp") 

;;;;;; H-C- 
;;;;;;; H-C-6 
;;;;;;;; H-C-6--1 
(("H-C-" . (6 -1 -3)) . "hint -qb") 
(("H-C-" . (6 -1 -1)) . "hint -qa") 
(("H-C-" . (6 -1 1)) . "hint -qz") 
(("H-C-" . (6 -1 2)) . "hint -qt") 
(("H-C-" . (6 -1 3)) . "hint -qr") 
(("H-C-" . (6 -1 4)) . "hint -qwp") 
(("H-C-" . (6 -1 5)) . "hint -qI") 

;;;;;;;; H-C-6-0 
(("H-C-" . (6 0 -4)) . "hint -q;") 
(("H-C-" . (6 0 -2)) . "hint -qi") 
(("H-C-" . (6 0 2)) . "hint -qs") 
(("H-C-" . (6 0 4)) . "hint -q#") 

;;;;;;;; H-C-6-1 
(("H-C-" . (6 1 -3)) . "hint -qP") 
(("H-C-" . (6 1 -2)) . "hint -qw") 
(("H-C-" . (6 1 3)) . "hint -qy") 
(("H-C-" . (6 1 4)) . "hint -qp"))

;;; d-tri-bindlists.el ends here
