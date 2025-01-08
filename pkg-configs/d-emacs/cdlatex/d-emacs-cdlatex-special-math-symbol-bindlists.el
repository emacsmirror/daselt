;;; d-emacs-cdlatex-special-math-symbol-bindlists.el --- The bindlist for cdlatex-math-symbol-alist-default  -*- lexical-binding: t; -*-

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

;; The bindlists for cdlatex-math-symbol-alist-default. Read in in d-emacs-cdlatex-constants.el.

;;;; cdlatex-mode-map
`(
;;;;; Strings
  ("∘" "\\odot" "\\astrosun") 
  ("⋉" "\\lftimes" "\\lfbowtie") 
  ("⋊" "\\rftimes" "\\rfbowtie") 

;;;;; Coordinates 
;;;;;;; 1 
  ((1 -1 -3) "\\longleftarrow" "\\dashleftarrow") 
  ((1 -1 -2) "\\leftharpoonup" "\\leftharpoondown" "\\iffalse") 
  ((1 -1 1) "\\rightharpoonup" "\\rightharpoondown" "\\fi") 
  ((1 -1 2) "\\longrightarrow" "\\dashrightarrow") 
  ((1 -1 3) "\\blacktriangle" "\\triangle") 
  ((1 -1 4) "\\diamond") 

;;;;;;;; 1-0 
  ((1 0 -3) "\\top") 
  ((1 0 -2) "\\dashv") 
  ((1 0 -1) "\\left" "\\bigl" "\\Bigl" "\\biggl" "\\Biggl") 
  ((1 0 1) "\\right" "\\bigr" "\\Bigr" "\\biggr" "\\Biggr") 
  ((1 0 2) "\\vdash") 
  ((1 0 3) "\\bot" "\\models") 

;;;;;;;; 1-1 
  ((1 1 -2) "\\pentagonblack" "\\pentagon") 
  ((1 1 0) "\\angle") 
  ((1 1 2) "\\mdlgblksquare" "\\mdlgwhtsquare") 
  ((1 1 3) "\\ell") 
  ((1 1 4) "\\hexagonblack" "\\hexagon") 
  ((1 1 6) "\\exclamdown") 

;;;;;;; 3 
;;;;;;;; 3--1 
  ((3 -1 -5) "\\neq") 
  ((3 -1 -3) "\\ominus" "\\boxminus") 
  ((3 -1 3) "\\oplus" "\\boxplus") 
  ((3 -1 4) "\\cdot" "\\star" "\\bullet") 

;;;;;;; 4 
;;;;;;;; 4--1 
  ((4 -1 -4) "\\mdlgblkcircle" "\\lgwhtcircle" "\\bigcirc") 
  ((4 -1 -2) "\\simeq") 
  ((4 -1 2) "\\equiv" "\\cong") 

;;;;;;;; 4-0 
  ((4 0 -4) "\\lcurvyangle") 
  ((4 0 -3) "\\llparenthesis") 
  ((4 0 -1) "\\&") 
  ((4 0 3) "\\{") 
  ((4 0 4) "\\left[") 
  ((4 0 4) "\\right[") 

;;;;;;;; 4-1 
  ((4 1 -4) "\\rcurvyangle") 
  ((4 1 -3) "\\rrparenthesis") 
  ((4 1 0) "\\pm" "\\mp") 
  ((4 1 2) "\\dagger") 
  ((4 1 3) "\\}") 
  ((4 1 5) "\\otimes" "\\boxtimes") 

;;;;;;; 5 
;;;;;;;; 5--1 
  ((5 -1 -3) "\\Uparrow" "\\Uuparrow") 
  ((5 -1 -2) "\\Leftarrow" "\\Lleftarrow") 
  ((5 -1 2) "\\Rightarrow" "\\Rrightarrow") 

;;;;;;;; 5-0 
  ((5 0 -3) "\\cap" "\\bigcap") 
  ((5 0 -2) "\\subseteq" "\\subset") 
  ((5 0 2) "\\supseteq" "\\supset") 
  ((5 0 3) "\\cup" "\\bigcup") 

;;;;;;;; 5-1 
  ((5 1 0) "\\Leftrightarrow" "\\Lleftrightarrow") 
  ((5 1 3) "\\Downarrow" "\\Ddownarrow") 

;;;;;;; 6 
;;;;;;;; 6--1 
  ((6 -1 -3) "\\hookuparrow" "\\twoheaduparrow" "\\upuparrows") 
  ((6 -1 -2) "\\hookleftarrow" "\\twoheadleftarrow" "\\leftleftarrows") 
  ((6 -1 2) "\\hookrightarrow" "\\twoheadrightarrow" "\\rightrightarrows") 
  ((6 -1 3) "\\hookdownarrow" "\\twoheaddownarrow" "\\downdownarrows") 

;;;;;;;; 6-0 
  ((6 0 -3) "\\bigwedge" "\\curlywedge") 
  ((6 0 -2) "\\leq" "\\preceq" "\\precsim") 
  ((6 0 2) "\\geq" "\\succeq" "\\succsim") 
  ((6 0 3) "\\bigvee" "\\curlyvee"))

(provide 'd-emacs-cdlatex-special-math-symbol-bindlists)
;;; d-emacs-cdlatex-special-math-symbol-bindlists.el ends here
