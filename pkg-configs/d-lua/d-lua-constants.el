;;; d-lua-constants.el --- Constants for Daselt's LuaLaTeX module              -*- lexical-binding: t; -*-

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

;;  d-lua constants.

;;; Code:


(defconst d-lua-default-envs
  (list
   (list '"Theorem" '"thm" ?h)
   (list '"Axiom" '"axm" ?a)
   (list '"Definition" '"dfn" ?d)
   (list '"Proposition" '"prp" ?p)
   (list '"Lemma" '"lmm" ?l)
   (list '"Conjecture" '"cnj" ?c)
   (list '"Remark" '"rmk" ?r)
   (list '"Construction" '"cst" ?k)
   (list '"Variation" '"vrn" ?v)
   (list '"Warning" '"wrn" ?w)
   (list '"figure" '"fig" ?f)
   (list '"proof" '"prf" ?P)
   (list '"tikzcd" '"tic" ?z)
   (list '"Notation" '"ntn" ?n)
   (list '"Terminology" '"trm" ?T)
   (list '"Example" '"xmp" ?x)
   (list '"Examples" '"xmps" ?X)
   (list '"Counterexample" '"cxmp" ?!)
   (list '"subfigure" '"sfig" ?F)
   (list '"equation" '"eq" ?q)
   (list '"Corollary" '"crl" ?y))
  "Default environments for `d-lua'.")

(defconst d-lua-prettify-symbols-list
  (list (list "\\lcurvyangle" ?\⧼)
        (list "\\rcurvyangle" ?\⧽)
        (list "\\mdlgblksquare" ?■)
        (list "\\llparenthesis" ?\⦇)
        (list "\\rrparenthesis" ?\⦈)
        (list "\\mathbb{F}" ?𝔽)
        (list "\\bigl" ? )
        (list "\\bigr" ? )
        (list "\\Bigl" ? )
        (list "\\Bigr" ? )
        (list "\\biggl" ? )
        (list "\\biggr" ? )
        (list "\\Biggl" ? )
        (list "\\Biggr" ? )
        (list "\\mo\{∣}" ?∣)
        (list "\\mc\{∣}" ?∣)
        (list "\\mo\{∥}" ?∥)
        (list "\\mc\{∥}" ?∥)
        (list "\\mathopen\{∣}" ?∣)
        (list "\\mathclose\{∣}" ?∣)
        (list "\\mathopen\{∥}" ?∥)
        (list "\\mathclose\{∥}" ?∥))
  "List of symbols that are added to prettify-symbols-alist in LaTeX-buffers by d-latex-add-to-prettify.")

(provide 'd-lua-constants)
;;; d-lua-constants.el ends here
