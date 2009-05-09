;; physics-algebra-rules.cl -- sets up the grammar for physics algebra
;; Author(s):
;;  Linwood H. Taylor (lht) <lht@lzri.com>
;;  Collin Lynch (cl) <CollinL@pitt.edu>
;;   3 April 2001 - (lht) -- created from previous work on ANDES2 parsing
;;; Modifications by Anders Weinstein 2002-2008
;;; Modifications by Brett van de Sande, 2005-2008
;;; Copyright 2009 by Kurt Vanlehn and Brett van de Sande
;;;  This file is part of the Andes Intelligent Tutor Stystem.
;;;
;;;  The Andes Intelligent Tutor System is free software: you can redistribute
;;;  it and/or modify it under the terms of the GNU Lesser General Public 
;;;  License as published by the Free Software Foundation, either version 3 
;;;  of the License, or (at your option) any later version.
;;;
;;;  The Andes Solver is distributed in the hope that it will be useful,
;;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;  GNU Lesser General Public License for more details.
;;;
;;;  You should have received a copy of the GNU Lesser General Public License
;;;  along with the Andes Intelligent Tutor System.  If not, see 
;;;  <http:;;;www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modified:
;;   8 April 2001 - (lht) -- version finalized and source cleaned
;;  10 April 2001 - (lht) -- added cull to end of file (helps time issues)
;;  15 May 2001 - (lht) -- new dnum treatment and scinum definition
;;  12 June 2003 - (cl) -- Added declarations to handle compiler warning
;;   about Physics-Algebra-Rules-Initialize.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; initialize rules for andes physics equations grammar
(defun physics-algebra-rules-initialize ()
  ;;
  ;; first make grammar for identifier terminals
  ;;
  (setf **identifier-grammar** nil)

  ;; now add the terminal rules for digits
  (grammar-add-terminal '**identifier-grammar** 'digit0 #\0)
  (grammar-add-terminal '**identifier-grammar** 'digit1 #\1)
  (grammar-add-terminal '**identifier-grammar** 'digit2 #\2)
  (grammar-add-terminal '**identifier-grammar** 'digit3 #\3)
  (grammar-add-terminal '**identifier-grammar** 'digit4 #\4)
  (grammar-add-terminal '**identifier-grammar** 'digit5 #\5)
  (grammar-add-terminal '**identifier-grammar** 'digit6 #\6)
  (grammar-add-terminal '**identifier-grammar** 'digit7 #\7)
  (grammar-add-terminal '**identifier-grammar** 'digit8 #\8)
  (grammar-add-terminal '**identifier-grammar** 'digit9 #\9)
  
  ;; terminals for lower-case letters
  (grammar-add-terminal '**identifier-grammar** 'la #\a)
  (grammar-add-terminal '**identifier-grammar** 'lb #\b)
  (grammar-add-terminal '**identifier-grammar** 'lc #\c)
  (grammar-add-terminal '**identifier-grammar** 'ld #\d)
  (grammar-add-terminal '**identifier-grammar** 'le #\e)
  (grammar-add-terminal '**identifier-grammar** 'lf #\f)
  (grammar-add-terminal '**identifier-grammar** 'lg #\g)
  (grammar-add-terminal '**identifier-grammar** 'lh #\h)
  (grammar-add-terminal '**identifier-grammar** 'li #\i)
  (grammar-add-terminal '**identifier-grammar** 'lj #\j)
  (grammar-add-terminal '**identifier-grammar** 'lk #\k)
  (grammar-add-terminal '**identifier-grammar** 'll #\l)
  (grammar-add-terminal '**identifier-grammar** 'lm #\m)
  (grammar-add-terminal '**identifier-grammar** 'ln #\n)
  (grammar-add-terminal '**identifier-grammar** 'lo #\o)
  (grammar-add-terminal '**identifier-grammar** 'lp #\p)
  (grammar-add-terminal '**identifier-grammar** 'lq #\q)
  (grammar-add-terminal '**identifier-grammar** 'lr #\r)
  (grammar-add-terminal '**identifier-grammar** 'ls #\s)
  (grammar-add-terminal '**identifier-grammar** 'lt #\t)
  (grammar-add-terminal '**identifier-grammar** 'lu #\u)
  (grammar-add-terminal '**identifier-grammar** 'lv #\v)
  (grammar-add-terminal '**identifier-grammar** 'lw #\w)
  (grammar-add-terminal '**identifier-grammar** 'lx #\x)
  (grammar-add-terminal '**identifier-grammar** 'ly #\y)
  (grammar-add-terminal '**identifier-grammar** 'lz #\z)

  ;; terminals for upper-case letters
  (grammar-add-terminal '**identifier-grammar** 'ua #\A)
  (grammar-add-terminal '**identifier-grammar** 'ub #\B)
  (grammar-add-terminal '**identifier-grammar** 'uc #\C)
  (grammar-add-terminal '**identifier-grammar** 'ud #\D)
  (grammar-add-terminal '**identifier-grammar** 'ue #\E)
  (grammar-add-terminal '**identifier-grammar** 'uf #\F)
  (grammar-add-terminal '**identifier-grammar** 'ug #\G)
  (grammar-add-terminal '**identifier-grammar** 'uh #\H)
  (grammar-add-terminal '**identifier-grammar** 'ui #\I)
  (grammar-add-terminal '**identifier-grammar** 'uj #\J)
  (grammar-add-terminal '**identifier-grammar** 'uk #\K)
  (grammar-add-terminal '**identifier-grammar** 'ul #\L)
  (grammar-add-terminal '**identifier-grammar** 'um #\M)
  (grammar-add-terminal '**identifier-grammar** 'un #\N)
  (grammar-add-terminal '**identifier-grammar** 'uo #\O)
  (grammar-add-terminal '**identifier-grammar** 'up #\P)
  (grammar-add-terminal '**identifier-grammar** 'uq #\Q)
  (grammar-add-terminal '**identifier-grammar** 'ur #\R)
  (grammar-add-terminal '**identifier-grammar** 'us #\S)
  (grammar-add-terminal '**identifier-grammar** 'ut #\T)
  (grammar-add-terminal '**identifier-grammar** 'uu #\U)
  (grammar-add-terminal '**identifier-grammar** 'uv #\V)
  (grammar-add-terminal '**identifier-grammar** 'uw #\W)
  (grammar-add-terminal '**identifier-grammar** 'ux #\X)
  (grammar-add-terminal '**identifier-grammar** 'uy #\Y)
  (grammar-add-terminal '**identifier-grammar** 'uz #\Z)

  ;; terminals for characters common to andes physics identifiers
  (grammar-add-terminal '**identifier-grammar** 'underscore #\_)
  (grammar-add-terminal '**identifier-grammar** 'dash #\-)
  (grammar-add-terminal '**identifier-grammar** 'period #\.)
  (grammar-add-terminal '**identifier-grammar** 'dollars #\$)

  ;; operators placed here because units can have these
  (grammar-add-terminal '**identifier-grammar** 'plus #\+)
  (grammar-add-terminal '**identifier-grammar** 'times #\*)
  (grammar-add-terminal '**identifier-grammar** 'divide #\/)
  (grammar-add-terminal '**identifier-grammar** 'raised #\^)
  ;; 'minus already exists as the rule 'dash
	      
  ;;
  ;; creation and initialization of the base andes physics grammar
  ;;
  (setf **common-grammar** nil)
  (grammar-add-grammar '**common-grammar** **identifier-grammar**)

  ;;
  ;; add some more terminals
  ;;

  ;; white space characters
  (grammar-add-terminal '**common-grammar** 'space '(#\Space #\Return #\NewLine #\Page #\Tab))

  ;; left and right parenthesis
  (grammar-add-terminal '**common-grammar** 'l-paren #\()
  (grammar-add-terminal '**common-grammar** 'r-paren #\))

  ;; equality
  (grammar-add-terminal '**common-grammar** 'equals #\=)

  ;;
  ;; after lexical terminals are defined let's define the non-terminal parses
  ;;

  ;; group the 'unary operators
  (grammar-add-nonterminal '**common-grammar** 'unary-op '((plus) (dash)))

  ;; only binary operators to be found in units
  (grammar-add-nonterminal '**common-grammar** 'unit-op '((times) (divide)))

  ;; binary operations
  (grammar-add-nonterminal '**common-grammar** 'bops '((plus) (dash) (unit-op) (raised)))

  ;; digits are 0-9 ... assumes base 10
  (grammar-add-nonterminal '**common-grammar** 'digit '((digit0) (digit1) (digit2) (digit3) (digit4)
						 (digit5) (digit6) (digit7) (digit8) (digit9)
						 ))

  ;; 'e lhs's for case insensitive parses ... 
  ;; like sin == Sin == SIN == SIn ..etc
  (grammar-add-nonterminal '**common-grammar** 'ea '((la) (uA)))
  (grammar-add-nonterminal '**common-grammar** 'eb '((lb) (uB)))
  (grammar-add-nonterminal '**common-grammar** 'ec '((lc) (uC)))
  (grammar-add-nonterminal '**common-grammar** 'ed '((ld) (uD)))
  (grammar-add-nonterminal '**common-grammar** 'ee '((le) (uE)))
  (grammar-add-nonterminal '**common-grammar** 'ef '((lf) (uF)))
  (grammar-add-nonterminal '**common-grammar** 'eg '((lg) (uG)))
  (grammar-add-nonterminal '**common-grammar** 'eh '((lh) (uH)))
  (grammar-add-nonterminal '**common-grammar** 'ei '((li) (uI)))
  (grammar-add-nonterminal '**common-grammar** 'ej '((lj) (uJ)))
  (grammar-add-nonterminal '**common-grammar** 'ek '((lk) (uK)))
  (grammar-add-nonterminal '**common-grammar** 'el '((ll) (uL)))
  (grammar-add-nonterminal '**common-grammar** 'em '((lm) (uM)))
  (grammar-add-nonterminal '**common-grammar** 'en '((ln) (uN)))
  (grammar-add-nonterminal '**common-grammar** 'eo '((lo) (uO)))
  (grammar-add-nonterminal '**common-grammar** 'ep '((lp) (uP)))
  (grammar-add-nonterminal '**common-grammar** 'eq '((lq) (uQ)))
  (grammar-add-nonterminal '**common-grammar** 'er '((lr) (uR)))
  (grammar-add-nonterminal '**common-grammar** 'es '((ls) (uS)))
  (grammar-add-nonterminal '**common-grammar** 'et '((lt) (uT)))
  (grammar-add-nonterminal '**common-grammar** 'eu '((lu) (uU)))
  (grammar-add-nonterminal '**common-grammar** 'ev '((lv) (uV)))
  (grammar-add-nonterminal '**common-grammar** 'ew '((lw) (uW)))
  (grammar-add-nonterminal '**common-grammar** 'ex '((lx) (uX)))
  (grammar-add-nonterminal '**common-grammar** 'ey '((ly) (uY)))
  (grammar-add-nonterminal '**common-grammar** 'ez '((lz) (uZ)))

  (grammar-add-nonterminal '**common-grammar** 'letter '((ea) (eb) (ec) (ed) (ee) (ef) (eg) (eh) (ei)
						  (ej) (ek) (el) (em) (en) (eo) (ep) (eq) (er)
						  (es) (et) (eu) (ev) (ew) (ex) (ey) (ez)))
  ;; white space
  (grammar-add-nonterminal '**common-grammar** 'wspace (expand-wild-symbols '(space ?wspace)))

  ;; integers
  (grammar-add-nonterminal '**common-grammar** 'integer (expand-wild-symbols '(digit ?integer)))

  ;; fixed point numbers
  (grammar-add-nonterminal '**common-grammar** 'fpnum '((period integer)))
  (grammar-add-nonterminal '**common-grammar** 'fpnum (expand-wild-symbols '(integer period ?integer)))

  ;; floating point numbers (scientific notation
  (grammar-add-nonterminal '**common-grammar** 'exponent (expand-wild-symbols '(eE ?unary-op integer)))
  (grammar-add-nonterminal '**common-grammar** 'exponent (expand-wild-symbols '(eE ?unary-op fpnum)))

  (grammar-add-nonterminal '**common-grammar** 'scinum
			   (expand-wild-symbols '(fpnum ?wspace exponent)))
  (grammar-add-nonterminal '**common-grammar** 'scinum
			   (expand-wild-symbols '(integer ?wspace exponent)))

  ;; unit prefixes
  (grammar-add-special '**common-grammar** 'unit-prefix "a" nil **identifier-grammar**)
  (grammar-add-special '**common-grammar** 'unit-prefix "f" nil **identifier-grammar**)
  (grammar-add-special '**common-grammar** 'unit-prefix "p" nil **identifier-grammar**)
  (grammar-add-special '**common-grammar** 'unit-prefix "n" nil **identifier-grammar**)
  (grammar-add-special '**common-grammar** 'unit-prefix "mu" nil **identifier-grammar**)
  (grammar-add-special '**common-grammar** 'unit-prefix "$m" nil **identifier-grammar**)
  (grammar-add-special '**common-grammar** 'unit-prefix "m" nil **identifier-grammar**)
  (grammar-add-special '**common-grammar** 'unit-prefix "c" nil **identifier-grammar**)
  (grammar-add-special '**common-grammar** 'unit-prefix "d" nil **identifier-grammar**)
  (grammar-add-special '**common-grammar** 'unit-prefix "k" nil **identifier-grammar**)
  (grammar-add-special '**common-grammar** 'unit-prefix "K" nil **identifier-grammar**)
  (grammar-add-special '**common-grammar** 'unit-prefix "M" nil **identifier-grammar**)
  (grammar-add-special '**common-grammar** 'unit-prefix "G" nil **identifier-grammar**)
  (grammar-add-special '**common-grammar** 'unit-prefix "T" nil **identifier-grammar**)
  (grammar-add-special '**common-grammar** 'unit-prefix "P" nil **identifier-grammar**)
  (grammar-add-special '**common-grammar** 'unit-prefix "E" nil **identifier-grammar**)
  ;;(grammar-add-special '**common-grammar** 'unit-prefix "atto" nil **identifier-grammar**)
  ;;(grammar-add-special '**common-grammar** 'unit-prefix "femto" nil **identifier-grammar**)
  ;;(grammar-add-special '**common-grammar** 'unit-prefix "pico" nil **identifier-grammar**)
  ;;(grammar-add-special '**common-grammar** 'unit-prefix "nano" nil **identifier-grammar**)
  ;;(grammar-add-special '**common-grammar** 'unit-prefix "micro" nil **identifier-grammar**)
  ;;(grammar-add-special '**common-grammar** 'unit-prefix "milli" nil **identifier-grammar**)
  ;;(grammar-add-special '**common-grammar** 'unit-prefix "centi" nil **identifier-grammar**)
  ;;(grammar-add-special '**common-grammar** 'unit-prefix "deci" nil **identifier-grammar**)
  ;;(grammar-add-special '**common-grammar** 'unit-prefix "da" nil **identifier-grammar**)
  ;;(grammar-add-special '**common-grammar** 'unit-prefix "deka" nil **identifier-grammar**)
  ;;(grammar-add-special '**common-grammar** 'unit-prefix "h" nil **identifier-grammar**)
  ;;(grammar-add-special '**common-grammar** 'unit-prefix "hecto" nil **identifier-grammar**)
  ;;(grammar-add-special '**common-grammar** 'unit-prefix "kilo" nil **identifier-grammar**)
  ;;(grammar-add-special '**common-grammar** 'unit-prefix "mega" nil **identifier-grammar**)
  ;;(grammar-add-special '**common-grammar** 'unit-prefix "giga" nil **identifier-grammar**)
  ;;(grammar-add-special '**common-grammar** 'unit-prefix "tera" nil **identifier-grammar**)
  ;;(grammar-add-special '**common-grammar** 'unit-prefix "peta" nil **identifier-grammar**)
  ;;(grammar-add-special '**common-grammar** 'unit-prefix "exa" nil **identifier-grammar**)
  
  ;; unit names
  (grammar-add-special '**common-grammar** 'unit-name "m" nil **identifier-grammar**)
;;  (grammar-add-special '**common-grammar** 'unit-rname "kg" nil **identifier-grammar**)
  (grammar-add-special '**common-grammar** 'unit-name "s" nil **identifier-grammar**)
  (grammar-add-special '**common-grammar** 'unit-name "C" nil **identifier-grammar**)
  (grammar-add-special '**common-grammar** 'unit-name "K" nil **identifier-grammar**)
  (grammar-add-special '**common-grammar** 'unit-name "g" nil **identifier-grammar**)
  (grammar-add-special '**common-grammar** 'unit-name "N" nil **identifier-grammar**)
  (grammar-add-special '**common-grammar** 'unit-name "J" nil **identifier-grammar**)
  (grammar-add-special '**common-grammar** 'unit-name "V" nil **identifier-grammar**)
  (grammar-add-special '**common-grammar** 'unit-name "A" nil **identifier-grammar**)
  (grammar-add-special '**common-grammar** 'unit-name "T" nil **identifier-grammar**)
  (grammar-add-special '**common-grammar** 'unit-name "Wb" nil **identifier-grammar**)
  (grammar-add-special '**common-grammar** 'unit-name "ohm" nil **identifier-grammar**)
  (grammar-add-special '**common-grammar** 'unit-name "$W" nil **identifier-grammar**)
  (grammar-add-special '**common-grammar** 'unit-name "Hz" nil **identifier-grammar**)
  (grammar-add-special '**common-grammar** 'unit-name "Pa" nil **identifier-grammar**)
  (grammar-add-special '**common-grammar** 'unit-name "F" nil **identifier-grammar**)
  (grammar-add-special '**common-grammar** 'unit-name "H" nil **identifier-grammar**)
  (grammar-add-special '**common-grammar** 'unit-name "W" nil **identifier-grammar**)
  (grammar-add-special '**common-grammar** 'unit-name "dB" nil **identifier-grammar**)
  ;;(grammar-add-special '**common-grammar** 'unit-rname "m/s" nil **identifier-grammar**)
  ;;(grammar-add-special '**common-grammar** 'unit-rname "m/s^2" nil **identifier-grammar**)
  ;;(grammar-add-special '**common-grammar** 'unit-rname "N.m" nil **identifier-grammar**)
  ;;(grammar-add-special '**common-grammar** 'unit-rname "J.s" nil **identifier-grammar**)
  ;;(grammar-add-special '**common-grammar** 'unit-rname "kg.m^2" nil **identifier-grammar**)
  ;;(grammar-add-special '**common-grammar** 'unit-rname "N/m" nil **identifier-grammar**)
  ;;(grammar-add-special '**common-grammar** 'unit-rname "N.s/m^2" nil **identifier-grammar**)
  ;;(grammar-add-special '**common-grammar** 'unit-rname "N/m^2" nil **identifier-grammar**)
  (grammar-add-special '**common-grammar** 'unit-name "deg" nil **identifier-grammar**)
  (grammar-add-special '**common-grammar** 'unit-name "rad" nil **identifier-grammar**)
  (grammar-add-special '**common-grammar** 'unit-rname "rev" nil **identifier-grammar**)
  (grammar-add-special '**common-grammar** 'unit-rname "lb" nil **identifier-grammar**)
  (grammar-add-special '**common-grammar** 'unit-rname "day" nil **identifier-grammar**)
  (grammar-add-special '**common-grammar** 'unit-rname "hr" nil **identifier-grammar**)
  (grammar-add-special '**common-grammar** 'unit-rname "h" nil **identifier-grammar**)
  (grammar-add-special '**common-grammar** 'unit-rname "min" nil **identifier-grammar**)
  (grammar-add-special '**common-grammar** 'unit-rname "yr" nil **identifier-grammar**)
  (grammar-add-special '**common-grammar** 'unit-name "liter" nil **identifier-grammar**)
  (grammar-add-special '**common-grammar** 'unit-rname "ft" nil **identifier-grammar**)
  (grammar-add-special '**common-grammar** 'unit-rname "in" nil **identifier-grammar**)
  (grammar-add-special '**common-grammar** 'unit-rname "mi" nil **identifier-grammar**)
  (grammar-add-special '**common-grammar** 'unit-rname "slug" nil **identifier-grammar**)
  (grammar-add-special '**common-grammar** 'unit-rname "gal" nil **identifier-grammar**)
  (grammar-add-special '**common-grammar** 'unit-rname "u" nil **identifier-grammar**)
  (grammar-add-special '**common-grammar** 'unit-name "eV" nil **identifier-grammar**)
  (grammar-add-special '**common-grammar** 'unit-name "dyne" nil **identifier-grammar**)
  (grammar-add-special '**common-grammar** 'unit-name "erg" nil **identifier-grammar**)
  (grammar-add-special '**common-grammar** 'unit-name "cal" nil **identifier-grammar**)
  (grammar-add-special '**common-grammar** 'unit-rname "lbs" nil **identifier-grammar**)
  (grammar-add-special '**common-grammar** 'unit-rname "ozW" nil **identifier-grammar**)
  (grammar-add-special '**common-grammar** 'unit-rname "ozVUS" nil **identifier-grammar**)
  (grammar-add-special '**common-grammar** 'unit-rname "knot" nil **identifier-grammar**)
  (grammar-add-special '**common-grammar** 'unit-rname "rpm" nil **identifier-grammar**)
  (grammar-add-special '**common-grammar** 'unit-rname "psi" nil **identifier-grammar**)
  (grammar-add-special '**common-grammar** 'unit-rname "block" nil **identifier-grammar**)
  (grammar-add-special '**common-grammar** 'unit-rname "blocks" nil **identifier-grammar**)

  ;;(grammar-add-special '**common-grammar** 'unit-name "meter" nil **identifier-grammar**)
  ;;(grammar-add-special '**common-grammar** 'unit-name "meters" nil **identifier-grammar**)
  ;;(grammar-add-special '**common-grammar** 'unit-name "gram" nil **identifier-grammar**)
  ;;(grammar-add-special '**common-grammar** 'unit-name "grams" nil **identifier-grammar**)
  ;;(grammar-add-special '**common-grammar** 'unit-name "second" nil **identifier-grammar**)
  ;;(grammar-add-special '**common-grammar** 'unit-name "seconds" nil **identifier-grammar**)
  ;;(grammar-add-special '**common-grammar** 'unit-name "amp" nil **identifier-grammar**)
  ;;(grammar-add-special '**common-grammar** 'unit-name "Ampere" nil **identifier-grammar**)
  ;;(grammar-add-special '**common-grammar** 'unit-name "Coulomb" nil **identifier-grammar**)
  ;;(grammar-add-special '**common-grammar** 'unit-name "Newton" nil **identifier-grammar**)
  ;;(grammar-add-special '**common-grammar** 'unit-name "Joule" nil **identifier-grammar**)
  ;;(grammar-add-special '**common-grammar** 'unit-name "degrees Kelvin" nil **common-grammar**) ;; ???
  ;;(grammar-add-special '**common-grammar** 'unit-name "volt" nil **identifier-grammar**)
  ;;(grammar-add-special '**common-grammar** 'unit-name "tesla" nil **identifier-grammar**)
  ;;(grammar-add-special '**common-grammar** 'unit-name "weber" nil **identifier-grammar**)
  ;;(grammar-add-special '**common-grammar** 'unit-name "Hertz" nil **identifier-grammar**)
  ;;(grammar-add-special '**common-grammar** 'unit-name "Pascal" nil **identifier-grammar**)
  ;;(grammar-add-special '**common-grammar** 'unit-name "Farad" nil **identifier-grammar**)
  ;;(grammar-add-special '**common-grammar** 'unit-name "Henry" nil **identifier-grammar**)
  ;;(grammar-add-special '**common-grammar** 'unit-name "Watt" nil **identifier-grammar**)
  ;;(grammar-add-special '**common-grammar** 'unit-name "degree" nil **identifier-grammar**)
  ;;(grammar-add-special '**common-grammar** 'unit-name "radian" nil **identifier-grammar**)
  
  ;;(grammar-add-special '**common-grammar** 'unit-rname "mol" nil **identifier-grammar**)
  ;;(grammar-add-special '**common-grammar** 'unit-rname "mole" nil **identifier-grammar**)
  ;;(grammar-add-special '**common-grammar** 'unit-rname "revolution" nil **identifier-grammar**)
  ;;(grammar-add-special '**common-grammar** 'unit-rname "inch" nil **identifier-grammar**)
  ;;(grammar-add-special '**common-grammar** 'unit-rname "foot" nil **identifier-grammar**)
  ;;(grammar-add-special '**common-grammar** 'unit-rname "mile" nil **identifier-grammar**)
  ;;(grammar-add-special '**common-grammar** 'unit-rname "Atomic mass" nil **common-grammar**) ;; ???
  ;;(grammar-add-special '**common-grammar** 'unit-rname "minute" nil **identifier-grammar**)
  ;;(grammar-add-special '**common-grammar** 'unit-rname "hour" nil **identifier-grammar**)
  ;;(grammar-add-special '**common-grammar** 'unit-rname "year" nil **identifier-grammar**)
  ;;(grammar-add-special '**common-grammar** 'unit-rname "pound" nil **identifier-grammar**)
  ;;(grammar-add-special '**common-grammar** 'unit-rname "pounds" nil **identifier-grammar**)
  ;;(grammar-add-special '**common-grammar** 'unit-rname "electron Volt" nil **common-grammar**) ;; ???
  ;;(grammar-add-special '**common-grammar** 'unit-rname "calory" nil **identifier-grammar**)
  ;;(grammar-add-special '**common-grammar** 'unit-rname "Btu" nil **identifier-grammar**)
  ;;(grammar-add-special '**common-grammar** 'unit-rname "British Thermal Unit" nil **common-grammar**) ;; ???
  ;;(grammar-add-special '**common-grammar** 'unit-rname "Wh" nil **identifier-grammar**)
  ;;(grammar-add-special '**common-grammar** 'unit-rname "watt-hour" nil **identifier-grammar**)
  ;;(grammar-add-special '**common-grammar** 'unit-rname "atm" nil **identifier-grammar**)
  ;;(grammar-add-special '**common-grammar** 'unit-rname "atmosphere" nil **identifier-grammar**)
  ;;(grammar-add-special '**common-grammar** 'unit-rname "degrees Fahrenheit" nil **common-grammar**) ;; ???

  ;; units ... NOTE: no white-space
  (grammar-add-nonterminal '**common-grammar** 'a-unit
			   (expand-wild-symbols '(?unit-prefix unit-name)))
  (grammar-add-nonterminal '**common-grammar** 'a-unit '((unit-rname)))

  (grammar-add-nonterminal '**common-grammar** 'a-unit '((a-unit raised integer)))
  (grammar-add-nonterminal '**common-grammar** 'a-unit '((a-unit raised unary-op integer)))
  (grammar-add-nonterminal '**common-grammar** 'a-unit '((l-paren a-unit r-paren)))
  
  (grammar-add-nonterminal '**common-grammar** 'unit '((a-unit)
						(a-unit unit-op unit)
						(a-unit period unit)
						(l-paren unit r-paren)
						))
  
  
  ;; function names ... NOTE: case-insensitive
  (grammar-add-nonterminal '**common-grammar** 'func '((es ei en)
						(ec eo es)
						(et ea en)
						(el eo eg digit1 digit0)
						(el en)
						(ea eb es)
						(es eq er et)
						(ee ex ep)
						(unary-op)
						))
  
  ;; variable/identifiers
  (grammar-add-nonterminal '**common-grammar** 'rest-of-unknown 
			   (expand-wild-symbols '(letter ?rest-of-unknown)))
  (grammar-add-nonterminal '**common-grammar** 'rest-of-unknown 
			   (expand-wild-symbols '(dollars letter ?rest-of-unknown)))
  (grammar-add-nonterminal '**common-grammar** 'rest-of-unknown 
			   (expand-wild-symbols '(digit ?rest-of-unknown)))
  (grammar-add-nonterminal '**common-grammar** 'rest-of-unknown 
			   (expand-wild-symbols '(underscore ?rest-of-unknown)))
  
  (grammar-add-nonterminal '**common-grammar** 'unknown
			   (expand-wild-symbols '(letter ?rest-of-unknown)))
  (grammar-add-nonterminal '**common-grammar** 'unknown
			   (expand-wild-symbols '(dollars letter ?rest-of-unknown)))

  ;;
  ;; rules are juggled to avoid forward references
  ;;
  
  ;; define number
  (grammar-add-nonterminal '**common-grammar** 'number '((integer)
						  (fpnum)
						  (scinum)
						  (dollars lp)
						  ))
  
  ;; arithmetic deals with numbers and operations on numbers
  (grammar-add-nonterminal '**common-grammar** 'p-arithmetic
			   '((number)))

  (grammar-add-nonterminal '**common-grammar** 'arithmetic
			   '((p-arithmetic)))
  
  (grammar-add-nonterminal '**common-grammar** 'p-arithmetic
			   (expand-wild-symbols '(l-paren ?wspace arithmetic ?wspace r-paren)))

  (grammar-add-nonterminal '**common-grammar** 'arithmetic
  			   (expand-wild-symbols '(arithmetic ?wspace bops ?wspace p-arithmetic)))


  (grammar-add-nonterminal '**common-grammar** 'dnum
			   (expand-wild-symbols '(arithmetic ?wspace unit)))

  ;; funarg are valid arguments to functions
  (grammar-add-nonterminal '**common-grammar** 'funarg '((dnum)
						  (unknown)))
  
  ;; funcall are functions with one argument
  (grammar-add-nonterminal '**common-grammar** 'funcall-a
			   (expand-wild-symbols '(func ?wspace arithmetic)))
  (grammar-add-nonterminal '**common-grammar** 'p-arithmetic
			   '((funcall-a)))
  
  (grammar-add-nonterminal '**common-grammar** 'funcall
			   (expand-wild-symbols '(func ?wspace funarg)))
  (grammar-add-nonterminal '**common-grammar** 'funcall
			   (expand-wild-symbols
			    '(func ?wspace l-paren ?wspace funarg ?wspace r-paren)))
  
  (grammar-add-nonterminal '**common-grammar** 'funcall '((funcall-a)))
  
  (grammar-add-nonterminal '**common-grammar** 'funarg '((funcall)))
  
  ;; an expression is a value
  (grammar-add-nonterminal '**common-grammar** 'p-expression '((funarg)
							(arithmetic)))

  ;; funcall may have parenthesized expression as an argument
  (grammar-add-nonterminal '**common-grammar** 'funcall
			   (expand-wild-symbols '(func ?wspace p-expression)))
  
  ;; expressions continued
  (grammar-add-nonterminal '**common-grammar** 'expression '((p-expression)))
  (grammar-add-nonterminal '**common-grammar** 'expression
			   (expand-wild-symbols '(expression ?wspace bops ?wspace p-expression)))
  
  (grammar-add-nonterminal '**common-grammar** 'p-expression
			   (expand-wild-symbols '(l-paren ?wspace expression ?wspace r-paren)))
  
  ;; final in this case is an equation something equals something
  (grammar-add-nonterminal '**common-grammar** 'final
			   (expand-wild-symbols '(expression ?wspace equals ?wspace expression)))

)
