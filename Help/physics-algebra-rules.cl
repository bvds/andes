;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; physics-algebra-rules.cl -- sets up the grammar for physics algebra
;; Copyright (C) 2001 by <Linwood H. Taylor's Employer> -- All Rights Reserved.
;; Author(s):
;;  Linwood H. Taylor (lht) <lht@lzri.com>
;;  Collin Lynch (cl) <CollinL@pitt.edu>
;; Modified:
;;   3 April 2001 - (lht) -- created from previous work on ANDES2 parsing
;;   8 April 2001 - (lht) -- version finalized and source cleaned
;;  10 April 2001 - (lht) -- added cull to end of file (helps time issues)
;;  15 May 2001 - (lht) -- new dnum treatment and scinum definition
;;  12 June 2003 - (cl) -- Added declarations to handle compiler warning
;;   about Physics-Algebra-Rules-Initialize.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; initialize rules for andes physics equations grammar
(defun physics-algebra-rules-initialize ()
  (declare (special **Identifier-Grammar**))
  ;;
  ;; first make grammar for identifier terminals
  ;;
  (grammar-initialize '**identifier-grammar**)

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
  (grammar-initialize '**grammar**)

  ;; now copy rules for identifier terminals
  (grammar-add-grammar '**grammar** **identifier-grammar**)

  ;;
  ;; add some more terminals
  ;;

  ;; white space characters
  (grammar-add-terminal '**grammar** 'space '(#\Space #\Return #\NewLine #\Page #\Tab))

  ;; left and right parenthesis
  (grammar-add-terminal '**grammar** 'l-paren #\()
  (grammar-add-terminal '**grammar** 'r-paren #\))

  ;; equality
  (grammar-add-terminal '**grammar** 'equals #\=)

  ;;
  ;; after lexical terminals are defined let's define the non-terminal parses
  ;;

  ;; group the 'unary operators
  (grammar-add-nonterminal '**grammar** 'unary-op '((plus) (dash)))

  ;; only binary operators to be found in units
  (grammar-add-nonterminal '**grammar** 'unit-op '((times) (divide)))

  ;; binary operations
  (grammar-add-nonterminal '**grammar** 'bops '((plus) (dash) (unit-op) (raised)))

  ;; digits are 0-9 ... assumes base 10
  (grammar-add-nonterminal '**grammar** 'digit '((digit0) (digit1) (digit2) (digit3) (digit4)
						 (digit5) (digit6) (digit7) (digit8) (digit9)
						 ))

  ;; 'e lhs's for case insensitive parses ... 
  ;; like sin == Sin == SIN == SIn ..etc
  (grammar-add-nonterminal '**grammar** 'ea '((la) (uA)))
  (grammar-add-nonterminal '**grammar** 'eb '((lb) (uB)))
  (grammar-add-nonterminal '**grammar** 'ec '((lc) (uC)))
  (grammar-add-nonterminal '**grammar** 'ed '((ld) (uD)))
  (grammar-add-nonterminal '**grammar** 'ee '((le) (uE)))
  (grammar-add-nonterminal '**grammar** 'ef '((lf) (uF)))
  (grammar-add-nonterminal '**grammar** 'eg '((lg) (uG)))
  (grammar-add-nonterminal '**grammar** 'eh '((lh) (uH)))
  (grammar-add-nonterminal '**grammar** 'ei '((li) (uI)))
  (grammar-add-nonterminal '**grammar** 'ej '((lj) (uJ)))
  (grammar-add-nonterminal '**grammar** 'ek '((lk) (uK)))
  (grammar-add-nonterminal '**grammar** 'el '((ll) (uL)))
  (grammar-add-nonterminal '**grammar** 'em '((lm) (uM)))
  (grammar-add-nonterminal '**grammar** 'en '((ln) (uN)))
  (grammar-add-nonterminal '**grammar** 'eo '((lo) (uO)))
  (grammar-add-nonterminal '**grammar** 'ep '((lp) (uP)))
  (grammar-add-nonterminal '**grammar** 'eq '((lq) (uQ)))
  (grammar-add-nonterminal '**grammar** 'er '((lr) (uR)))
  (grammar-add-nonterminal '**grammar** 'es '((ls) (uS)))
  (grammar-add-nonterminal '**grammar** 'et '((lt) (uT)))
  (grammar-add-nonterminal '**grammar** 'eu '((lu) (uU)))
  (grammar-add-nonterminal '**grammar** 'ev '((lv) (uV)))
  (grammar-add-nonterminal '**grammar** 'ew '((lw) (uW)))
  (grammar-add-nonterminal '**grammar** 'ex '((lx) (uX)))
  (grammar-add-nonterminal '**grammar** 'ey '((ly) (uY)))
  (grammar-add-nonterminal '**grammar** 'ez '((lz) (uZ)))

  (grammar-add-nonterminal '**grammar** 'letter '((ea) (eb) (ec) (ed) (ee) (ef) (eg) (eh) (ei)
						  (ej) (ek) (el) (em) (en) (eo) (ep) (eq) (er)
						  (es) (et) (eu) (ev) (ew) (ex) (ey) (ez)))
  ;; white space
  (grammar-add-nonterminal '**grammar** 'wspace (expand-wild-symbols '(space ?wspace)))

  ;; integers
  (grammar-add-nonterminal '**grammar** 'integer (expand-wild-symbols '(digit ?integer)))

  ;; fixed point numbers
  (grammar-add-nonterminal '**grammar** 'fpnum '((period integer)))
  (grammar-add-nonterminal '**grammar** 'fpnum (expand-wild-symbols '(integer period ?integer)))

  ;; floating point numbers (scientific notation
  (grammar-add-nonterminal '**grammar** 'exponent (expand-wild-symbols '(eE ?unary-op integer)))
  (grammar-add-nonterminal '**grammar** 'exponent (expand-wild-symbols '(eE ?unary-op fpnum)))

  (grammar-add-nonterminal '**grammar** 'scinum
			   (expand-wild-symbols '(fpnum ?wspace exponent)))
  (grammar-add-nonterminal '**grammar** 'scinum
			   (expand-wild-symbols '(integer ?wspace exponent)))

  ;; unit prefixes
  (grammar-add-special '**grammar** 'unit-prefix "a" nil **identifier-grammar**)
  (grammar-add-special '**grammar** 'unit-prefix "f" nil **identifier-grammar**)
  (grammar-add-special '**grammar** 'unit-prefix "p" nil **identifier-grammar**)
  (grammar-add-special '**grammar** 'unit-prefix "n" nil **identifier-grammar**)
  (grammar-add-special '**grammar** 'unit-prefix "mu" nil **identifier-grammar**)
  (grammar-add-special '**grammar** 'unit-prefix "$m" nil **identifier-grammar**)
  (grammar-add-special '**grammar** 'unit-prefix "m" nil **identifier-grammar**)
  (grammar-add-special '**grammar** 'unit-prefix "c" nil **identifier-grammar**)
  (grammar-add-special '**grammar** 'unit-prefix "d" nil **identifier-grammar**)
  (grammar-add-special '**grammar** 'unit-prefix "k" nil **identifier-grammar**)
  (grammar-add-special '**grammar** 'unit-prefix "K" nil **identifier-grammar**)
  (grammar-add-special '**grammar** 'unit-prefix "M" nil **identifier-grammar**)
  (grammar-add-special '**grammar** 'unit-prefix "G" nil **identifier-grammar**)
  (grammar-add-special '**grammar** 'unit-prefix "T" nil **identifier-grammar**)
  (grammar-add-special '**grammar** 'unit-prefix "P" nil **identifier-grammar**)
  (grammar-add-special '**grammar** 'unit-prefix "E" nil **identifier-grammar**)
  ;;(grammar-add-special '**grammar** 'unit-prefix "atto" nil **identifier-grammar**)
  ;;(grammar-add-special '**grammar** 'unit-prefix "femto" nil **identifier-grammar**)
  ;;(grammar-add-special '**grammar** 'unit-prefix "pico" nil **identifier-grammar**)
  ;;(grammar-add-special '**grammar** 'unit-prefix "nano" nil **identifier-grammar**)
  ;;(grammar-add-special '**grammar** 'unit-prefix "micro" nil **identifier-grammar**)
  ;;(grammar-add-special '**grammar** 'unit-prefix "milli" nil **identifier-grammar**)
  ;;(grammar-add-special '**grammar** 'unit-prefix "centi" nil **identifier-grammar**)
  ;;(grammar-add-special '**grammar** 'unit-prefix "deci" nil **identifier-grammar**)
  ;;(grammar-add-special '**grammar** 'unit-prefix "da" nil **identifier-grammar**)
  ;;(grammar-add-special '**grammar** 'unit-prefix "deka" nil **identifier-grammar**)
  ;;(grammar-add-special '**grammar** 'unit-prefix "h" nil **identifier-grammar**)
  ;;(grammar-add-special '**grammar** 'unit-prefix "hecto" nil **identifier-grammar**)
  ;;(grammar-add-special '**grammar** 'unit-prefix "kilo" nil **identifier-grammar**)
  ;;(grammar-add-special '**grammar** 'unit-prefix "mega" nil **identifier-grammar**)
  ;;(grammar-add-special '**grammar** 'unit-prefix "giga" nil **identifier-grammar**)
  ;;(grammar-add-special '**grammar** 'unit-prefix "tera" nil **identifier-grammar**)
  ;;(grammar-add-special '**grammar** 'unit-prefix "peta" nil **identifier-grammar**)
  ;;(grammar-add-special '**grammar** 'unit-prefix "exa" nil **identifier-grammar**)
  
  ;; unit names
  (grammar-add-special '**grammar** 'unit-name "m" nil **identifier-grammar**)
;;  (grammar-add-special '**grammar** 'unit-rname "kg" nil **identifier-grammar**)
  (grammar-add-special '**grammar** 'unit-name "s" nil **identifier-grammar**)
  (grammar-add-special '**grammar** 'unit-name "C" nil **identifier-grammar**)
  (grammar-add-special '**grammar** 'unit-name "K" nil **identifier-grammar**)
  (grammar-add-special '**grammar** 'unit-name "g" nil **identifier-grammar**)
  (grammar-add-special '**grammar** 'unit-name "N" nil **identifier-grammar**)
  (grammar-add-special '**grammar** 'unit-name "J" nil **identifier-grammar**)
  (grammar-add-special '**grammar** 'unit-name "V" nil **identifier-grammar**)
  (grammar-add-special '**grammar** 'unit-name "A" nil **identifier-grammar**)
  (grammar-add-special '**grammar** 'unit-name "T" nil **identifier-grammar**)
  (grammar-add-special '**grammar** 'unit-name "Wb" nil **identifier-grammar**)
  (grammar-add-special '**grammar** 'unit-name "ohm" nil **identifier-grammar**)
  (grammar-add-special '**grammar** 'unit-name "Hz" nil **identifier-grammar**)
  (grammar-add-special '**grammar** 'unit-name "Pa" nil **identifier-grammar**)
  (grammar-add-special '**grammar** 'unit-name "F" nil **identifier-grammar**)
  (grammar-add-special '**grammar** 'unit-name "H" nil **identifier-grammar**)
  (grammar-add-special '**grammar** 'unit-name "W" nil **identifier-grammar**)
  (grammar-add-special '**grammar** 'unit-name "dB" nil **identifier-grammar**)
  ;;(grammar-add-special '**grammar** 'unit-rname "m/s" nil **identifier-grammar**)
  ;;(grammar-add-special '**grammar** 'unit-rname "m/s^2" nil **identifier-grammar**)
  ;;(grammar-add-special '**grammar** 'unit-rname "N.m" nil **identifier-grammar**)
  ;;(grammar-add-special '**grammar** 'unit-rname "J.s" nil **identifier-grammar**)
  ;;(grammar-add-special '**grammar** 'unit-rname "kg.m^2" nil **identifier-grammar**)
  ;;(grammar-add-special '**grammar** 'unit-rname "N/m" nil **identifier-grammar**)
  ;;(grammar-add-special '**grammar** 'unit-rname "N.s/m^2" nil **identifier-grammar**)
  ;;(grammar-add-special '**grammar** 'unit-rname "N/m^2" nil **identifier-grammar**)
  (grammar-add-special '**grammar** 'unit-name "deg" nil **identifier-grammar**)
  (grammar-add-special '**grammar** 'unit-name "rad" nil **identifier-grammar**)
  (grammar-add-special '**grammar** 'unit-rname "rev" nil **identifier-grammar**)
  (grammar-add-special '**grammar** 'unit-rname "lb" nil **identifier-grammar**)
  (grammar-add-special '**grammar** 'unit-rname "day" nil **identifier-grammar**)
  (grammar-add-special '**grammar** 'unit-rname "hr" nil **identifier-grammar**)
  (grammar-add-special '**grammar** 'unit-rname "h" nil **identifier-grammar**)
  (grammar-add-special '**grammar** 'unit-rname "min" nil **identifier-grammar**)
  (grammar-add-special '**grammar** 'unit-rname "yr" nil **identifier-grammar**)
  (grammar-add-special '**grammar** 'unit-name "liter" nil **identifier-grammar**)
  (grammar-add-special '**grammar** 'unit-rname "ft" nil **identifier-grammar**)
  (grammar-add-special '**grammar** 'unit-rname "in" nil **identifier-grammar**)
  (grammar-add-special '**grammar** 'unit-rname "mi" nil **identifier-grammar**)
  (grammar-add-special '**grammar** 'unit-rname "slug" nil **identifier-grammar**)
  (grammar-add-special '**grammar** 'unit-rname "gal" nil **identifier-grammar**)
  (grammar-add-special '**grammar** 'unit-rname "u" nil **identifier-grammar**)
  (grammar-add-special '**grammar** 'unit-name "eV" nil **identifier-grammar**)
  (grammar-add-special '**grammar** 'unit-name "dyne" nil **identifier-grammar**)
  (grammar-add-special '**grammar** 'unit-name "erg" nil **identifier-grammar**)
  (grammar-add-special '**grammar** 'unit-name "cal" nil **identifier-grammar**)
  (grammar-add-special '**grammar** 'unit-rname "lbs" nil **identifier-grammar**)
  (grammar-add-special '**grammar** 'unit-rname "ozW" nil **identifier-grammar**)
  (grammar-add-special '**grammar** 'unit-rname "ozVUS" nil **identifier-grammar**)
  (grammar-add-special '**grammar** 'unit-rname "knot" nil **identifier-grammar**)
  (grammar-add-special '**grammar** 'unit-rname "rpm" nil **identifier-grammar**)
  (grammar-add-special '**grammar** 'unit-rname "psi" nil **identifier-grammar**)
  (grammar-add-special '**grammar** 'unit-rname "block" nil **identifier-grammar**)
  (grammar-add-special '**grammar** 'unit-rname "blocks" nil **identifier-grammar**)

  ;;(grammar-add-special '**grammar** 'unit-name "meter" nil **identifier-grammar**)
  ;;(grammar-add-special '**grammar** 'unit-name "meters" nil **identifier-grammar**)
  ;;(grammar-add-special '**grammar** 'unit-name "gram" nil **identifier-grammar**)
  ;;(grammar-add-special '**grammar** 'unit-name "grams" nil **identifier-grammar**)
  ;;(grammar-add-special '**grammar** 'unit-name "second" nil **identifier-grammar**)
  ;;(grammar-add-special '**grammar** 'unit-name "seconds" nil **identifier-grammar**)
  ;;(grammar-add-special '**grammar** 'unit-name "amp" nil **identifier-grammar**)
  ;;(grammar-add-special '**grammar** 'unit-name "Ampere" nil **identifier-grammar**)
  ;;(grammar-add-special '**grammar** 'unit-name "Coulomb" nil **identifier-grammar**)
  ;;(grammar-add-special '**grammar** 'unit-name "Newton" nil **identifier-grammar**)
  ;;(grammar-add-special '**grammar** 'unit-name "Joule" nil **identifier-grammar**)
  ;;(grammar-add-special '**grammar** 'unit-name "degrees Kelvin" nil **grammar**) ;; ???
  ;;(grammar-add-special '**grammar** 'unit-name "volt" nil **identifier-grammar**)
  ;;(grammar-add-special '**grammar** 'unit-name "tesla" nil **identifier-grammar**)
  ;;(grammar-add-special '**grammar** 'unit-name "weber" nil **identifier-grammar**)
  ;;(grammar-add-special '**grammar** 'unit-name "Hertz" nil **identifier-grammar**)
  ;;(grammar-add-special '**grammar** 'unit-name "Pascal" nil **identifier-grammar**)
  ;;(grammar-add-special '**grammar** 'unit-name "Farad" nil **identifier-grammar**)
  ;;(grammar-add-special '**grammar** 'unit-name "Henry" nil **identifier-grammar**)
  ;;(grammar-add-special '**grammar** 'unit-name "Watt" nil **identifier-grammar**)
  ;;(grammar-add-special '**grammar** 'unit-name "degree" nil **identifier-grammar**)
  ;;(grammar-add-special '**grammar** 'unit-name "radian" nil **identifier-grammar**)
  
  ;;(grammar-add-special '**grammar** 'unit-rname "mol" nil **identifier-grammar**)
  ;;(grammar-add-special '**grammar** 'unit-rname "mole" nil **identifier-grammar**)
  ;;(grammar-add-special '**grammar** 'unit-rname "revolution" nil **identifier-grammar**)
  ;;(grammar-add-special '**grammar** 'unit-rname "inch" nil **identifier-grammar**)
  ;;(grammar-add-special '**grammar** 'unit-rname "foot" nil **identifier-grammar**)
  ;;(grammar-add-special '**grammar** 'unit-rname "mile" nil **identifier-grammar**)
  ;;(grammar-add-special '**grammar** 'unit-rname "Atomic mass" nil **grammar**) ;; ???
  ;;(grammar-add-special '**grammar** 'unit-rname "minute" nil **identifier-grammar**)
  ;;(grammar-add-special '**grammar** 'unit-rname "hour" nil **identifier-grammar**)
  ;;(grammar-add-special '**grammar** 'unit-rname "year" nil **identifier-grammar**)
  ;;(grammar-add-special '**grammar** 'unit-rname "pound" nil **identifier-grammar**)
  ;;(grammar-add-special '**grammar** 'unit-rname "pounds" nil **identifier-grammar**)
  ;;(grammar-add-special '**grammar** 'unit-rname "electron Volt" nil **grammar**) ;; ???
  ;;(grammar-add-special '**grammar** 'unit-rname "calory" nil **identifier-grammar**)
  ;;(grammar-add-special '**grammar** 'unit-rname "Btu" nil **identifier-grammar**)
  ;;(grammar-add-special '**grammar** 'unit-rname "British Thermal Unit" nil **grammar**) ;; ???
  ;;(grammar-add-special '**grammar** 'unit-rname "Wh" nil **identifier-grammar**)
  ;;(grammar-add-special '**grammar** 'unit-rname "watt-hour" nil **identifier-grammar**)
  ;;(grammar-add-special '**grammar** 'unit-rname "atm" nil **identifier-grammar**)
  ;;(grammar-add-special '**grammar** 'unit-rname "atmosphere" nil **identifier-grammar**)
  ;;(grammar-add-special '**grammar** 'unit-rname "degrees Fahrenheit" nil **grammar**) ;; ???

  ;; units ... NOTE: no white-space
  (grammar-add-nonterminal '**grammar** 'a-unit
			   (expand-wild-symbols '(?unit-prefix unit-name)))
  (grammar-add-nonterminal '**grammar** 'a-unit '((unit-rname)))

  (grammar-add-nonterminal '**grammar** 'a-unit '((a-unit raised integer)))
  (grammar-add-nonterminal '**grammar** 'a-unit '((a-unit raised unary-op integer)))
  (grammar-add-nonterminal '**grammar** 'a-unit '((l-paren a-unit r-paren)))
  
  (grammar-add-nonterminal '**grammar** 'unit '((a-unit)
						(a-unit unit-op unit)
						(a-unit period unit)
						(l-paren unit r-paren)
						))
  
  
  ;; function names ... NOTE: case-insensitive
  (grammar-add-nonterminal '**grammar** 'func '((es ei en)
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
  (grammar-add-nonterminal '**grammar** 'rest-of-unknown 
			   (expand-wild-symbols '(letter ?rest-of-unknown)))
  (grammar-add-nonterminal '**grammar** 'rest-of-unknown 
			   (expand-wild-symbols '(dollars letter ?rest-of-unknown)))
  (grammar-add-nonterminal '**grammar** 'rest-of-unknown 
			   (expand-wild-symbols '(digit ?rest-of-unknown)))
  (grammar-add-nonterminal '**grammar** 'rest-of-unknown 
			   (expand-wild-symbols '(underscore ?rest-of-unknown)))
  
  (grammar-add-nonterminal '**grammar** 'unknown
			   (expand-wild-symbols '(letter ?rest-of-unknown)))
  (grammar-add-nonterminal '**grammar** 'unknown
			   (expand-wild-symbols '(dollars letter ?rest-of-unknown)))

  ;;
  ;; rules are juggled to avoid forward references
  ;;
  
  ;; define number
  (grammar-add-nonterminal '**grammar** 'number '((integer)
						  (fpnum)
						  (scinum)
						  (dollars lp)
						  ))
  
  ;; arithmetic deals with numbers and operations on numbers
  (grammar-add-nonterminal '**grammar** 'p-arithmetic
			   '((number)))

  (grammar-add-nonterminal '**grammar** 'arithmetic
			   '((p-arithmetic)))
  
  (grammar-add-nonterminal '**grammar** 'p-arithmetic
			   (expand-wild-symbols '(l-paren ?wspace arithmetic ?wspace r-paren)))

  (grammar-add-nonterminal '**grammar** 'arithmetic
  			   (expand-wild-symbols '(arithmetic ?wspace bops ?wspace p-arithmetic)))


  (grammar-add-nonterminal '**grammar** 'dnum
			   (expand-wild-symbols '(arithmetic ?wspace unit)))

  ;; funarg are valid arguments to functions
  (grammar-add-nonterminal '**grammar** 'funarg '((dnum)
						  (unknown)))
  
  ;; funcall are functions with one argument
  (grammar-add-nonterminal '**grammar** 'funcall-a
			   (expand-wild-symbols '(func ?wspace arithmetic)))
  (grammar-add-nonterminal '**grammar** 'p-arithmetic
			   '((funcall-a)))
  
  (grammar-add-nonterminal '**grammar** 'funcall
			   (expand-wild-symbols '(func ?wspace funarg)))
  (grammar-add-nonterminal '**grammar** 'funcall
			   (expand-wild-symbols
			    '(func ?wspace l-paren ?wspace funarg ?wspace r-paren)))
  
  (grammar-add-nonterminal '**grammar** 'funcall '((funcall-a)))
  
  (grammar-add-nonterminal '**grammar** 'funarg '((funcall)))
  
  ;; an expression is a value
  (grammar-add-nonterminal '**grammar** 'p-expression '((funarg)
							(arithmetic)))

  ;; funcall may have parenthesized expression as an argument
  (grammar-add-nonterminal '**grammar** 'funcall
			   (expand-wild-symbols '(func ?wspace p-expression)))
  
  ;; expressions continued
  (grammar-add-nonterminal '**grammar** 'expression '((p-expression)))
  (grammar-add-nonterminal '**grammar** 'expression
			   (expand-wild-symbols '(expression ?wspace bops ?wspace p-expression)))
  
  (grammar-add-nonterminal '**grammar** 'p-expression
			   (expand-wild-symbols '(l-paren ?wspace expression ?wspace r-paren)))
  
  ;; final in this case is an equation something equals something
  (grammar-add-nonterminal '**grammar** 'final
			   (expand-wild-symbols '(expression ?wspace equals ?wspace expression)))

)
