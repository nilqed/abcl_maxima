;; -*- Mode:Common-Lisp;Package:mma; Base:10 -*-
;; Mock MMA (A Lisp language mathematica-like system)
;;(c) copyright 1990, 1991 by Richard J. Fateman and Univ. of California
;; see enclosed notice (file copyright)

;; this file should be loaded in at COMPILE time for every file in
;; the mma package.  It should also be loaded in (once) when the
;; mma package is set up.


;; Mathematica, on which this is based,
;; is described in S. Wolfram: Mathematica, a
;; System for Doing Mathematics By Computer, (Addison-Wesley).

;; ok, I give up.  I will use CL standard losing case insensitivity
;; so that GCL will work, at least for the moment.

;; Allegro had it right prior to the standard, but had to yield. The
;; result, that the case-sensitive stuff did not always work in later
;; releases, seems to  makes it advisable to just use case-insensitive-upper.
;; it also increases compatibility with other CLs  RJF. 10/30/97

;;;(eval-when (:compile-toplevel :load-toplevel)
;;;	   #+Allegro(cond((eq *current-case-mode* :case-sensitive-lower))
;;;		(t (excl::set-case-mode :case-sensitive-lower))))

;; obsolete (provide 'mma)

(defpackage :mma (:nicknames "MockMMA") (:use :common-lisp
					      #+allegro     :excl
;;  (shadow '(and or not plusp minusp) :mma) ;make these unbound in :mma
  
					      ))
;; keep pattern vars and maybe local function names away from user

(defpackage :loc (:nicknames "MockMMAlocal") 	    (:use :common-lisp ))
(defpackage :pat (:nicknames "MockMMApatvars") 	    (:use :common-lisp ))

(in-package :mma)
;; this next line is not enough.. need to have these macros
;; available at compile time.
;;(declaim (ftype macro ulist uconsm))
;;(load "ucons1")
(defparameter  built-in-syms
    ;; these are the atoms used by the parser, evaluator, display,
    ;; etc.  They must be the same in each of the separate packages,
    ;; and so each package should be in this package ( :mma).
	  
  '(|$Line| |Abs| |AddTo| |Alias| |Alternatives| |And| |Apply|
  |ArcCos| |ArcCosh| |ArcSec| |ArcSech| |ArcSin| |ArcSinh| |ArcTan|
  |ArcTanh| |Attributes| |Batch| |Blank| |BlankNullSequence| |BlankSequence|
  |Block| |Clear| |Comparison| |Complex| |CompoundExpression|
  |Condition| |Constant| |Cos| |Cot| |D| |Decrement| |Default|
  |Delayed| |Derivative| |Display| |DivideBy| |Do| |Dot| |E| |Equal| |Erf| |Every|
  |Exit| |Exp| |ExpIntegralEi| |Factorial| |Factorial2| |First| |Flat|
    |FullForm| |Function| |Gradef| |Greater| |GreaterEqual| |HoldAll|
    |HoldFirst| |HoldRest| |If| |Im|
  |In| |Increment| |Inequality| |Int| |Integer| |IntegerQ| |Less|
  |LessEqual| |List| |ListQ| |Log| |Map| |MapAll| |MessageName|
  |Module| |N| |NonCommutativeMultiply| |Not| |Null| |Null| |NumericQ|
  |Optional| |Or| |Orderless| |Out| |Part| |Pattern| |PatternTest|
  |Pi| |Plus| |Plus| |PossibleZeroQ| |Power| |Power| |PreDecrement|
  |PreIncrement| |Precision| |Print| |PutAppend| |Quote| |Rat| |RatExpand|
  |Rational| |Re| |Real| |Repeated| |RepeatedNull| |Replace|
  |ReplaceAll| |ReplaceRepeated| |Rest| |Rest| |Return| |Rule|
  |RuleDelayed| |SameQ| |Scan| |Sec| |Sequence| |Set| |SetAttributes|
  |SetDelayed| |Simp| |Sin| |Sinh| |Slot| |SlotSequence| |Some| |Sqrt|
  |SubtractFrom| |Table| |TagSet| |TagSetDelayed| |Tan| |Times| 
  |Timing| |TimesBy| |UnAlias| |UnRat| |UnSameQ| |UnSet| |Unequal|
  |UpSet| |UpSetDelayed| |mPut| ) )

#| GCL does not have readtable-case, which is a pain.

  This list provides data so that if the user types in If, and the GCL
  version of the parser reads it as IF, then a suitable program knows
  that it is really the symbol If.  Of course if the user types in IF
  and intends it to be IF, not If, we lose. A way around it by
  simulating the lisp reader is too painful.  We depend upon it for
  lexical analysis.  Except for Scieneer Lisp, other implementations
  we know of do not provide the Franz/Allegro "modern" mode, and so
  accessing a Lisp program, say print, requires currently typing
  PRINT[..], because that's its name, and we can't auto-uppercase it
  without making a hash of MockMMA compatibility with Mathematica. And
  it is displayed as PRINT. This is visually distracting.  Modern mode
  renames the built-in system functions in lower-case, so print is
  simply print. 

  Oh, maybe we will hack both input and output so that identifiers all
of one case are mapped all to the other case on input, and are
are remapped back to the other case on output.  Thus print becomes
PRINT internally, and  foobar becomes FOOBAR internally. The Maxima
computer algebra system does something like this. 

 |#

;;(eval-when (:load-toplevel :compile-top-level)

  (defparameter caps-built-in-syms 
    (map 'list    #'(lambda(r) (intern (map 'string #'char-upcase (symbol-name r)) :mma))
	 built-in-syms))
  (export '(tl mread1))
  
;;  )
