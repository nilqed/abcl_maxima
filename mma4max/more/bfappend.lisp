;; -*- Mode:Common-Lisp;Package:mma; Base:10 -*-
;; Mathematica(tm)-like evaluator / bigfloat appendix
;; copyright (c) 1990 Richard J. Fateman; pieces by Tak Yan, Derek Lai
;;
(proclaim
 '(special Precision BigfloatChop BigfloatPrintTruncate 
	   BigfloatPrintPrecision funnyvars *numer* symtab))

(require 'math-eval "eval")
(require 'ucons1 "ucons1")
(require "bfelem")

;; There is an erroneous treatment of the scope of these variables.
;; We use global bindings when other, local, bindings are probably
;; appropriate.  This can be fixed later, when
;; we implement Set for local variables.  Right now we 
;; are using local bindings only in pattern matching.
;; -- not done as of 2/22/91 (RJF)

(setf (gethash '|Precision| funnyvars)
      #'(lambda(x)(bigfloat-init (the integer x))
	  ;; this next line is like Set, except ignoring funnyvars
	  (setf (gethash '|Precision|  (gethash '|Precision| symtab)) x)))

;; BigfloatChop=0 is off  or   BigfloatChop=<anything else> is the choice

(setf (gethash 'BigfloatChop funnyvars)
      #'(lambda(x)(setq bigfloat-chop (cond((=(the integer x) 0)nil)
					   (t nil)))
	  (setf (gethash 'BigfloatChop  
			 (gethash 'BigfloatChop symtab)) x)))

(setf (gethash 'BigfloatPrintTruncate funnyvars)
      #'(lambda(x)(setq bigfloat-print-trunc (cond((=(the integer x) 0)nil)
						  (t t)))
	  (setf (gethash 'BigfloatPrintTruncate
			 (gethash 'BigfloatPrintTruncate symtab)) x) ))

(setf (gethash 'BigfloatPrintPrecision funnyvars)
      #'(lambda(x)(setq bigfloat-chop (the integer x))
	  (setf (gethash 'BigfloatPrintPrecision 
			 (gethash 'BigfloatPrintPrecision symtab)) x)))

(bigfloat-init 3)

(mapc #'chash '(Precision BigfloatChop BigfloatPrintTruncate BigfloatPrintPrecision))

(Set 'Precision 3)
(Set 'BigfloatChop 0) ;off
(Set 'BigfloatPrintTruncate 1) ;on
(Set 'BigfloatPrintPrecision 3) ;low precision

(require 'math-eval "eval")

#+ignore
(setf (gethash 'Precision funnyvars) #'(lambda(h)(format t "Precision set to ~s" h)))
  
;;; This appendix assumes you've read in eval.lisp already,
;;; These are examples of how to write Lisp functions for lmath evaluation.
;;; Look especially at Sin, for a simple 1-argument function.

;;; substitute an appropriate operator for the built-in
;;;      operators like * + sin cos etc.

#+ignore
(defun Real(a b)
  (bigfloat-+  (intofp a) (bigfloat-/ (intofp b)
				      (intofp(expt 10 (decimalsin b))))))

(defun Real(a b) 
  ;; parser for 3.123 will give (Real 3 123/1000). 
  ;; For -3.1 gives (Times -1 (Real 3 1/10))
  (bigfloat-+  (intofp a) (bigfloat-convert b)))

(defun decimalsin(x)
  (ceiling (* (integer-length x) 0.30102999566398114d0)))

;; This is the place where we have to determine the nature of 
;; result for the product of any member of the set of types below
;; by any other member of the set
;;  { 
;; (a) CL number
;;    subcases include integer, floats (single, double, extended)
;;                     ratio, 
;;                     complex (integer), complex (float) etc.
;; (b) bigfloat (our defstruct).
;;      subcase: complex bigfloat
;; (c) rational form  (that is, our defstruct, rat)
;;     plausible subcases might be various rings or fields such as
;;         Z[x1,...,xn], Z(x1,...xn), Q[x1,...,xn], Z[x1,...x(n-1)] (xn)
;;         algebraic number or algebraic function fields, etc.
;; (d) Element of a finite field (e.g. 10 mod 23)
;; (e) Interval (endpoints any manifest scalar CL number or bigfloat)
;;     subcases interior or exterior, open or closed.
;; (f) matrix 
;;     subcases: sparse, dense, different element types
;;     vector (cross or dot or by scalar)
;; (g) formal Sum (e.g. Sum[f[#]&,{#,low,high}],

;; (h) Series object (e.g. 1+x + O[x]^2)
;; (i) Operator (e.g. D[x]* x^2 => 2*x, or D*D [x] for 2nd derivative)
;; (j) Sets
;; (h) other symbolic form (e.g. z, Pi, 3*x, or x+y^2)}

;; Rules:  (Note: there seem to be at least 11x11 cases, and maybe far more
;;          if you consider subcases and alternatives not listed.
;;          This is why Scratchpad II seems like a good idea.)

;; (1) The product of any two types in the same category is treated
;;     by a program that depends on the type.  For example, the product
;;     of any two CL types (elements in (a)) is computed by using 
;;     the CL coercions.
;;     (This is not so good because an overflowing flt.pt. product
;;     can still be represented by a large ratio. Also, we might differ
;;     with CL on its treatment of 1/0 and 0/0.)
;;     The product of (for example) two formal sums can be dealt with by
;;     a program that need not know about other stuff.

;; (2) The product of any element in (b) by an element in (a) is in (b).
;;     (Bigfloats rule over other CL numbers)

;; (3) The product of any element in (c) by an element of (a) or (b)
;;     requires either adoption of a new coefficient domain for the
;;     rationals or conversions from rational to "symbol" (h).
;;     Do we want /rational/  0.7500 x^2  ? For (c) by elements of (d)-(h),
;;     encapsulate them as rational kernels.

;; (4) Finite field arithmetic isn't handled at all because we aren't
;;     representing those objects yet.


;; (n) (Stopgap) elements in (d)-(h) are treated indistinguishably.

;; Assume that, after collection, we have at most one element in each
;; category.  We then combine them if appropriate.  That's pretty bad.

;; Another strategy:  if you have disparate rational forms, disparate
;; bigfloat forms, etc., etc. then you proclaim at any one time
;; a preferred form.  e.g. order of variables, type of coefficient,
;; bigfloat precision.  Convert to that global form.  This is already
;; the strategy in bigfloats.  Can it be generalized?
 
(defun Times (&rest x &aux (nums 1) oths)
  (dolist (h x   ;; iterate with h running over all args
	     ;; two sub-results are formed. The numerical coefficient is
	     ;; accumulated in  nums.  The symbolic stuff, if any, is
	     ;; accumulated in  oths.
	     
	   ;;resultform is computed as the product of nums and oths
	   (cond((eql 1 nums)
		 (if (null oths) 1 (ucons '|Times| (uniq(nreverse oths)))))
		((null oths) nums)
		(t (ucons '|Times| (ucons nums (uniq(nreverse oths)))))))
	  
	 ;; body
	 (typecase h
		   ((and number (not complex))  ;; would need bigfloat-complex
		    (setq nums (cond ((bigfloat-p nums)
				      (bigfloat-* (bigfloat-convert h)
						  nums))
					    (t(* nums h))))) ;; collect CL numbers
		   (bigfloat (setq nums
				   (if (eql nums 1) h
				       (bigfloat-* h (bigfloat-convert nums)))))
		   (rat 	   ; if you find a rat, break out, apply Rule 3!
		    (return-from |Times|
				 (reduce-rat #'rat* 
					     (into-rat (car x))
					     (cdr x))))
		   (t (push h oths)))))

;; reprogrammed along the model for Times
(defun |Plus|  (&rest x &aux (nums 0) oths)
  (dolist (h x   ;; iterate with h running over all args
	     ;; two sub-results are formed. The numerical coefficient is
	     ;; accumulated in  nums.  The symbolic stuff, if any, is
	     ;; accumulated in  oths.
	     
	   ;;resultform is computed as the product of nums and oths
	   (cond((eql 0 nums)
		 (if (null oths) 0 (ucons '|Plus| (uniq(nreverse oths)))))
		((null oths) nums)
		(t (ucons '|Plus| (ucons nums (uniq(nreverse oths)))))))
	  
	 ;; body
	 (typecase h
		   ((and number (not complex))
		    (setq nums (cond ((bigfloat-p nums)
				      (bigfloat-+ (bigfloat-convert h)
						  nums))
					    (t(+ nums h))))) ;; collect CL numbers
		   (bigfloat (setq nums
				   (if (eql nums 0) h
				       (bigfloat-+ h (bigfloat-convert nums)))))
		   (rat 	   ; if you find a rat, break out, apply Rule 3!
		    (return-from |Plus|
				 (reduce-rat #'rat+ 
					     (into-rat (car x))
					     (cdr x))))
		   (t (push h oths)))))

  ;;; this will be a bigfloat if b is bigfloat, e is integer
(defun Power (b e)

		    (cond((and (bigfloat-p b)(integerp e))
			  (bigfloat-expt b e))
			 ;; what if b and e are both bigfloats
			 ((and (numberp b)(numberp e))(expt b e))
			 (t (ulist 'Power b e))))

;; these programs deal fully only with non-complex bigfloats.
;; this should probably be fixed at some point.  The kind of
;; changes are hinted at in the definition of Log.

(defun Log(x)
  (typecase x
	    (bigfloat (if (bigfloat-posp x)
			  (bigfloat-log x) ;; x must positive 
			(ulist 'Log x))) ;; really should produce complex
	    (number (log x))
	    ;; here we could have some checks on special arguments
	    (t (ulist 'Log x))))


(defun Sin(x)
  (typecase x
	    (bigfloat (bigfloat-sin x))
	    ;; here, should check for 0 to make sure it's exact.
	    
	    (number (sin x))
	    ;; here, check for complex/bigfloat
	    ;; here, should check for familiar multiples of Pi,
	    ;; here, could also check sin(ArcSin), sin(ArcCos), 
	    ;; ArcTan special formulas
	    ;; perhaps Sin[x+y]-> ...
	    ;; perhaps Sin[I*x] -> Sinh[x]
	    ;; distribute over compound objects e.g. Sin[Matrix...]
	    ;; 
	    (t (ulist 'Sin x))))
(defun Cos(x)
  (typecase x
	    (bigfloat (bigfloat-cos x))
	    (number (cos x))
	    (t (ulist 'Cos x))))

(defun Tan(x)
  (typecase x
	    (bigfloat (bigfloat-tan x))
	    (number (tan x))
	    (t (ulist 'Tan x))))
(defun Exp(x)
  (typecase x
	    (bigfloat (bigfloat-exp x))
	    (number (exp x))
	    (t (ulist 'Exp  x))))



;; Pi, to precision  Precision.

(defun Pi()(fppi))




	 



