;; -*- Mode:Common-Lisp;Package:user; Base:10 -*-

;;; Common Lisp AFFINE rational number package based on IEEE 754 standard
;;; for binary floating point arithmetic.
;;; (c) copyright 1991, Richard J. Fateman
;;;  Part of our intention here is to show how little
;;; a change would be needed to replace the CL rationals by this.


;;; This package includes operations and representations for rationals and
;;;  Infinities (=1/0, -1/0)
;;;  NotaNumber  (0/0)
;;;  signed zeros  0, -0  (= 0/-1)

;;; the option of trapping on divide by zero or invalid is provided.

;;; this "signed zero"  means that the denominator of an erat might be
;;; negative, but then the denominator is in fact -1 and
;;; the numerator is 0. 

;; version 4.0 ACL lisp should have "CLOS" built in, making this unnecessary.
(require "pcl")(use-package "pcl")

;; the user shouldn't use these two global variables
;; except via programs rat-dbyz-catch or rat-invalid-catch
;; or similar programs. So we made their names long.

(defvar *rat-dbyz-trap-enable* nil)
(defvar *rat-invalid-trap-enable* nil)

;; these two global variables can be queried by the user to
;; see if an un-trapped NaN was produced in a sequence of
;; operations.

(defvar *rat-dbyz-flag* nil)
(defvar *rat-invalid-flag* nil)

;; define a class holding the extended rationals
;; and how to print them.  

(defclass erat() 
 ((numerator :initarg :numerator :reader num :initform 0)
  (denominator :initarg :denominator :reader den :initform 0)))

(defvar nan-rat (make-instance 'erat :numerator 0 :denominator 0))
(defvar inf-rat (make-instance 'erat :numerator 1 :denominator 0))
(defvar minf-rat (make-instance 'erat :numerator -1 :denominator 0))
(defvar mzero-rat (make-instance 'erat :numerator 0 :denominator -1))

(defmethod print-object ((x erat) s)
 (if (eql (den x) 1) (format s "~s" (num x)) ;; an integer  n/1
  (format s "~s./~s" (num x)(den x))))  ;; use ./ instead of / in printing

;; create-erat is the top-level function to use to make a new erat.
;; it assumes nothing much about the inputs x and y except that they
;; are either CL real numbers or possibly erats. If the 2nd arg is missing,
;; it is taken to be 1.

(deftype realnum()'(and number (not complex)))

(defun create-erat(x &optional(y 1));;create x/y as erat, or ratio
  (cond((eql y 0)(create-erat-simple x y))
       ((typep x 'erat)(quotient x (create-erat y)))
       ((typep y 'erat)(quotient (create-erat x) y))
       ((or (not (typep x 'realnum))(not (typep y 'realnum)))
	(error "~s/~s cannot be converted to erat"  x y))
       ;; we could actually return a CL rational or integer by just
       ;; saying (rationalize (/ x y)) here..
       ;; we should (but don't) check for float-nans etc.
       ;; rationalize will give an error for inf or nan stuff.
       ;; if we had a portable way of checking for this, we could
       ;; do the coercion.
       (t(let ((h (rationalize (/ x y))))  ;; handles normal floats etc
	   (create-erat-simple (numerator h)(denominator h))))))


;; this function should be used whenever possible internally
;; to avoid another gcd computation.

#+ignore  ; see redefinition below
(defun create-erat-simple (x y) 
;; this assumes  integers x and y, where x/y is in lowest terms etc.
  (case y
	(0 (cond ((eql x 0) nan-rat)
		 ((> x 0) inf-rat)
		 ((< x 0) minf-rat)))
	   
	(1 x) ;just return the integer x
	(t (make-instance 'erat 
		   :numerator x
		   :denominator y))))

;; these accessors make it irrelevant as to whether x is ratio or erat

(defmethod numer ((x rational))(numerator x))
(defmethod numer ((x erat))(num x))

(defmethod denom ((x rational))(denominator x))
(defmethod denom ((x erat))(den x))

;; define extended plus

;; here's a backstop for nonsensical arguments

(defmethod plus (a b) `(Plus ,a ,b))

(defmethod plus (x (y erat))
  (plus y x)); put erat first if anywhere

;; adds both normal integers or ratios. not used for erats...
(defmethod plus ((x rational)(y rational))(create-erat (+ x y)))

(defmethod plus ((x erat) y)
 (let* ((a (numer x))(b (denom x))
	(c (numer y))(d (denom y))  ;a/b + c/d  = r/s
	(r(+ (* a d)(* b c)))
	(s (* b d))
	g)
   (cond((eql s 0)
	 (cond((> r 0) inf-rat)
	      ((< r 0) minf-rat)
	      (t
	       ;; a*d+b*c = 0. Look at the cases. We know that b*d=0.
	       ;; go through the cases
	       (cond
		;; b=0   ==>  a*d=0. 
		;; case 1: a=0  ==> 0/0 + ?? -> 0/0
		((eql a 0) nan-rat)
		;; case 2:  a != 0 ==>  d=0. But then b*c = 0.  
		;; case 3: c=0  ==> ?? + 0/0 -> 0/0
		((eql c 0) nan-rat)
		;; that also takes care of the case d=0.
		;; case 4: b=d=0 ==>  a,c = + or - 1.
		((not (eql a b)) nan-rat) ;; 1/0 + -1/0 -> 0/0
		((> a 0) inf-rat)
		(t minf-rat)))))
	
	(t (setq g (gcd r s))  
	   (create-erat-simple (/ r g) (/ s g))))))

;; define extended times

;; here's a backstop for nonsensical arguments

(defmethod times (a b) `(Times ,a ,b))

(defmethod times (x (y erat))
  (times y x)); put erat first if anywhere

(defmethod times ((x rational)(y rational))(* x y))

(defmethod times ((x erat) y)
 (let* ((a (numer x))(b (denom x))
	(c (numer y))(d (denom y))  ;a/b * c/d  = r/s
	(r (* a c))
	(s (* b d))
	g)

   (cond((eql s 0)(create-erat-simple (signum r) s))
	(t (setq g (gcd r s))
	   (create-erat-simple (/ r g) (/ s g))))))

;; quotient

(defun quotient(x y)(times x (reciprocal y)))

;; reciprocal

;; non-trapping versions ...
;(defmethod reciprocal((x erat)) (create-erat-simple (denom x)(numer x)))
;(defmethod reciprocal((x rational)) (create-erat-simple (denom x)(numer x)))

;; traps in *rat-dbyz-trap-enable* is non-nil

(defmethod reciprocal((x erat))(recip2 x))
(defmethod reciprocal((x rational))(recip2 x))
(defmethod reciprocal(x) `(Power ,x -1))

(defun recip2(x)
  (cond((and *rat-dbyz-trap-enable* (eql (numer x) 0))
	(throw 'rat-dbyz 'rat-dbyz))
       (t (setf *rat-dbyz-flag* t)
	  (create-erat-simple (denom x)(numer x)))))

#+ignore ;; if we never trapped or looked at the flags, this would do..
(defun recip2 (x)(create-erat-simple (denom x)(numer x)))

;; comparisons (taking IEEE float rules as gospel)

;;         .. 1/0  >  0/-1      true
;;        ... 0/1  >  0/-1      false
;;        ... any  >  0/0       false
;;        ... 0/0  >  any       false

#+ignore  ;; see redefinition below
(defun greaterp(x y)
  (cond((eql (denom x) 0) 
	(if (eql (denom y) 0) (> (numer x) 0 (numer y))))  ; 1>0> -1 passes
       ;; do we need to check if (denom y) is zero?
       ;; no, because if that is the case, (numer y) is one,
       ;; and the test below is then (> 0 (denom x)).
       ;; but (denom x) is either 0 or positive, so result is nil
       (t (> (* (numer x)(denom y))(* (numer y)(denom x))))))
#+ignore ;; see redefinition below
(defun lessp (x y)
  (cond((eql (denom x) 0) 
	(if (eql (denom y) 0) (< (numer x) 0 (numer y))))  ; -1 <0< 1 passes
       ;; do we need to check if (denom y) is zero?
       ;; no, because if that is the case, (numer y) is one,
       ;; and the test below is then (< 0 (denom x)).
       ;; but (denom x) is either 0 or positive, so result is nil
       (t (< (* (numer x)(denom y))(* (numer y)(denom x))))))

;;  1/0 = any     false
;; -1/0 = any     false
;;  0/0 = any     false

;;  0/1 = 0/-1    true


(defun erat= (x y)
   (and (not (eql (denom y) 0))  ; checking one denom is enough

	(eql (numer y)(numer x))      ;; if nums are equal then check if
	(or (eql (denom y)(denom x))  ;; denoms are equal or case of
	    (eql (minusp (denom x))(denom y)) ;; 0/1 = 0/-1
	)))

;; actually IEEE 754 has 26 functionally distinct comparisons composed
;; from the setting of 4 relation flags and an exception flag.  We
;; have provided only 3 of them.

;;         greater    less    equal    unordered   (exception if invalid
;;          than      than                          or unorder)

;;  >         T          -        -         -           T
;;  <         -          T        -         -           T
;;  =         -          -        T         -           -

;; There are some subtleties because not-equal is not the same as
;; less-than-or-greater-than.  The latter gives an exception for
;; unordered.


;; other items needed include expt
;; coercion to and from other formats
;; difference, remainder, zerop plusp minusp notequal etc.

;; for "normal" rationals, these are already defined.

;; we can use nans for results of
;; (coerce inf-rat 'float) (coerce inf-rat 'single-float)
;; (coerce inf-rat 'double-float) (coerce inf-rat 'long-float)

;; coercion of nan-rat to a float type produces nans of that type.
;; in Franz Inc's allegro CL we have the forms

;; #.excl::*nan-double* #.excl::*infinity-double*
;; #.excl::*negative-infinity-double* 

;; #.excl::*nan-single*  #.excl::*infinity-single* 
;; #.excl::*negative-infinity-single* 

;; semantics for the float-nans is not specified in the proposed CL
;; standard.  It may be, in fact, something we should specify....


;; IEEE 754 defines invalid, underflow, overflow, divide-by-zero, inexact.
;; for this model, only divide-by-zero and invalid can even happen.

;; in medieval towns, rat-catchers roamed the streets...

;; rat-dbyz-catch is an example of a program which can be
;; used to interact with the exception handling.  In this
;; case, (rat-dbyz-catch e val) evaluates the expression
;; e and in case there is a divide by zero, returns the value
;; represented by val.  If for some reason you wish a divide-by-zero in (f x)
;; to return "1",  use  (rat-dbyz-catch (f x) 1)  instead of (f x).


(defmacro rat-dbyz-catch (e val)
  `(let* ((*rat-dbyz-trap-enable* t)
	  (res (catch 'rat-dbyz ,e)))
     (if (eq res 'rat-dbyz) ,val res)))

;; the only place divide by zero can occur is reciprocal

;; this shows how to catch "invalid"

(defmacro rat-invalid-catch (e val)
  `(let* ((*rat-invalid-trap-enable* t)
	  (res (catch 'rat-invalid ,e)))
     (if (eq res 'rat-invalid) ,val res)))


(defun greaterp(x y)  ;; sample greaterp version with invalid
  (cond((eql (denom x) 0) 
	(if (eql (denom y) 0)
	    (> (numer x) 0 (numer y)) ; 1>0> -1 passes. others are invalid
	  (check-invalid-trap nil)))
       ((eq y nan-rat)
	(check-invalid-trap nil))
       (t (> (* (numer x)(denom y))(* (numer y)(denom x))))))

;; this program is called when we are using or producing
;; a NaN.  If we are set up to trap on invalid,
;; it throws an to appropriate error catch program.
;; otherwise it sets the global invalid flag and returns x.
;; x is usually nil, but may be NaN.

(defun check-invalid-trap(x)
  	  (cond(*rat-invalid-trap-enable*
		(throw 'rat-invalid 'rat-invalid))
	       (t (setf *rat-invalid-flag* t)
		  x)))

(defun lessp (x y) ;; checks for invalid
  (cond((eql (denom x) 0) 
	(if (eql (denom y) 0)
	    (< (numer x) 0 (numer y))  ; -1 <0< 1 passes. others are invalid
	(check-invalid-trap nil)))
       ((eq y nan-rat) 	(check-invalid-trap nil))
       (t (< (* (numer x)(denom y))(* (numer y)(denom x))))))

(defun create-erat-simple (x y)  ;; sample create-erat-simple with invalid
;; this assumes  integers x and y, where x/y is in lowest terms etc.
  (case y
	(0 (cond ((eql x 0) (check-invalid-trap nan-rat))
		 ((> x 0) inf-rat)
		 ((< x 0) minf-rat)))
	   
	(1 x) ;just return the integer x
	(t (make-instance 'erat 
		   :numerator x
		   :denominator y))))


;; trapping on invalid can happen on any NaN producing operation. This
;; is always done by create-erat-simple. The other place an invalid
;; signal can occur is when an unordered operand is used in comparisons. 

;; samples of greaterp and create-erat-simple are shown above.


;; IEEE754 also specifies the setting of global flags on invalid, dbyz
;; and other exceptions  so that even if they don't stop execution,
;; they can be tested after a series of operations.  Here the flags
;; are called *rat-dbyz-flag* and *rat-invalid-flag* .

;; bottom line: what haven't we done here?

;; THINGS WE MIGHT DO.
;; signalling vs. quiet nans;
;;  depending on their purpose, signalling nans could be non-numbers
;;  like "uninitialized array element" or a real interval or another
;;  number format like arbitrary precision floating point.

;; the other 23 comparisons or a "compare and branch".
;; remainder, difference, copysign, finite, isnan, unordered, class.
;; 

;; THINGS WE WOULD NOT DO (irrelevant in the rationals)
;;   extra traps, namely inexact, over/underflow.
;;   directed rounding modes 
;;   square-root  (not closed in the rationals)
;;   binary <-> decimal conversion (done by CL)
;;   single/double/extended formats and their combinations
;;   denormalized numbers and their consequences
;;   scalb, logb, nextafter.

;; next: intervals.

