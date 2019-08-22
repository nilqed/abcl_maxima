;;; -*- Mode:Common-Lisp;Package:user; Base:10 -*-
;;; extended projective rational package
;;;  (rationals + Infinity (=1/0) + NaN (0/0))

;; newer lisp should have "CLOS" built in.
(require "pcl")
(use-package "pcl")

;;; Define an extended set for rational numbers. 
;;; This is a model of the PROJECTIVE LINE.

;;; We add two symbols, nan-rat (0/0) and inf (1/0)
;;; 0/0, meaning "no particular real number".  
;;; Projective model has one  Infinity. Imagine the
;;; projective numbers on a circle with +/- Infinity opposite 0.

;;       infinity
;;         _-^-_
;;        /     \
;;       (       )   The projective "circle"
;;        \__.__/
;;        -  0  +


;; define a class holding the extended rationals
;; and how to print them.  The intention here is to show how little
;; a change would be needed to replace the CL rationals by this 

(defclass erat() 
((numerator :initarg :numerator :reader num :initform 0)
 (denominator :initarg :denominator :reader den :initform 0)))

(defvar nan-rat (make-instance 'erat :numerator 0 :denominator 0))
(defvar inf-rat (make-instance 'erat :numerator 1 :denominator 0))

(defmethod print-object ((x erat) s)
(if (eql (den x) 1) (format s "~s" (num x)) ;; an integer  n/1
  (format s "~s//~s" (num x)(den x))))  ;; use // instead of / in printing

;; create-erat is the top-level function to use to make a new erat.
;; it assumes nothing much about the inputs x and y except that they
;; are either CL numbers or possibly erats. If the 2nd arg is missing,
;; it is taken to be 1.

(defun create-erat(x &optional(y 1));;create x/y as erat, or ratio
  (cond((eql y 0)(create-erat-simple x y))
       ((typep x 'erat)(quotient x (create-erat y)))
       ((typep y 'erat)(quotient (create-erat x) y))
       ((or (not (typep x 'number))(not (typep y 'number)))
	(error "~s/~s cannot be converted to erat"  x y))
       ;; we could actually return a CL rational or integer by just
       ;; saying (rationalize (/ x y)) here..
       ;; we should (but don't) check for float-nans etc.
       (t(let ((h (rationalize (/ x y))))  ;; handles normal floats etc
	   (create-erat-simple (numerator h)(denominator h))))))


;; this function should be used whenever possible internally
;; to avoid another gcd computation.

(defun create-erat-simple (x y)
;; this assumes  integers x and y,
;; where x/y is in lowest terms etc.
  (case y
	(0 (if (eql x 0) nan-rat inf-rat))
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
   (cond((eql s 0)(create-erat-simple r s))
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
   (cond((eql s 0)(create-erat-simple r s))
	(t (setq g (gcd r s))
	   (create-erat-simple (/ r g) (/ s g))))))

;; quotient

(defun quotient(x y)(times x (reciprocal y)))

;; reciprocal

(defmethod reciprocal((x erat)) (create-erat-simple (denom x)(numer x)))
(defmethod reciprocal((x rational)) (create-erat-simple (denom x)(numer x)))
(defmethod reciprocal(x) `(Power ,x -1))

;; comparisons

(defun greaterp(x y)
  (cond((eql (denom x) 0) nil)
       ;; do we need to check if (denom y) is zero?
       ;; no, because if that is the case, (numer y) is one,
       ;; and the test below is then (> 0 (denom x)).
       ;; but (denom x) is either 0 or positive, so result is nil
       
       (t (> (* (numer x)(denom y))(* (numer y)(denom x))))))

(defun lessp (x y)
  (cond((eql (denom x) 0) nil)
       ;; do we need to check if (denom y) is zero?
       ;; no, because if that is the case, (numer y) is one,
       ;; and the test below is then (< 0 (denom x)).
       ;; but (denom x) is either 0 or positive, so result is nil
       (t (< (* (numer x)(denom y))(* (numer y)(denom x))))))

(defun erat= (x y)
  (and (not (eql (denom y) 0))  ; checking one denom is enough
	    (eql (denom y)(denom x))
	    (eql (numer y)(numer x))))

;; other items needed include expt
;; coercion to other formats
;; zerop plusp minusp notequal etc.

;; for "normal" rationals, these are already defined.


;; because 1/0 is NOT SIGNED infinity, we'd better use nans for results of
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



