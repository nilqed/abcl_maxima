;; -*- Mode:Common-Lisp;Package:user; Base:10 -*-
;;; extended rational package  (rational + signed Infinity + signed zero)

;; Our intention is to make this the basis for an interval package.
;; A somewhat out of date reference (also, out of print) is
;;  W.M. Kahan. A more complete interval arithmetic,
;; lecture notes prepared for a summer course at the Univ. of Michigan,
;; June 17-21, 1968.

;; newer lisp should have "CLOS" built in.
;(require "pcl")
;(use-package "pcl")

;;; First, define an extended set for rational numbers. 

;;; This is a model of the real line that looks like
;;;  -infinity... negative rationals.. -0 0 ... positive rationals ... infinity
;;; Note that there are two infinities and two zeros.

;;; We start with the CL rationals, but 
;;;  add five symbols, RatNaN (RationalNotaNumber), Infinity,
;;;  minus Infinity, negative zero, and omega.  We use the already
;;;  available 0 for (positive) 0.

;;; RatNaN is useful for 0/0, and for numbers not in the
;;; domain of rationals, should we wish to generate them (e.g. sqrt(-1)).

;;; Infinity is like 1/0.
;;; -Infinity is like  -1/0, or also 1/(-0) ... see below.

;;; -0 helps with 1/(-Infinity), but in most other ways behaves just like
;;; +0  or unsigned 0.   It actually has a more important
;;; role in intervals. In particular [+0, 1] is used for the Open
;;; interval at 0, but [-0, 1] is CLOSED -- includes the point AT 0.
;;; Similarly (symmetrically) [-1,0] includes 0 but [-1, -0] doesn't.

;;; Omega is an interval -- the interval of all real numbers from 
;;; -Infinity to Infinity, including infinity.

;;; For building interval arithmetic, it is convenient to have
;;; signed 0s and infinities.

;; define a class holding the extensions to the rationals
;; and how to print them.

(defclass erat()  ; infinity or minus infinity, minus zero, or ratnan
((name :initarg :name :reader name :initform nil)))

(defmethod print-object ((x erat) s) (format s "~a" (name x)))

;;; the only four instances of this class

(setf minf (make-instance 'erat :name '|-1/0|))
(setf inf  (make-instance 'erat :name '|1/0|))
(setf mzero (make-instance 'erat :name '|-0|))
(setf ratnan (make-instance 'erat :name '|RatNaN|))
(setf omega (make-instance 'erat :name 'Omega))

;; Officially, omega would be, including the point at infinity, [<1/0,-1/0>]
;; This is, however, somewhat confusing.
;; define extended plus

;; note the programming technique here looks almost like Prolog
;; or like "rules" in Mathematica.

;; here's a backstop for nonsensical arguments

(defmethod plus (a b) `(Plus ,a ,b))

;; here's the usual case
(defmethod plus ((a rational)(b rational))(+ a b))

;; if there is a rational, put it in the second arg spot

(defmethod plus ((x rational)(y erat)) (plus y x)) ;reverse args

;; here are the rest of the rules

;; rule for ratnan +  ?

(defmethod plus ((x (eql ratnan)) y) ratnan)

;; rules for omega +  ?

(defmethod plus ((x (eql omega)) y)
  (cond ((eq y ratnan) ratnan)
	(t omega)))


;; rules for mzero +  ?

(defmethod plus ((x (eql mzero)) y) y)

;; rules for inf +    ?

(defmethod plus ((x (eql inf))y)
  (cond ((eq y minf) omega)
	((eq y omega) omega)
	((eq y ratnan) ratnan)
	(t inf))) ;covers mzero inf rational

;; rules for -inf +   ?

(defmethod plus ((x (eql minf)) y)
  (cond((eq y inf) omega)
       ((eq y omega) omega)
       ((eq y ratnan) ratnan)
       (t minf))) ; covers mzero, minf, rational

;; times

;; here's a backstop for nonsensical arguments to times

(defmethod times (a b) `(Times ,a ,b))

;; here's the usual case
(defmethod times ((a rational)(b rational))(* a b))

;; if there is a rational, put it in the second arg spot

(defmethod times ((x rational)(y erat)) (times y x)) ;reverse args

;; rules for ratnan *

(defmethod times ((x (eql ratnan)) y) ratnan)

;; rules for omega *

(defmethod times ((x (eql omega)) y)
  (cond ((eq y ratnan)ratnan)
	((eq y mzero) ratnan)
	((eq y 0)     ratnan)
	(t omega))) ;covers omega, rational, inf, minf

;; rules for mzero *


(defmethod times ((x (eql mzero)) y)
  (cond ((eq y ratnan) ratnan)
	((eq y omega)  0)
	((greaterp y 0) mzero)
	(t 0)))

;; rules for inf *

(defmethod times ((x (eql inf)) y)
  (cond ((eq y ratnan)  ratnan)
	((eq y omega)   omega)
	((eq y mzero)   ratnan)
	((eql y 0)      ratnan)
	((eq y minf)    minf)
        (t inf))) ;covers rational inf

;; rules for minf *

(defmethod times ((x (eql minf)) y)
  (negate (times inf y))

;; reciprocal

;; here's a backstop for nonsensical arguments to reciprocal

(defmethod reciprocal (a)`(Power ,a -1))

(defmethod reciprocal((x erat))
  (case x
	(ratnan ratnan)
	(omega omega)
	(mzero minf)
	(inf 0)
	(minf mzero)
	;; there should be no cases left
	(t (error "~s arg to reciprocal" x)))

(defmethod reciprocal ((x rational))
  (if (= x 0) inf (expt x -1)))

(defun quotient (x y)
	(times x (reciprocal y)))

;; power (integer power only) generalizes multiplication.
;; This is not x^y.

(defmethod power (a b) `(Power ,a ,b))

(defmethod power ((a erat) (b integer))
  (case a 
	(ratnan ratnan)
	(inf
	 (cond ((> b 0) inf)
	       ((= b 0) 1)
	       ((< b 0) 0)))
	(mzero
	 (cond ((> b 0) (if (oddp b) mzero 0))
	       ((= b 0) 1)
	       ((< b 0) (if (oddp b) minf inf))))
	(minf
	 (cond ((> b 0) (if (oddp b) minf inf))
	       ((= b 0) 1)
	       ((< b 0) (if (oddp b) mzero 0))))
	(omega
	 (if (eql 0) 1 omega))))

(defmethod power ((a rational)(b integer))(expt a b))

;; difference

(defun difference (x y)
  (plus x (negate y)))

(defmethod negate (y) `(Times -1 y))

(defmethod negate ((y rational))(if (= y 0) mzero (- y)))

(defmethod negate ((y erat))
  (case y
	(ratnan ratnan)
	(mzero 0)
	(minf inf)
	(inf minf)
	(omega omega)
	(t (error "~s arg to negate" y))))

;; need to be able to compare these guys.
;; note that for ratnan and omega, the law of
;; "trichotomy" fails.  That is,  if one or more of a,b, are ratnan, omega,
;; then it is the case that
;;  a>b, a<b, and a=b are all false.

(defmethod greaterp((x rational)(y rational))(> x y))
(defmethod greaterp((x (eql ratnan)) y) nil) ;; not equal or lessp, either
(defmethod greaterp((x (eql omega))  y) nil)

(defmethod greaterp((x (eql inf)) y)
  (case y
	(ratnan nil)
	(omega nil)
	(inf nil) ;Q 
	(t t) ; minf, mzero, rational
	))

(defmethod greaterp((x (eql minf)) y) nil)

(defmethod greaterp((x (eql mzero))y)
  (case y
	(minf t)
	(t (and (rationalp y)(> 0 y))))) ;ratnan, inf, omega, mzero
	(ratnan nil)	(omega nil)
	(inf nil) ;Q 
	(t t) ; minf, mzero, rational
	))

(defmethod greaterp((x rational)(y erat))
  (case y
	(minf t)
	(mzero (> x 0))
	(t nil)))

;;; lessp

(defmethod lessp((x rational)(y rational))(< x y))
(defmethod lessp((x (eql ratnan)) y) nil) ;; not equal or greaterp, either
(defmethod lessp((x (eql omega))  y) nil)

(defmethod lessp((x (eql minf)) y)
  (case y
	(ratnan nil)
	(omega nil)
	(minf nil) ;Q 
	(t t) ; inf, mzero, rational
	))

(defmethod lessp((x (eql inf)) y) nil)

(defmethod lessp((x (eql mzero))y)
  (case y
	(minf nil)
	(ratnan nil)
	(omega nil)
	(inf t) ;Q 
	(t (and (rationalp y)(< 0 y)))))

(defmethod lessp((x rational)(y erat))
  (case y
	(inf t)
	(mzero (< x 0))
	(t nil)))

(defmethod ext=  ((x rational)(y rational))(= x y))
(defmethod ext=  ((x erat) y) nil)  ;;ratnan, inf, minf, omega 
(defmethod ext=  ((x (eql mzero)) y)
  (or (eql y mzero)(eql y 0)))  ;Q
(defmethod ext=  ( y (x (eql mzero)))
  (or (eql y mzero)(eql y 0)))


;; one tests for inf, minf, mzero, ratnan or omega by lisp's eq.

;; We will need max and min later.  Let's define them here.
;; Ref: W. Kahan.  How should Max and Min be defined? 
;; (work in progress, May 25, 1989).

;; conditions for M := max(r,s).
;;  (1) neither r>M  nor  s>M
;;  (2) M is either r or s
;;  (3) M is omega when both r and s are omega.

;; only (3) is controversial, and the alternative might be
;;  (3') M is omega when either r or s is omega.

(defmethod maximum (x y) `(Max ,x ,y))
(defmethod maximum ((x rational) (y rational)) (max x y))
(defmethod maximum ((x erat)y) (if (lessp x y) y x))
(defmethod maximum ( x (y erat)) (if (lessp x y) y x))

(defmethod minimum (x y) `(Min ,x ,y))
(defmethod minimum ((x rational) (y rational)) (min x y))
(defmethod minimum ((x erat)y) (if (greaterp x y) y x))
(defmethod minimum (x (y erat)) (if (greaterp x y) y x))


;;; now, intervals.  

"  A bunch of comments follow..

Criteria:

 We want to be able to do all the rational operations (+, -, *, /)
on intervals, returning (usually) intervals.

Also, we want to be able to compute for point argument values
 x,w in Q, f(x,w) -> [r1,r2], an interval of width <= w containing
the actual value for f(x). (for f= sqrt, sin, exp, log, sqrt etc.)

Also, we want to be able to compute for interval argument values X
 f(X) -> [r1,r2], an interval.  In general there are many possible
interval answers.  We want to be able to ask for a narrower one if
possible -- but how to specify the width?
We can't achieve arbitrarily small widths.

Also we will need additional functions including comparison relations
on intervals
R(X,Y) -> boolean  R = < , > , tests for disjoint

Also binary operations 
B(X,Y) -> interval  for intersection, union, inclusion

Also element-of(point, interval) -> boolean

Consider `proper intervals' with rational endpoints a,b.

We have  interior intervals [a,b] where a<b,  
         exterior intervals [b,a] where a<b.  

Rational x is in the exterior interval [b,a] if x<=b or x>=a. This is
one continuous projective interval if we think of the point at infinity
as being both + and - infinity, and is the complement of the
interior interval [a,b]  except that the points a and b are included
in both.

Notes:

Here are some of the special intervals
 nanint, empty, omega, [minf,inf]  (open at the infinities) 
 [inf,minf] (everything including infinity -- this is omega)
 [-0,+0] is just 0  [0,-0]  has everything except 0
 [+0, +0],  [-0 -0]  these representations are not in use. 


  The extended rational number omega is in the interval omega, no other.

  There should be no interval with identical left and right endpoints
  (unless you allow omega = [infinity, infinity])

  If you add (say) [0,1] to [1,0], you get, not [1,1] but omega.

  There are only a few ways to creating a 0 length interior interval. 

  Example:
  [1,2]^0  -> 1 (note [-1,1]^0 would be omega).
  0 * [1,2] -> 0

Now consider `improper intervals' with one or both endpoints not in Q.


Do we need the Empty interval? (Null set).

 It seems to be convenient if you
wish to describe as a real interval, sqrt([-2,-1]}.

There presumably are lots of questions for the extension to other
functions like  sqrt([-1,1]) -> ??    Is sqrt([-1,0]) the point 0?
"

(defclass interval()
 ((left  :initarg :left  :reader left :initform 0)
  (right :initarg :right :reader right :initform 0)))

(defclass oe()  ;omega or empty
 ((oe :initarg :oe :reader oe :initform nil)))

(defmethod print-object ((x oe) s) (format s "~s" (oe x)))

(defun make-interval (l r)
  ;;should we do some checking??
  (make-instance 'interval :left l :right r))

(setf omega-interval (make-instance 'oe :oe '[Omega]))
(setf empty-interval (make-instance 'oe :oe '[Empty]))


;; interval plus
;; assume each interval is of positive width
(defmethod plus ((a interval)(b interval))
  (cond ((or (eq empty-interval a)(eq empty-interval b)) empty-interval)
	((or (eq omega-interval a)(eq omega-interval b)) omega-interval)
	(t

	 (let ((l (plus (left a)(left b)))
	       (r (plus (right a)(right b))))

	   (if (eq l omega)(setq l infinity));; because -i + -i= -i, here

	   (if (eq r omega)(setq r infinity));; because   i + i = i, here
					;; here we  check for interior+exterior = omega
	   (if (eql l r) omega (make-interval l r))))))
		
;; one interval, one "other"

(defmethod plus ((a interval) b)
 (make-interval (plus (left a)b)
	        (plus (right a)b)))

(defmethod plus (b (a interval)) (plus a b))

;; interval times

(defmethod times ((x interval)(y interval))
 (let((a (left x))(b (right x))
      (c (left y))(d (right y))
      n1 n2 n3 n4)

   (cond((and (proper-interval x)(proper-interval y))
	 (setq n1 (times a c) n2 (times a d) n3 (times b c) n4 (times b d))
	 (make-interval (min n1 n2 n3 n4)(max n1 n2 n3 n4)))
	((
	)))

(defmethod times ((x interval)y)
  (let((a (left x))
      (b (right x))
      n1 n2 n3 n4)
    (setq n1 (times a y) n2 (times b y))
    (make-interval (min n1 n2)(max n1 n2))))

(defmethod times ((y t)(x interval)) (times x y))

;; need min and max to be defined better if we really think it is important
;; that '(Plus x y) doesn't crash them.

;; probably need min2 and min4, max2 max4 
;; 

(defmethod quotient ((x t)(y t))
 (times x (reciprocal y)))

(defmethod reciprocal(y t))`(Power y -1))
(defmethod reciprocal
  ((y rational)) (if (eql y 0) 'Infinity (expt y -1)))

(defmethod reciprocal ((y interval))
  (let* ((l (left y))
	(r (right y))
	(li (inverse l))
	(ri (inverse r)))
    ;; check for inclusion of 0
    (if (inside 0 y) ;;exterior interval
	(make-interval (max li ri)(min li ri))
      ;;interior interval
      (make-interval (min li ri)(max li ri)))))


;;; test for inside: 

;; every point is inside omega.

(defmethod inside (pt (x (eql omega))) t)

(defmethod inside ((pt rational)(x interval))
  (let ((l (left x))(r (right x)))
    (if (interiorp x)
	(or (and  (eq r infinity)(>= pt l))
	    (and (<= pt r)(>= pt l))) ;interior
      (or (and (eq l infinity) (<= r))
	  (and(<= pt r)(>= pt l)))) ;exterior
	  )))

;; infinity is inside every exterior interval

(defmethod inside ((pt (eql infinity)) (x interval))
  (exteriorp x))

;; omega is inside no interval except omega

(defmethod inside ((pt (eql omega)) x)
  (eql x omega))

(defmethod inside ((pt rational)(x rational)) (eql pt x))


;; check for type of interval

(defmethod interiorp((x interval))
  (or (eq (right x) infinity)
      (and (not (eq (left x) infinity)) (<= (left x)(right x)))))

(defun exteriorp (x) 
  (or (eq (left x) infinity)
      (and (not (eq (right x) infinity)) (<= (right x)(left x)))))

(defun proper-interval(x)(and (rationalp (left x))(rationalp (right x))))


;; check for inclusion  is the point pt included in interval x?
;; in case the interval is a point, use "inside"

(defmethod includes ((pt rational) x) (inside pt x))

;; omega or infinity
(defmethod includes ((pt oi) x) (inside pt x))

;; any pt or interval is in omega
(defmethod includes (pt ((x (eql omega)))) t)

(defmethod includes 
  ((x interval)(y interval))
					;; there are 4 cases:  int/ext for x, y
  (cond ((interiorp y)			;; cases 1 and 2
	 (if (interiorp x)
	     (and (inside (left x) y)(inside (right x) y))
					;; if x is exterior, it can't be in y
					;; -- y = omega handled separately.
	   ))
	(t				;(exteriorp y)=t,  cases 3 and 4
	 (if
	     (exteriorp x)
	     (and (inside (left x) y)(inside (right x) y))
					;; if x is interior, it may either 
					;; be in either portion.
	   ;; is this ok for [infinity, b]???

	   (or (and (<= (left x) (right y)) (<= (right x)(right y)))
	       (and (>= (left x) (left y)) (>= (right x)(left y))))))))

      
(defmethod
  print-object ((x interval) s)   	;s stream
	 (let ((l (left x))(r (right x)))
	   (cond ((and (numberp l)(numberp r))
		  (if (<= l r)
					;; interior interval
		      (format s "[<~s , ~s>]" l r)
					;; exterior interval
		    (format s "[>~s , ~s <]" l r)))
					;; not a numeric interval.
					;;Who knows if interior or not..
		 (t (format s "[<~s , ~s>]" l r)))))



;; Questions/extensions
;; What about floating-point or arb. precision flt. point endpoints?

;; what if we want to have arithmetic on special numbers like
;; +Infinity, -Infinity, Indefinite (or perhaps Undefined)
;; and intervals that exclude endpoints (selectively) 



