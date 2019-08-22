;; -*- Mode:Common-Lisp;Package:user; Base:10 -*-

;;; interval package ("affine rational" endpoints)

(require 'affinrat2)

(defclass arint()  ;affine rational intervals
  ((left-value :initarg :left :reader left-value :initform 0)
   (right-value :initarg :right :reader right-value :initform 0)))

(defun left(x)(typecase x
			(arint (left-value x))
			(t x))) ;; if x is rational, for example

(defun right(x)(typecase x
			(arint (right-value x))
			(t x)))

(defmethod
  print-object ((x arint) s)   	;s stream
	 (let ((l (left x))(r (right x)))
	   (cond ((and (numberp l)(numberp r)) ;; should compare erats
		  (if (<= l r)
					;; interior interval
		      (format s "[<~s , ~s>]" l r)
					;; exterior interval
		    (format s "[>~s , ~s <]" l r)))
					;; not a numeric interval.
					;;Who knows if interior or not..
		 (t (format s "[<~s , ~s>]" l r)))))


;;[ -inf , inf]  whole real including infs
(defvar omega-arint (make-instance 'arint :left inf-rat :right minf-rat))
;; empty? how to represent?
;; 

(defun make-arint (l r)(make-instance 'arint :left l :right r))


;; what do the funny endpoints mean: +/- 0, +/- inf, 0/0.  

;; Consider adding internal
;; and external regular rationals.

(defun plus-arint (x y)
  ;; This is not quite right. What if x or y is empty? or add up to [z,z]
  (make-arint (plus (left x)(left y))(plus (right x)(right y))))

(defmethod plus ((x arint)(y arint)) (plus-arint x y))
(defmethod plus ((x arint) y)        (plus-arint x y))
(defmethod plus (y (x arint))        (plus-arint x y))

(defun times-arint (x y)
  (let ((a (left x))(b (right x))(c (left y))(d (right y)))
    ;; need to check if either is external?
;; internal proper intervals 
;; a*c, a*d, b*c , b*d --> return [min[..], max[...]]

;; reciprocal

;; here's a backstop for nonsensical arguments to times

(defmethod reciprocal (a)`(Power ,a -1))

(defmethod reciprocal ((x (eql omega)) omega)

(defmethod reciprocal ((x (eql infinity)) 0)

(defmethod reciprocal ((x rational))
  (if (= x 0) infinity (expt x -1)))

(defmethod quotient ((x oi)(y rational))
	(times x (reciprocal y)))

(defmethod quotient ((x rational)(y oi))
	(times x (reciprocal y)))

;; power (integer power only) generalizes multiplication

(defmethod power (a b) `(Power ,a ,b))

(defmethod power ((a (eql infinity)) (b integer))
  (cond ((> b 0) infinity)
	((= b 0) 1)
	((< b 0) 0)))

(defmethod power ((a (eql omega)) (b integer))omega)

(defmethod power ((a rational) (b integer))(expt a b))

;; difference

(defmethod difference ((x rational)(y oi))
  (plus x (negate y)))

(defmethod difference ((x oi)(y rational))
  (plus x (negate y)))

(defmethod negate (y) `(Times -1 y))

(defmethod negate ((y rational))(- y))

(defmethod negate ((y oi)) y)  ;; omega and infinity are unchanged

;; need to be able to compare these guys.
;; note that infinity is + or - and that for the extended numbers,
;; "trichotomy" fails.  That is,  there are a, b such that 
;;  a>b, a<b, and a=b are all false.

(defmethod greaterp((x rational)(y rational))(> x y))
(defmethod greaterp((x oi) (y oi)) nil) ;; not equal or lessp, either
(defmethod greaterp((x oi) (y rational)) nil)  ;+ or - inf, or omega

(defmethod lessp   ((x rational)(y rational))(< x y))
(defmethod lessp   ((x oi) y) nil) ;; not equal or greaterp, either
(defmethod lessp   (x (y oi)) nil) 

(defmethod isequalp  ((x rational)(y rational))(= x y))
(defmethod isequalp  ((x oi) y) nil)  ;; omega is not equal to itself
(defmethod isequalp  (x (y oi)) nil)  ;; nor is infinity

;; one tests for omega-ness or infinity-ness by using lisp's eq.

;; We will need max and min later.  Let's define them here.
;; Ref: W. Kahan.  How should Max and Min be defined? 
;; (work in progress, May 25, 1989).

;; conditions for M := max(r,s).
;;  (1) neither r>M  nor  s>M
;;  (2) M is either r or s
;;  (3) M is omega when both r and s are omega.

;; only (3) is controversial, and the alternative might be
;;  (3') M is omega when either r or s is omega.

;;; HERE WE RUN INTO TROUBLE if we expect to be able to use the
;;; sign of infinity...
" 
Previously we had only one Infinity, but we do not, in that case,
have a way to compute M := max(5,Infinity) since either 5 or Infinity
satisfies conditions 1,2,3.

We cannot satisfy these rules.  These choices come to mind:
(a) allow signed infinity.  Return to Go, Lose $200.
(b) respond with Omega
(c) respond with Infinity
(d) use max/min to round up or round down, as if we know which
direction that the particular infinity really is.
 "

(defmethod maximum (x y)`(Max ,x ,y))
(defmethod maximum ((x (eql omega))(y (eql omega))) omega)
(defmethod maximum ((x (eql omega))(y rational)) y)
(defmethod maximum ((y rational)(x (eql omega))) y)
(defmethod maximum ((rational x)(rational y))(max x y))

;;similarly for minimum

(defmethod minimum (x y)`(Min ,x ,y))
(defmethod minimum ((x (eql omega))(y (eql omega))) omega)
(defmethod minimum ((x (eql omega))(y rational)) y)
(defmethod minimum ((y rational)(x (eql omega))) y)
(defmethod minimum ((rational x)(rational y))(min x y))

;;; now, intervals.  

"

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
one continuous projective interval, and is the complement of the
interior interval [a,b]  except that the points a and b are included
in both.

Notes:
  Since Infinity in the exterior interval [1,0], we must
  make sure this happens.

  The extended rational number omega is in the interval omega, no other.

  There should be no interval with identical left and right endpoints
  (unless you allow omega = [infinity, infinity])

  If you add (say) [0,1] to [1,0], you get, not [1,1] but omega.

  There are only a few ways to creating a 0 length interior interval. 

  Example:
  [1,2]^0  -> 1 (note [-1,1]^0 would be omega).
  0 * [1,2] -> 0

Now consider `improper intervals' with one or both endpoints not in Q.

There is only one interval with both funny endpoints, and that is
[infinity, infinity] which we do not allow explicitly.  It's omega.

All numbers greater than or equal to rational b would be denoted
 by [b, infinity]

The complement of the numbers in [b, infinity] is the exterior interval
    [infinity, b]. 
This is all numbers less than or equal to rational b.

You can look at this latter interval as the interior interval from
 -infinity to b. 


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

;; (defmethod minimum) (defmethod maximum) 
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

      




;; Questions/extensions
;; What about floating-point or arb. precision flt. point endpoints?

;; what if we want to have arithmetic on special numbers like
;; +Infinity, -Infinity, Indefinite (or perhaps Undefined)
;; and intervals that exclude endpoints (selectively) 
