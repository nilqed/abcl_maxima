;;; elementary functions: take rationals return intervals

(require 'affinrat2)
;(require 'intclass)

;; here's the explanation for EXP

Arbitrary precision rational exponential function.
input: rational number x, interval width w.
output: an interval of two rational numbers I=[a,b]  of width less than or
equal to w, such that exp(x) is in I.

Let r = log2(e)   [ note: log2(e) = 1/ln(2) = 1.442695040888963...].
Decompose x as
x = U*r + V where  0<=V<r and U is an integer. 
  (put aside for the moment how to compute r. We might need it very
   accurately.  But given r, we can computer U and V easily...
 e.g. (truncate x r) in CL.)

Next we compute exp(x) = exp(U*r)*exp(V).  The first part is easy since

exp(U*r) = 2^U , and U is an integer.

The value of exp(V) will be between 1.0 and exp(1.44269..) = 4.23208610655708..

We can compute a lower bound on exp(V) simply by truncating the
monotonically-increasing convergent sequence
exp(V) = 1+V+V^2/2+ ... +V^n/n! + ....

exp_low(v,n):= exp_(v,n-1)+v^i/i!

Unless v=0 this will always be an underestimate, since all the omitted 
terms are positive.  
To get an upper bound we use Taylor's theorem with remainder, knowing
that the truncation error is bounded by the maximum (exp(x)*x^n/n!) for
 x between  0 and v. This occurs at v, so any (over) estimate
of exp(v) can be used.  
let q= (9/4*v+1 ) be a cheap fast rational overestimate for e^v on
the interval 0<v<1.44268. That is,

exp_high(v,n):=exp_(v,n-1)+(q+1)*v^i/i!

How to make the width small enough?  We have to 
figure out how to compute log2(e) accurately enough..

Then figure exp of an interval

;; ratexp1 works for  0<x<log2(e).
(defun ratexp1 (x tol)  ;; input is rat. argument and tolerance (positive rat.)
  (let
      ((q (+ 1 (* 9/4 x))))
    (do (( n 1  (1+ n))
	 (ans 0 (+ ans term))
	 (term 1 (/ (* x term) n))
	 (err q (* term q)))
      ((< err tol);quit when error bound is met
       ;;(make-interval ans (+ ans err))
       (list ans (+ ans err))
       ))))

(defun ratexp (x tol)  ;; input is rat x, any size
  (let ((log2e (/ (log e)(log 2)))) ;; gotta do better than this
    
    (multiple-value-bind(u v)(truncate x log2e)
			(times (ratexp1 v tol)(expt 2 u)))
     ))


(defun rat-e(tol);; should cache this, actually
   (ratexp1 1 tol))

;; here's an attempt at ln

(defun ratlog (x tol)  ; log base e
	(cond ((not (> x 0)) (error "non-pos arg to ratlog")))
	(let*((e (car (rat-e tol)))
	      (over (/ 1  e))      )
	  ;; here we should do some argument reduction
	  ;; to get argument between 1/e and e.
	  (do ((n 0 (1+ n))
	       (oldans 2 ans) ;; huh..
	       (ans 0
		    (+
		     ans
		     (* 2 (/ term (1+ (* 2 n))))))
	       (term 1 (* term x)))
	      ((< (abs(- ans oldans)) tol) (list ans oldans)))))


