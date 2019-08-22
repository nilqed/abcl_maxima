;; -*- Mode:Common-Lisp;Package:mma; Base:10 -*-
;; Mathematica(tm)-like stuff for Maxima.
;; copyright (c) 2011 Richard J. Fateman

(in-package :mma)

(eval-when (:compile-toplevel :load-toplevel)
  (if (find-package "MAXIMA") nil  (defpackage :maxima)))

(defparameter macsubs ;; some of these equivalences are slightly bogus
    
    '((|Set| . maxima::msetq) (|Equal| . maxima::mequal)
 (|Pattern| . maxima::$Pattern) (|Blank| . maxima::$Blank)
 (|Increment| . maxima::$Increment) (|Part| . maxima::$Part)
 (|Greater| . maxima::mgreaterp) (|GreaterEqual| . maxima::mgeqp)
 (|LessEqual| . maxima::mleqp) (|Plus| . maxima::mplus)
 (|Times| . maxima::mtimes) (|Power| . maxima::mexpt)
 (|Sin| . maxima::%sin) (|Cos| . maxima::%cos) (|List| . maxima::mlist)
 (|N| . maxima::Numeric_eval) (|CompoundExpression| . maxima::mprogn)
      (|If| . maxima::mcond) (|Module| . maxima::mprog) (/ . maxima::rat)
      (|Real| . maxima::mplus)
  
  ))
;; etc etc
    

(defun mma2max(r)
  (cond ((atom r)
	 (cond ((numberp r) 
		(cond ((floatp r) r)
		      ((complexp r);; do a+b*%i
		       (list '(maxima::mplus) (mma2max (realpart r))
			     (list '(maxima::mtimes) 'maxima::$%i(mma2max (imagpart r)))))
		      #+ignore
		      ((ratiop r)  ;; gcl has no ratiop
		       (list '(maxima::rat) (numerator r)(denominator r)))
		      (t r))) ;other number e.g. float
	       ;; an atom but not a number.
	       ;; if it is a user name e.g. foo, do we make it $foo?
	       ;; how can we tell?
	       ;; for now, we don't. but the dollarsign
	       ((symbolp r)
		(let ((l (assoc r macsubs)))
		  (cond (l (cdr l))	;found a translation
			(t (intern (format nil"$~a"r) :maxima)))))
	       
	       
	       
	       (t r)))
	(t
	 (cons  (list (mma2max (car r)))
		(mapcar #'mma2max (cdr r))))))

;; . while, for, ordinary function calls.

;; while n>0 do (print(n),n:n-1) looks like this...
; ((MDO) NIL NIL NIL NIL NIL ((MNOT) ((MGREATERP) |$n| 0))
; ((MPROGN) (($PRINT) |$n|) ((MSETQ) |$n| ((MPLUS) |$n| ((MMINUS) 1)))))


 ;; etc etc

(defparameter macsubs-more  ;; some of these equivalences are very bogus
    '())
     

;; we could handle Comparison .   mockmma doesn't have Inequality, Unequal, Equal.
;; (Comparison x Greater y)  <-> ((mgreaterp) $x $y)


(defun wrapcar(x)(cons (list (car x))(cdr x))) ; change (a b c) to ((a) b c)

(defun max2mma(e)
  (cond ((atom e) 
	 (cond ((symbolp e) (st$ e)) ;;fiddle with stripdollar,
	       ((complexp e)`(|Complex| (realpart e)(imagpart e)))
	       (t e)))
	
	(t(cons (max2mmaop (caar e))
		(mapcar #'max2mma (cdr e))))))

(defun max2mmaop(k)
  (let ((r (rassoc k macsubs)))
    (if r (car r) (intern (symbol-name k) :mma))))

(defun st$(x) ;; remove $ if any
  (let ((r (symbol-name x)))
    (if (or (char= (aref r 0) #\$)
	    (char= (aref r 0) #\%))
	(intern (subseq r 1) :mma)  (intern r :mma) )))




 
#| problems.  Check on "AND" in lisp vs mma.
print[mma2max[n]] is no good.  n is mapped to "numeric_eval"...

what is n//N?

in Mathematica, symbol N is protected.  symbol n is something else.
|#


;;; convert from mma char string to maxima internal form.
;;; Uses mma2max, probably in need of additional diddling around
;;; to convert more "stuff" from mma language to maxima,
;;; and of course some mma stuff, esp. patterns, does not
;;; make much sense in maxima, anyway.  Still to do, add a few
;;; hundred special functions etc. if you want them.

(defun maxima::$from_mma (x)
    (mma::mma2max(mma::pstring x)))

;;; THIS IS THE IMPORTANT INTERFACE PROGRAM, I THINK

;;; Read the char string to mma internal form, then
;;; evaluate it in mma, and then convert the result to maxima.

(defun maxima::$eval_string_mma (x)
  (mma::mma2max(mma::meval(mma::pstring x))))

;; One can write simple maxima functions for simple
;; interfaces. Here int_by_mma  takes two
;; ordinary maxima arguments f,x and integrates: Int(f,x) in MMA!!
;; It then returns the answer in maxima form.
;; It is a special case of fun_by_mma, for function "Int".

;; Let us say you want to execute the MockMMA function FooBar on arguments
;; x+y, w+z.   In Maxima, do this:   fun_by_mma("FooBar",x+y,w+z);

;; Thus:  int_by_mma(f,x):=fun_by_mma("Int",f,x);

(defun maxima::$fun_by_mma (fun &rest args)
  (mma::mma2max  (mma::meval 
		  (cons (mma::pstring fun) 
			(mapcar #'mma::max2mma args)))))

;;   eval_string_mma("foo[x_]:=x+42");
;;   eval_string_mma("foo[4]");    
;;  or ...
;;   fun_by_mma ("foo", sin(q));
;;  eval_string_mma("Clear[a,b,c,x, z,quad]");
;;  eval_string_mma("quad[a_.*z_^2+b_.*z_+c_. ,z_] := ans[a,b,c,z]");
;;  eval_string_mma("quad[z^2+4 z, z]");
;;     returns ans(1,4,0,z)






