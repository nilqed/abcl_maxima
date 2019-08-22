;; -*- Mode:Common-Lisp;Package:mma; Base:10 -*-

(eval-when (compile eval)
  (load "mma")
  (load "poly")
      (in-package :mma))

;; change definition of make-real in parser.lisp  
;; could be to 'single-float if you wish

(defun make-real (int frac)(coerce (+ int frac) 'double-float))

;; in poly.lisp,

;;change one line in definition in p/ 

(defun p/ (p s)	
  (cond ((coefzerop s) (throw 'p/ (values nil "Division by zero")))
	((coefonep s) p)
        ((coefzerop p) p)
        ((coefp p)
         (cond ((coefp s)
                (cond ((eql 0 (mod p s))(/ p s))
 ;;; add one line to do floats, instead of bombing out
		      ((or (floatp p)(floatp s)) (/ p s))
		      
                      (t (throw 'p/
				(values nil "Inexact integer division")))))
               (t (throw	
                   'p/
                   (values nil "Division by a polynomial of higher degree")))))
        ((or (coefp s) (var> (mainvar p) (mainvar s))) (p/vc p s))
        ((var> (mainvar s) (mainvar p))
         (throw 'p/
                (values nil "Division by a polynomial of higher <degree")))
        (t (p/helper p s))))

;; also in simp1.lisp
 (defun into-rat (x)
  
  (typecase x 
	    (integer (coef2rat x))
	    (ratio  (make-rat :numerator `((,(numerator x) . 1))
			      :denominator `((,(denominator x) . 1))))
	    (rat x) ;already a rat form
	    (float  (coef2rat x))
;;	    (array  whatever)
	    (t
	     (cond((gethash x reptab))       ; see if remembered
		  ((setf (gethash x reptab) (into-rat-1 x)))))
	    ))


(defun intol2 (p var)
  (let (res term)
    (do ((i (1- (length p)) (1- i)))
	((= i 0) res)
	(setq term (intol* (outof-rat-check (svref p i)) (intol^ (1- i) var)) )
      (cond ((eq term 0)) 
	    ;;add this line
	      ((and (numberp term)(= term 0)))
	      (t (setq res (ucons term res)))))))

(defun intol* (a b)
  (cond ((eql a 0) 0)
	;; add this line
	((and (numberp a)(= a 0)) 0)
	((eq a 1) b)
	((eq b 1) a)
	(t (ucons 'Times 
		  (uappend (intol*chk a) (intol*chk b))))))


;;; dunno about this one... 

(defun pgcdsr (u v)
  (cond ((coefzerop u) v)
	((coefzerop v) u)
	((coefp u) (cond ((coefp v)
			  (if (or (floatp u)(floatp v)) v ; could be u, 1, or ..  
			    (gcd u v)))
			 (t (pcontentxsr v u))))
	((coefp v) (pcontentxsr u v))
	((var> (mainvar v) (mainvar u)) (pcontentxsr v u))
	((var> (mainvar u) (mainvar v)) (pcontentxsr u v))
	(t (pgcd2sr u v))))

(defun fcoefonep(x)(and (numberp x) (= x 1)))
(defun fintol (f)
  (let ((c (caar f))); constant term
    (do ((j (cdr f)(cdr j))
	 (lis nil))
	((null j)
	 (cond ((null lis) c)
	       ((and (fcoefonep c)(null(cdr lis)))
		(car lis))
	       (t (cond ((fcoefonep c) (ucons 'Times (uniq(nreverse lis))))
			(t (ucons 'Times(ucons c (uniq (nreverse lis)))))))))
	;; the loop body:
	(setq lis (cons (fintol2 (caar j)(cdar j)) lis)))))