;; -*- Mode:Common-Lisp;Package:mma; Base:10 -*-
;; (c) 1986, 1991 Richard Fateman
;; Common Lisp Bigfloats, part II:
;; some elementary functions: sin, cos, exp,log,atan

;(proclaim '(optimize (speed 3)))
;(proclaim '(optimize (safety 0)))
(eval-when (compile) (load "bf"))
(provide "bfelem")

(proclaim '(special bigfloat-bin-prec  bigfloatone bigfloatzero
		    bigfloat%e bigfloat%pi))

(defmacro fpsin1 (x) `(fpsincos1 ,x t)) 

(defmacro fpcos1 (x) `(fpsincos1 ,x nil))

;; initialization

(defun bigfloat-init (q)  ;; q is an integer: how many decimal digits needed.
  (setq bigfloat-bin-prec (+ 2 (integer-length (expt 10 q)))
	bigfloatone (intofp 1) 
	bigfloatzero (bcons2 0 0)
	bigfloat%e (fpexp1  (intofp 1))
	bigfloat%pi (fppi1 bigfloat-bin-prec))
  q)

(defun bigfloat-atan (x)
	(let*(term x2 tmp
		  (one (intofp 1))
		  (two (intofp 2))
		  (ans one)
		  (oans two))
	(cond ((bigfloat-> (bigfloat-abs x) one)
	       (setq tmp (bigfloat-/ (fppi) two))
	       (setq ans (bigfloat-- tmp (bigfloat-atan (bigfloat-/ one x))))
	       (cond ((bigfloat-> ans tmp)
		      (setq ans (bigfloat-- ans (fppi))))))
	      ((bigfloat-> (bigfloat-abs x) (bigfloat-/ one two))
	       (setq tmp (bigfloat-/ x (bigfloat-+ (bigfloat-* x x) one)))
	       (setq x2 (bigfloat-* x tmp) term (setq ans one))
	       (do ((n 0 (1+ n)))
		   ((bigfloat-eql ans oans) ans)
		   (setq 
		    term
		    (bigfloat-* term 
				(bigfloat-* x2 
					    (bigfloat-/ 
					     (intofp (+ 2 (* 2 n)))
					     (intofp (+ (* 2 n) 3))))))
		   (setq oans ans ans (bigfloat-+ term ans)))
	       (setq ans (bigfloat-* tmp ans)))
	      (t (setq ans x x2 (bigfloat-- (bigfloat-* x x)) term x)
		 (do ((n 3 (+ n 2)))
		     ((bigfloat-eql ans oans) ans)
		     (setq term (bigfloat-* term x2))
		     (setq oans ans 
			   ans (bigfloat-+ ans 
					   (bigfloat-/ term (intofp n)))))))
	ans))

(defun bigfloat-atan2 (y x) ;atan(y/x) from -pi to pi
  (cond ((bigfloat-zerop x)       ; atan(inf), but what sign?
	 (cond ((bigfloat-zerop y) (error "atan(0/0)"))
	       ((bigfloat-posp y) (bigfloat-/ (fppi) (intofp 2)))
	       (t(bigfloat-/ (fppi) (intofp -2)))))
	((bigfloat-posp x)
	 (cond ((bigfloat-posp y) (bigfloat-atan (bigfloat-/ y x)))
	       (t (bigfloat-- (bigfloat-atan (bigfloat-/ y x))))))
	((bigfloat-posp y)
	 (bigfloat-+ (fppi) (bigfloat-atan (bigfloat-/ y  x))))
	(t (bigfloat-- (bigfloat-atan (bigfloat-/ y x)) (fppi)))))

(defun bigfloat-tan (a) 
  (bigfloat-convert
   (let* ((bigfloat-bin-prec (+ 8. bigfloat-bin-prec))
	  (y (bigfloat-convert a )))
	 (bigfloat-/ (bigfloat-sin1 y t) (bigfloat-sin1 y nil)))))

(defun bigfloat-exp (x)       ;; exponential for arbitrary arg
       (prog (r s)
	     (cond ((bigfloat-posp x)) ;; for negative x, compute 1/e^(-x)
		   (t (return (bigfloat-inv (bigfloat-exp (bigfloat-- x))))))
	     (setq r (bigfloat-intpart x))  ;; for large x, compute e^r * exp(x-r)
	     (return (cond ((< r 2)  (fpexp1 x))
			   (t (setq s (fpexp1 (bigfloat-- x (intofp r))))
			      (bigfloat-* 
			       s
			       (bigfloat-convert

				(let ((bigfloat-bin-prec
				       (+ bigfloat-bin-prec (integer-length r) -1)))
				     (bigfloat-expt (fpe) r)))))))))

;; exponential for small arg
(defun fpexp1 (x) 
  (do (( n 1  (1+ n))
       (ans (intofp 1))
       (term (intofp 1))
       (oans (intofp 0)))
      ((bigfloat-eql ans oans) ans) ;quit when two iterations same
      (setq term(bigfloat-/ (bigfloat-* x term) (intofp n))
	    oans ans
	    ans(bigfloat-+ ans term))))



;; (fpe) return big floating point %e.  
;; it returns truncated bigfloat%e if possible, else recomputes.
;; in any case, bigfloat%e is set to last used value. 

(defun fpe nil 
       (cond ((= bigfloat-bin-prec (bigfloat-precision bigfloat%e))  bigfloat%e)
	     ((< bigfloat-bin-prec (bigfloat-precision bigfloat%e))
	      (setq bigfloat%e (bigfloat-convert bigfloat%e)))
	     (t (setq bigfloat%e (fpexp1 (intofp 1))))))

(defun fppi nil 
       (cond ((= bigfloat-bin-prec (bigfloat-precision bigfloat%pi)) bigfloat%pi)
	     ((< bigfloat-bin-prec (bigfloat-precision bigfloat%pi))
	      (setq bigfloat%pi (bigfloat-convert bigfloat%pi)))
	     (t (setq bigfloat%pi (fppi1 bigfloat-bin-prec)))))

(defun fpone nil 
       (cond
	    ((= bigfloat-bin-prec (bigfloat-precision bigfloatone)) bigfloatone)
	     (t (setq bigfloatone(intofp 1))))) 

;; comppi computes pi to n bits.
;; that is, (comppi n)/(2^n) is an approximation to pi.

(defun comppi (n) 
  (let* ((a (expt 2 n))
	 (b (truncate a 8))
	 (c (+ (* 3 a) b))) 
	(do ((i 4 (+ i 2)))
	    ((zerop b) c)
	    (setq b (truncate (* b (1- i) (1- i))
		       (* 4 i (1+ i))))
	    (setq c (+ c b)))
	))

(defun fppi1 (n) ;; compute pi to that many bits plus a few
  (bigfloat-/ (intofp (comppi (+ n 3)))
	      (intofp (expt 2 (+ 3 n)))))
			  
;; fl= t for sin, nil for cos	
(defun bigfloat-sin (x) (bigfloat-sin1 x t)) 

(defun bigfloat-cos (x) (bigfloat-sin1 x nil)) 

;; Bigfloat-sin1 computes sin or cos to precision bigfloat-bin-prec,
;; It uses extra precision for the value of pi for large argument
;; values. It is still possible to get fooled somewhat by an argument
;; value that is surprisingly accurate in its approximation to an
;; integer multiple of pi/2. (More accurate than precision+exponent+4)
;; in which case the result will not be as accurate as the
;; precision would permit.

(defun bigfloat-sin1 (x fl) 
  (flet((insine
	 (bigfloat-bin-prec xt) 
	 (let*
	     ((x (bigfloat-convert xt))
	      (piby2 (bigfloat-/ (fppi) (intofp 2)))
	      (r (bigfloat-intpart (bigfloat-/ x piby2))))
;	   (format t "~a, ~a, ~a~%" x piby2 r) ;debug info
	   
	   (setq x (bigfloat-+ x
			       (bigfloat-* (intofp (- r))
					   piby2)))
	   (setq r (mod r 4))
	   (cond (fl (cond ((= r 0) (fpsin1 x))
			   ((= r 1) (fpcos1 x))
			   ((= r 2) (bigfloat-- (fpsin1 x)))
			   ((= r 3) (bigfloat-- (fpcos1 x)))))
		 (t (cond ((= r 0) (fpcos1 x))
			  ((= r 1) (bigfloat-- (fpsin1 x)))
		  ((= r 2) (bigfloat-- (fpcos1 x)))
			  ((= r 3) (fpsin1 x))))))))
	
       (let ((sign (if fl (bigfloat-posp x) nil))
	     res)
	 (setq x (bigfloat-abs x))
	 (setq res (cond ((bigfloat-zerop x)
			  (if fl (intofp 0)	;sin(0)
			    (intofp 1)	;cos(0)
			    ))
			 (t
			  (bigfloat-convert
			   (insine
			    (max bigfloat-bin-prec
				 (+ 4 bigfloat-bin-prec (bigfloat-exponent x)))
			    x)))))
	 (cond (sign res) (t (bigfloat-- res))))))


;; compute sin or cos in (0,pi/2).  fl is t for sin, nil for cos.
(defun fpsincos1 (x fl)
       (prog (ans term oans x2)
	     (setq ans (cond (fl x)(t(intofp 1)))
		   x2 (bigfloat-- (bigfloat-* x x)))
	     (setq term ans)
	     (do ((n (cond (fl 3) (t 2)) (+ n 2)))
		 ((and oans(bigfloat-eql ans oans)) ans) ;;oans is initial nil.. bug
		 (setq term (bigfloat-* term (bigfloat-/ x2 (intofp (* n (1- n))))))
		 (setq oans ans ans (bigfloat-+ ans term)))
	     (return ans)))


(defun bigfloat-log (x) 
  (prog (over two ans term e) 
	(cond ((not (bigfloat-posp x)) (error "non-pos arg to bigfloat-log")))
	(setq e (fpe) over (bigfloat-inv e) ans 0)
	(do () (nil)
	    (cond ((bigfloat-eql x e) (setq x nil) (return nil))
		  ((and (bigfloat-< x e) (bigfloat-< over x))
		   (return nil))
		  ((bigfloat-< x over)
		   (setq x (bigfloat-* x e))
		   (setq ans (1- ans)))
		  (t (setq ans (1+ ans))
		     (setq x (bigfloat-/ x e)))))
	(cond ((null x) (return (intofp (1+ ans)))))
	(setq x (bigfloat--  x (fpone)) ans (intofp ans))
	(setq 
	 x
	 (bigfloat-expt (setq term
		       (bigfloat-/ x (bigfloat-+ x (setq two (intofp 2)))))
		 2))
	(do ((n 0 (1+ n))
	     (oldans ans)
	     (ans ans
		  (bigfloat-+
		   ans
		   (bigfloat-* two (bigfloat-/ term (intofp (1+ (* 2 n)))))))
	     (term (bigfloat-* term x)))
	    ((bigfloat-eql ans oldans) ans))))


