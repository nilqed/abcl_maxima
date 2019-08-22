;;; -*- Mode:Common-Lisp;  Base:10 -*-
;; Written by: Richard J. Fateman
;; File: diffrat.lisp
;; (c) 1991 Richard J. Fateman

;; This is a derivative-divides integration program
;; written to use rational forms. Much of the work is
;; in peripheral issues, like computing derivatives of
;; random functions and testing to see if expressions are
;; free of variables.  This program depends on simplification
;; to find out if expressions divide out evenly, and will lose
;; (fail to integrate), if it is required to know identities
;; that are algebraic or transcendental (e.g. sin^2+cos^2 =1).

;; Also, this program produces ANTIDERIVATIVES, not really integrals.
;; That is, the domain of the answer may not be the same as the domain
;; of integration (unstated, but lurking in most applications).
;;(provide 'diffrat)
(in-package :mma)
;;(require "poly")(require "simp1")(require "rat1")

(defun integrate (e x)(diffdiv e x))	;testing program
(defparameter integraterules  '(((|Int| (|Pattern| z(|BlankSequence|))) (lispapply integrate z)))) ;; works

(defun |D|(e x)(outof-rat(ratdiff (into-rat e) x)))

;; Some useful utility programs

;; pfreevar returns t if the poly p is free of the variable numbered v.

(defun pfreevar (p v)
  (labels
   ((pf1(x)
	(if (coefp x) t
	  (and (freevar (gethash (svref x 0) revtab) (gethash v revtab))
	       (every #'pf1 x)))))
   (pf1 p)))

;; freevar returns t if  x is free of the variable or kernel v.
;; x and v are in list representation. Only explicit dependencies
;; are discovered here.

(defun freevar (x v)
  (labels ((f1 (x)
	       (cond ((eq x v) nil)
		     ;; here could check for "x depends on v indirectly."
		     ((consp x) (every #'f1 (cdr x)))
		     ;; could check for poly or rat stuff
		     (t t))))
	  (f1 x)))

;; see if a rational form r is free of a variable, in list form, v. v may
;; be inside a kernel as, say (Log v) or a regular variable.
;; As a particular use, we know that if rfreevar (r v) is t, 
;; then r is a constant wrt v, and integrate(r,v) = r*v {+ a const}.

(defun rfreevar(r v) ;; r is a rat form. v is a list-form
  (let ((v (look-up-var v)))
    (flet ((rf1 (x)
		(pfreevar (car x) v)))
	  (and (every #'rf1 (rat-numerator r))
	       (every #'rf1 (rat-denominator r))))))

;; (ratderiv r v) computes the partial derivative
;; of the rational expression r wrt to the list-form expression v,
;; which is presumed to be either an indeterminate, or perhaps
;; a kernel, in which case it should be simplified. The answer is
;; in rational form.

;; In an attempt to do this efficiently in rational form we consider
;; two cases:
;; (a) v is an indeterminate and the only kernel in r that
;; involves v is exactly v.  This is the "fast" case, in the sense
;; that almost all the arithmetic can be done on polynomials.  It is
;; a subset of case b, and if we had only one program to write, we'd
;; have to write case b.
;; (b) r includes kernels that depend on v in other ways.  For example,
;; (Log v) or (Sin v). All the arithmetic must be done in rational form
;; if the derivative is rational (not polynomial) in v. (Log v) is like
;; that.

;; This may seem like a long way to do it, but let's try, anyway.

;; collectvars returns a list of all the variable (numbers) in a
;; rational form r.  E.g. (collectvars #(3 #(1 1 1) 0 1)) -> (1 3)

(defun collectvars (r &aux (cv nil))
  (labels
   ((cv2 (pr) ;; cv2 is applied to each pair: poly . power
	 (cv3 (car pr)))
    (cv3 (p) ;; cv3 is applied to each polynomial
	 (cond ((coefp p) nil)
	       (t (pushnew (svref p 0) cv)		  
		  (do ((i (1- (length p))(1- i)))
		      ((= i 0) nil)
		      (cv3 (svref p i)))))))
	      
   (mapc #'cv2 (rat-numerator r))
   (mapc #'cv2 (rat-denominator r))
   cv))

;; for each variable in a rational form, we want to compute (or remind
;; ourselves that we already know) its derivative wrt *var*


(declaim (special result difftab *var* *varnum* *sign* *r*))


(defun ratdiff(r v)
  (flet
   ((setupderiv (h) ; variable is global, h is a number..
  ;; we store derivative on a special derivative hash-table
		(setf (gethash h difftab) 
		      (into-rat(gendiff (gethash h revtab) *var*)))))
   (let*
      ((c (collectvars r))
       (*var* v)
       (*r* r)
       (*varnum* (gethash v vartab))
       (difftab (make-hash-table :size (+ 2 (length c))))
       (*sign* 1)
       (result (poly2rat 0 1)))

    ;; set up derivatives for all kernels in this rational expression
    (mapc #'setupderiv c)
    ;;   (showhash difftab)   
    ;; for each (poly . pow) do result= pow*r*poly'/poly + result
    (mapc #'ratdiff1 (rat-numerator r))
    (setq *sign* -1)
    (mapc #'ratdiff1 (rat-denominator r))
    result)))

(defun ratdiff1 (pr);

  (let ((poly (car pr))
	(pow (* (cdr pr) *sign*))) ;; takes care of numerator/denominator
    (cond ((coefp poly) nil)
	  ((pfreevar poly *varnum*) nil)
	  (t 
	   ;; for each (poly . pow) do result= pow*r*poly'/poly + result
	   (setq result
		 (rat+ result
		       (rat* *r* (rat* (poly2rat pow 1)
				       (rat* (ratdiff2 poly)
					     (poly2rat poly -1))))))))))

;; ratdiff2 takes a polynomial and returns a rational form
;; which is its derivative

(defun ratdiff2 (p)
  (if (coefp p) (return-from ratdiff2 (poly2rat 0 1)))
  (let ((dp (gethash (svref p 0) difftab))
	(ans (poly2rat 0 1))
	(x (vector (svref p 0) 0 1))) ;;mainvar as poly
 ;; (format t "~%dp=~s,x=~s" dp x)
 (cond ((fpe-coefzero-p (rat-numerator dp))
	;; main var of poly is independent of *var*

	   (do ((i (1- (length p))(1- i)))
	       ((= i 0) ans)
	       (setq ans (rat+ ans (rat* (poly2rat x (1- i))
					 (ratdiff2 (svref p i)))))))
	  (t  ;; main var of poly depends on *var*
	   (do ((i (1- (length p))(1- i)))
	       ((= i 0) ans)
	       (cond ((coefzerop (svref p i))) ; add zero term.. do nuthin.
		     ;; given c*p^n, set
		     ;;ans := ans + n*c*p^(n-1)*dp + dc *p^n
		     (t 
		      (setq ans
			    (rat+ ans
				  (rat+
				   (rat* (ratdiff2 (svref p i))
					 (poly2rat x (1- i)))
				   (rat* (poly2rat(1- i) 1)
					 (rat* (poly2rat (svref p i)1)
					       (rat* (poly2rat x (- i 2))
						     dp)))))))))))))

;;; set up a table of derivatives .. or here we are
;;; using atom property lists.  Is that fair?
;;; maybe a hash table would be better?

#+ignore
(defmacro make1dfun(r)  `(list (lambda(%) ,r)));; list of one function, deriv wrt arg 1 

(defun make1dfun(r)  `((|Function| (|List| %) ,r)));; list of one function, deriv wrt arg 1 


;;... etc could add the rest of ArcHyperbolic functions ..Cot,Csc
;;A better approach would be to allow the derivative information to be
;;augmented from the top level.
;; e.g. a command Gradef[F, { d1F,d2F}]
;; says that derivative(F[a(x),b(x)], x) = (da/dx)* d1F[a,b] + (db/dx) d2F[a,b]

(defun |Gradef|(f lst)
  (setf (get f 'deriv)  
    (if (eql (|Head| lst) '|List|)	
	(cdr lst)
      (list lst)))		; singleton derivative
  f)
   



;; Eventually we have to do some non-rational differentiation, and this
;; program is the one that does it. 
;; Restrictions that remain: it doesn't grok derivatives of
;; symbolic Derivatives.  That is, (gendiff '(((Derivative 1) f) x) 'x)
;; should mean something, namely (((Derivative 2) f) x)

(defun gendiff (h v)
  (declare (special env))
  (cond ((eq h v) 1)
	((freevar h v) 0)
	((not (consp h)) (error "Gendiff of ~s" h))
	((member (car h) '(|Plus| |Times|) :test #'eq)
	 (outof-rat (ratdiff (into-rat h) v)))
	((eq (car h) '|Power|)
	 (if (integerp (caddr h))
	     (outof-rat (ratdiff (into-rat h) v))
	   (ged h v))) 
	
	;; fake a derivative if necessary
	(t 
	 (let((k (and(atom (car h)) (get (car h) 'deriv))))
		   
;;	   (format t "~% deriv=~s h=~s  var=~s" k h  v)
	     ;; Note that Mathematica (tm) uses the notation
	     ;; equivalent to (((Derivative 1) f) x) for f'[x].
	     ;; This allows for the handling of
	     ;; f(u(x),v(x)) ...
	     (cond
		  ((null k);;unknown deriv. Use chain rule
		    (do ((i (cdr h)(cdr i))
			 (dlist '(1) (ucons 0 dlist))
			 (ans nil; initialize answer
			      ;; all the other times through
			      (let ((thispart (gendiff (car i) v)))
				(if (eql 0 thispart) ans
				  (ucons `(|Times| ,thispart
						   ,(uniq 
						     `(((|Derivative| ,@dlist),(car h))
						       ,@(cdr h))))
					 ans)))))
			 ;; termination test
			((null i) (ratsimp (ucons '|Plus| ans)))
			;;do loop body is empty
		      ))

		  (t  
;;		   (format t "~%  k=~s"  k)
		     (do ((i (cdr h)(cdr i))
			  (difflist k (cdr difflist))
			  (ans nil	; initialize answer
			      ;; all the other times through
			       (let ((thispart (gendiff (car i) v)) )
				
		;;		 (format t "~%thispart = ~s " thispart)
				 
				(if (eql 0 thispart) ans
				  (cons
				   (list '|Times|  thispart  
					 (mapply (car difflist) 
						 (cdr h)
						 (cons (car difflist)
						       (cdr h))
						 env
						       ))
				   ans)))))
			 ((null i) (ratsimp (cons '|Plus|  ans)))
		       ;; empty loop body
		       ;;(format t "~% arg=~s diff=~s args=~s~%ans=~s" (car i)(car difflist) i ans)
		       )
		     ))))))

;; generalexponentdiff

(defun ged (e x)
  ;; x is not an integer  and e = (|Power| a b)
  (let ((a (cadr e))
	(b (caddr e)))
    (ratsimp
     (cond((freevar b x)
	   ;; one form would be b*a^(b-1)*d(a,x)
	   ;; a better form would be b*a^b/a *d(a,x), using same kernels
	   `(|Times| ,b ,e (|Power| ,a -1) ,(gendiff a x)))
	  ((eq a '|E|);; exponential
	   `(|Times| ,e ,(gendiff b x)))
	  (t `(|Times| ,e ,(gendiff `(|Times| ,b (|Log| ,a)) x)))))))
	  
      
;; This is the main derivative-divides integration program.
;; Integrate exp-in  wrt var, both in list form.  It returns
;; an answer in list form, as well.

(defun diffdiv (exp-in var) 
  (let*
      ;; factoring exp would be "most" effective, but let's just
      ;; put it into (partially) factored rational form.
      ((exp (into-rat exp-in))
       ;; bind factors to a list of all the (term . power) pairs. reverse
       ;; the sign of the powers in the denominator.
       ;; example,  x^2*y^2/(log(x)+1)^2 comes out as
       ;; ( (1 . 1) (x . 2)  (y . 2) (1 . -1) ((log(x)+1) . -2))
       
       (factors (append (rat-numerator exp)
			(mapcar #'(lambda(x)(ucons (car x)(- (cdr x))))
				(rat-denominator exp))))
       ;; initialize constcoef, a rat term that does not 
       ;; involve integration variable
       (constcoef (poly2rat 1 1))
       (vnum (look-up-var var))
       (den nil)
       (ans nil)			; better init val?
       (vfactors nil))
    (do ((f factors (cdr f)))
	((null f) nil)
	;; several possibilities for (car f)  which is a (factor . power)
	(cond
	 ;; perhaps (caar f) is free of the variable var?
	 ((pfreevar (caar f)(look-up-var var))
	  ;;in which case we multiply constcoef by (caar f)^(cdar f)
	  (if (> (cdar f) 0) (setf (rat-numerator constcoef)
				   (fpe-insert (caar f) (cdar f)
					       (rat-numerator constcoef)))
	    (setf (rat-denominator constcoef)
		  (fpe-insert (caar f) (- (cdar f))
			      (rat-denominator constcoef)))))
	 (t (setq vfactors (cons (car f) vfactors)))))
    
  ;;(format t "~%constcoef=~s, vfactors=~s" (outof-rat constcoef) vfactors)
    ;; if all the factors are free of the variable of integration,
    ;; quit now with the answer.
    (if (null vfactors) 
	(return-from diffdiv
		     (outof-rat
		      (rat* constcoef(into-rat var)))))
    ;; vfactors is a list of the (factor . power) pairs containing var.
    (setq exp (rat/ exp constcoef))
    ;; Next, let's do some quick checks.
    ;; if the integrand is just a polynomial, we can do it
    ;; another way. Here's how...
     (if (and
	  ;; only the variable itself depends on var
	  (every
	   #'(lambda(h) (or (eql vnum h) 
			    (freevar (gethash h revtab) var))) 
	 (collectvars exp))
	  ;; and the denominator of exp is free of v entirely
	  (rfreevar (setq den (make-rat :numerator '((1 . 1))
					:denominator (rat-denominator exp)))
		    var))
	 (return-from 
	  diffdiv 
	  (outof-rat 
	   (rat* constcoef
		 (rat* den
		       (pintegrate (fpe-expand (rat-numerator exp))
				   vnum))))))

     ;; ok, we do not have a polynomial in var.  We could check here for
     ;; a simple rational function in var by the same kind of
     ;; check on the denominator (is it a simple poly in var also?)
     ;; But maybe derivative-divides will work on it, anyway..
     (do ((f vfactors (cdr f)))
	;; if we've exhausted vfactors unsuccessfully, return the "input".
	;; this is perhaps not what is wanted -- we could provide, for
	;; example, an error message, or we could call another routine.
	 ((null f) (return (ulist 'integrate exp-in var)))

	 (let* 
	     ((y (caar f))
	      (ypow (cdar f))
	      (yprime (ratdiff (poly2rat y 1) var)))
;;	  (format t "~% y=~s, ypow=~s yprime=~s" y ypow yprime)
;;	  (format t "~% y=~s, ypow=~s yprime=~s" (intol y) ypow (outof-rat yprime))
	  (setq ans (rat/ exp (rat* 
			       (poly2rat y ypow)
			       yprime)))
	  ;;(format t "~%ans=~s" (outof-rat ans))
	  (if (rfreevar ans var);; check if y*y' or y^n*y' divides
	      ;;; AHA -- WE've GOT IT!
	      (cond
	       ;; case of f=power[n], n not -1. we have y^n*y'
	       ((not(eql ypow -1))
		(return-from diffdiv
			     ;; const* (y^(p+1))/(p+1)
			     (outof-rat
			      (rat* (rat* constcoef ans)
				    (rat* (poly2rat y (1+ ypow))
					  (poly2rat (1+ ypow) -1))))))
	       
	       ((= ypow -1);; we have y^-1*y' -> log(y)
		(return-from 
		 diffdiv
		 ;; const* log(y)
		 (outof-rat(rat* constcoef
				 (rat* ans
				       (into-rat (ulist '|Log|
							(outof-rat (poly2rat y 1)))))))

		 ))
)
	    
	    ;; rfreevar test fails on y^n*y'.  Try extracting the head of
	    ;; y, that is, y=f(u), and look for f(u)*u'  (if we know
	    ;; an antiderivative for f, anyway.)
	    (cond 
	     ((not(= ypow 1))nil)
	  (t 
	     (let* ((ylist (intol y));; list form
		    
		    ;; if ylist has as head, a function of 1 variable 
		    ;; e.g. Sin
		    (head nil)
		    (antideriv nil)
		    (arg nil))
	       (cond ((and (consp ylist)
			   (atom (setq head (car ylist))))
		      ;;usual case
		      (setq antideriv (get head 'integ))
		      (setq arg (cadr ylist))))
;;	       (format t "~% y=~s, antideriv=~s yprime=~s" ylist antideriv		       (gendiff arg var))
	       ;; another case is (((Derivative list) fun) x1 ...)
	       ;; We haven't programmed that yet .. How to look up
	       ;; the antiderivative should not be toooo hard.
		     
	       (cond((atom ylist) nil);; if ylist is an atom, deriv is 0 or 1
		    ((or (cddr ylist)(null antideriv)) 
		     ;; we don't know antiderivative 
		     ;; or f has more than one argument
		     nil)
		    ;; check the exact division situation:
		    ((rfreevar
		      (setq ans (rat/ exp (rat* (poly2rat y 1)
						(into-rat (gendiff arg var)))))
		      var)
		     ;; AHA -- WE'VE GOT IT!

	       (return-from diffdiv
			    (outof-rat
			     (rat* (into-rat (subst (cadr ylist) '% antideriv))
				   (rat* ans constcoef))))))))))))))

;;Derivative divides cannot integrate polynomials, in general. This
;; program (Rint) can.  It leaves the answer in factored form, which
;; is sometimes neat, but sometimes distracting.

;;The task: Integrate a polynomial over the integers. 

;;The ploy is as follows:
;; We want to do it as much as possible as a polynomial over the integers,
;; so we can just do x^2 -> x^3/3 which requires rational numbers.
;; But we can accumulate denominators separately.  Then consider, for example,
;; integrating  9 + x + 3*x^2 + 7*x^3 - 8*x^4. 
;; Since the poly is of degree 4 set the denominator to 5!= 120.

;;Let g(k) := 5!/(k+1).  That is g(4) =24, g(3)=30, ... g(0) =120.

;;Then the answer is 1/120 times 
;;   9*g(0)*x + g(1)*x^2 +3*g(2)*x^3+7*g(3)*x^4-8*g(4)*x^5.
;; Now note that we can factor out the common factor of x, so the answer is
;; (x/120)* (9*g(0) +g(1)*x +3*g(2)*x^2 +7*g(3)*x^3-8*g(4)*x^4).
;; or after removing common factors, 
;;                                   2       3       4
;;Out[24] = 1/20 x (180 + 10 x + 20 x  + 35 x  - 32 x )

;; What about  (1+x) + (1+x+x^2)*y , integrated wrt x?  Let the highest
;; degree of x ANYWHERE be the denominator.  The analysis still works.

;;; rint makes the assumption that other kernels in the numerator
;;; are free of the variable of integration.

(defun rint (r v)
  (let ((den (make-rat :numerator '((1 . 1))
		       :denominator (rat-denominator r))))
    (if (rfreevar den v)
	(rat* (pintegrate (fpe-expand (rat-numerator r)) (look-up-var v))
	      den)
      (error "~%Denominator ~s is not free of ~s" (outof-rat den) v) )))

(defun |Rint|(r v)(outof-rat(rint (into-rat r)v)))

;; pintegrate takes a polynomial and a variable but returns a
;; RATIONAL form

(defun pintegrate (p v &aux maxfact (maxdegv 0))
  (labels
   
   ((g (k) (/ maxfact k))
    
    (factorial(k)(if (= k 0) 1 (* k (factorial (1- k)))))
    
    ;; set maxdegv to maximum degree that v occurs in p. return value nil.
    (maxdegree (p)
	       (cond((coefp p))
		    ((var> v (svref p 0)))
		    ((eql (svref p 0) v)
		     (setq maxdegv (max maxdegv (- (length p) 2))))
		    (t (do ((i (1- (length p)) (1- i)))
			   ((= i 0)) ;; returns nil. size effect to maxdegv
			   (maxdegree (svref p i))))))
    (pd1 (p &aux r)
	 (cond ((coefp p) (* p maxfact))
	       ((var> v (svref p 0)) p)
	       ((eql (svref p 0) v)

		(setq r (make-array (length p)))
		(setf (svref r 0) v)
		(do ((i (1- (length p)) (1- i)))
		    ((= i 0)
;;		     (format t "~%integrated p=~s to get r=~s" p r)
		     r)
		    (setf (svref r i)(p* (g i) (svref p i)))))
	       (t (setq r(copy-seq p))
		  (do ((i (1- (length r))(1- i)))
		      ((= i 0) r)
		      (setf (svref r i)(pd1 (svref r i))))))))
   (maxdegree p)
   (setq maxfact (factorial (1+ maxdegv)))
;; (format t "~s, ~s, ~s" (pd1 p) v maxfact)
   (rat/ (rat* (poly2rat (vector v 0 1) 1)
	       (make-rat :numerator (make-fpe (pd1 p) 1)
			 :denominator (list '(1 . 1))))
	(poly2rat maxfact 1) )))


;;; Notes for further heuristics.
;;  for symbolic n, make Int[x^n,x] into (x^(n+1)-1)/(n+1).
;; This has a different constant from the usual, but the nice property that
;; if n-> -1, the answer is Log[x]. (suggested by WK 12/90)

;; similar stuff possible for Cos[n*x]*Sin[m*n] formulas when n=+-m.


#| rational function integration, notes from Davenport "Computer Algebra"
2nd ed.
Horowitz-Ostrogradski method.

We want to write 
(*) int(q/r,x) =    q1/r1 + int(q2/r2)

where the 2nd part of rhs, if not zero, is a sum of logarithms. Assume
also that q1/r1 and q2/r2 are not yet necessarily in reduced form.

We know that r1 has the same factors as r, but exponents reduced by one,
and that r2 has no multiple factors, and furthermore, the factors
of r2 are also factors of r.
In particular,  r1 = gcd(r, d[r,x])
and r2 divides r/r1. In fact we may assert that r2=r/r1, if we
do not reduce by q2/r2.

Then by differentiating (*) we get 

q/r = (q1'-q1*s+q2*r1)/r  where s=r1'*r2/r1, and the division is exact.

First we need to find r1 and r2  (by differentiation and division), and thus
s. Then solve for q1 and q2 in 

q = q1'*r2- q1*s+ q2*r1

Write out q1 as a polynomial sum(ai*x^i) of degree m one less than r1's degree
and q2 as a polynomial sum(bi*x^i) of degree n one less than r2's degree
and solve the linear system of m+n equations. Solve them.
Write out the rational part of the answer as q1/r1, plus int(q1/r2).
To do:
(a) write this out as an algorithm in lisp
(b) write linear equation solver
(c) put the pieces together

You could stop here or go further.  Here is one way to  deal with
the q2/r2 part that is integrated to a sum of logarithms. Call
this q/r.
By suitable construction, and without loss of generality,
we can assume 
(**) int(q/r) = sum(ci*log(vi)) 

where vi are polynomials and also that the vi are relatively prime.

differentiation of (**) leads to 
q/r = sum (ci*vi'/vi)
and since no cancellation can take place,
r = prod(vi)
Davenport's argument on p 189-190 continues...

We need to find each vk and ck.  The ck are precisely
the roots of the resultant polynomial f(y) that is resultant(q-y*r',r)wrt x.
and that for each root of f where f(ck)=0, 
we have  vk = gcd(q-y*r',r), giving a term in the integral of ck*log(vk).

needed: resultant, at least up to sign.
q: do all these pieces really add up to getting the coefficients just
right?  factoring out rational constant so the denominators are monic,
etc?


|#



#|  from partitions.mac

intable[otherwise]:=false, /*backstop for undefined kernels */
 intable[sin] : lambda([u],  [-cos(u),diff(u,%voi)]),
 intable[cos] : lambda([u],  [sin(u),diff(u,%voi)]),
 intable[tan] : lambda([u],  [log(sec(u)),diff(u,%voi)]),
 intable[sec] : lambda([u],  [log(tan(u)+sec(u)),diff(u,%voi)]),
 intable[csc] : lambda([u],  [log(tan(u/2)),diff(u,%voi)]),
 intable[cot] : lambda([u],  [log(sin(u)),diff(u,%voi)]),
 intable[atan]: lambda([u],  [-(log(u^2+1)-2*u*atan(u))/2,diff(u,%voi)]),
 intable[acos]: lambda([u],  [-sqrt(1-u^2)+u*acos(u),diff(u,%voi)]),
 intable[asin]: lambda([u],  [sqrt(1-u^2)+u*asin(u),diff(u,%voi)]),
 intable[sinh]: lambda([u],  [cosh(u),diff(u,%voi)]),
 intable[cosh]: lambda([u],  [sinh(u),diff(u,%voi)]),
 intable[tanh]: lambda([u],  [log(cosh(u)),diff(u,%voi)]),
 intable[asinh]: lambda([u],  [-sqrt(1-u^2)+u*asinh(u),diff(u,%voi)]),
 intable[acosh]: lambda([u],  [-sqrt(u^2-1)+u*acosh(u),diff(u,%voi)]),
 intable[atanh]: lambda([u],  [log(1-u^2)/2+u*atanh(u),diff(u,%voi)]),
 intable["^"] : lambda([u,v], if freeof(%voi,u) then [u^v/log(u),diff(v,%voi)]
                                else if freeof(%voi,v)
                                then if (v#-1) then [u^(v+1)/(v+1), diff(u,%voi)]
                                else [log(u),diff(u,%voi)]),
 intable[polygamma] :lambda([u,v], if freeof(%voi,u) then [polygamma(u-1,v),diff(v,%voi)]),
 intable[log] : lambda([u], [u * log(u) - u, diff(u, %voi)]),
 intable[Ci]: lambda([u], [u*Ci(u)-sin(u),diff(u,%voi)]),
 intable[Si]: lambda([u], [u*Si(u)-cos(u),diff(u,%voi)]),
 gradef(Ci(w), cos(w)/w),
 gradef(Si(w), sin(w)/w),
 intable[nounify(bessel_j)] :lambda([u,v], if(u=1) then [-bessel_j(0,v),diff(v,%voi)]),
 intable[nounify(bessel_i)] :lambda([u,v], if(u=1) then [ bessel_i(0,v),diff(v,%voi)]),
 intable[nounify(bessel_k)] :lambda([u,v], if(u=1) then [-bessel_k(0,v),diff(v,%voi)]),

/* Legendre polynomials P[n](x), presumably if n is an explicit
   integer this symbolism would be removed, so we assume it is of unknown
   order n. */

intable[legendre_p] :
  lambda([n,u], if freeof(n,u) then 
          [(legendre_p(n+1,u)-legendre_(n-1,u))/(2*n+1),
            diff(u,%voi)]),
intable[abs] : lambda([u], [u * abs(u) / 2, diff(u, %voi)]),
intable[signum] : lambda([u], [abs(u), diff(u, %voi)]),
intable[unit_step] : lambda([u], [(u + abs(u))/2, diff(u, %voi)])
)$

/*etc etc add functions */

|#