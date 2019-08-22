;;; -*- Mode:Common-Lisp; Package:mma; Base:10 -*-

(declaim (optimize (speed 3) (safety 0) (space 0) (compilation-speed 0)))
(eval-when (compile) (load "mma")) ;; need the symbols like Plus
(in-package :mma)

;; RatExpand[(a5+b5)*(c5+d5)]
;;  old version                        changed version
;; (a5 + b5) c5 + (a5 + b5) d5     a5 c5 + b5 c5 + a5 d5 + b5 d5


;; extracted from simp1.lisp

(defun intol* (a b)
  (cond ((eql a 0) 0)
	((eq a 1) b)
	((eq b 1) a)
	;; additional line to promote distributed form
	((and *expand* (IsPlus a))
	 (ucons 'Plus (mapcar #'(lambda(r)(intol* r b)) (cdr a))))
	(t (ucons 'Times 
		  (uappend (intol*chk a) (intol*chk b))))))


;; extracted from rat1.lisp

(defun fpe-insert (poly exp u &optional (monom nil) &aux ctest)
  (labels
   ((fi1(ul)
	     ;; this routine recurses down the list of factors
	     (cond ((null ul)
		    ;; Since this factor is nowhere to be seen in
		    ;; this fpe, insert the new factor and its power
		    ;; at the end of the list.
		    (list (cons poly exp)))

;;; added  for stein@uni-paderborn.de	     1/5/94
		   
		   ((coefonep (caar ul))(cons (car ul)(fi1 (cdr ul))))
		  		   
		   ;; fully expand. multiply into first factor that
		   ;; better be of multiplicity 1. Presumably is the
		   ;; only factor, but we can manage if it is not.
		   (*expand* 
		    (if (= (cdar ul) exp) ;probably 1=exp
			(cons (cons (p* poly (caar ul)) exp) (cdr ul))
		      (error "illegal use of expand")))
;;; end 
		   
		   ((eq 'e (setq ctest (pcompare poly (caar ul))))
		    ;; We found this factor. Increment the exponent.
		    (cons (cons (caar ul) (+ exp (cdar ul)))(cdr ul)))
		   ;; Otherwise we keep looking for the factor.
		   ((eq 'l ctest)
		    ;; this poly is less in ordering than (caar ul),
		    ;; so place it right here.
		    (cons (cons poly exp) ul))
		   ;; This keep searching 
		   (t (cons (car ul)(fi1 (cdr ul)))))))
	(cond ((coefp poly)
	       ;; inserting an integer factor: modify the first element
	       (cond ((coefonep poly) u)
		     ((coefzerop poly) (make-fpe 0 1))
		     (t (cons (cons (coef* (caar u)
						 (coef^ poly exp))
					  1)
			      (cdr u)))))
	      ((= exp 0) u) ;;multiplying u by z^0 gives u
	      ((and monom (monomialp poly)(or (> (degree poly) 1)
					      (not (coefonep (lc poly)))))
	       ;; the poly is something like (3*x^2) ^4
	       ;; insert 81  (i.e. 3^4) into  1*(x)^8.
	       ;; should work recursively for (3*y^2*x^2)^4
	       
	       (fpe-insert (lc poly) exp (fpe-insert 
					  (vector (svref poly 0) 0 1)
					  (* (degree poly) exp) u )
			   monom))
	      
	      ;; we can, more generally, not insist on monomial,
	      ;; but only 0 x^1 term.  That is,
	      ;; (a*x^5+b*x^2) --> x^2*(a*x^3+b).  or
	      ;; (x^5+x^2)^3 --> x^6*(x^3+1)^3. etc.
	      ;; note that 0 constant term is not useful because
	      ;;  x is encoded as (vector n 0 1) and has 0 const term

	      ((and monom 
		    (not *expand*)
		    (coefzerop(constc poly))
		    (> (length (the simple-vector poly)) 3)
		    (coefzerop (svref poly 2)))
	       (do ((i 2 (1+ i)))
		   ((null (coefzerop (svref poly i)));there's a non-0
		    (fpe-insert
		     (vector (svref poly 0) 0 1)
		     (* exp (1- i))  ;; the degree of the factor
		     (fpe-insert (polyshift poly (1- i)) exp u)))
		   (declare (fixnum i))
		   ;; no do-body
		   ))		    		    
	      (t (fi1 u)))))