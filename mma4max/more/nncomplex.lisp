;; -*- Mode:Common-Lisp;Package:mma; Base:10 -*-
(in-package :mma)
;;; Extended numbers ...

;;; generalized Complex numbers stored as structures containing two 
;;; generalized numbers
;;; which are interpreted as real and imaginary parts of a quantity.
;;; Often, each part is a bigfloat, but could be some CL numeric
;;; quantity (not a complex, please.)

;; declare the structure of complex 
;; we use a rectangular form, but could presumably use (r, theta)
;; if we wished to change a few of the routines.

(shadow '(complex) :mma)

(deftype supercomplex () '(or lisp::complex mma::complex))  

;; if we shadow the old "complex", we can get away with this...

(defstruct (complex  ;; this is in the mma package only
	    (:constructor complex (real imag))
	    (:print-function complexprintfunction))
real imag)

(defun complexprintfunction (x s pl)
  (declare (ignore pl))
  (format s "(~s + ~s I)" (Realpart x)(Imagpart x)))

(defun Realpart(x)(typecase x
			    (lisp::number (realpart x))
			    (complex (complex-real x))
			    (t `(Re ,x))))
(defun Imagpart(x)(typecase x
			    (lisp::number (imagpart x))
			    (complex (complex-imag x))
			    (t `(Im ,x))))

;; test for equality of two complexes

(defun complex-eql(x y)
  (and (eql (complex-real x)(complex-real y))
       (eql (complex-imag x)(complex-imag y))))

;; takes whatever x is, and returns a complex bigfloat.  
(defun complex-bigfloat-convert(x)
;;; somehow check conversion possibility?
	 (make-complex
		   (bigfloat-convert (Realpart x))
		   (bigfloat-convert (Imagpart x))))
	 
;;; re-examine this... what we might want to do is determine
;;; the types of a and b, and depending on that, use various addition
;;; routines. contract the answer, sometimes..
;;;; Rearing its head --   Inheritance again..  see ~/newarith/gen.l

(defun complex-+ (a b)
;;; we want check for Imagpart = 0, and then return 0
(let ((r  (bigfloat-+ (Realpart a)(Realpart b)))
      (i  (bigfloat-+ (Imagpart a)(Imagpart b))))
(if (bigfloat-zerop i) r
  (make-complex r i))


;;;; rewrite beyond here...


;; cbigfloat-* multiplies two cbigfloats of arbitrary (perhaps different)
;; precisions, and return a cbigfloat of global (bigfloat-bin-prec)
;; precision.
;; (note:  (a+bi)*(c+di)=  (a*c-b*d) +(a*d+b*c)i )

(defun complex-* (r s) 
  (let ((a (cbf-real r))
	(b (cbf-imag r))
	(c (cbf-real s))
	(d (cbf-imag s)))
    (make-cbig
     (bigfloat-- (bigfloat-* a c)(bigfloat-* b d))
     (bigfloat-+ (bigfloat-* a d)(bigfloat-* b c)))))

;; cbigfloat-/ divides two cbigfloats of arbitrary (perhaps different)
;; precisions, and return a bigfloat of global (bigfloat-bin-prec)
;; precision.
;; (a+bi)/(c+di) = 1/(c^2+d^2) * ( (ac+bd) + (bc-ad) i).

(defun cbigfloat-/ (r s)
    (let ((a (cbf-real r))
	(b (cbf-imag r))
	(c (cbf-real s))
	(d (cbf-imag s))
	denom)
      (setq denom (bigfloat-+ (bigfloat-* c c)(bigfloat-* d d)))
      (make-cbig
     (bigfloat-/(bigfloat-+ (bigfloat-* a c)(bigfloat-* b d))denom)
     (bigfloat-/(bigfloat-- (bigfloat-* b c)(bigfloat-* a d))denom))))
    

;; coerce-cbigfloat is like the CL function coerce, but allows
;; the first argument to be of type bigfloat.  It converts the
;; first argument to  ratio, double-float, single-float (= float),
;; integer (= bignum or fixnum)

(defun coerce-cbigfloat(x typ) 
  (cond ;; convert bigfloat to bigfloat of global precision
        ((eq typ 'cbigfloat) 
	 (cbigfloat-convert x))
	(complex (coerce-bigfloat (cbf-real x) typ)
		 (coerce-bigfloat (cbf-imag x) typ)))


;; compute x-y or -x  (if y is missing)

(defun cbigfloat-- (x &optional (y nil))
  (cond (y (cbigfloat-+ x (cbigfloat-- y)));; compute x-y
	((cbigfloat-zerop x) x)
	(t (makecbig
	    (bigfloat-- (cbf-real x))
	    (bigfloat-- (cbf-imag x))))))



;; compute p^n for bigfloat p.  nn is positive or negative integer.
;; Note that we do NOT allow bigfloat exponent here. (use log/exp for that)

(defun cbigfloat-expt (p nn) 
  (format t "cbigfloat-expt not implemented yet")
)

(defun cbigfloat-abs (x) 
  (let ((c (cbf-real x))(d (cbf-imag x)))
    (bigfloat-+ (bigfloat-* c c)(bigfloat-* d d))))


;;how to print these guys?



