;; -*- Mode:Common-Lisp;Package:mma; Base:10 -*-
(in-package :mma)
;;; Common Lisp Complex Bigfloat Package, Part I: Basic arithmetic only.

;;  copyright (c) 1990    Richard Fateman, UC Berkeley


;;; Complex Bigfloats are stored as structures containing two numbers,
;;; which are interpreted as real and imaginary parts of a quantity.
;;; Often, each part is a bigfloat, but could be some other number
;;; quantity (not a complex, please.)

(require "mma")
(require "bf")
(provide 'cbf)

(proclaim 
  '(special bigfloatone bigfloatzero))

;; declare the structure of complex bigfloats
;; we use a rectangular form, but could presumably use (r, theta)
;; if we wished to change a bunch of routines.

(defstruct (cbigfloat 
	    (:constructor make-cbig
			  (real imag))
	    (:print-function cbigfloatprintfunction)
	    )
  (real :type bigfloat :read-only t) ;;real
  (imag :type bigfloat :read-only t) ;; imag
  )
(defun cbigfloatprintfunction (x s pl)
  (declare (ignore pl))
  (format s "(~s + ~s I)" (cbigfloat-real x)(cbigfloat-imag x)))


(defun cbf-real(x)(typecase x
			    (cbigfloat (cbigfloat-real x))
			    (bigfloat x)
			    (number (realpart x)) ;CL number
			    (t `(Real ,x))))


(defun cbf-imag(x)(typecase x
			    (cbigfloat (cbigfloat-imag x))
			    (bigfloat  0)
			    (number (imagpart x)) ;CL number
			    (t `(Imag ,x))))

;; test for equality of two cbigfloats

(defun cbigfloat-eql(x y)
  (and (bigfloat-eql (cbf-real x)(cbf-real y))
       (bigfloat-eql (cbf-imag x)(cbf-imag y))))


;; takes whatever x is, and returns a complex bigfloat.  
(defun cbigfloat-convert(x)
  (cond ((and (typep x 'cbigfloat)
	  (= bigfloat-bin-prec 
	     (bigfloat-precision (cbf-real x)))
	  (= bigfloat-bin-prec 
	     (bigfloat-precision (cbf-imag x)))) x)
	 (t (make-cbig
		   (bigfloat-convert (cbf-real x))
		   (bigfloat-convert (cbf-imag x))))))
	 


;; add two bigfloats of any precision; converts each to global precision.
;; c = a+b.

(defun cbigfloat-+ (a b)
  (make-cbig (bigfloat-+ (cbf-real a)(cbf-real b))
	     (bigfloat-+ (cbf-imag a)(cbf-imag b))))

;; cbigfloat-* multiplies two cbigfloats of arbitrary (perhaps different)
;; precisions, and return a cbigfloat of global (bigfloat-bin-prec)
;; precision.
;; (note:  (a+bi)*(c+di)=  (a*c-b*d) +(a*d+b*c)i )

(defun cbigfloat-* (r s) 
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



