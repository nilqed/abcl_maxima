;; -*- Mode:Common-Lisp;Package:mma; Base:10 -*-
(in-package :mma)
;;; Extended numbers ...

;;; generalized Complex numbers stored as structures containing two 
;;; generalized numbers
;;; which are interpreted as real and imaginary parts of a quantity.
;;; Often, each part is a bigfloat, but could be some other number
;;; quantity (not a complex, please.)

;; declare the structure of complex 
;; we use a rectangular form, but could presumably use (r, theta)
;; if we wished to change a few of the routines.

(deftype mma::newcomplex () '(or lisp::complex mma::Complex)) ;; wha??

(defstruct (Complex  ;; this is in the mma package
	    (:constructor make-Complex (real imag))
	    (:print-function complexprintfunction))
  (real  :read-only t) ;;real
  (imag  :read-only t) ;; imag
  )

(defun complexprintfunction (x s pl)
  (declare (ignore pl))
  (format s "(~s + ~s I)" (Realpart x)(Imagpart x)))

(defun Realpart(x)(typecase x
			    (lisp::number (realpart x))
			    (Complex (Complex-real x))
			    (t `(Real ,x))))
(defun Imagpart(x)(typecase x
			    (lisp::number (imagpart x))
			    (Complex (Complex-imag x))
			    (t `(Imag ,x))))


