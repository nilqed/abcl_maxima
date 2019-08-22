;; -*- mode: common-lisp; package: interval; -*-
(defpackage :interval)
(in-package :interval)

(defclass interval()
  ((left-value :initarg :left :reader left-value :initform 0)
   (right-value :initarg :right :reader right-value :initform 0)))

(defun left (x)
  (typecase x
            (interval (left-value x))
            (t x)))

(defun right (x)
  (typecase x
            (interval (right-value x))
            (t x)))

(defmethod print-object ((x interval) s)
  (format s "[~s ~s]" (left x) (right x)))

(defvar piby2 (/ pi 2)) ;double precision pi / 2

(defun make-int(l r)(make-instance 'interval :left l :right r))

(defvar m1to1 (make-int -1 1))  ;; the interval -1 to 1

#+Allegro (defun badguy(x)
 (excl::exceptional-floating-point-number-p x))

(defun intsin(z)
"real interval sin of an interval of machine floats."
;; result endpoints are same precision as inputs, generally, except if
;; we note that extrema -1, 1 are reached, in which case they may be integers
 (let ((low (left z))
       (hi  (right z)))
   (cond 
;;; here: insert more code to 
;;; do some checking to make sure l and r are proper floats, not too large
;;; and if they are OK, also check to see if it is an external interval.

    ((or (badguy low)(badguy hi)(> low hi)) m1to1) 
    ((eql low hi)(sin low)) ;;maybe should widen interval a little?
    (t(let (u v min max
	    (l (ceiling low piby2))
	    (h (floor hi piby2)))
	(cond ((>= (- h l) 4) (return-from intsin m1to1)))
	(setf u (sin low))
	(setf v (sin hi))
	(setf minval (min u v)) ;lower value. should round down
	(setf maxval (max u v)) ;upper value. should round up
	(do 
	 ((k  l (1+ k)))
	 ((> k h)(make-int minval maxval))
	 (case (mod k 4)
	       (1 (setf maxval 1))
	       (3 (setf minval -1)))))))))

;; here are tests: (intsin (make-int 0 pi))
;;                 (intsin (make-int pi (* 2 pi))
;;                 (intsin (make-int (- pi) pi))
;;                 (intsin (make-int -1.0 1.0))
;;                 (intsin (make-int -1.0d0 1.0d0))


(shadow '(sin))
(defmethod sin((z interval)) (intsin z))
(defmethod sin (z) (lisp::sin z))

;; to use this definition of sin, you must do (in-package :interval)
