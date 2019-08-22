;; Common Lisp  interval arithmetic and plotting

;;; (c) 1992 by Cindy Ruan and Richard Fateman
;;; Real Intervals are represented as pairs of floats in a class.
;;; left and right endpoints are assumed to be numbers. This is
;;; checked when creating intervals with make-int.
;;; this package assumes left <= right.  No exterior intervals for now.
;;; an interval can have endpoints that are +/- infinity or NaN

(defclass interval()
  ((left-value :initarg :left :reader left-value :initform 0)
   (right-value :initarg :right :reader right-value :initform 0)))

(defmethod left  ((x interval)) (left-value x))
(defmethod right ((x interval)) (right-value x))

;; for anything else, (left x) == x  etc

(defmethod left(x) x)
(defmethod right(x) x)

(defmethod print-object ((x interval) s)
  (format s "[~s ~s]" (left x) (right x)))

(defmethod make-int((l number) (r number))
  (make-instance 'interval :left l :right r))

(defvar onemse (- 1.0 lisp:single-float-epsilon))
(defvar onepse(+ 1.0 lisp:single-float-epsilon))
(defvar onemde (- 1.0d0 lisp:double-float-epsilon))
(defvar onepde(+ 1.0d0 lisp:double-float-epsilon))

;; to make a floating point number into an interval, bump it
;; up and down by 1 unit in last place.  Could be parameterized by epsilon..

(defmethod make-int1((l float))
 (typecase l
	   (single-float
	    (if (> l 0.0) (make-int (* onemse l)(* onepse l ))
	       (make-int (* onepse l )(* onemse l))))
	   (double-float
	    (if (> l 0.0d0) (make-int (* onemde l)(* onepde l ))
	       (make-int (* onepde l)(* onemde l))))))

(defmethod make-int1((l number)) (make-int l l)) ;non-floats are exact


(defmethod intervalp ((x interval))
  ;; true if X is an interval whose leftvalue <= rightvalue
         (<= (left X) (right X)))

(defmethod intervalp (y) nil)

;;; Interval Computation

;; The following procedures assume that the interval 
;; [a, b] is a valid interval
;; i.e. [a, b] = {t | a <= t <= b   for all real numbers t}

;; basic operations. We use Allegro Common Lisp conventions here.
;; other CL supporting IEEE arithmetic must have similar conventions.

(defvar infinity #.EXCL::*INFINITY-DOUBLE*)

(defvar neginfinity #.EXCL::*NEGATIVE-INFINITY-DOUBLE*)

(defun efpnp (x) ;; returns nil if x is not exceptional. otherwise returns
;; a SYMBOL with names as indicated below.
 (excl::exceptional-floating-point-number-p x))

(defun infinityp(x)
 (let ((ty (efpnp x)))
   (case ty 
	 ((EXCL::*INFINITY-SINGLE* EXCL::*INFINITY-DOUBLE*) t)
	 (t nil))))

(defun nanp(x)
 (let ((ty (efpnp x)))
   (case ty 
	 ((EXCL::*NAN-SINGLE* EXCL::*NAN-DOUBLE*) t)
	 (t nil))))


(defmethod intadd((A interval) (B interval))
  (let ((a1 (left A)) (a2 (right A))
	(b1 (left B)) (b2 (right B)))
    (make-int (+ a1 b1) (+ a2 b2))))

(defmethod intminus((A interval) (B interval))
  (let ((a1 (left A)) (a2 (right A))
	(b1 (left B)) (b2 (right B)))
    (make-int (- a1 b2) (- a2 b1))))

(defmethod intmult((A interval) (B interval))
  (let* ((a1 (left A)) (a2 (right A))
	 (b1 (left B)) (b2 (right B))
	 (v1 (* a1 b1))
	 (v2 (* a1 b2)) 
	 (v3 (* a2 b1)) 
	 (v4 (* a2 b2)))
    (make-int (min v1 v2 v3 v4) (max v1 v2 v3 v4))))

(defmethod intdiv((A interval)(B interval)) (intmult A (inverse B)))

(defmethod inverse((B interval))  ; 1/B
  (let ((b1 (left B)) (b2 (right B)))
    (cond ((and (>= 0 b1) (<= 0 b2))
	   ;; 0 is in [b1, b2]
	   (make-int neginfinity infinity))
	  (t (make-int (/ 1 b1) (/ 1 b2))))))

;; sin and cos

(defvar m1to1 (make-int -1 1))  ;; the interval -1 to 1

(defvar twopi (* 2 pi))

(defvar piby2 (/ pi 2)) ;double precision pi / 2

(defvar intpiby2 (make-int piby2 piby2))

(defmethod intsin((z interval))
  (intcos (intminus z intpiby2)))	; sin(x) = cos(x - pi/2)

(defmethod intcos ((z interval))
  (let ((low (left z))
	(hi (right z)))
    (cond
     ;; if the interval contains infinity or nan, return [-1 1]
     ((or (efpnp low) (efpnp hi)) m1to1)
     ((> low hi) m1to1) ;external interval
     ((eql low hi) (make-int1 (sin low))) ; convert to an interval
     (t (let* ((u (cos low))  
	       (v (cos hi))
	       (m (ceiling low pi))
	       (n (floor hi pi))
	       (minval (min u v))
	       (maxval (max u v)))
	  (if (>= (- n m) 1) (return-from intcos m1to1))
	  (if (eql m n)
	      ;; 1 or -1 exists between cos(low) and cos(hi)
	      (case (mod m 2)
		(0 (setq maxval 1))
		(1 (setq minval -1))))
	  (make-int minval maxval))))))

(defmethod inttan ((z interval))
  (intdiv (intsin z) (intcos z)))

(defmethod intcot ((z interval))
  (intdiv (intcos z) (intsin z)))    ; need more work.....

(defmethod intcsc ((z interval))     ; need more work.....
  (inverse (intsin z)))

(defmethod intsec ((z interval))
  (inverse (intcos z)))

;; interval version of exp
 
(defmethod intexp ((A interval))
    (make-int (exp (left A)) (exp (right A))))

;; interval version of log

(defmethod intlog ((A interval) &optional (base (exp 1)))
  ;; assumes base is a positive number.
  (let ((a1 (left A))
	(a2 (right A)))
    (cond ((< a1 0)
	   (make-int neginfinity infinity))
	  ((= a1 0)
	   (make-int neginfinity (log a2 base)))
	  (t (make-int (log a1 base)
		       (log a2 base))))))

;; interval version of expt
;; not really correct right now...
(defmethod intexpt ((A interval) (B interval))
  (intexp (intmult B (intlog A))))


(defmethod intabs ((A interval))
  (let ((a1 (left A))
	(a2 (right A)))
    (cond ((> 0 a2)			;A < 0
	   (make-int (abs a2) (abs a1)))
	  ((> 0 a1)                      ;0 is in A
	   (make-int 0 (max (abs a1) a2)))
	  (t A))))

;; interval plot

(defmacro intplot (f (xvar xmin xmax) &key (points 100))
  `(block ()
     (with-open-file (xtmp "/tmp/xtmp"
		      :direction :output :if-exists :supersede)
		     
	 (multiple-value-bind (xlist ylist ymin ymax)
	     (intplot1 ',f ',xvar ,xmin ,xmax ,points)
	   (drawboxes xlist ylist ymin ymax xtmp))
       (run-shell-command "xgraph /tmp/xtmp" :wait nil)
       'XgraphOutput)))

(defun drawboxes (xlist ylist ymin ymax xtmp 
		  &aux (neginf (- ymin (* 2 (abs ymin))))  ;in case of infinity
		       (posinf (+ ymax (* 2 (abs ymax))))) 
  (do* ((xl xlist (cdr xl))
	(yl ylist (cdr yl))
	(xint (car xl) (car xl))
	(yint (car yl) (car yl)))
      ((null xl) nil)
    (let ((xl (coerce (left xint) 'single-float))
	  (xh (coerce (right xint) 'single-float))
	  (yl (coerce (left yint) 'single-float))
	  (yh (coerce (right yint) 'single-float)))
      (cond ((and (infinityp yl) (infinityp yh))
	     ;; y goes form +inf to -inf
	     ;; don't draw top and bottom of the box
	     (format xtmp "move ~12,4G ~12,4G
~12,4G ~12,4G
move ~12,4G ~12,4G
~12,4G ~12,4G~%" xl posinf xl neginf xh posinf xh neginf))
	    ((or (nanp yl)(nanp yh)));don't draw anything
	    ((infinityp yl)
	     ;; y goes to -inf
	     (format xtmp "move ~12,4G ~12,4G
~12,4G ~12,4G
~12,4G ~12,4G
~12,4G ~12,4G~%" xl neginf xl yh xh yh xh neginf))
	    ((infinityp yh)
	     ;; y goes to +inf
	     (format xtmp "move ~12,4G ~12,4G
~12,4G ~12,4G
~12,4G ~12,4G
~12,4G ~12,4G~%" xl posinf xl yl xh yl xh posinf))
	    (t 
	     ;; draw a complete box
	     (format xtmp "move ~12,4G ~12,4G
~12,4G ~12,4G
~12,4G ~12,4G
~12,4G ~12,4G
~12,4G ~12,4G~%" xl yl xl yh xh yh xh yl xl yl))))))

(defun intplot1 (f xvar xmin xmax points)
  ;; Let y=f(xvar) where xmin <= xvar <= xmax
  ;; Returns a list of x-intervals, a list of y-intervals,
  ;; min and max (except +infinity and -infinity) of the range
  (let (step xlist ylist xint yint ylo yhi
	(ymax neginfinity) ;; initialize ymax and ymin
	(ymin infinity)
	(alist (list (cons xvar xmin))))
;;check: that xmin, xmax are both finite numbers and that they differ
    (if (and (numberp xmin)(numberp xmax)(not(>= xmin xmax))) nil
      (error "Bad interval to intplot ~s, ~s" xmin xmax))

;;see if rescaling is necessary so that the plot axes etc are
;; useful
    (if (> (abs (/ (max xmin xmax) (- xmax xmin))) 100)
    (return-from intplot1 (rescale-intplot1 f xvar xmin xmax points)))
								  
    (setq step (/ (- xmax xmin) points))
    (do ((xlow xmin xhigh)
	 (xhigh (+ xmin step) (+ xhigh step)))
	((>= xhigh xmax)
	 ;; set the right value of the last interval to be exactly xmax
	 (setq xint (make-int xlow xmax))
	 (setq yint (ieval f (list (cons xvar xint))))
	 (setq ylo (left yint))
	 (setq yhi (right yint))
	 (if (and (not (infinityp yhi)) (> yhi ymax))
	     (setq ymax yhi))
	 (if (and (not (infinityp ylo)) (< ylo ymin))
	     (setq ymin ylo))
	 (setq xlist (cons xint xlist))
	 (setq ylist (cons yint ylist))
	 (values xlist ylist ymin ymax))
      (setq xint (make-int xlow xhigh))
      (setf (cdar alist) xint) ; update alist binding for xvar
      (setq yint (ieval f alist))
      (setq ylo (left yint))
      (setq yhi (right yint))
      (if (and (not (infinityp yhi)) (> yhi ymax))
	  (setq ymax yhi))
      (if (and (not (infinityp ylo)) (< ylo ymin))
	  (setq ymin ylo))
      (setq xlist (cons xint xlist))
      (setq ylist (cons yint ylist)))))


(defun rescale-intplot1 (f xvar xmin xmax points)
  (let ((newf (subst `(+ ,xvar ,xmin) xvar f))
	(newxmin 0.0)
	(newxmax (- xmax xmin)))
    (format t "~% To make the plot more meaningful~%
 we rescale ~s ~%to~% ~s"
	    `(intplot ,f (,xvar ,xmin ,xmax))
	    `(intplot ,newf (,xvar ,newxmin ,newxmax)))
    (intplot1 newf xvar newxmin newxmax points)))



;; ieval...

(setf (get '+ 'intervalmethod) #'intadd)
(setf (get '- 'intervalmethod) #'intminus)
(setf (get `* 'intervalmethod) #'intmult)
(setf (get '/ 'intervalmethod) 'intdiv)
(setf (get 'sin 'intervalmethod) 'intsin)
(setf (get 'cos 'intervalmethod) 'intcos)
(setf (get 'exp 'intervalmethod) 'intexp)
(setf (get 'log 'intervalmethod) 'intlog)
(setf (get 'expt 'intervalmethod) 'intexpt)
(setf (get '^ 'intervalmethod) 'intexpt)
(setf (get 'abs 'intervalmethod) 'intabs)
(setf (get 'tan 'intervalmethod) 'inttan)
(setf (get 'cot 'intervalmethod) 'intcot)
(setf (get 'csc 'intervalmethod) 'intcsc)
(setf (get 'sec 'intervalmethod) 'intsec)

(defun intervalmathp(op)
  (get op 'intervalmethod))

;; default case for r being atoms
(defmethod ieval (r alist);r is not an interval
  (cond ((numberp r) (make-int1 r))	;is r a number?
	((let ((s (assoc r alist)))	;is r on alist?
	   (cdr s)))		
	(t r)))				;if r is not on alist return r itself

;; r is an interval
(defmethod ieval ((r interval) alist)
  r)

;; r is a function expression
(defmethod ieval ((r cons) alist)
  (let* ((op (car r))
	 (intmeth (get op 'intervalmethod)))
    (if intmeth 
	(apply intmeth (ievalargs (cdr r) alist))
      (error "Unknown interval math function."))))

;; could do this as a local function..

(defun ievalargs (l alist)
  (mapcar #'(lambda(z) (ieval z alist)) 
	  l))



(defun foo (xmin xmax points)
 (declare (fixnum points))
  (let 
    ((width (- xmax xmin))  xhigh xint
     (result (make-array points)))
    
    (do ((count 1 (1+ count))
	 (xlow xmin xhigh))
	((>  count points) result)
	(declare (fixnum count))
	(setq xhigh (+ xmin (/ (* width count) points)))
	(setq xint (make-int xlow xhigh))
	(format t "~%~s" xint)
	(setf (svref result (1- count)) xint)
)))




