;; generic arithmetic

;; index is computed at compile time!

(eval-when (compile eval)
(load 'macros))
;; macros which might be used by more than one file

(declare (macros t))

(defmacro get-value-vector(x) `(cxr 1 ,x))

(def call-method(macro(l)(cons 'funcall (cdr l))))

(defmacro get-type-vector(x) 
`(cond ((eq 'hunk0 (type ,x))(cxr 0 ,x))
       (t (get (type ,x) 'type-vector)))) 
;; this 2nd clause is for fix, big, flonum, symbol, list

(defmacro get-type(x)`(cond ((eq (type ,x)'hunk0)
			     (cxr 0 (get-type-vector ,x)))
			    (t (type ,x))))

(defmacro convert (t1 x) `(call-method (get-method1 ,t1 #.(vector-index 'convert)) ,x))

;; get vector from looking at x, where x is an object.  tt is offset (e.g. 5) of
;; function (e.g. 'plus) in generic vector.  Offset computed by vector-index
;; at compile time.

(defmacro get-method (x tt) `(cxr ,tt (get-type-vector ,x)))

;; get method from name of type which is x (e.g. 'rat)

(defmacro get-method1 (x tt) `(cxr ,tt (get ,x 'type-vector)))

;; from rat.l

(defmacro rat_p(x) `(memq (get-type ,x) '(fixnum bignum rat)))

; these macros might seem to be only of local interest to some
; data type (e.g. integer)
; they are here because other files use them sometimes.

(defmacro integer_minus(x) `(- ,x))
(def integer_plus (macro(l) (cons '+ (cdr l))))
(defmacro integer_minusp(x) `(minusp ,x))
(def integer_times (macro(l) (cons '* (cdr l))))
(defmacro integer_equal (x y) `(equal ,x ,y))
(defmacro integer_> (x y) `(> ,x ,y))

(declare (special generic_vector))
(eval-when (compile load eval)
 (defvar generic_vector 
       '(generic print convert equal 
		 zerop plus times minus diff quotient inv
		 remainder intpart sqrt expt abs min max 
		 real imag conj sin cos atan exp log > <
		 poly ;;univariate poly evaluation f(z) over z
		 posp valid infp undefp))) ;; what else?

(eval-when (load eval)
(cond ((status saved-old-arith))
(t (sstatus saved-old-arith t) ;; save only those guys redefined below.
;; others get re-named in "integer" file.
  (mapc 
   #'(lambda(h)(setf (symbol-function (car h))(symbol-function (cdr h))))
   '((symbol_equal . eq)
     (list_equal . eql)
     (flonum_print . print)
     (flonum_zerop . zerop)
     (flonum_sin . sin)
     (flonum_cos . cos)
     (flonum_tan . tan)
     (flonum_sqrt . sqrt);; etc
     (integer_print . print)
     (integer_convert . fix) ;; not really
     (integer_plus . +)
     (integer_times . *)
     (integer_minusp . minusp)
     (integer_minus . -)
     (integer_abs . abs)
     (integer_diff . -)
     (integer_zerop . zerop)
     (integer_equal . equal)
     (integer_> . >)))))) ;;etc

(defun uniontype(t1 t2)(cond((eq t1 t2)t1)
			    ((> (typerank t1)(typerank t2))t1)
			    (t t2)))

(defun convert-fail (t1 x)(format t "~%failure to convert ~s to type ~s" x t1))

(defun typerank (r)(get r 'typerank))
;; set up ranking

(do ((i 0 (1+ i))
     (j '(fixnum bignum ;; subclasses of integer
		 ratio single-float double-float
		 bigfloat complex interval 
		 poly;; polynomial ???
		 matrix ;; square matrix of ??
		 math;; arbitrary algebraic object 
		 symbol;; lisp symbol?
		 list;;  lisp list?
		 )
	(cdr j)))
  ((null j) 'done)
  (putprop (car j) i 'typerank))


(eval-when (load compile eval)
(defun vector-index (x) (get x 'vector-index))

(do 
  ((i 1 (1+ i))
   (g (cdr generic_vector) (cdr g)))
  ((null g) nil)
  (putprop (car g) i 'vector-index)))

;;; got to redo this stuff ... it's CLOS-like. maybe we should just use
;;; CLOS?

(defun make-dispatch-vector 
  (h) 
  (apply 'hunk `(,h ,@(mapcar '(lambda(zz)
			      (or (getd (setq zz(concat h "_" zz))) zz))
		      (cdr generic_vector)))))


;; here is generic stuff!
;; plus of 2 args
;; if we were daring, we would use the name "+" here..
(defun gplus (x y &optional (targettype nil))
  (let ((utype (uniontype (type-of x)(type-of y)))
	res)
    (setq x (convert utype x) y (convert utype y))
    (setq res 
	  (call-method (get-method x #.(vector-index 'plus)) x y))
    (if targettype (convert targetype res) res)))

;;;continue rewriting here.. 

(defun gtimes (x y)
  (let ((utype (uniontype (get-type x)(get-type y))))
       (setq x (convert utype x) y (convert utype y))
       (call-method (get-method x #.(vector-index 'times)) x y)))

(defun gdiff (x y)
  (let ((utype (uniontype (get-type x)(get-type y))))
       (setq x (convert utype x) y (convert utype y))
       (call-method (get-method x #.(vector-index 'diff)) x y)))

(defun gmax (x y)
  (let ((utype (uniontype (get-type x)(get-type y))))
       (setq x (convert utype x) y (convert utype y))
       (call-method (get-method x #.(vector-index 'max)) x y)))

(defun gmin (x y)
  (let ((utype (uniontype (get-type x)(get-type y))))
       (setq x (convert utype x) y (convert utype y))
       (call-method (get-method x #.(vector-index 'min)) x y)))

(defun gquotient (x y)
  (let ((utype (uniontype (get-type x)(get-type y))))
       (setq x (convert utype x) y (convert utype y))
       (call-method (get-method x #.(vector-index 'quotient)) x y)))

(defun gzerop (x)
       (call-method (get-method x #.(vector-index 'zerop)) x))

(defun gminus (x)
       (call-method (get-method x #.(vector-index 'minus)) x))

(defun gabs (x)
       (call-method (get-method x #.(vector-index 'abs)) x))

(defun gsin (x)
       (call-method (get-method x #.(vector-index 'sin)) x))
(defun gcos (x)
       (call-method (get-method x #.(vector-index 'cos)) x))
(defun gatan (x)
       (call-method (get-method x #.(vector-index 'atan)) x))
(defun gsqrt (x)
       (call-method (get-method x #.(vector-index 'sqrt)) x))

(defun g> ( x y)
  (call-method (get-method x #.(vector-index '>)) x y))

;; in common lisp document, eq is "pointer equality"
;; (eql x y) is t if (eq x y) or numerically of same type and equal (= x y),
;; (equal x y) means, roughly, x and y print the same.
;; (equalp x y) means that  x-y=0 but x and y may undergo type conversion.
;; This equal is none of the above, since 1/0-1/0=0/0, but in projective mode
;; 1/0 = 1/0.  But it's close.

(defun gequal (x y)
(let ((utype (uniontype (get-type x)(get-type y))))
       (setq x (convert utype x) y (convert utype y))
       (call-method (get-method x #.(vector-index 'equal)) x y)))

