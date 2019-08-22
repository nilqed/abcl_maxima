;; -*- Mode:Common-Lisp;Package:mma; Base:10 -*-
(in-package :mma)
;; More of the evaluator stuff needed
;; also need some special case simplification, e.g. multiples of |Pi| 
;; also, bigfloats in more/bf*.lisp change these.
(defvar *numer* nil)
(defvar env nil)
(defun |Sin|(x)(cond ((or (floatp x)(and *numer* (numberp x)))(cl:sin x))(t (ulist '|Sin| x))))
(defun |Cos|(x)(cond ((or (floatp x)(and *numer* (numberp x)))(cl:cos x))(t (ulist '|Cos| x))))
(defun |Tan|(x)(cond ((or (floatp x)(and *numer* (numberp x)))(cl:tan x))(t (ulist '|Tan| x))))
(defun |Log|(x)(cond ((or (floatp x)(and *numer* (numberp x)))(cl:log x))(t (ulist '|Log| x))))
(defun |Exp|(x)(cond ((or (floatp x)(and *numer* (numberp x)))(cl:exp x))(t (ulist '|Exp| x))))
(defun |Sinh|(x)(cond ((or (floatp x)(and *numer* (numberp x)))(cl:sinh x))(t (ulist '|Sinh| x))))(defun |Cosh|(x)(cond ((or (floatp x)(and *numer* (numberp x)))(cl:cosh x))(t (ulist '|Cosh| x))))(defun |Tanh|(x)(cond ((or (floatp x)(and *numer* (numberp x)))(cl:tanh x))(t (ulist '|Tanh| x))))(defun |Abs|(x)(cond ((numberp x)(cl:abs x))(t (ulist '|Abs| x))))
(defun |Sqrt|(x)(cond ((or (floatp x)(and *numer* (numberp x))) 
		       (cl:sqrt x))
		      (t (powersimp x 1/2)))) ;maybe simplify it, eg (Sqrt 4) -> 2

;;; etc etc
;;(defun plusp(x)(cond ((numberp x)(cl:plusp x))(t (ulist 'plusp x))))
;; hmmm, what to do with "and"??

;; if the user types in and[a,b] then it becomes (and a b) 
;; which is plausible lisp, and there is no way to avoid it compared to And[a,b]
;; unless we observe case..


;;12/18/2010 RJF

;;;Replace [Sin[q],{Sin[x_]->fff[x],fff[z_]->ggg[z+1]}] produces
;;;ggg[1+q]

;;; mathematica allows   a+b+c /. a+c-> x  to produce b+x , and so do we.
;;; We hack this up so that the rule becomes  a+b+c/.  a+c+y_. -> x+y

(defun hackrule(r)
  (let* ((lhs (cadr r))
	 (h (|Head| lhs))
	 (ha (|Attributes| h)))
    
    (cond ;;#+ignore ;; too messy ??
	  ((and (numberp lhs)(complexp lhs))
	   ;; the rule is   I == Complex(0,1) -> x.  change to (Complex 0,q) -> q*x
	   ;; the rule is 3*I == Complex(0,3) -> x   change to (Complex 0, 3*q) -> q*x
	   ;; the rule is 3+4I == Complex(3,4) -> x   NO CHANGE
	   
	   (let* ((q (gensym))
		  (k (gensym))
		   (impart (imagpart lhs))
		  ; (repart (realpart lhs)) ; not used
		   (ipat  (if (= impart 1)`(|Pattern| ,q (|Blank|))
			    `(|Times| ,impart (|Pattern| ,q (|Blank|))))))
	    ;;  (format t "~% q=~s im=~s re=~s" q repart impart)
	       (list (car r);; rule head
		     `(|Complex| (|Pattern| ,k (|Blank|))
				 ,ipat)
		    `(|Plus| ,k (|Times| ,q ,(caddr r))))
	       
	       ))
	  
	  #+ignore
	  ((and (numberp lhs)(complexp lhs))
	  ;; less confusing, less ambitious
	   (list (car r);; rule head
		     `(|Complex| ,(realpart lhs) ,(imagpart lhs))
				 
		    (caddr r)))
	    
	   ((member '|Flat| ha :test 'eq)
	    (let ((q (gensym)))
	      (list (car r)
		    (append (cadr r) (uniq `((|Optional|(|Pattern| , q (|Blank|)))))) ;new lhs
		    (list h   (caddr r) q))))
	   (t  r))))

;; 2/2/2011
;n Replace
(defun |Replace|(expr rules &optional levelspec) ;; where to feed in Conditions?
  (declare (ignore levelspec))			 ;; for now.
  ;; rules is a rule or list of rules. 
  ;; currently we ignore levelspec
  (if (and (listp rules)(eq (|Head| rules) '|List|))
      (dolist (a (cdr rules) expr)
	(setf expr (|Replace| expr a)))	;; that takes care of list of rules case
    ;; just one rule
    (cond ((not (member (|Head| rules) '(|Rule| |RuleDelayed|)))
	   (error "Replace: ~s is not a proper rule" rules )) ;; not a rule 
	  (t
	   
	   (setf rules(hackrule (bindfix rules)))
	   (let ((lhs (cadr rules))
		 (rhs (caddr rules)))
	     ;;(spushframe env 'replace) ;;ZZZ
	
	     (cond ((match lhs expr)
		    (setf expr (meval rhs))))
	     (spopframe env)
	     expr)))))

;;  v[10] /.  v[x_] /; x > 5-> aha     returns aha
;;    v[4] /. v[x_] /; x > 5-> aha     returns v[4]

					;n ReplaceAll	
#-:allegro (defun ratiop(r)(and (rationalp r)(not(integerp r))))


;; the way to do this is to replace from the bottom (leaves) of the tree, upward.


(defun |ReplaceAll|(expr rules) ;; syntax is  expr /. rules
  (let (				;(k (|Replace| expr rules))
	(k expr))
;;    (format t "~% k is ~s" k)
    (|Replace|
     (if (atom k) ;; dissect an atom to see if we can operate on pieces
	(if (numberp k) (cond((ratiop k)
			      (let ((n (|ReplaceAll| (numerator k) rules)) ;might be negative?
				    (d (|Replace| (denominator k) rules)))
				(|Rational| n d)))
			 
			     ((complexp k)
			      (let ((r (|ReplaceAll| (realpart k) rules)) ;; in case re or im is a Rational!
				    (i (|ReplaceAll| (imagpart k) rules)))
				(|Complex| r i)))
			     
			     ((minusp k)(uniq `(|Times| -1 ,(|ReplaceAll| (- k) rules))))
			     (t  k))
	  k);; atom, not number

       (umapcar #'(lambda(r) (|ReplaceAll| r rules)) k))	; not atom
				rules)

    ))



;;fff[aaa]+fff[bbb] /. fff->ggg    results in ggg[aaa] + ggg[bbb]

;;

(defun |Rest|(r)
 ;; (format t "~% rest got ~s eval to ~s, env=~s" r (meval r) env)

    (let ((h (|Head| r)))
      (if (consp r)
	  (|Simp|(ucons h (cddr r)))
	(cond ((complexp r)(ulist '|List| (realpart r)(imagpart r)))
	      ((and (rationalp r)(not (integerp r)))
	       (ulist '|List| (numerator r)(denominator r)))
	      (t (error "Rest expects non-atomic expression, not ~s"r))))))

(defun |And| (&rest s &aux ans)
  ;; And short circuits and does not evaluate all its args, necessarily
;;  (format t "~%And -- called on ~s with env=~s, meval(car s)=~s " s env (meval (car s)))
  (do* ((i s (cdr i))
	(a (meval (car i))(meval (car i))))
      ((null i)
       (cond ((null ans)'|True|)	; everything was True
	     (t (cond ((null(cdr ans))	; length 1
		       ans)
		      (t(uniq `(|And| ,@(nreverse ans))))))))
					
   ;; (print a)
       
       (cond ((null a) (return nil)) ;; False is nil
	     ((eql a  '|True|) nil)	;keep going if True
	     ;; neither true nor false
	     (t (push a ans)
		;;(format t "~% pushed ~s on ans=~s" a ans)
		))))

(defun |Not|(s)(case s 
		 ((nil) '|True|)
		 (|True| nil)
		 (otherwise (ulist '|Not| s))))


(defun |IntegerQ|(x)(and (integerp x) '|True|))

(defun |Or| (&rest s &aux ans)
  ;; Or short circuits and does not evaluate all its args, necessarily
  (do* ((i s (cdr i))
	(a (meval (car i))(meval (car i))))
      ((null i)
       (cond ((null ans) nil)		; nothing was True
	     (t (cond ((null(cdr ans))	; length 1
		       ans)
		      (t(uniq `(|Or| ,@(nreverse ans))))))))
    
					
    (cond 
     ((eql a  '|True|) (return '|True|)) ;even if only one is True, 
     ((null a) nil) ;; term is False is nil. Keep looking
	;; neither true nor false
     (t (push a ans)
	   ;;(format t "~% pushed ~s on ans=~s" a ans)
	))))


(defun |First|(r)(cond ((atom r) (format t "~%Atomic argument to First invalid~s" r)(signal 'error))
		       (t (cadr r))))


(defun |Re|(x) (typecase x (complex (realpart x))(real x)(otherwise (ulist '|Re| x))))
(defun |Im|(x) (typecase x (complex (imagpart x))(real 0)(otherwise (ulist '|Im| x))))
(defun |PossibleZeroQ|(x) (if (eql 0 (|Simp| x)) '|True| nil))


;;; tricky wrt constants

(defun |NumericQ|(x)
  (cond ((atom x) ;; check if x is a number or a constant
	 (cond ((numberp x) '|True|)
	       ((member x '(|Pi| |E|) :test 'eq) '|True|) ;; should check ConstantQ
	       ((symbolp x) nil)))
	;;
	((and (member '|Numeric| (|Attributes| (|Head| x)))
	      (every #'|NumericQ| (cdr x))
	      '|True|))))


(defun |N|(x)     
 (let ((*numer* t))
   (meval x)))

;; compare  Sin[4]   and   Sin[4]//N









;; more notes
;; Sin[x_]/; x>3-> S[x] 
;; parses to
;; (Rule (Condition (Sin (Pattern x (Blank))) (Comparison x Greater 3)) (S x))

;; we don't have evaluation of Block  implemented.
;; 

;; probably should add evaluation of functions and slots. 
;; eg  #+1&[4]  should return 5.
#|
;;(break "t")

;;(ww (Pattern x (Blank))) Condition aha
;;(Comparison x Greater 5)
;;  w[x_]:= aha /; x>5    parses into something like ...
;;((  (ww (Pattern x (Blank)))    . (Condition aha  (Comparison x Greater 5))   ))

;;but we need (Rule (Condition (ww (Pattern x (Blank))) (Comparison x Greater 5)) aha)
;;or something close to that

;;(trial '(Condition (ww (Pattern x (Blank))) (Comparison x Greater 5)) '(w 10))
;;(trial '(Condition (ww (Pattern x (Blank))) (Comparison x Greater 5)) '(w 4))

|#

;;just a hack ..
#|
(defun macint (x y)
   (max2mma  (maxima::$integrate   (mma2max x)  (mma2max y))))

|#
;; what if a user types in Equal[x,y]?
;; or Greater[x,y,z]...
(defun opexpand(op args)
  (do*  
      ((i args (cdr i))
       (j (car i)(car i))
       (k  (ulist j '|Comparison|)
	   (ucons j (cons op k))))
      ((null (cdr i)) (meval (reverse k))) ))

(defun |Greater|(&rest args) (opexpand '|Greater| args))
(defun |GreaterEqual|(&rest args) (opexpand '|GreaterEqual| args))
(defun |Less|(&rest args) (opexpand '|Less| args))
(defun |LessEqual|(&rest args) (opexpand '|LessEqual| args))
;; SameQ is NOT consistent with Mathematica's program, exactly,
;; since 2 numbers of different precision may be SameQ in mma.

#+ignore
(defun |SameQ|(&rest args) (opexpand '|SameQ| args))
(defun |SameQ|(x y) (and (equal x y) '|True|)) ;; lisp equal
#+ignore
(defun |UnSameQ|(&rest args) (opexpand '|UnSameQ| args))
(defun |UnSameQ|(x y) (if (equal x y) nil '|True|))
(defun |Equal|(&rest args) (opexpand '|Equal| args))
(defun |Unequal|(&rest args) (opexpand '|Unequal| args))
(defun |Inequality|(&rest args)(ucons '|Comparison| args))

(defun |ListQ|(x)(and (consp x)(eql (|Head| x) '|List|) '|True|))

;; this is really cute:  if x matches an element of l, return true
;; e.g. if pat = (m1 x l) (|PatternTest| (|Pattern| x (|Blank|)) |Integer|Q) 
;; then (MemberQ '(a b 3 z) pat)  will return True
;; example  MemberQ[{2,3},_?OddQ]  returns True.
;; example  MemberQ[{2,4},_?OddQ]  returns nil.

(defun match_no_frame(a b)(prog1 (match a b)(spopframe env)))
(defun |MemberQ|(l x)(if (member x l :test #'match_no_frame) '|True|)) 


;; have not defined PrimeQ PolynomialQ VectorQ MatrixQ ValueQ OrderedQ 

#+ignore
(defun |Map|(f exp) 
  ;(declare (special functionht))
  (ucons (car exp) 
	 (mapcar #'(lambda(r)(mapply f (list r) (ulist f r) env)) (cdr exp))))

(defun |Map|(f exp) 

  ;;(format t "~% map exp=~s, eval(exp)=~s env=~a" exp (meval exp) env)
  (cond ((atom exp)(format t "can't map over atom ~s" exp)(signal 'error)))
  (ucons (car exp) 
	 (mapcar #'(lambda(r)(mapply f (list r) (ulist f r) env)) (cdr exp))))


(defun |Scan|(f exp) 
  (map nil #'(lambda(r)(mapply f (list r) (list f r) env) ) (cdr exp))
  '|Null|   )


(defun |Table| (exp &rest iters)
  
  ;;; Table [exp, spec1, spec2] is the same as
  ;;; Table[Table[exp,spec1],spec2]
  (if (null (cdr iters)) (table1 exp (car iters))
    (let ((args (reverse iters)))
    (apply #'|Table| (cons (table1  exp (car args)) (reverse (cdr args))) ))))

(defun |Do| (exp &rest iters)
  ;; (spushframe env)
  (catch :ret (do0 exp iters) 
;;(spopframe env)
	 )  )

(defun do0(exp iters)
  (cond ((null (cdr iters)) (do1 exp (car iters))) ;; this is OK.
    ;; the inner iteration varies slowest. uh, why can't I do this neatly?
	(t(format t "~%only one iterator for Do,please ~s" iters)
	  (signal 'error))))
 ;;   (do1  `(|Do| ,exp ,(car iters)) (cdr iters)))) ;; no agood

    

(defun table1 (exp iter)
  ;;Table is an expression with a free variable itvar
  ;;iter looks like {i,low,hi}  or (|List| i low hi) in Lisp
  (case (length iter)
    (1 (error "invalid iterator ~s" iter))
    (2 ;; (List count)
     (let ((count (second iter)))
       (cond((not(integerp count)) (format t "~%expected integer iterator ~s" iter)
				   (signal 'error))
	    ((< count 0)(format t "~%expected non-negative iterator ~s" iter)(signal 'error))
	    (t (setf exp (meval exp))
	       (uniq (cons '|List| (loop for i from 1 to count collect exp)))))))
    (3 
     ;; (List i hi);  no low, assumed 1
     ;; or (List i (List a b c ...))
     (let ((tt (third iter))
	   (itvar (second iter)))
       (cond((and(integerp tt) (>= tt 1))
	     (table1 exp `(|List| ,(second iter) 1 ,tt))) ;; just count from 1.
	    ((and (consp tt)(eql (car tt) '|List|))
	     (spush env itvar nil)
	     (do ((i (cdr tt) (cdr i))
		  (res (list '|List|) 
		       (progn (schange env itvar (car i))
			       (cons (meval exp) res)
			      )))
		 ((null i) 
		  (spop env) 
		  (uniq(nreverse res))))))))

  
    ((4 5) 
     (let ((itvar (second iter)) ;; (List i low hi [step])
	   (hi  (meval (fourth iter))) ;hi
	   (step (or (meval (fifth iter)) 1))) ;if missing, then 1
       (spush env itvar 0); reserve a space
       ;; the case of {i, 1, 10}   or {i,1,10,2} ;; set step
       (do ((i (meval (third iter)) (+ step i))
	    (res (list '|List|) 
		 (progn (schange env itvar i)
			(cons (meval exp) res))))
	   ((> i hi) (spop env) (uniq(nreverse res))))))
    ))

    
    ;; we do the case of iter =  {i,{a,b,c}}
    ;; we do the case of iter = {i,max}  which is {i,1,max} 
    ;; we do the case if iter= {count} which is count copies.


;;; If we wanted to allow for return out of a Do..
;;; we would have to check somehow for an evaluation inside a compound expression, if, etc
;;; that had a return in it, and then somehow do this..
;;;    (if (eql (|Head| exp) '|Return|)(return-from do1  (meval (or (cadr exp) '|Null|))))

(defun do1 (exp iter)
  ;;Do is an expression with a free variable itvar
  ;;iter looks like {i,low,hi}  or (|List| i low hi) in Lisp
  (case (length iter)
    (1 (error "invalid iterator ~s" iter))
    (2 ;; (List count)
     (let ((count (second iter)))
       (cond((not(integerp count)) (format t "~%expected integer iterator ~s" iter)
	     (signal 'error))
	    ((< count 0)(format t "~%expected non-negative iterator ~s" iter)(signal 'error))
	    (t ;(setf exp (meval exp))
	       (loop for i from 1 to count do (meval exp))
	       '|Null|))))
    (3 
     ;; (List i hi);  no low, assumed 1
     ;; or (list i (List a b c ...))
     (let ((tt (third iter))
	   (itvar (second iter)))
       (cond((and(integerp tt ) (>= tt 1))
	     (do1 exp `(|List| ,(second iter) 1 ,tt))) ;; just count from 1.
	    ((and (consp tt)(eql (car tt) '|List|))
	     (spush env iter nil)
	     (do ((i (cdr tt) (cdr i))
		  (res nil 
		       (progn (schange env itvar (car i))
			      (meval exp))))
		 ((null i) (spop env) '|Null|))))))
    ((4 5) 
     (let ((itvar (second iter))	     ;; (List i low hi [step])
	   (hi  (meval (fourth iter)))	     ;hi
	   (step (or (meval (fifth iter)) 1))) ;if missing, then 1
       (spush env itvar 0)		       ; reserve a space
       ;; the case of {i, 1, 10}   or {i,1,10,2} ;; set step
       (do ((i (meval (third iter)) (+ step i))
	    (res nil
		 (progn (schange env itvar i)
			(meval exp))))
	   ((> i hi) (spop env) '|Null|))))
    ))
    

;; can module be made like function / function application?

;; Module[{x,y,z}, stuff]  is like Apply[ Function[{x,y,z} stuff] {(gensym)(gensym)(gensym)}
;; Module[{x=x0,y}, stuff]  is like Apply[ Function[{x,y} stuff] {x0,(gensym)(gensym)}

;;; got to fix Module/ Function to return the last value...
(defun |Module| (args body)
  
  (let ((argsonly  ;; assume args is (List (Set x x0) ..../or/ y )
	 (mapcar #'(lambda(z)(cond ((atom z) z)
				   ((and (consp z)(eql (car z) '|Set|))
				    (cadr z))
				   (t (format t "~%illegal Module varlist spec ~s" z)
				      (signal 'error))))
		 (cdr args)))
	
	(valsonly 
	 (mapcar #'(lambda(z)(if (atom z) (gensym (symbol-name z))
			       (caddr z))) (cdr args))))
    ;(print (list argsonly valsonly))
     (|Apply| `(|Function| (|List| ,@argsonly) ,body) `(|List| ,@valsonly))
     ))

(defun |Apply| (fun args)
  (cond ((eql (|Head| args) '|List|)  (mapply fun (cdr args) (ucons fun args) env))
	(t (format t "~%second arg to Apply not a List ~s" args)(signal 'error))))




  
  ;;

;; in mockmma and WRI
;;  a*b^3 /. a_.*b^n_. -> aha[a,n]  
;;(works, aha[a,3])
;; AA*b^3 /. a_.*b^n_. -> aha[a,n]  
;;(works, aha[AA,3] )
;;    b^3 /. a_.*b^n_. -> aha[a,n]  
;;(works, aha[1,3] )
;;    b   /. a_.*b^n_. -> aha[a,n]  
;;(works  aha [1,1] )

;;  b /. a_.+b^n_. -> aha[a,n] 
;; (works with aha[0,1] )




;; etc etc  Timesby SubtractFrom PreDecrement DivideBy 
(defun |AddTo|(target val)
  (meval `(|Set| ,target (|Plus| ,target ,val))))

(defun |Increment|(target)(|AddTo| target 1))
(defun |PreIncrement|(target)(prog1 (meval target) (|AddTo| target 1)))
(defun |Decrement|(target)(|AddTo| target -1))

(defun showenv()(format t "~% env=~%~s" env))



