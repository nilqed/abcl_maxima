;; -*- Mode:Common-Lisp;Package:mma; Base:10 -*-o
;; Mathematica(tm)-like evaluator
;; copyright (c) 1990 Richard J. Fateman; pieces by Tak Yan, Derek Lai
;; hacked more 2/2011 to make it work in GCL, not complete. Need to put 
;; two-case symbols within | |.

;;BIG CHANGE FROM EARLIER VERSION.  Use Mathematica "Own Values" and memoizing
;;subject to review when variables on which a value depends, changes.
#| Here's the idea.
A symbol container in the global symbol table or on a stack becomes a more
complicated object.  In particular, any symbol with a binding to anything
other than a constant must have the following properties, or at least
a possibility of having these properties:

*  value, e.g.  x
*  function e.g. list of rules from  x[u_]:= ...
*  extra values like x[0,1]:=45
*  Attributes like Listable, Flat, NumericFunction.  All stack objects have attribute Temporary.
*  OwnValues.. This are apparently the values when evaluated at assignment-time     value from assignment.
   That is, x1=x2; x2=x3; x4=x1;  x1  has ownvalue x2. x4 has ownvalue x3.

*  A TimeStamp or counter: the order of the assignment in the process run. 
   
* Depends: associated with each ownvalue is a bag of every variable on which this depends.
* Thus x1 above depends on x2.  x2 depends on x3.

To evaluate x1 which was last evaluated at time t=100, and at its
ownvalue, x2. Look at the time at which x2 was last evaluated. 
Say it was time t=200. use its current value for x1, thereby updating
x1. Set its timestamp to "now".

Then just use x2222 for x1.  and Update the time on x1.
That is, t=200 is more 

example..

Module[{x = y, z}, z = x		; OwnValues[z]]  == {HoldPattern[z$652] :> y}  ....

Changes needed.

To SetDelayed[a, b],

Set T= TimeStamp.  Also for a[0] etc. Increment the TimeStamp.
Set the Depends for a
then evaluate b to get expression c.
Set the "current value of b at timestamp T" to c.
Set the Ownvalue for a  to b.

Oh, make a[[4]]=10 work, via Part[] on lhs of assignment.

To meval. specifically for meval-atom[q]. Look at q's TimeStamp. It must
be higher than every TimeStamp for q's depends list.  If not, say
m in depends has a higher timestamp, then re-evaluate q from its ownvalue
list, and reset q's timestamp.  (increment TS.)

what about flags that might affect evaluation?? uh, we lose them. 
Mathematica loses them too.

consider
lim=4
q[x_]:= 0 /; x>lim
s={q[1],q[2],q[3],q[4]}
lim=2
s
(* a seemly unrelated assignment *)
q[5]=0
s



|#


(eval-when (compile) (load "mma"))
(in-package :mma)
(eval-when (load) (export '(tl mread1))
;;**********
)
(defvar COUNT 1 "line number for top-level. (same as (meval |$Line|))")
(declaim (special env *expand* *numer*)) ;; environment

;; The Mathematica semantics avoids repeated evaluation by putting a time stamp
;; on each object, as well as dependencies.  If the object is newer than any
;; thing it depends on, then it is already "evaluated".  Otherwise it
;; has to be re-evaluated and re-time-stamped. We don't do this right now,
;; but we might have to.  Mathematica is a little vague and dependencies are
;; not listed explicitly (we think), but approached approximately, e.g.
;; expression x+y+z depends on stuff on some pages (which include the
;; residences of x,y,z.  But they might all be on one page.  And there
;; may be other things on those pages that change, so x+y+z might be
;; unnecessarily re-evaluated.  

(defun |Set| (lhs rhs);; lhs=rhs
  ;; the value associated with the lhs will be stored
  ;; in the symbol table symtab, with the key h,
  ;;  which is either the head of the lhs,
  ;; or the lhs itself.  That is  f=45 is stored in the
  ;; hash table for f, under the indicator "f"
  ;; and f[0]=7 is stored in the hash table for f, under
  ;; the indicator (0).
  (declare (special symtab funnyvars))
  (let ((h nil)(fun nil))
    (cond ((symbolp lhs);; common case,  "x=stuff"
	   ;; 3 possibilities, x is bound on stack or or a funny-var or is global
	  ;; (format t "~%setting symbol ~s" lhs)
	   (multiple-value-bind (val found)(sfind env lhs)
	    ;; 	   (format t "~%mvb result for symbol  ~s is ~s ~s " lhs val found)
	     
	     (cond (found		;found on stack.
		    (schange env lhs rhs)) ;;change it there.
		   ((setf fun (gethash lhs funnyvars)) ;one of the vars needing special work
		  ;;  (format t "~%~s is in funnyvars"~s)
		    (funcall fun rhs))
		   (t ;;global case. Not found on stack
		   ;; (format t "~%symtab is ~s" symtab)
		    (setf h (gethash lhs symtab)) ;odd. should be there from parser.
		  ;;  (format t "~%hashtable for ~s is ~s" lhs h)
		    (cond((null h)(chash lhs) (setf h (gethash lhs symtab))))
			;; set h to the hash table for "x"
		    (setf  (gethash lhs h) rhs))))) ;  put value on key "x" in "x" hashtable
	  
	  ;; not not so simple an assignment, e.g., x[i,j]= stuff,
	  ((consp lhs)
	   (setq h (gethash (car lhs) symtab)) ;symtab for x
	   (cond((null h)(chash (car lhs)) (setf h (gethash (car lhs) symtab))))
;;	   (format t "~% Set has non-atom lhs ~s" lhs)
;;	   (format t "~%hashtable for ~s is ~s" (car lhs) h)
	   (setq lhs (mevalargs (|Attributes|(car lhs))(cdr lhs))) ; evaluate i,j
	   (setf (gethash lhs h) rhs))
	  (t (error "illegal assignment target ~s" lhs)))
    rhs)) ;return value
  
  ;;(format t "Set ~s to ~s, h=~s~%" lhs rhs h)
  ;; this stores the material in the hash table.
  ;; QUESTION: M'ma doesn't do this, but we could, by storing
  ;; stuff on a local environment... f[x_,y_]:=Block[{},x=y+1];
  
  ;; what if (gethash h symtab) is a matrix, and this is a valid matrix
  ;; setting?  Then we should try to store the value in the array.

    ;; Next, check for special variables which, when set, cause other
  ;; things to happen. E.g. Precision= number means, in the
  ;; bigfloat system, (bigfloat-init number) gets executed.

;; there is another file  (nmatrix) that defines a matrix type..
;; this should be tied in to the matrix stuff from franz, perhaps.
;; also, we have to decide which bigfloat to use... mpfun or rjf's
;; old bfstuff.
;; or MPFR. 12/2010


(defun |SetQQ|(lhs rhs &aux h);; lhs=rhs, but don't mevaluate either.
  
  (setq h(cond ((atom lhs) lhs)
	       (t (prog1 (car lhs) (setq lhs (cdr lhs))))))
  (setf (gethash lhs (gethash h symtab))rhs)
  
  (if (setq h (gethash h funnyvars ))(funcall h rhs))
  rhs)

(defun |SetDelayed|(lhs rhs) ;; this is the function definition f[x_] := ...
  (let* ((visible (ulist '|SetDelayed|  lhs rhs))
	 (hidden (bindfix(fixopts visible))))
    (setdelayed1 hidden)
    (setf (gethash visible optimruleht) hidden) ;; store the optim rule/def here
    '|Null|))

(defun setdelayed1(thedef)
  (let* ((lhs (ucons (caadr thedef)
		     (umapcar #'meval (cdadr thedef)))) ;; see note re eval below
	 (hh (|Head| lhs)) ;; lets hope the Head is a symbol 
	 (spot (gethash hh symtab)))
;    (format t "~%lhs=~s" lhs)
    ;; should we evaluate the formal arguments?  we need to do something, e.g.
    ;; f[Sqrt[x_]]  should be changed to f[x_^(1/2)]
    ;; this could be merely simplification, not evaluation .. ugh.
   ;; (setf lhs (ucons (car lhs)(mevalargs nil (cdr lhs))))
	 ;;(args (mevalargs (|Attributes|(|Head| hh)) (cdr lhs-in)))
	 ;(args (cdr lhs-in)) ;; these should not be evaluated; they are like formal params, no?
    ;; push all but the 'SetDelayed  on the list of definitional forms.
    (if (null spot)(setf spot (setf (gethash hh symtab)  (make-hash-table :test #'equal :size 4)))) ; not needed except bug?
    (cond ((null (gethash '|SetDelayed| spot))
	   (setf (gethash '|SetDelayed| spot) (ulist (ucons lhs (cddr thedef)))))
	  (t (push (ucons lhs (cddr thedef)) (gethash '|SetDelayed| spot))))))
    

;; this assumes the value of a mathematica symbol is its lisp value
;; if it is simply a constant or non-hash-table. That means that
;; a lisp dynamically bound variable could be used to block access
;; to the globally bound variable of the same name.  Better not
;; use local-variable names like Sin or Cos unless you mean to
;; inhibit access to the global names.

(defun meval-atom(h)  
  (declare (special env))
  
  (if (constantp h) h
    (multiple-value-bind 
	(val found)
	(sfind env h)
      (if found val ;; return val
	;; if we find it here on the env stack.
	;; otherwise
	;; val is a symbol, same as h  see if it has a value
	(let ((r (gethash val symtab)))
	  (if (hash-table-p r)
	      (gethash val r val) ; 
	    val))))))


;; look up the value of e on e's hashtable, key e
;; look up the value of e[10] on e's hashtable, key (10)

(defun msymbol-value (h) 
    (cond
     ((atom h) (meval-atom h))
     (t (let ((tabentry (gethash (|Head| h) symtab)))
	  (if tabentry (gethash (cdr h) tabentry h)
	    h)))))


;; this guy looks on the stack; returns if there. looks on global symtab; returns if there.
;; no loop. works, evaluates "once" if that's what you want.
(defun msymbol-function(h)
  ;; hm, how much work should we do here?
  ;; it could be that h is just h, and has no binding of any sort.
  ;; it returns either a non-function or a (global) SetDelayed list or rules

     (setf h (cond 
	      ;; if we think we should look on the stack for it, then we do this.
	      ;; if the value is yet another name, what then? Do we use that
	      ;; name or continue looking??
   
	      ((multiple-value-bind 
		   (val found)
		   (sfind env h)
		 (if found val nil))) ;; if the function is found on stack, it is val
	      ;; we go on to the next clause if the function is not found
	      ;; on the stack.
	      
	      ((multiple-value-bind 
		   (val found)
		   (gethash h symtab)
		 (if found (return-from msymbol-function (gethash '|SetDelayed| val h))
		   ;; if not found return h
		   h)))))
     h)

 ;; is this going to have the right scope?

(defun mapply (hraw args expr env);; note that args= (cdr expr)
  (let* ((fun nil)
	 (h (meval-to-function hraw))
	 (msymq (gethash h symtab)))	; is a mma symbol?
    
    ;; there are 2 kinds of applications here.
    ;; (Function ...)  which is just held.
    ;; (w ...)  where w is (Function ...)
 ;;     (format t "~% h=~s hraw=~s args=~s" h hraw args)
    
    (cond ((eql h '|Function|) 		       
	   (return-from mapply (ucons '|Function| (funfix (cdr expr)))))
	  ((and (consp h)(eql (car h) '|Function|))
	   ;; this next line should grab the attributes of this Function, if any
;;	   (format t "~%before args to function ~s are ~s, with env=~s" h args env)
	 ;;  (setf args (mevalargs (cdddr h) args))  will be done in mappfun
;;	   (format t "~%after args to function ~s are ~s" h args)	   	   
	   (return-from mapply (mappfun h args expr env))))
    
    ;; (format t "~%before args.. ~s are ~s, with env=~s" h args env)	
    ;;get info on evaluating arguments and do it to args
    (setf args (mevalargs (|Attributes| h) args)) 
    (cond
     ;; I don't believe the comment below...

     ((constantp h) 
     ;; (format t "~%h=~s" h)
      h) ;; allows any lisp function, almost, to slip through
     ;; check for constant values pre-stored
     ((not (symbolp h)) (setq expr(ucons h args)))
     
     ;; oo... this makes lisp function go first.  not right??
     #+ignore
     ((and				;(not msymq)
       (symbolp h)(fboundp h))
      ;(format t "~% applying a lisp function ~s to args ~s " h args )
      (setq expr (apply h args)))

     ;;((not msymq)(setq expr (ucons h args)))
     ;; maybe put in a check for array here? Not now.
     ;; next check for user-defined function
     ((consp (setq fun (msymbol-function h)))
      ;;(setq args (mevalargs h args)) ;; not always ..
     ;; (format t "~%applying a user function ~s to args ~s" fun args)
      (setq expr(rulesetapply h fun args)))
     ;; next check for built-in LISP function
     ;; (clearly not something that Mathematica does)
     
     ((and				;(not msymq)
       (symbolp h)(fboundp h))
      ;(format t "~% applying a lisp function ~s to args ~s " h args )
      (setq expr (apply h args	)))
   
     (t ;;(format t "~% eval fell through ~s ~s" h args)
	(setq expr(ucons fun args))))
 ;;(format t "~% mapply result: expr=~s" expr)
    expr))


(defun meval-to-function(x) (meval x))
(defvar *evalonce* t)  ;; should be t to make quote (etc etc) work

(defun meval (e)
    (let ((saved e)(hd nil) (ha nil))
      (if(atom e)(return-from meval  (meval-atom e)))
      (cond ((eql (car e) 'lispapply)
	  ;;   (format t "~% lispapply found, fun= ~s, args=~s" (cadr e) (cdr (meval (caddr e))))
	     (return-from meval (apply (cadr e) (cdr (meval (caddr e)))))))
      (if (eq (setf ha (msymbol-value e))e)	;didn't find a value
	  nil	(return-from meval ha))	; DID find a value.

      
      ;; check off other constant expressions that don't evaluate.
      ;; perhaps Messages?
      ;;((patternobjectp e) e) .. What about Patterns?
      ;; (mapply (car foo)(cdr foo)  foo env) ==> foo  with no conses...
      (setf e (cons (meval-to-function (|Head| e))(cdr e)))
      ;; (return-from meval (mapply (car e) (cdr e) e env))      ))
      (setf e (mapply (car e) (cdr e) e env))
      ;; note the 3rd arg to mapply, just in case you want to
      ;; return the expression as the result without any change.
      ;; next step --
      ;;
      ;; do we keep evaluating until there is no change???
      (setf hd (|Head| e))
      (setf ha (|Attributes| hd))

;;       (format t" ~%ha=~s hd=~s e=~s" ha hd e)

      (cond (*evalonce* e)
	    ((eql hd '|Function|) (funfix (cdr e)) e) ;; compute new body
	  
	    ((and (member '|Listable| ha) (some #'|ListQ| (cdr e)))
	     ;; uh, (h a,b,..(List c d) ..) becomes
	     ;; (List (h a b ..c..) (h a b .. d..)  ).
	     (listify e))
	     ((equal e saved) e)
	    ((eql hd '|Hold|)  e)
	    (t (meval e)))
      ))

;; Each global object X is associated with a hash table
;; and we can, for each, 
;; to get the value, do (gethash X X), (gethash 'rules X) etc.

;; Local bindings hide everything.

;;Do we want to do infinite evaluation? 
;;How do we keep single copies of items in memory?
;;set up initial symbol-table entries for built-in stuff
;; should also set attributes

(mapc #'chash built-in-syms)

;; All system-level $-stuff can be initialized and stored this way
(defun globinit(name val)
  (chash name); just in case it isn't already there
  (setf (gethash name (gethash name symtab)) val))
  
(globinit '|$Line| 1)
;;(globinit '|False| '|False|)
(globinit '|False| nil) ;; maybe, maybe not
(globinit '|$Showtime| nil)
(globinit '|I| #c(0 1)) ;; imaginary unit  I^2 = -1.


;; simple debugging tool: examine contents of symtable entry

; |Attributes| that evaluation routines use. maybe
;    - Flat [associative, flatten out nested expressions]
;    - Orderless [commutative, put args in order]
;    - |Hold|First [don't evaluate first arg yet]
;    - |Hold|Rest [only evaluate first arg now]
;    - |HoldAll| [don't evaluate any args now]
;    - Procedure [procedure to call to do actual evaluation]
;    - Default [default value for optional variables]

;;[version 3]?

;; the attribute semantics are probably OK as long as only global properties
;; are being recorded.
;; define ClearAttribute similarly
;; mma does not allow setting of |Attributes| other than
;;Protected, ReadProtected, Locked, Temporary, |Hold|First, |Hold|Rest, |HoldAll|, Flat, Orderless, OneIdentity, |List|able, Constant, Stub, N|Hold|First, N|Hold|Rest, N|HoldAll|, NumericFunction, |Sequence||Hold|, and |HoldAll|Complete.  [version 7]

;;bunch of additions, 1/2011 RJF

(defun setattribute(h att &optional (val '|True|)) ;; not in mathematica
  (if (typep h 'hash-table) nil (setq h (gethash h symtab)));; now its a ht
  (setf (gethash att h) val) ;; in h's hashtable, set the attribute att's value to true
  (setf(gethash '|Attributes| h);; also in h's hashtable, set its property of |Attributes| to a list
    (adjoin att (gethash '|Attributes| h)))
  )
(defun |Default|(h)
 (gethash '|Default| (gethash h symtab emptyht) '(|Sequence|))) 
  

(defun |SetAttributes|(hi attlist)  ;; this is in Mathematica.
  (let ((h (gethash hi symtab))) ;; get the hashtable for the symbol h.
  (cond ((atom attlist) ;; just one
	 (setattribute h attlist))
	(t (map nil #'(lambda(r)(setattribute h r))(cdr attlist))
	    (cons '|List| (gethash '|Attributes| h))
	    ;attlist
	    ))
  (|Attributes| hi)))

(defun |Attributes|(h)
  (cons '|List| (gethash '|Attributes| (gethash h symtab emptyht ) nil)))


;; more notes
;; Sin[x_]/; x>3-> S[x] 
;; parses to
;; (Rule (Condition (Sin (Pattern x (Blank))) (Comparison x Greater 3)) (S x))

;; we don't have evaluation of Block  implemented.
;; 

;; probably should add evaluation of functions and slots. 
;; eg  #+1&[4]  should return 5.



(defvar *vt* 0) ;;virtual time
(defun timestamp()(incf *vt*))

(defvar size 100)

(defstruct (stack (:print-function stackprinter))
  (size 100 :type fixnum) ;if no size is specified, how about 100?
  ;; ptr points to top of stack.  0<=ptr<size : the index in which to
  ;; store at the next "push".
  (ptr 0 :type fixnum) 
  ;;frameptr points to the bottom of current call-frame 
  ;; a pair that looks like  <name of function> <next lower frameptr>
  ;; -1 <= frameptr < ptr
  (frameptr -1 :type fixnum) 
  (vars (make-array size))
  (vals (make-array size))
  (time (make-array size)))  ;; alternative, with timestamp

(defun spush(s var val)
  (setf (aref (stack-vars s) (stack-ptr s)) var)
  (setf (aref (stack-vals s) (stack-ptr s)) val)
  (setf (aref (stack-time s) (stack-ptr s)) (timestamp)
  ;;could check for overflow here
  (incf (stack-ptr s))
  s)


(defun sfind(s var)
  (let ((loc (position var (stack-vars s)
		       :start (1+(stack-frameptr s)) :end (stack-ptr s))))
    (if loc (values (aref (stack-vals s) loc) 
		    loc			; found: 2nd val is index
		    (aref (stack-time s))); 3rd val is timestamp
		    )	
      (values var loc)  ;;2nd value will be nil, first will be var itself
      )))




