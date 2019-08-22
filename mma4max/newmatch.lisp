;;; -*- Mode:Common-Lisp; Package:mma; Base:10 -*-

;; not including some newer stuff, e.g. in version 6,0, specifically,
;; PatternSequence, Longest, Shortest, Repeated (? modified) 12/2010 RJF

;; note: In the past I equivocated on the use of 'True and 'False or nil
;; as Boolean values in Mma. Problem: most lisp functions naturally
;; use nil for false, versus "anything else" but sometimes just 't
;; for true.  
;; Here's what I am doing now. You type in False. It is an atom with
;; value= the Lisp constant nil.   Lisp is happy with that.  If mockmma
;; displays nil, it shows as False.  A display hack.  With luck this
;; all holds together 2/2011.

;; Here's the decision: 
;;   Common   Our       WRI's             Meaning        Input Form    Output
;;    Lisp    mma       Mathematica (tm)
;; ----------------------------------
;; non-nil     ..True?   True            Boolean Truth     True        True
;;    nil     nil        False           Boolean False     False       False
;;    nil     Null       Null            Null value        Null        
;;    nil    (List)     {} (i.e. List[]) Empty List        {}          {}

;; note that we allow the use of  atom  t as
;; a symbol in the :mma package, separate from  the
;; global name space.

;;(provide 'match)
;;(require "stack1")
(in-package :mma)

(declaim (optimize (speed 3)(safety 0)))
;;; definition: a patternobject is like an expression but it can
;;; have (|Pattern| ...) or |PatternTest| or |Blank|  or etc in it.
;;; Actually, any expression can be used as a pattern. It would ordinarily
;;; match only itself if both the expression and the pattern were simplified
;;; but in a world where both a+b and b+a can exist, they should match.
;;; 

;;; there is special attention given to a variety of situations.

;;; first of all, there are external conditions, sometimes.
;;; (x_->f[x])/; pred[x]
;;; is |Condition|[Rule[x_,f[x]] , pred[x]]
;;; which means that we must consider matching (and possibly failing/rematching)
;;; x subject to the condition pred[x].
;;; 

;; sic = subject to inherited conditions

;;; All that matches here
;;; (a) structurally identical matches. [sic]
;;; (b) (|Blank|) matches <anything> [sic]
;;;     (|Blank| foo) matches anything with a |Head| of foo [usually the lisp car)
;;;     but also if foo is in {|Integer|, Rational, |Complex|, rat...?}
;;; matching rational or complex requires some special deconstruction of these.
;;; program is in ReplaceAll, but maybe should be somewhere else like Replace.

;;; (c) (|Pattern| x <patternobject>) matches whatever would
;;;   otherwise be matched by <patternobject> and also binds the value of
;;;   patternobject to the name x.  From that time onward during the match
;;;   x will only match identical items. [sic]
;;; (d) [must fix]
;;;    f[pat1, pat2, ...,patn] matches expression g[e1,e2, ..., en] {sic} if
;;; f matches g, and pat1 matches e1 subject to the conditions:
;;; {pat2, ...,patn} matches {e2, ...,en}  [sic]
;;; recursively. 
;;; {} matches {} [sic]
;;; This is only the simple case, since if p1 is a |BlankSequence| it can absorb
;;; one OR MORE of e1, e2 ...  If p1 is a |BlankNullSequence| it can absorb 0, one or more
;;; of e1, ...

;;; There is also a result of binding, that a variable
;;; can only match an expression consistently.
;;; f[x_,x_] will match f[3,3] but not f[3,4].
;;; f[x_,x_^2] will match f[a,a^2].  Though it won't match, at the moment f[3,9]
;;; since 9 is not explicitly (|Power| 3 2).
;;; However, f[_,_]  matches f[3,4].
;;; (|PatternTest| <patternobj> Test) matches whatever <patternobj> would
;;; match, but only if (Test <patternobj>) returns lisp t. 
;;;  (|PatternTest| x_ Test) is usually written x_?Test.
;;;Question:
;;; perhaps we should ask that (meval (list Test <patternobj>)) be True not t?
;;; we keep to t.


;;; check to see if the following is broken now.

;;; (d)  (|BlankSequence| ...) or __ inside (non-Orderless) functions. Note:
;;;  If f is "Flat" then f[a,x_] matches f[a,z,w]
;;;   with x-> f[z,w]. 
;;;  If f is "Flat" then f[b,x__] matches f[b,z,w]
;;;   with x-> Sequence[z,w] .  That's Sequence[z,w]. Look it up...

;;; (e) (|BlankNullSequence| ...) or ___ inside (non-Orderless) functions.

;;; (f) Orderless functions are matched only if they have a fixed
;;; number of arguments.

;;; g[x__,x__] matches g[1,2,3,1,2,3]  with x->(1,2,3)
;;; broken

;;; Some Flat functions are |Plus|, |Times|.  There are hardly any
;;; others.  They are also Orderless (see below) complicating
;;; the matching considerably.  

;;; Functions which are both Flat and Orderless functions are
;;; not handled by this version. yet.


;;; Orderless is handled...  (not, "If you can't eat it all, order less"  but
;;; "The universe tends toward a state without order -- orderless"..)

;;; if (say) |Plus| is orderless, then |Plus|[x_,Sin[x_]] matches
;;; |Plus|[3,Sin[3]] or |Plus|[Sin[3],3].

;;; Also allowed: x_|Integer|, x_foo,  or x:f[_] or x_?pred . 
;;;The form x_foo has the
;;; meaning that x's |Head| must be foo, or x must be of type foo.
;;;  x_foo parses into (|Pattern| x (|Blank| foo))
;;; x:f[_] parses into (|Pattern| x (f (|Blank|)))
;;; x_:pred parses into (|PatternTest|(|Pattern| x (|Blank|)) pred)

;; It is also possible to go into a pattern matching situation with a
;; condition specified externally, e.g. {note parens..}
;;  (x_-> f[x]) /;pred[x],  where this is parsed as
;; (|Condition| (Rule (|Pattern| x (|Blank|))(f x))
;;            (pred x))

;; note there can be Rule { ->} and Ruledelayed  { :> }
;; but this does not affect matching.  A Rule definition
;; means that the lhs and rhs are evaluated at definition time.
;; the rhs is evaluated AGAIN at application time.
;; RuleDelayed means only the lhs is evaluated at defintion time.

;;; Return value for a match is  nil or non-nil.
;;; If the value is non-nil, the stack structure env
;;; will have a set
;;; of bindings of all the pattern-variable bindings. If the return value
;;; is nil, env will be unchanged.

;; define match stack, using up to 100 pattern variables if no-one else
;; has done it.

(defvar env (make-stack :size 100))
(defparameter *matdebug* nil)
(defparameter *matdebug2* nil)
(defmacro dformat(&rest x)`(if *matdebug* (format ,@ x)))
(defmacro eformat(&rest x)`(if *matdebug2* (format ,@ x)))
(defun deb()(setf *matdebug* (not *matdebug*)))
(defun deb2()(setf *matdebug2* (not *matdebug2*)))

;; dosublist iterate over all sublists of items
;; try (dosublist (kk '(1 2 3)) (print kk))

(defmacro dosublist (iter &rest exp)
  (let ((ii (gensym))
	(len (gensym))
	(count (gensym)))
    
    `(let* ((,ii ,(cadr iter))
	    (,len (length ,ii))
	    (,(car iter) nil))
       
       ;; we can return from the middle of this..
       ;; try (dosublist (kk '(1 2 3 5 6 7)) (print kk) (if (and kk (> (car(last kk)) 5))(return 'toobig )))
       
       (do ((,count 0 (1+ ,count)))
	   ((> ,count ,len) ,(caddr iter))
	 (setf ,(car iter) (subseq ,(cadr iter) 0 ,count))
	 ,@ exp))))


;; iterate over all combinations of items
;; (docomb (i l default) (f i) (if (pred i)(return x))) suppose l is a
;; list '(1 2 3) successively sets i to (), (1), (2),(3),(1 2)
;; .... (1 2 3) and executes (f i)
;; exit from the block on (return..).  otherwise continue iteration, returns default


       ;; try (docomb (kk '(1 2 3)) (print kk))
       ;; or  (docomb (kk '(1 2 3)) (print kk) (if (equal kk '(1 3)) (return 'gotcha)))
       ;; or  (docomb (kk '(1 2 3) 'default-return) (print kk) (if (equal kk '(1 7)) (return 'gotcha)))

(defmacro docomb (iter &rest exp)
  (let ((ii (gensym))
	(l (gensym)))
    
    `(let* ((,ii (append ,(cadr iter) nil))) ;make a copy
       (block nil
	 (labels((mc (,l ,(car iter))
		     (cond ((null ,l) ,@exp)
			   (t (mc (cdr ,l) ,(car iter))
			      (mc (cdr ,l) (cons (car ,l) ,(car iter)))))))
	   (mc 	 (nreverse ,ii)    nil))
	 ,(caddr iter)
	 ))))




;; match provides a top level match of pattern to expression.
;; both pattern and expression are assumed to be Lisp lists
;; from the parser (lmath:parser)

;; Usually match won't be used by a program ... rather, it will use m1.
;; Typical usage would then be as follows... (see definition of trial function)

;; match pat and exp in an environment which is a stack (perhaps empty)

(declaim (special env phead isflat isorderless isfol))
(defvar isorderless nil)
(defvar isflat nil)
(defvar isfol nil)


;; a note on the data structuring.  
;; It would be possible to "abstract away" some of the cars and cdrs
;; but the representation from the parser is fairly definitive, close
;; to Mma, and unlikely to benefit from revision. Consequently it would
;; seem to be mostly coyness to pretend that we didn't know how things
;; were stored, and frankly, defining map-over-args as mapcar and
;; (head x) as (car x) gets tiresome, especially when you "pun" and
;; use argument lists as lisp lists. Of course, if we change the
;; data representation, this program will be harder to change as a
;; result of this decision.

;; a simple test function

#|note.  Many, of the subfunctions end up carrying
a condition which is a function of 0 arguments which, when
called, must return t to allow the match to succeed.  The idea is that if you are
trying to match {a_,b_,c, ...}  against {e1,e2, e3...} subject to test[a,b,c]
  then you match a to e1 subject to 
     matching {b_,c_, ...} to {e2, e3, ...}
      AND
     test[a,b,c]  evaluated with a=e1
which operates recursively for matching down the list.

At the moment the functions may also need to
know if matching is subject to attributes like Flat and Orderless.
We do this by transmitting as an argument
the |Head| that governs this (gh) so we can compute its attributes.

Hazardous.  Consider f[a,b,s] /.  f[x:(y:__),z___]->g[x,y,z]
need to bind x to whatever Sequence y matches.





|#

(defun trial(pat-in exp)
;;  (spushframe env 'trialmatch);; make a frame to store results of match ZZZ
  
  (let* ((pat (if (isanyopt pat-in) (mapremopts pat-in) pat-in))
	 (res 	  
	  (if  (match pat exp) ;; match is the matching program. env is global, result bindings.
	      ;;  (format t "~%The match succeeded. ~%Binding Stack is ~%~s" (env2alist env))
	      (list 'success (env2alist env))
	    (list 'failure)
	    )))

  ;;  env will simply be removed via unbinding on return
    (spopframe env) 
       res))

;; this next version shows how to return the answer by
;; adding to the conditions

(defun trial2(pat exp)
  
  (let* ((ans nil)
	 (res  (match pat exp  #'(lambda() 
				      (setf ans (env2alist env))
				      t))))

    ;; reset ptr to former position  after match
    (spopframe env)

    (if res (cons 'Success ans) '(Failure))))


;; match returns t/nil depending on whether it matches or
;; not. Typically if match returns non-nil, then the global variable
;; env will be set up with a new frame: a stack-frame called "match"
;; or "matchlist". In that frame there will typically be some
;; bindings. There will be no bindings if there were no pattern names
;; identified during the match (e.g. matching 3 to 3 does not produce
;; bindings.)  After using the bindings in that frame, the caller of
;; match (or matchlist) should do an (spopframe env) to remove that
;; frame after appropriate use of the environment env for (say) the
;; evaluation of the rhs of a pattern-replacement rule.
;; Values on the stack can be manipulated by functions spush, spop, 
;; spushframe, etc. in the file stack1.lisp

;;note that  w_ /; (w > 3) -> aha is 
;;(Rule  (|Condition|  (|Pattern|   w   (|Blank|))  (Comparison   w   Greater   3))
;;       aha)

;; w_?(#>3&)->hah is
'(Rule (|PatternTest|  (|Pattern|   w   (|Blank|)) (Function (Comparison   (slot 1)   Greater   3)))
       hah)


;;  ff[xx_] + rr_ :> gg[xx] + rr /; |OddQ|[xx]
'(RuleDelayed (|Plus| (ff (|Pattern| xx (|Blank|))) (|Pattern| rr (|Blank|)))
	      (|Condition| (|Plus| (gg xx) rr) (|OddQ| xx)))

;; this rule works to back up over the pattern match.
;; ff[1]+ff[2] /. %

;; 10 /. w_?((# > 3) & ) -> aha   returns aha
;; 10 /. w_?(# > 3 & ) -> aha     returns aha

;;; 12/2010
;;;  1/2011
;;;  2/2011
(defun truth() '|True|)

;;; match is a top-level function which does not allow for  conditions on the overall match.
;;; Here is the Mathematica name

(defun |MatchQ|(exp pat-in) ;; similar to trial
  
  (let* ((pat (if (isanyopt pat-in) (mapremopts pat-in) pat-in))
	 (res 	  
	  (if  (match pat exp) ;; match is the matching program. env is global, result bindings.
	      ;;  (format t "~%The match succeeded. ~%Binding Stack is ~%~s" (env2alist env))
	     '|True|
	 nil
	 )))
	 (spopframe env)
	 res))

(defun rmatch (a b)(|MatchQ| b a))

;; I prefer the pattern to come first.
(defvar *anyopts* nil)

;;; IMPORTANT!!!  match pushes a frame on env 
;;; BUT LEAVES IT THERE!!!
;;; whoever calls match  should access the values and the
;;; do (spopframe env) to remove them
(defun match (pat exp &optional (condition #'truth))
  (let ((*anyopts* (isanyopt pat)))
  (spushframe env 'match)
 (if  (m1 pat exp nil nil condition) '|True|)))

(defun isanyopt(x)
  ;; fast scan of pattern to see if there are any Optionals ANYwhere inside
  (cond ((atom x)(eq x '|Optional|))
	(t(some #'(lambda(r)(isanyopt r)) x))))

  
(defun matchall(pat exp &optional (condition #'truth))
  ;; return a list, the collection of all matches
  (let ((res nil))
    (match pat exp #'(lambda()
			    (eformat t "~%x1 p=~s e=~s c=~s" pat exp condition)
			    (if (funcall condition)(push (env2alist env) res)) nil))
    (spopframe env)
    res))

(defun matchtest(pat exp &optional (condition #'truth))
  ;; test for match, but do not leave bindings around
  (spushframe env 'match)
  (prog1  (match pat exp condition)
    (spopframe env))) ;;won't work for checking extra conditions. then use mlist.
  

(defun matchlist(pl el condition)  ;;alternative top level for simultaneous matches
  (spushframe env 'matchlist)
  ;;    pattern expression name condition
  (mlist pl el nil nil condition))

(defvar *extended-atom-match* t) ;; do better than Mathematica in matching 1/2 or 3+4I

;;; M1 main pattern matcher
(defun m1 (p e name gh c)
  ;; p is pattern, e is expression, name (maybe nil) is the variable name
  ;; that would be bound in case a binding is indicated. m1 doesn't make
  ;; bindings directly, but through mblanks or related subroutines.
  ;; oddly, the "main" program here can't truly process, directly,
  ;; most interesting patterns. Mlist, below, is usually doing the job.
  ;; Why is this?  While a pattern like x_ can "match anything", a pattern
  ;; like x__ {match a sequence} does not make much sense alone.
  ;; Rather f[x__] makes sense, where x__ will match all the arguments to f.
  
  ;; the |Alternatives| clause shows how backtracking can be done.
  ;; gh stands for governing head.
  ;; gh is the |Head| of the pattern that governs the attributes to make the pattern
  ;; e.g. in (f  (|Pattern| x ...) (|Pattern| y ...) z ...)
  ;; in the arguments of |Pattern| [x] and [y], as well as z, the governing head is f.
  ;; c is a function to be called, accumulated in the matching,
  ;; that must return t for the match to succeed. That is,
  ;; The success of m1 matching requires that (funcall c)
  ;; result in a non-nil value. As a side effect, the global variable env
  ;; will contain the bindings for pattern variables.
  ;; 
  ;;(print env)
  (let* ((hp (|Head| p))
	 (ha (and gh (cdr (|Attributes| gh)))) ;; if non-null gh, get attributes. could cache.
	 (hpa (|Attributes| hp))
	 )
  ;;  (format t "~% hp=~s, hpa=~s" hp hpa)
    ;; could this be called at rule-definition time??
   ;; (if (and *anyopts* (hasoptionalp p))(return-from m1 (m1 (remopts p hp) e name gh c)))
    
    (if (or (member '|Flat| hpa :test 'eq)
	    (member '|Orderless| hpa :test 'eq))
	(return-from m1 (m2 p e name gh c)))
    
    (dformat t "~%ha=~s" ha) ;; attributes of head of p
    (cond 
     ;; first check for simple termination conditions in which
     ;; the pattern is exhausted and all that needs to be checked
     ;; is the functional condition that has been accumulated to this
     ;; point and is the 'c' function
     ((null p)(if (null e) (funcall c) nil)) ; check the conditions.
     ((atom p)(and (equal p e)(bindifposs name e) (funcall c)))
     ((equal p e)(and (bindifposs name e) (funcall c)))
     ((and (atom e)*extended-atom-match* (member hp '(|Rational| |Complex|) :test 'eq))
    ;;  (print 'hi)
      ;; check below is for "ratiop" not in GCL?
      (cond ((and (rationalp e) (not (integerp e))) (m1 p `(|Rational| ,(numerator e),(denominator e)) name gh c))
	    
	    ;; the expression is a complex number
	    ((complexp e) 
	   ;  (print 'AHA)
	     (let ((re (realpart e))
		   (im (imagpart e)))
	       #+ignore
	       (m1 p `(|Plus| ,re  (|Times| ,im #c(0 1))) name gh c)
	       (m1 p `(Complex  ,re   ,im) name gh c)
	       )				  )))

     ;; check for a |Pattern|.  This construction provides a NAME for
     ;; the result of the sub-expression matching.  That is, matching
     ;; (|Pattern| Q  <stuff>)  is exactly the same as matching <stuff>
     ;; but with the side consequence of making the binding of the name Q
     ;; to the expression matching <stuff>
     ((eq hp '|Pattern|) 
      (dformat t "~% calling mpattern from m1 with  p=~s, e=~s gh=~s" p e gh)
      (mpattern  p 
		 e			;the expression
		 name;; ??
		 ;;nil
		 gh ;governing head, whose |Attributes| may be important
		 c)) ;condition, necessary for the rest of the pattern match

     ;; generally, the execution of this next clause signals a problem
     ;; in the composition of a pattern if gh has attribute Flat
     ((member hp '(|Blank|) :test 'eq)
      (dformat t "~% ~s match to expr ~s with name ~s" p e name)
      (mblanks p e name gh c) )
     ;; generally, the execution of this next clause signals a problem
     ;; in the composition of a pattern anytime.
     ;; the reason is, we need to have the context in the pattern -- what
     ;; else follows the X=blank[Null]sequence, to see how much X should
     ;; absorb.
      
     ((member hp '(|BlankSequence| |BlankNullSequence| :test 'eq))
      (dformat t "~% ~s match to expr ~s with name ~s" p e name)
      (mblanks p e name gh c))
     ;; next, go through the cases
     ;; Condition belongs only on the top level of a function, and
     ;; probably should not be encountered here, ever
     
    ((eq hp '|Condition|)
     (dformat t "~%processing |Condition| ~s "(caddr p))
     ;; transform the condition to be THIS condition AND whatever else.
     (m1 (cadr p)e name gh (let ((ccc (caddr p)))
			     (eformat t "~%x2 p=~s e=~s c=~s" p e ccc)
			     #'(lambda()(and (meval ccc)(funcall c))))) )
    ((eq hp '|Alternatives|) 
     ;; if we change |Alternatives| to be n-ary, 
     ;; this still works.
     (dolist (alt (cdr p) nil)		; range through alternatives.
       (dformat t "~% check alternative ~s" alt)
      ;; (spushframe env '|Alternatives|)
       (if (m1 alt e name gh c)
	   (return t)
	nil;; (spopframe env) ; if this choice failed, try another til exhausted
	 )))
    ((eq hp '|Action|) ;; would be (Action pattern (Bind name val)) ;; only case now handled
     (let* ((apat (cadr p))
	    (act (caddr p))		; action clause looks like (Bind name val)
	    (theact (car act))
	    (bindvar (cadr act))
	    (bindval (caddr act)))
       ;; assume it is a bind
       (dformat t "~% processing Action ~s" p)
       (if (eq '|Bind| theact)
	   (m1 `(|Pattern| , bindvar (|Blank|)) bindval name gh #'(lambda()(m1 apat e name gh c)))
	 (format t "~%invalid action specified in ~s" act))
       ))

    ((eq hp '|Except|)
     (not (m1 (cadr p) e name gh c)));; not really right since bindings are lost!!
    ((member hp '(|Repeated| |RepeatedNull|))
     (error "Matching of |Pattern| objects with |Head| ~s not installed, used in ~s" hp p))
    ((eq hp '|PatternTest|) ;;  a_?pred  for example
     (m1 (cadr p)e name gh
	 (let ((ccc (caddr p)))
	   #'(lambda()
	       (eformat t "~%x3 p=~s e=~s c=~s" p e c)
	       (and (meval (list ccc e))
			   (funcall c))))))
    ;; mlist takes care of the rest here..
    ;; if (|Head| p) is neither flat nor orderless, match in order.
    ;; Here, we match the heads and go through the rest via mlist.
    ;; simplified coding here
    ;; Note. (|Head| p) is NOT "|Pattern|" at this point in m1
     
    ;; now both p and e are expressions.
    ;; we match (recursively) their heads, and match,
    ;; in sequence, elements in their cdrs, by mlist.
    ;; in mlist we worry about attributes of phead, e.g. flat, orderless
    ((and (member '|Flat| ha :test 'eq)
	  (member '|Orderless| ha :test 'eq))
     (matchfol p e phead (|Default| phead) name phead c))

     ;;; THIS NEXT CLAUSE IS THE MAIN LOCUS OF ACTIVITY, usually
     
    (t

     ;; start matching in order.
     ;; (format t "~%in m1 should bind name ~s to ~s" name e)
     (cond (name
	    (multiple-value-bind (val found)(sfind env name)
	      ;; (print 'ww)

	      (cond (found (and (equal val e) ; if found and equal, don't change
				(m1 hp 
				    (|Head| e)
				    nil
				    gh
				    #'(lambda()
					(eformat t "~%x4 p=~s e=~s c=~s" p e c)
					(mlist (rest p)(rest e) nil  gh c))))) ;; no binding for name was found
		    (t  (spush env name e)
		  
			(if  (m1 hp 
				 (|Head| e)
				 nil
				 gh
				 #'(lambda()
				     (eformat t "~%x5 p=~s e=~s c=~s" p e c)
				     (mlist (rest p)(rest e) nil  gh c)))
			    t
			  (let ()(spop env) nil))))))
	   ((atom e)
 ;    (format t"~% atomic e e=~s p=~s" e p)
	    nil)
	    ;; if no name
	    (t (m1 hp    (|Head| e)  name  gh  #'(lambda()
						   (eformat t "~%x6 p=~s e=~s c=~s" p e c)
						   (mlist (rest p)(rest e) nil  hp c)
						   )))))))) ;end m1


(defun hasoptionalp(p) ;; check for Optionals just under the top level
  (if (consp p) (some #'(lambda(r)(and (consp r)(eq (car r) '|Optional|))) p) nil))


(defun remopts (p gh)			; restructure pattern to remove |Optional| in favor of |Alternatives|.
  ;; caution, there may be more than one optional...
  ;; find first optional.
  (let ((r (append p nil))
	(s (append p nil))				;two copies.
	(default nil)
	(pattern nil))
    
    (do ((rp r (cdr rp))
	 (sp s (cdr sp)))
	((null rp) p)			; won't exit this way..
      (cond ((and(consp (car rp))
		 (eq (caar rp) '|Optional|))
	     (setf pattern (cadar rp))
;;	     (format t "~%pattern=~s" pattern)
	     (setf default (or (caddar rp)(|Default| gh))) ;; the default
;;    (format t "~%default=~s" default)
	     (setf (car rp) pattern) ;; the pattern
;;	    (format t "~%sp=~s" sp)
	     ;;   #+ignore.. yuck a hack here to make (Power x 1) -> x?
	     ;;   clearly not the best spot for it.
	     (if (eql gh '|Power|)
		      (setf (car sp) (list '|Sequence|  default))
	     (setf (car sp) (list '|Sequence| )))
	     
	     
;;	       (format t  "~% in remopts r=~s  s=~s cadr rp=~s" r s (cadr rp))
	     (return `(|Alternatives| ,r (|Action| ,(meval s) (|Bind| ,(cadar rp),default))))	     
	     )))))


#| 
not good enough?

 consider (f a b (|Power| x (|Optional| (|Pattern| n (|Blank|)) 1)) c)
 we want  (|Alternatives| (f a b (|Power| x (|Pattern| n (|Blank|))) c)
                        (f a b x c)); bind n to 1, also.  
 is this no good: (f a b (|Alternatives| (|Power| x (|Pattern| n (|Blank|))) ;; note (simp '(|Power| x 1)) ==> x
 x) c) ;bind n to 1

 I think this requires that (|Power| x )  -> (|Power| x 1) -> x.  Done.
 
 We introduce an 'action' recognized by m1 m2 etc
 as part of a pattern.. (action (|Pattern|) (lisp program))  e.g.  
 (action (f a b x c) #'(lambda(p e name gh condition) (m1 n 1 nil nil condition)))
 We could define  perhaps an action that is not lisp, but some simple matcher language.
 so the |Optional| becomes ..
(|Alternatives| (f a b (|Power| x (|Pattern| n (|Blank|))) c)
                        (|Action| (f a b x c) (|Bind| n 1) )); 

;; this  works for |Power|, but also |Times| and |Plus|. Example:

(f a b (|Times|  (|Optional| (|Pattern| n (|Blank|)) 1) x )  c) is

(f a b  (Alternatives (Times (Pattern n (Blank)) x)
		      (Action x (Bind n 1))) c)


and even for other programs like this
(f a b  (|Optional| (|Pattern| n (|Blank|)) foo) c) could be

f[a,b,n_:foo,c]
(|Alternatives| (f a b  (|Pattern| n (|Blank|)) c)  ;; f[a,b,n,c]
	      (Action (f a b c) (Bind n foo) )) ;; f[a,b,c] with n=foo

if the program returns nil, the match fails. if it succeeds, it is not matched against anything.

;; when is remopts called?  it could be called when the pattern is defined, before it
;; is matched.
|#

;;; m2 is like m2 but for gh= |Plus|, |Times| or other Flat and Orderless operators.
;;; Are there any operators that are just one or the other [Flat, Orderless]?
;;; M2

(defun m2 (p e name gh c)  

  ;; the governing head is Flat or Orderless.
  ;; otherwise like m1, whose comments pertain here.
  (let* ((hp (|Head| p))
	 (hpa (|Attributes| hp))	       
	 (ha (and gh (cdr (|Attributes| gh)))) ;; if non-null gh, get attributes. could cache.
	 )
 ;;    (if (and *anyopts* (hasoptionalp p))(return-from m2 (m2 (remopts p hp) e name gh c)))
    (dformat t "~%ha=~s" ha) ;; attributes of head of p
    (cond 
     ((null p)(if (null e) (funcall c) nil)) ; check the conditions.
     ((atom p)(and (equal p e)(bindifposs name e) (funcall c)))
     ((equal p e)(and (bindifposs name e) (funcall c)))

     ;; check for a |Pattern|. 
     ((eq hp '|Pattern|) 
      (dformat t "~% calling mpattern from m2 with  p=~s, e=~s gh=~s" p e gh)
      (mpattern  p 
		 e			;the expression
		 name;; ?? yes, it can have a name YY other than the name XX in (|Pattern| XX stuff)
		 gh ;governing head, whose |Attributes| may be important
		 c)) ;condition, necessary for the rest of the pattern match

     ;; in this flat/orderless context, |Blank| operates like |BlankSequence|,
     ;; except the governing head of the answer is not Sequence, but gh.
     ((eq hp '|Blank|)
      (dformat t "~%m2 ~s match to expr ~s with name ~s" p e name)
      ;; (mblanks (cons '|BlankSequence| (cdr p)) e name gh c) 
      ;;???????
      ;;  (mblank2list (list(cons '|BlankSequence| (cdr p))) e name gh c)
      ;;     (mblanks p e name gh c)
      ;;(mblank1 p e name gh c)
      ;;(print 'hi)
      
      (mblank1 (cdr p) e name gh c)
      )
     ;; probably wrong
     ;; in this flat/orderless context, |BlankSequence| operates like |BlankSequence|,
     ;; with the governing head of the answer as Sequence, not gh.
      
     ((member hp '(|BlankSequence| |BlankNullSequence| :test 'eq))
      (dformat t "~%m2 ~s match to expr ~s with name ~s" p e name)
      (mblanks p e name nil c))  ;; note, nil   ;; probably wrong

     ;; in general, '|Plus|/'|Times| is the operator, 0/1 is the identityk
     ;; e.g. we could implement this by setting |Default|[|Plus|]=0, |Default|[|Times|]=1, 
     ;; maybe |Default|[_]=Sequence[] ???
	
     ;;((eq hp '|Plus|)      (matchfol p e '|Plus| 0 name gh c))
     ;;((eq hp '|Times|)     (matchfol p e '|Times| 1 name gh c)) 
     ;; covered by the case below
     ((and  (member '|Flat| hpa :test 'eq)
	    (member '|Orderless| hpa :test 'eq))
      (matchfol p e hp (|Default| hp) name gh c))

     ;; next, go through the cases
          ;; Condition belongs only on the top level of a function, and
     ;; probably should not be encountered here, ever

    ((eq hp '|Condition|)
     (dformat t "~%processing |Condition| ~s "(caddr p))
     ;; transform the condition to be THIS condition AND whatever else.
     (m1 (cadr p) e name gh (let ((ccc (caddr p)))
			     #'(lambda()
				 (eformat t "~%x7 p=~s e=~s c=~s" p e c)
				 (and (meval ccc)(funcall c))))) )
    ((eq hp '|Alternatives|) 
     ;; if we change |Alternatives| to be n-ary, 
     ;; this still works.
     (dolist (alt (cdr p) nil)		; range through alternatives.
       (dformat t "~% check alternative ~s from ~s" alt (cdr p))
     ;;  (spushframe env '|Alternatives|)
       (if  (m1 alt e name nil c);;(m1 alt e name gh c) ;; (m2 alt e name gh c)
	   (return t)
	 nil ;;(spopframe env) ; if this choice failed, try another til exhausted
	 )))
    ((eq hp '|Action|) ;; would be (Action pattern (Bind name val)) ;; only case now handled
     (let* ((apat (cadr p))
	    (act (caddr p))		; action clause looks like (Bind name val)
	    (theact (car act))
	    (bindvar (cadr act))
	    (bindval (caddr act)))
       ;; assume it is a bind
       (dformat t "~% processing Action ~s" p)
       (if (eq '|Bind| theact)
	   (m2 `(|Pattern| , bindvar (|Blank|)) bindval name gh #'(lambda()(m2 apat e name gh c)))
	 (format t "~%invalid action specified in ~s" act))
       ))
    ((eq hp '|Optional|)
     (dformat t "~%|Pattern| ~s using |Optional| is probably misplaced" p)
     )
    ((eq hp '|Except|)
     (not (m1 (cadr p) e name gh c)))
    ((member hp '(|Repeated| |RepeatedNull|))
     (error "Matching of |Pattern| objects with |Head| ~s not installed, used in ~s" hp p))
    ((eq hp '|PatternTest|) ;;  a_?pred  for example
     ;;PPP
     (m2 (cadr p)e name gh
	 (let ((ccc (caddr p)))
	   #'(lambda()
	       (eformat t "~%x8 p=~s e=~s c=~s" p e c)
	       (and (meval (list ccc e))
			   (funcall c))))))
    ;; mlist takes care of the rest here..
    ;; if (|Head| p) is neither flat nor orderless, match in order.
    ;; Here, we match the heads and go through the rest via mlist.
    ;; simplified coding here
    ;; Note. (|Head| p) is NOT "|Pattern|" at this point in  m1
     
    ;; now both p and e are expressions.
    ;; we match (recursively) their heads, and match,
    ;; in sequence, elements in their cdrs, by mlist.
    ;; in mlist we worry about attributes of phead, e.g. flat, orderless
    ((and (member '|Flat| ha :test 'eq)
	  (member '|Orderless| ha :test 'eq))
     (matchfol p e hp (|Default| hp) name hp c)) ;WWW

     ;;; THIS NEXT CLAUSE IS THE MAIN LOCUS OF ACTIVITY, usually
     
    ((and (not (member '|Flat| ha :test 'eq))
	  (not (member '|Orderless| ha :test 'eq)))
    ;;(print 'xxxx)
     ;; start matching in order.
     ;; (format t "~%m1 should bind name ~s to ~s" name e)
     (cond (name
	    (multiple-value-bind (val found)(sfind env name)
	      (cond (found (and (equal val e) ; if found and equal, don't change
				(m1 hp 
				    (|Head| e)
				    nil
				    gh
				    #'(lambda()
					(eformat t "~%x9 p=~s e=~s c=~s" p e c)
					(mlist (rest p)(rest e) nil  gh c)))))			      
		    (t  (spush env name e)
			(if  (m1 hp 
				 (|Head| e)
				 nil
				 gh
				 #'(lambda()
				     (eformat t "~%x10 p=~s e=~s c=~s" p e c)
				  
				     (mlist (rest p)(rest e) nil  gh c)))
			    t
			  (let ()(spop env) nil))))))
	   ((atom e);;(print 'rrr) 
	    nil)
	   ;; if no name
	   (t				;(print 'yyy)
	    (m1 hp 
		(car e)			;(|Head| e) ?YYYY
		name
		gh
		#'(lambda()
		    (eformat t "~%x11 p=~s e=~s c=~s" p e c)
		    ;; (mlist (rest p)(rest e) nil  gh c)
		    (mlist (rest p)(rest e) nil  hp c)
		    ) ))
	   ))
      
    ;;I think the comment below is not true..	  
    ;; if (|Head| p) is orderless, mlist will allow jumbling the args if necessary.
    ((not (member '|Flat| ha :test 'eq)) ;; it is  Orderless though
					;  (print '**)
     (m1 hp 
	 (|Head| e)
	 name
	 gh
	 #'(lambda()
	     (eformat t "~%x12 p=~s e=~s c=~s" p e c)
	     (mlist (rest p)(rest e) hp  gh c)) ))
    
    ((and (atom e)*extended-atom-match* (member hp '(|Rational| |Complex|) :test 'eq))
;;     (print 'hi)
      ;; check below is for "ratiop" not in GCL?
      (cond ((and (rationalp e) (not (integerp e))) (m1 p `(|Rational| ,(numerator e),(denominator e)) name gh c))
	    
	    ;; the expression is a complex number
	    ((complexp e) 
	   ;  (print 'AHA)
	     (let ((re (realpart e))
		   (im (imagpart e)))
	       #+ignore
	       (m1 p `(|Plus| ,re  (|Times| ,im #c(0 1))) name gh c)
	       (m1 p `(Complex  ,re   ,im) name gh c)
	       )				  )))
      
	 
	 
  
   
    ;; non-atom, non-blank p can't match atom?

    ((member '|Flat| ha)
     ;; separate Flat and Orderless

     (if (member '|Orderless| ha :test 'eq)
	 (dformat t "~% ran into unimplemented section of m1 with ~s" p)
       ;; merely Flat. If head is H,
       ;;this has the effect of sortof making
       ;;|Blank| into |BlankSequence|, except with head H.

       (m1 hp (car e) name gh 
	   #'(lambda() 
	       (eformat t "~%x13 p=~s e=~s c=~s" p e c)
	       (mlist (cdr p) (cdr e) hp gh c)))))
    (t					; anything left??
     (format t "~% ran off end of m1 with ~s" p)
     nil))))

(defun bindifposs (name e)
    (cond ((null name) t)		; nothing to do to succeed
     (t (multiple-value-bind (val found)(sfind env name) ;; has the name a value?
	   (cond
	    (found ;; it has a value, 
	     (and (equal val (meval e))))
	    (t ;; it doesn't already have a value
	     (dformat t "~%binding ~s to ~e" name e)
	     (spush env name e) 
	     t))))))

(defun mpattern (p e name gh c) 
  ;; match pattern p to expression e and bind it to name, governing head gh
  
  ;; first of all, see if name already has a binding.
  ;; if it does, make sure it is compatible with the expression here.
  ;; it also has to match the pattern pat.
  
  ;; if the name does not have a binding, then 
  ;;(tentatively) bind name to e.
  ;; next, try to match, pat to e, 
  ;; if we CAN match pat to e, 
  ;; evaluate c.  If it is true, return t.
  ;; otherwise (pat doesnt match or c is not true), unbind name and return nil.
  ;; note that if pat begins with |BlankSequence| or gh is Flat and/or Orderless
  ;; the process of trying to match pat to e is elaborate, requiring perhaps
  ;; separating p into parts and grouping parts of e.  (done in mlist)
  
  ;; p looks like  (pattern n stuff)
  
 ;; (dformat t "~% mpattern will try to bind name ~s to ~s if it matches pattern ~s"	  name e p)
  (cond ((null name)
	 (m1 (third p) e (second p) gh c)) ;; now we have a name for pattern
	(t ;; there is a name
	 (multiple-value-bind (val found)(sfind env name) ;; has the name a value?
	   (cond
	    (found ;; it has a value, continue to try match pattern, though.
	     (m1 (third p) e (second p) gh 
		 #'(lambda() 
		     (eformat t "~%x14 p=~s e=~s c=~s" p e c)
		     (and (equal val (meval e))(funcall c)))))
	    (t ;; it doesn't already have a value
	     (spush env name e) 
	     (dformat t "~%tentatively assigning  ~s := ~s" name e)
	     ;; there is potentially a problem here.  If (third p) is a |BlankSequence|,
	     ;; it may match, but not against e.  [issue is for mlist, not m1 though]
	  
	     (if (m1   (third p) ; the subpattern X within (|Pattern| newname X)
		     e (second p);; the new name
		     gh 
		     #'(lambda()
			 (eformat t "~%x15 p=~s e=~s c=~s" p e c)
			 (schange env name (sfind env (second p))) ;adjust this name too.
			 (funcall c)))
		 t ;; it matches, and all conditions are GO
	       ;; it does not match
	       (progn (spop env)
		      (dformat t "~%retracting last assignment to ~s"  name)
		      nil))	     
	     ))))) )


(defun mpatternlist (pl el name gh c)
  ;; called only from mlist, once.
  ;; pl is list of patterns, el is list of expressions
  ;; match the pattern p= (car pl) where p looks like (|Pattern| name ...)
  ;; to some part(s) of expression el 
  ;; and bind it to name, governing head gh
  
  ;; first of all, see if name already has a binding.
  ;; if it does, make sure it is compatible with the expression here.
  ;; it also has to match the pattern pat.
  
  ;; if the name does not have a binding, then 
  ;;(tentatively) bind name to e1.
  ;; next, try to match, pat to e1, 
  ;; if we CAN match pat to e1, 
  ;; evaluate c.  If it is true, return t.
  ;; otherwise (pat doesnt match or c is not true), 
  ;; try some other binding of  name until ultimate success or failure.
  ;; note that if pat begins with |BlankSequence| or gh is Flat and/or Orderless
  ;; the process of trying to match pat to e is elaborate, requiring perhaps
  ;; separating p into parts and grouping parts of e.  (done in mlist)
  
  ;; pl looks like  (  (pattern n stuff)  ....)
  (let ((p (car pl)))
    (dformat t "~% mpatternlist will try to bind name ~s 
  to parts of  ~s  ~%if those parts  match pattern ~s"
	  name el p)
  (cond ((null name) ;; no name, coming in ****
	 ;; this effectively replaces ( (pattern X stuff) ...)
	 ;; with  ( stuff ...)  and sets name to X
	 (mlist (cons (third p)(cdr pl)) el (second p) gh c)) ;; now we have a name for pattern
	(t ;; there is a name
	 (multiple-value-bind (val found)(sfind env name) ;; has the name a value?
	   (cond
	    (found
	     ;; it has a value.  We continue to try match pattern, though.
	     ;; A thought, we could try to match value val to 
	     ;; (car el) or some prefix of el.
	     ;; but this won't work if val is some Sequence.
	     ;; I think we have to independently match p' and then see if
	     ;; p==p'
	     (let ((altname (gensym))) ;generate a p'
	       (spush env altname nil)  
	      (if  (mlist (third p) el altname gh 
			;;::***** something hairy here **** is this right?
			  #'(lambda() 
			      (eformat t "~%x16 pl=~s el=~s c=~s" pl el c)
			      (and (equal val (meval altname))(funcall c))))
		  t
		(spop env)		; get rid of altname
		)))
	    (t ;; it doesn't already have a value
	    ;; (print 'hh)
	     (spush env name (car el)) 
	     (dformat t "~%tentatively assigning  ~s := ~s" name (car el))
	     ;;  If (third p) is a |BlankSequence|,
	     ;;  we have to iterate to try to suck up enough parts of el
	     ;; to make it match.
	     
	     (if (m1 (third p) ; the subpattern X within (|Pattern| newname X)
		     el 
		     (second p);; the new name
		     gh 
		     #'(lambda()
			 (eformat t "~%x17 pl=~s el=~s c=~s" pl el c)
			 (schange env name (sfind env (second p))) ;adjust this name too.
			 (mlist (cdr pl) (cdr el) name gh c)))

		 t ;; it matches, and all conditions are GO
	       ;; it does not match
	       (progn (spop env)
		      (dformat t "~%retracting last assignment to ~s"  name)
		      nil))  	     ))))) ))

;; mblanks is called if  m1 encounters a |Blank|* pattern.
;; it should not -- |Blank|* should be within mlist
;; but this may happen anyway, and should be made to work

;;MBLANKS
(defun mblanks   ;; we need this, it seems
  (p e name gh condition)
  (let ((phead (car p))  ;; e.g. |Blank|, |BlankSequence|, BlackNullSequence
	(plist (cdr p))) ;; e.g. nil or some head to match. X__head
    (cond
     ((eq phead '|Blank|)
      ;;(mblank1 plist e name gh condition)) ;do simple |Blank| matching
      (mblank1 plist e name gh condition)) ;do simple |Blank| matching
	      
     ;; __foo .. this next line is like (|Pattern| name (|BlankSequence| foo))
     ;; where name=nil means no name. foo is the "head" to match
     ;; this pattern matches one or more objects EACH of which has
     ;; head foo.
     ;; f[foo[3], foo[4]] /. f[z__foo] -> hah[z]  
     ;; binds z to Sequence[foo[3],foo[4]].
     ((eq phead '|BlankSequence|)
      
      ;;; at this point we have to try to make a complete match of
      ;;; blanksequence to e, naming it name.

      (mblank2 plist e name gh condition)
      )
     ;; ___foo .. this next line is like (|Pattern| nil (|BlankNullSequence| foo))
     ;; where nil means no name. foo is the "head" to match
     ;; this pattern matches zero or more objects each of which has
     ;; head foo.
	      
     ((eq phead '|BlankNullSequence|)
            (mblank3 plist e name gh condition))
	      
     ;; we get here in a case like  x:f[_] which parses to
     ;; (|Pattern| x (f (|Blank|)))
     ;; that is, we use the name x and bind it to f[something] if
     ;; we can match [something]

     (t (error "mblanks called on ~s " phead)))))


(defun patfree(p) 
   (cond ((atom p)t) ;; null or atomic has no Sequence
	(t nil)))


;; mblank1 matches a (|Blank| h) or (|Blank|).
;; this checking plist requires that e.g. (|Blank| foo) matches (foo ...))

(defun mblank1			
    (plist e name gh condition)
  (cond (name  
	 (multiple-value-bind (val found)(sfind env name) ;; has the name a value?
	   
	   (cond (found   ;; it has a value
		  (dformat t "~% mblank1 found binding on variable  ~s  = ~s compare to e=~s" name val e)
		  (cond
		   ((and(or (null plist)(eq (|Head| e) (car plist))) ;; the head matches
			(equal val e)) ;; the value matches
		    (return-from mblank1 (funcall condition)))
		   (t (return-from mblank1 nil))) )
		 ;; here, there is a name but it does not have a previous value
		 ((or (null plist)(eq (|Head| e) (car plist))) ; head matches, if necessary..
			  (spush env name 
				 (or e (|Default| gh))) ;; since no value yet, push it.
			  (dformat t "~% new mblank1 ~s=~s, condition= ~s" name e condition )
			  (cond ((funcall condition) t)(t (spop env) nil)))
	
		 )))			;
	;; if there is no name, just match anything  with the right |Head| 
	(t  
	 (dformat t "~% plist=~s e=~s" plist e)
	 (if (or (null plist) (eq (|Head| e) (car plist))) (funcall condition) nil))))

	   
(defun satisfies-head(test try)
  (cond ((null test) t)
	((eq (|Head| try) test) t)
	;; this isn't going to work for lists, e.g. (Sin x) is a list ((Sin x)(Sin y)) needs
	;; different treatment
	(t  (every #'(lambda (r) (eq (|Head| r) test)) try))))
	   
(defun mblank2(plist e name  gh condition)
   (dformat t "~%for mblank2, name=~s, val=~s" name (sfind env name))
   (cond 
    ((null e) nil) ;; have to match at least one thing.
    (name  
	 (multiple-value-bind (val found)(sfind env name) ;; has the name a value?
	   (cond (found ;; it has a value
		  (cond
		   ((and(or (null plist)
			    (if (cdr e)	; it's a list
				(every #'(lambda (r) (eq (|Head| r) (car plist)))
				       e)
			      (eq (|Head| e) (car plist))
			      ))       ;; the heads match if needed
			(equal val e)) ;; the value matches
		    (return-from mblank2 (funcall condition)))
		   (t (return-from mblank2 nil)))) ;found but not matching
		 ;; here, there is a name but it does not have a previous value
		 ((or (null plist) (if (cdr e) ; it's a list
				       (every #'(lambda (r) (eq (|Head| r) (car plist)))
					      e)
				     (eq (|Head| e) (car plist))
				     )) ; head matches?
		  (spush env name  
			 ;;(if (and (consp e)(cdr e)(null gh))(cons '|Sequence| e) e  )
			 (or e 
			     (|Default| gh)
			     ;;(|Sequence|)
			     ))
		  ;; since no value yet, push it.
		  (cond ((funcall condition) (return-from mblank2 t )) ; return success
			(t (spop env) nil)) )))) ;;if it fails, pop it.
	;; if there is no name, there is no value to check either.
	;; just match anything  with the right |Head| and condition
	((null plist) (funcall condition)) ;; no |Head| to check
	((if (cdr e)	 ; There is a head to check and expr is a list
	     (every #'(lambda (r) (eq (|Head| r) (car plist)))
		    e)
	   (eq (|Head| e) (car plist))	; just check the single head
	   )
	 (return-from mblank2 (funcall condition)))
	(t nil	   )))

(defun mblank3(plist e name  gh condition)
  (declare (ignore gh))
   (dformat t "~%for mblank3, name=~s, val=~s" name (sfind env name))
  (cond (name  
	 (multiple-value-bind (val found)(sfind env name) ;; has the name a value?
	   (cond (found ;; it has a value
		  (cond
		   ((and(or (null plist)
			    (if (cdr e)	; it's a list
				(every #'(lambda (r) (eq (|Head| r) (car plist)))
				       e)
			      (eq (|Head| e) (car plist))
			      ))       ;; the heads match if needed
			(equal val e)) ;; the value matches
		    (return-from mblank3 (funcall condition)))
		   (t (return-from mblank3 nil)))) ;found but not matching
		 ;; here, there is a name but it does not have a previous value
		 ((or (null plist) (if (cdr e) ; it's a list
				       (every #'(lambda (r) (eq (|Head| r) (car plist)))
					      e)
				     (eq (|Head| e) (car plist))
				     )) ; head matches?
		  (spush env name  
			 ;;(if (and (consp e)(cdr e)(null gh))(cons 'Sequence e) e  )
			 (or e 
			     '(|Sequence|) ;; different from mblank2
			     ))
		  ;; since no value yet, push it.
		  (cond ((funcall condition) (return-from mblank3 t ))
					; return success
			(t (spop env) nil)) )))) ;;if it fails, pop it.
	;; if there is no name, there is no value to check either.
	;; just match anything  with the right |Head| and condition
	((null plist) (funcall condition)) ;; no |Head| to check
	((if (cdr e)	 ; There is a head to check and expr is a list
	     (every #'(lambda (r) (eq (|Head| r) (car plist)))
		    e)
	   (eq (|Head| e) (car plist))	; just check the single head
	   )
	 (return-from mblank3 (funcall condition)))
	(t nil 	   )))

(defun blank1p(x)  (and (consp x) (eq (car x) '|Blank|)))
(defun blank2p(x)  (and (consp x) (eq (car x) '|BlankSequence|)))
(defun blank3p(x)  (and (consp x) (eq (car x) '|BlankNullSequence|)))

;; check for case of |Pattern|[name,blank[]]   i.e. name_ or (|Pattern| x (|Blank|))

(defun patternnameblank(x)(and (patternp x)(blank1p (third x))))

(defun patternp(x)(and (consp x)(eq (car x) '|Pattern|)))

;; mlist matches two lists, ordered. If the pl (pattern list)
;; contains a |Blank| (_), |BlankSequence| (_), or |BlankNullSequence| (___).
;; then special routines must be used.

;; If phead has attribute flat, (isflat is true), then |Blank| matching
;; operates the same as |BlankSequence|.  Why?  So
;; a+x_ matches a+b+c with x == b+c, instead of
;;                         x == Sequence[b,c]

;; k[r,s,t]/. k[r,x__]-> x  works, result is Sequence[s,t]
;; k[r,s,t]/. k[r,x__]-> g[x] bug. results in g[Sequence[s,t] 12/20/2010

;;(defun mlist-notfol-noxblanks(pl el name condition)  )

(defun simple-pat-or-constant (x)
  (or (patternnameblank x) ;; just a named pattern, e.g. x_
      (pattern-free x)))		; no pattern at all

(defun pattern-free (x)
  (cond ((atom x) (not (member x '(|Pattern| |Blank| |Alternatives| 
				   |Condition| |Optional| ;; do we need these?
				   ) :test 'eq)))
	(t (every #'pattern-free (cdr x)))))
  
;;MLIST
(defun mlist(pl el name gh condition)
  ;;   (mlist pl el name governing-head condition) matches (car pl) to some part of el,
  ;; and the remainder of pl gets matched to remainder of el.

  ;; The simple case is where (car pl) can match exactly one item (car el).
  ;; The complicated case is where (car pl) can match 0, 1, or more items from el.
  ;; This complicated case can occur when (car pl) looks like 
  ;; (|BlankSequence| head)  ;; one or more items x with |Head|[x]=head
  ;; (|BlankNullSequence| head) ;; zero or more items x with |Head|[x]=head
  ;; (|Pattern| x (|BlankSequence| head))  ;; one or more items x with |Head|[x]=head
  ;; (|Pattern| x (|BlankNullSequence| head))  ;; zero or more items x with |Head|[x]=head
  ;;  Other options? like |Alternatives|, |Optional|, |Condition|, |PatternTest|, ...  more messy stuff.
 (dformat t "~%mlist env=~s" (env2alist env))
  (cond ((null pl) 
	 (and(null el) 
	     (funcall condition))) ;; this is the terminating condition for matching!!

	;; sort of simple case, no sequences [|BlankSequence|, |BlankNullSequence|]
	;; not difficult governing head like |Plus| or |Times|
	((patfree (car pl))
	 (if (not (member '|Orderless| (|Attributes| gh)  :test 'eq))
	     ;; no sequences and in order. especially simple
	     (m1 (car pl)(car el) ;; match the first elements
		 name
		 gh
		 #'(lambda()
		     (eformat t "~%x19 pl=~s el=~s c=~s" pl el condition)
		     
		     (and (mlist (cdr pl)(cdr el) 
					;nil
					name ;??
					gh condition) ;then the rest
				 (funcall condition))))  
	   ;; ELSE
	   ;; gh is ORDERLESS
	   ;; we have definitely got to match p1 to something.
	   ;; if gh is also FLAT, we could match sequences.  notprogrammed here.
	   (let ((p1 (car pl)))
	     ;; (format t "orderless ~s" p1)
	     (dolist (e1 el nil)
	       (if (m1 p1 e1 ;; match the chosen elements if possible
		       name gh
		       #'(lambda()
			   (eformat t "~%x20 pl=~s el=~s c=~s" pl el condition)
				(and (mlist 
					(remove p1 pl :count 1)
					(remove e1 el :count 1)
					nil ;name  ;??
					gh condition) ;then the rest
				       (funcall condition))))
		   (return-from mlist t)
		 nil ))	    ; failed pattern match, go to next element
	     )))
	
	;; the next 3 clauses should not just call m1.

	((and (member '|Orderless| (|Attributes| gh) :test 'eq)
	      (member '|Flat| (|Attributes| gh) :test 'eq))
	 (dformat t "~%Oh dear, we have Orderless and |Flat| matching of ~s to ~s" pl el)
	  ;; (matchfol pl el gh 0 nil gh condition)) ;;??
	 (m2 (car pl)(car el) nil gh #'(lambda()
					 (eformat t "~%x21 p=~s e=~s c=~s" pl el condition)
					 (mlist (cdr pl)(cdr el) nil gh condition)))	)
	
	((member '|Orderless| (|Attributes| gh) :test 'eq)
	 (dformat t "~%Oh dear, we have Orderless matching of ~s to ~s" pl el)
	 (m2 (car pl)(car el) nil gh #'(lambda()
					 (eformat t "~%x22 p=~s e=~s c=~s" pl el condition)
					 (mlist (cdr pl)(cdr el) nil gh condition))))
	
	#+ignore
	((member '|Flat| (|Attributes| gh) :test 'eq)
	 (dformat t "~%Oh dear, we have Flat matching of ~s to ~s" pl el)
	 (m1 (car pl)(car el) nil gh #'(lambda()
					 (eformat t "~%x23 p=~s e=~s c=~s" pl el condition)
					 (mlist (cdr pl)(cdr el) nil gh condition))))
	;; first pattern in the list pl is not simple.
	;; say that pl looks like    (  (|Pattern| N (BlackNullSequence ...)) ...)
	;; we change pl to look like  (  (BlackNullSequence ...) ...)
	;; but with name N
	((eq (caar pl) '|Pattern|)
	 ;; *** make tentative assignment for name and etc etc
	 (mpatternlist pl el name gh condition) ;; possibly correct
	 )

	((eq (caar pl) '|Blank|)
	 (mblank1list pl el name gh condition))
	((eq (caar pl) '|BlankSequence|)
	 (mblank2list pl el name gh condition))
	((eq (caar pl) '|BlankNullSequence|)
	 (mblank3list pl el name gh condition))
	((eq (caar pl) '|Alternatives|) 
	 (let ((p (car pl)))
	   (dolist (alt (cdr p) nil) ; range through alternatives. Exhausted? fail
	     (dformat t "~% check alternative ~s" alt)
	     (if (mlist (cons alt (cdr pl)) el nil gh condition)
		 (return t)
	       nil))))		  ; keep looping through alternatives.
	((eq (caar pl) '|Optional|)
	 (format t "unexpected encounter with OPTIONAL ~s" pl)
	 )
	
	(t ;;(print 'gg)
	   (m1 (car pl)(car el) 
	       name ;nil 
	       gh 
	       #'(lambda()
		      (eformat t "~%x24 pl=~s el=~s c=~s" pl el condition)
		      (mlist (cdr pl)(cdr el) nil gh condition))) )))
	
	;;(t (format t "~% reached unimplemented MLIST section pl=~s el=~s alist=~s" pl el (env2alist env)) nil)

;;; f[a,b,d] /. f[x__,d]-> x   returns Sequence[a,b]  ;2blanks
;;; f[a,b,d] /. f[x___,d]-> x   returns Sequence[a,b] ;3blanks
;;; f[a,b,d] /. f[x_,d]-> x   returns f[a,b,d]  no match   ;1blank
;;; Set|Attributes|[ff,Flat]
;;; ff[a,b,d] /. ff[x_,d]-> x   returns f[a,b]            ;1blank and FLAT;;;

(defun mblank1list (pl el name gh condition)
  ;; we have pl=((|Blank| <head, maybe>) p2 p3 ...)
  ;; and   (e1 e2 ....)
  (dformat t "~%in mblank1list, env= ~s" (env2alist env))
  (let* ((p (car pl))
	 (headmatch (or (null(cdr p))(equal (|Head|(car el)) (cadr p)))))
    (dformat t "~%headmatch =~s" headmatch)
  
    (cond ((null headmatch) nil)	; if required head doesn't match, return nil immediately
	  ((null el) nil)  ;(|Blank|) has to match SOMEthing
	  (name  
	   (multiple-value-bind (val found)(sfind env name) ;; has the name a value?
	   
	     (cond (found ;; it has a value
		     (dformat t "~% mblank1 found binding on variable  ~s  = ~s" name val)
		    (cond ((equal val (car el)) ;; the value matches
			   (mlist (cdr pl)(cdr el) nil gh condition)))) ;; check the rest
		   ;; here, there is a name but it does not have a previous value
		   (t (spush env name (car el)) ;; since no value yet, push it.
		      (cond ((mlist (cdr pl)(cdr el) nil gh condition)
			     t)		; if everything else works, it's a match
			    (t (spop env) nil)))))) ;otherwise not
	  ;; if there is no name, and head matches, just follow through..
	  (t ;;(dformat t "~% plist=~s e=~s" plist e)
	   (mlist (cdr pl)(cdr el) name gh condition)))))

(defun mblank2list (pl el name gh condition)
  ;; we have pl=((|BlankSequence| <head, maybe>) p2 p3 ...)
  ;; and   (e1 e2 ....)
  (let ((prevbind nil)
	;;(newbinding nil)
	(tempbind nil)
	(p (car pl))
	(shorterel nil)
	(newbinding nil))
    
    (cond (name  
	   (multiple-value-bind (val found)(sfind env name) ;; has the name a value?
	     (cond (found  (setf prevbind val))  
	     ;; the named variable  has a value. later we will check to see 
	     ;; if it is the same as what we collect here.
	   
	     ;; there is no binding for name
	     ;; make one
		   (t (setf newbinding t) (spush env name nil )))))) ;
    ;; we are going to try all sequences of e1, e2,  until we find one that works.
    ;; to speed this up we can first remove ineligible items
    (setf shorterel (if (cdr p)
			(do ((i el(cdr i)) 
			     (res nil (push (car i) res)))
			    ((not (eq (|Head| i)(cadr p))) (nreverse res)))
		      el))
			
    (dosublist 
     (ee shorterel 
	 (progn (if newbinding (spop env)) nil)) ;; successively, bind ee to () (e1) (e1 e2) ....
     (dformat t "~% in mblank2list pl=~s ee=~s env=~s" pl ee (env2alist env))
     (cond 
      ((null ee) nil) ;; 2 blanks requires at least one item. next iteration

      ((and
	;; tentatively set the value
	(schange env name (setf tempbind(if (cdr ee)(cons '|Sequence| ee) (car ee))))
	    
	;; there are 2 requirements now.  We have already assured that     
	;; every element in ee has the same required head,
	;; if there is a required head in the pattern.
	
	;; the matched expression, either e1, or (Sequence e1 e2..)
	;; ??? or perhaps (gh e1 e2..)
	;; must be equal to the previous binding prevbind, if that is non-nil
	(or (null prevbind)(equal prevbind tempbind))
	;;finally, the rest of the pattern(s) must match the rest of the
	;; elements in the expression. (plus the inherited condition).
	(mlist (cdr pl) (nthcdr (length ee) el) nil gh condition))
       ;; if these are all satisfied, then
	 (return-from mblank2list t))
      ;; We get here if we didn't satisfy that so we
      (t nil))				; keep iterating
     )))

(defun mblank3list (pl el name gh condition)
  ;; we have pl=((|BlankSequence| <head, maybe>) p2 p3 ...)
  ;; and   (e1 e2 ....)
  (let ((prevbind nil)
	(p (car pl))
	(tempbind nil)
	(shorterel nil))
    
    (cond (name  
	   (multiple-value-bind (val found)(sfind env name) ;; has the name a value?
	     (cond (found  (setf prevbind val)  )
		   ;; the named variable  has a value. later we will check to see 
		   ;; if it is the same as what we collect here.
		   ;; there is no binding for name)
		   ;; make one
		   (t (setf tempbind t) (spush env name nil )))))) ;
    ;; we are going to try all sequences of e1, e2,  until we find one that works.
    ;; to speed this up we can first remove ineligible items
	(setf shorterel (if (cdr p)
			(do ((i el(cdr i)) 
			     (res nil (cons (car i) res)))
			    ((or (null i)(not (equal (|Head| (car i))(cadr p))))
			     (nreverse res)) )el))

            (dformat t "~% shorterel=~s el=~s" shorterel el)
			
    (dosublist 
     (ee shorterel 
	 (let()(dformat t "~%prevbind=~s env=~s" prevbind (env2alist env))
	     (cond ((null prevbind) nil)
	       (t (spop env) nil)))
	 ) ;; successively, bind ee to () (e1) (e1 e2) ....
     (dformat t "~% in mblank3list ee= ~s env=~s" ee (env2alist env))
    	;; tentatively set the value
	(schange env name 
		 (setf tempbind 
		   (cond ((null ee)'(|Sequence|))
			 ((null (cdr ee))  (car ee) ) ;;(cons '|Sequence|(car ee)))
			 (t  (cons '|Sequence| ee)) )))
	
	 (dformat t "~% tempbind=~s prevbind=~s " tempbind prevbind)
	(cond 
      ;; ((null ee) nil) ;; 3 blanks requires 0 or more items  
	;; there are 2 requirements now.  We have already assured that     
	;; every element in ee has the same required head,
	;; if there is a required head in the pattern.
	;; the matched expression, either e1, or (Sequence e1 e2..)
	;; ??? or perhaps (gh e1 e2..)
	;; must be equal to the previous binding prevbind, if that is non-nil
	((and(or (null prevbind)(equal prevbind tempbind))
	;;finally, the rest of the pattern(s) must match the rest of the
	;; elements in the expression. (plus the inherited condition).
	(mlist (cdr pl) (nthcdr (length ee) el) nil gh condition))
       ;; if these are all satisfied, then
	 (return-from mblank3list t))
      ;; We get here if we didn't satisfy that so we
      (t nil)))))		; keep iterating

	

;; another version, where we look for the simple bindings first.
;; not up-to-date


;;uu[vv_[w_^2,w_],w_]:=yes[vv,w]
;; uu[kk[r^2,r],r]   result yes[kk,r]. 


;; this one handles orderless and flat and blanks..
;; try to match (car pl) against 0 or more elements of el.
;; start with element i of expressionlist el, length of el is h.
	       
;; match a pattern and an expression
;; Changed from version 1 to allow for pattern vars to appear repeated
;; Changed from version 2 and 3 to deal with a:f[_]  etc.
     
;; if x is atomic, if typ is not a type, or x is not of type typ, return nil.
;; in case x is a cons, see if its head is eq to typ.

;; extra cases to consider: 

;;  x_.
;;    = (|Optional| (|Pattern| x (|Blank|))) 

;; Also, Orderless + Flat.  Any other |Attributes|?  How about |Default|s?,
;; repeated, what else?

;; also f[x_]:=g[x]  /; x>0

;;; some programs to help for combinatorial matching.
;;; mapcomb applies the function f to each combination of
;;; elements in l. Non-destructive.  Try (mapcomb 'print '(1 2 3))

(defun mapcomb(f l)
  (labels((mc (l c)
	      (cond ((null l) (funcall f c))
		    (t (mc (cdr l) c)
		       (mc (cdr l) (cons (car l) c))))))
    (mc  (nreverse l)   nil)))

;;; (mapcombl #'(lambda(r)(every #'oddp r)) '(1 2 3))
;;; computes a list of all subsets of (1 2 3 that are all odd.
;;; returns ((1 3)(3)(1)())

(defun mapcombl(f l &aux ans)
  (labels((mc (l c)
	      (cond ((null l) (if(funcall f c) (push c ans)))
		    (t (mc (cdr l) c)
		       (mc (cdr l) (cons (car l) c))))))
    (mc (nreverse l)  nil))
  ans)

;; map over all combinations of l, and return the first combination c
;; for which (f c) is true.
;; try  (mapcomb-if #'(lambda(r)(> (length r) 3)) '(1 2 3 4 5 ))

(defun mapcomb-if(f l)
  (labels((mc (l c)
	      (cond ((null l)
		     (if (funcall f c) (return-from mapcomb-if c)))
		    (t (mc (cdr l) c)
		       (mc (cdr l) (cons (car l) c))))))
    (mc  (reverse l)  nil)))



(defun set-minus( r s) 
  ;; tail of r that remains after its prefix s is removed.
  (nthcdr (length s) r))

;; debug tool. wipe stack clear.
(defun clrs()(setf env (make-stack)))

;; A slow pattern matcher for +, 
;; perhaps more general than mma? or just the same?
;; p is a pattern with head |Plus|.  That is, for x_+y_,  
;; p would be (|Plus| (|Pattern| x (|Blank|))(|Pattern| y (|Blank|)))

;; e is either an expression with head |Plus|, or not.
;; That is, it might be something like (|Plus| a b c), or
;; perhaps something else like (Sin q) or (|Times| a b).
#| 
(trial '(|Plus| (|Pattern| x (|Blank|)) a) 'a) ;; x is 0 
(trial '(|Times| 0 (|Blank|)) '(Times 0 x))
(trial '(|Times| 0(|Pattern| z (|Blank|))) '(Times 0 x))


|#

;;FOL
(defun matchfol (p e op ident name gh c)
  ;;pattern, expression, operation (|Plus| or |Times|), identity (0 or 1),
  ;; expression may be e=nil, empty
  ;; governing head, and condition c. 
  ;; I suspect gh is always op.
  ;; comments are pertinent to |Plus|, since we wrote that first.
  (let ((pl (cdr p)) ;; list of pattern inside |Plus|
	(el nil)
	(pk nil))
    
    (cond
         ;; nothing left to match?, we hope expression e is zero
     ((null pl) (return-from matchfol (and (or (null e)(equal ident e)); expression empty
				   (funcall c))))
     ;; only one pattern item term remaining to be matched?
     ((null (cdr pl))
      (dformat t "~% singleton ~s to match against ~s" (car pl) e)
      (dformat t" ~% args to m2 are ~s"(list  (car pl) 
					     ;;(if (and (consp e)(eq (car e) op)(null(cddr e))) (cadr e) e)
					     (cadr e) name op c))
      ;;(format t "~%AHA e = ~s op=~s"e op )
      
       (return-from matchfol  ;;
	 (m2 (car pl)
	     (if (and (consp e)(or (null op)(eq (car e) op)) (null(cddr e)) ) (cadr e) e)
	    ;  (cadr e) ;; ?????? or (cdr, sometimes.)
	     name op c)))    
     ((null e) ;; empty expression...
      (dformat t "~%expression is empty, must match pl=~s to nothing or fail" pl)
      (setf pk (car pl))		;
      ;; pk is either (|Pattern| ..) or (|Blank|* ..) or (|Optional| ...) or .. none of those...
      ;; must match pk against nil or perhaps ident, and also match
      ;; (cdr pl) elements EACH against nil or ident.
      ;;		;;(p e op ident name gh c)
      (cond 
       ((atom pk) nil)
	(t   
	 (m2 pk 
	     (if (or (eq (car pk) '|Blank|)  ;; insert nil or 0 [ident] for expression for matching
			 (and (eq (car pk) '|Pattern|)
			      (eq (caaddr pk) '|Blank|))) 
		 ident 
	       nil)
	     name op
	     #'(lambda()
		 (eformat t "~%x28 p=~s e=~s c=~s" pl el c)
		 (matchfol (cons op (cdr pl))
			    el    ; every else matches to empty expression
			   op ident name gh c))))))
     ;; whew
      
     ;; more than one pattern term remaing to be matched. p1+p2+..

     ;; case one:  e is not a |Plus|.
     ((not(eq (|Head| e) op))
    ;  (print 'qqq)
      ;; must match one of p1, p2, say pk ... against e, 
      ;; and the others of p1, ... against 0;  say  pj_.  which would be pj_.0 given
      ;; identity for |Plus| is 0.
      ;; pj__ or pj___ are bad form, though mma doesn't forbid them, they don't seem to
      ;; really work, viz:    x /. x+c_.-> f[c] is ok. x /. x+c__->f[c]  returns x.
      ;; yet x+y/. x+c__->f[c] returns f[]
      (dolist (pk pl nil) ;; go through items in pl. None fit, return nil.
	(dformat t "~%matching pattern ~s in expr ~s" pk e)
	(if (m2 pk e nil op #'(lambda() 
				(eformat t "~%x29 pl=~s el=~s c=~s" pl el c)
				(matchfol (remove pk p)
						  nil
						  ;; e
						  ;;(remove pk e) ;;
						  op ident name gh c)))
	    (return t)))) ;return out of dolist
     

     ((not (member '|Orderless|(|Attributes| op))) ;; yeah, not going to happen with |Plus|, |Times|
      (mlist pl(cdr e) nil gh c))  ;; this works except not for Orderless
     
     ;; case two:  e IS a |Plus|.
     (t
      (setf el (cdr e));  remove the |Plus|
      (dformat t "~%p is a ~s of ~s and e is a(n)  ~s of  ~s" op pl op el)
      ;; go through items in pl, recursively
      (setf pk (car pl))
	;; if we cannot match pk, the first of these, we obviously cannot
	;; match ALL of them, so return nil.  If we match pk, we
	;; still must match (cdr pl).
      (dformat t "~%matching pattern ~s to (subset of) expr ~s " pk e)
      (cond ((atom pk)
	     (cond ((find pk el)
		    ;;(p e op ident name gh c)
		    (matchfol (remove pk p :count 1)
			      (let ((k(remove pk e :count 1) ))
				(if (null (cddr k)) (cadr k) k)) ;; (|Plus| b) -> b
			      op ident name gh c)) 
		   (t nil)))		; pattern element is atom and not found
	    ;;
	    
	    ;; pattern element is NOT an atom
	    ;; try to match it.
	    ;; this is inner loop over combinations of parts of expressions
	    (t  
	     (dformat t "~% matchfol: to match ~s we run through combinations from expression e=~s" pk e)
	     (docomb 
	      (e1 el nil) ;; go through all combinations. If nothing matches, return nil
	      (dformat t "~%matching pattern ~s to expr e1= ~s"
		      pk e1)
	      (dformat t  "~%matchfol e1=~s pk=~s env=~s" e1 pk (env2alist env))
	     ;; (dformat t "~%~s test"(and(not e1) (not(eq (car(third pk)) '|BlankNullSequence|))))
	      ;;(format t "~%pk=~s" pk)
	      (cond
	      ; ((and (null e1) nil) ; do next combination.
	       ((and(null e1) ;; e1 = nil and also |BlankSequence|, then skip this. 
		    (eq (car pk) '|Pattern|)
		    (consp (third pk))
		    (member (car(third pk)) '(|Blank| |BlankSequence|)))
		;;(print 'BBB)
		nil)			;skip empty if |BlankSequence|
	       ;; if the chosen pattern part matches
	       ;; against the selection of parts of the expression
	       (t
		 (let* ((pp (remove pk (cdr p) :count 1))
			(ee (cdr e)))

		   (dolist (i e1 ee)(setf ee (remove i ee :count 1)))
		     (dformat t "~%matching p=~s to e=~s" pk e1)
		     (if (m2 pk 
			     (if (null (cdr e1)) (car e1) (cons op e1)) ;; singleton plus is simpler
		    ;;name op ??
			     name 
			     nil ;;op 
		    #'(lambda()
			(eformat t "~%x30 pp=~s ee=~s c=~s" pp ee c)
			(matchfol	;; and the rest matches too,
			 (cons op pp)
			 (cons op ee) ;; need to simplify?? need ucons not cons?
			 op ident name gh c)))
			 (return t);out of loop if matches
		       nil )))))))))))	

			  
;; return a list of permutations of the list l. Not used at the moment.
;; could be used for orderless but not flat? doubt it.

(defun permlist (l)
  (cond ((null l) nil)
	((null (cdr l)) (list l))
	(t (let*((res nil)
		 (this (list(car l)))
		 (prev (permlist (cdr l))))
	     ;; insert this in each position in elements in prev
	     ;; and put on the list res
	     (do ((p prev (cdr p)))
		 ((null p) res)
		 (do* ((left nil (append left (list (car right))))
		       (right (car p) (cdr right)) )
		      ((null right) 
		       (setq res (cons (append left this right) res)))
		   (setq res (cons (append left this right) res))))))))

(defmacro doperm (iter &rest exp)
  (let ((ii (gensym)))
    
    `(let* ((,ii ,(cadr iter))
	    (,(car iter) ()))
       (block doperm
       
       ;; try (doperm (kk '(1 2 3)) (print kk))
       ;; or  (doperm (kk '(1 2 3)) (print kk) (if (equal kk '(3 1 2)) (return-from doperm 'gotcha))) ;ugh
       ;; or  (doperm (kk '(1 2 3) 'default-return) (print kk) )
  
       (cond ((null ,ii) (let((,(car iter) nil)) ,@exp ,(caddr iter)))
	     ((null (cdr ,ii)) (let((,(car iter) ,ii)) ,@exp ,(caddr iter)))

	     (t (let*((res nil)
		      (this (list(car ,ii)))
		      (prev (permlist (cdr ,ii))))
		  (do ((p prev (cdr p)))
		      ((null p) ,(caddr iter))
		    (do* ((left nil (append left (list (car right))))
			  (right (car p) (cdr right)) )
			((null right)  (let((,(car iter) (append left this right)) ,@exp)
					 ))
			 (let((,(car iter)  (append left this right))) ,@exp))))))
	     ))))
;; these are not part of the matcher, but they are often used in
;; the matching process.  For the moment, we'll keep them in the
;; same file.
		 
;;(defun |Integer|Q (x)(integerp x))
(defun |EvenQ| (x)(and (integerp x)(evenp x) '|True|))
(defun |OddQ| (x)(and (integerp x)(oddp x) '|True|))
(defun |NumberQ|(x)(and(numberp x) '|True|))



;;(defun |FreeQ| (l x) (not (member x l)))

(defun |FreeQ| (l x) 
 ;;(format t "~% env=~s  x=~s mevalx=~s" env x (meval x))
  (labels( ;;(freeqx (h)(FreeQ h x))
	   (dependsx (h)(null (|FreeQ| h x)))) ;;returns t or nil
    (cond ((|MatchQ| l x) nil)
	  ((consp l)(if (some #'dependsx (cdr l))
			nil '|True|))
	  (t '|True|))))


(defun |ZeroQ|(r) (Equal 0 (meval r)))
	       


(defun |AtomQ|(x)(if(atom x) '|True|))

;;
#+ignore ;;for now..
(defun |EqualQ|(x y)(cond 
		  ((member x '(|Indeterminate| |Infinity|) :test #'eq)
		   `(|Inequality| ,x |Equal| ,y))
		  ((equalp x y)  '|True|) ;; handles numbers too, if equal
		  ((and(numberp x)(numberp y)) '|False|)		  ;; for now, we dump anything we can't prove into Inequality
		  (t `(|Inequality| ,x |Equal| ,y))))

(defun |Comparison|(&rest h)
  (cond ((null (cdr h)) '|True|) ;; only zero or one arg
	(t
	 (cond ((null (cdddr h))	; no 4th op.  Comparison[a,Op,b]

		(let ((x (car h))
		      (op (cadr h))
		      (y (caddr h)))
		 ;; (format t "~% comparison op=~s x=~s y=~s" op x y)
		 (cond ((and (numberp x)(numberp y))
			(if
			    (apply (case op 
				     ((|SameQ| |Equal|) #'=)
				     
				     (|Greater| #'>)
				     (|GreaterEqual| #'>=)
				     (|Less| #'<)
				     (|LessEqual| #'<=)
				     ((|Unequal| |UnSameQ|) #'/=))
				   (list x y)) '|True| )) ;otherwise nil.
		       ;; not both numbers, but maybe we can figure out something?
		       ((member  op '(|Equal| |SameQ|) )
			(if (equal x y) '|True| nil))
		       ((member  op '(|Unequal| |UnSameQ|) )
			;(print 'hi)
			(if (equal x y) nil '|True|))
		       (t (ucons '|Comparison| h))))) ; not numbers, not special cases
	       ;; too many args
	       (t(ucons '|Comparison| h) )))))

;; need LessEqual  etc.

(defun meval-to-bool(fun)(if (funcall fun) t nil )) ; non-nil --> t

;; this next program is for debugging.  Actual
;; access to the variables on the env should be done
;; with access programs, not this.  ;; 12/18/10 RJF

(defun env2alist( a )			; a is an environment
  ;; return (all) the bindings in an alist.
  ;; all the frames are dumped into one alist.
   (let ((fp (1- (stack-frameptr a)))
	 (sp (stack-ptr a))
	 (ans nil))
     (do((i (1- sp) (1- i)))
     ((< i 0) (nreverse ans)) ;; most recent binding at front of list.

     (cond((eql i (1+ fp))
	   (setq fp (1- (aref (stack-vals a) i))))
	  ;;((null (aref (stack-vals a) i)) nil) ;; keep bindings of variable to nil out. hack
	  (t    
	   (push  (cons (aref (stack-vars a) i)
	     (aref (stack-vals a) i)) ans))
	  ))  ))
#|

(* let us say we want to define a rule that changes all numbers n in an expression
that are greater than 3, into the expression "aha[n]".

 How can we construct such a rule?
Here are some trials, most demonstrated in one line. We give some of the answers.*)

Clear[Great3,gg1,gg2,gg3,gg4,w]
Great3[x_]:= x>3
w=12
10 /. w_?Great3 -> aha[w]        (* aha[12] *)
10 /. w_?#>3& -> aha[w]
10 /. w_?(#>3)& -> aha[w]
10 /. w_?(#>3&) -> aha[w]        (* aha[12] *)
10 /. w_?(#>3&) :> aha[w]        (* aha[10] *)

2 /. w_?(w > 3 &) -> aha[w]    (* aha[12] *)
2 /. w_?(w > 3) & -> aha[w]
2 /. w_?(w > 3 &) :> aha[w]    (* aha[2] *)



 10 /. w_ ->  aha[w]  /; Great3[w]
 10 /. w_ :>  aha[w]  /; Great3[w]   (* aha[10] *)
 10 /. w_ -> (aha[w] /; Great3[w])
 10 /. w_ :> (aha[w] /; Great3[w])   (* aha[10] *)
(10 /. w_ -> aha[w]) /; Great3[w]
 10 /. (w_ -> aha[w] /; Great3[w])
 10 /. (w_ :> aha[w] /; Great3[w])  (*aha[10]*)
 10 /. (w_ :> aha[w] /; #>3&[w])    (*aha[10]*)

 2 /. w_?( Print[w]; Print[#]; (w > 3) &) :> aha[w]  (* aha[2] *)
10 /. w_?( Print[w]; Print[#]; (# > 3) &) :> aha[w] (*aha [10] * prints 12, #1 *)
{2, 3, 10, 12} /.  w_?((w > 3) &) :> aha[w]  
{2, 3, 10, 12} /.  w_?((# > 3) &) :> aha[w] (* ok *)

 10 /. w_?(Print[w];#>3&) -> aha[w] 
  2 /. w_?( Print[w]; Print[#]; (w > 3) &) -> aha[w]


gg1[w_]:=aha /;#>3&[w] 
gg2[w_]:=aha /; (#>3&)[w] 
gg3[w_]:=aha /;w>3
gg4[w_]:=aha/;Great3[w]
{gg1[10],gg2[10],gg3[10],gg4[10]}

		     
10 /. (w__ -> aha /; Great3[w])
 
10/.10->aha  (* just for fun *)

Different semantics.

a. RuleDelayed[w_,|Condition|[aha,Great3[w]]]
b. Rule       [w_,|Condition|[aha,Great3[w]]]

When defining a rule by version a., the condition is not evaluated until after w
is bound. this is probably what you want to do.

When defining by version b, Great3[w] is never evaluated at all ??
|#

(defun dispb(x)(disp(BuildFormat x)))




   #|
 (mma::tl)
 SetAttributes[g,Orderless]
 SetAttributes[h,Flat]
 SetAttributes[gh,{Flat,Orderless}]
 Exit
 |#




 

;; stubs. If we need these, I suspect we are in trouble.
;; also for mblanks..


;;; TRY THIS for debugging
;; (trial '(|Plus| (|Pattern| x1 (|BlankSequence|)) e) '(|Plus| d e))


(defun stackprinter (a stream pl) (declare (ignore pl))
       (format stream "~%~s" (env2alist a)))


;;| questions: is there no difference between
;;x___foo [3 blanks] and x:(Sequence[] |__) ;; yes.

;;try 2 rules
;;w[a, v, c] /. {w[] -> Sequence[], w[x__] -> x}



#|
(trial '(|Plus|   (|Times| (|Pattern| y (|Blank|)))    (|Pattern| x (|Blank|))) '(|Plus| a (|Times| b d))) ;; uh, not simplified arg
;;gives (success ((x . a) (y . 1))), should be (success ((x . a) (y . (|Times| b d)));; perhaps
(trial '(|Plus|   (|Pattern| y (|Blank|))    (|Pattern| x (|Blank|))) '(|Plus| a (|Times| b d))) '(success ((x |Plus| a (|Times| b d)) (y . 0)))
							

 (trial '(|Plus|  (|Pattern| y (|BlankSequence| |Integer|))
   (|Pattern| z (|Blank|)))
	'(|Plus|  3  x y ))

;; screwy case ..
 (trial '(w a (|Power| x (|Optional|(|Pattern| n (|Blank|))1)))
	'(w a (|Power| x )))  ;; illegal use of |Power|, sortof . |Power|[x] evals to x in Mathematica. fixed up.
'(success ((n . 1)))
#| 
to match this, we could do this.
try matching (w a (|Power| x (|Pattern| n (|Blank|)))) to (w a x).  That will fail because
(|Power| x (|Pattern| n (|Blank|))) cannot be found.
so we could go back and replace (|Optional|(|Pattern| n (|Blank|))1) by 1 in the expression (|Power| x ...) and
then simplify (|Power| x 1) to x.  The pattern is then w a x.

Mathematica doesn't back up entirely, e.g.

Cos[x] + (-x + x^(n_:1))*Sin[x] -> foo[n]

would work for Cos[x] with n=1 .
Maybe it works only for one level, e.g. f[ ..., |Optional|[...],...]
re-evals f[...].  Maybe a Rule of the form
f[...,|Optional|[|Pattern|[n,|Blank|[]],default],...]  should be changed to
|Alternatives|

f[...,|Pattern|[n,|Blank|[]],...],
simp[f[..., default, ...]]  also bind n to default...
.  gack. need to check for pattern with any optional/ defaults?

;;(y_?(# == Sin &) | y_?(# == Cos &))[x] -> Trig[y]  |
|#

(trial '(|Plus|   (|Times| b (|Pattern| y (|Blank|))(|Pattern| z (|Blank|)))    (|Times| a (|Pattern| x (|Blank|)))) '(|Plus| (|Times| a c) (|Times| b d gg)))
'(success ((x . c) (z . gg) (y . d)))

;; this is correct
(trial '(|Plus| a b (|Pattern| z (|BlankNullSequence|))) '(|Plus| a b)) ;should be (success ((z . (|Sequence|))))


For Rubi we need to process patterns like
Rubi[c_*(a_+b_.*x_),x_Symbol] :=  c*(a+b*x)^2/(2*b) /; FreeQ[{a,b,c},x]

or
MatchQ[u,(d_.+e_.*x)/(a_+b_.*x+c_.*x^2) / ; FreeQ[{a,b,c,d,e},x]] ||
MatchQ[u,(c_.*(a_.+b_.*x)^n_)^m_ /	; FreeQ[{a,b,c,m,n},x] && ZeroQ[m*n+1]]), ...

|#
;; pattern is ..(a_+b_*Sin(x))^n_ or
;; (a_:0+b_:1*Sin(x))^n_:1  for explicit |Optional| defaults.

(defparameter kp 
  '(|Power| (|Plus| (|Optional| (|Pattern| a (|Blank|)) 0) 
		(|Times| (|Optional| (|Pattern| b (|Blank|)) 1) (Sin x)))
    (|Optional| (|Pattern| n (|Blank|)) 1)))

;; pattern is .. 1/((a_.+b_.*x)(c._+d_.*x))

(defparameter kq 
  '(|Power| (|Times| 
	   (|Plus| (|Optional| (|Pattern| a (|Blank|)) 0) 
		 (|Times| (|Optional| (|Pattern| b (|Blank|))1) x))
	   (|Plus| (|Optional| (|Pattern| c (|Blank|)) 0) 
		 (|Times| (|Optional| (|Pattern| d (|Blank|))1) x)))
	  -1))



#|

;;; IntRule= INT[u_.*(a_.+b_.*x_^m_.)^p_.*(c_.+d_.*x_^n_.)^q_., x_|Symbol|] ->  (a+b*x^m)^p*(c+d*x^n)^q/x^(m*p)*Int[u*x^(m*p),x] /;FreeQ[{a,b,c,d,m,n,p,q},x] && ZeroQ[a+d] && ZeroQ[b+c] && ZeroQ[m+n] && ZeroQ[p+q]

|#



#|

|#
	 
	 ;;need to fix  display of a:f[b_]

	 

;;(trial  '(Power (Pattern x (Blank)) (Optional (Pattern n (Blank))))'(Power w z))

;;; There was a real problem 2/9/2011  f[Sin[x],y] /. f[a_,x_]-> a+x
;;; WRI  returns y+Sin[x], and so do we.
;;;  WRI renames pattern variables uniformly. e.g x==>x$
;;; or something like that.
;;; Here's how, using bindfix, 2/9/2011 RJF
;;; now newmatch returns  y+Sin[x]

;;; The whole point of generating new pattern vars is to make sure that the
;;; user of the system doesn't accidentally overload a name with 
;;; a pattern binding. e.g.  f[Sin[x],y]/. f[a_,x_]-> a+x  should result in Sin[x]+y
;; because it means f[Sin[x],y]/. f[apat_,xpat_]-> apat+xpat 
;; and there is no interference between xpat and x.
;; The scope and binding expectation with "infinite evaluation" makes a cleaner
;; solution difficult. 

(defun patvar(a)(or (get a 'patvar)
		    (let ((newname (intern (symbol-name a) :pat)))
		      (setf (get a 'patvar) newname)
		      newname)))

;;hash table  for rules with parameters appropriately hidden
(defparameter rulefixht (make-hash-table :test 'eq)) ;; should use EQ table if ucons used

(defun bindfix(rule)
  (let((r (gethash rule rulefixht))) ;; did we already fix this rule?
    (or r
	(setf (gethash rule rulefixht)  ;; make a fixed copy now.
	  (bindfixcompute (copy-tree rule))))))


(defun bindfixcompute(rule)		; destroys input
  ;; input is (Rule lhs rhs)
  (let ((varlist nil)
	(lhs (cadr rule)))
    (labels((bindfixer1
	     (lhs)
	     (map nil 
	       #'(lambda(r)
		   ;;(format t "~% test binding for ~s" r)
		      (cond ((atom r) nil)
			    ((consp r)
			     (cond((and (eq (car r)'|Pattern|)
					(not(member (cadr r) 
						    varlist :test 'eq)))
				   (push (cadr r) varlist) 
				   (setf (cadr r)(patvar (cadr r))))
				  (t 
				   (bindfixer1 r))))))

		  lhs)))

      (cond ((atom lhs)rule)
	    (t(bindfixer1 (if (eq (car lhs) '|Pattern|)
			      (list lhs)
			    lhs)) ;; collect all the pattern variable names on varlist

	   ;;   (format t "~% varlist =~s" varlist)
	      
	   
		  (sublis (loop for i in varlist 
			      collect (cons i (patvar i)))
			  rule))))))

;;prepare rule with optionals..

(defun fixopts(r)			; r is a rule. Maybe Rule or RuleDelayed.
  
  (cond ((null (isanyopt (cadr r))) r)	; nothing to do
	(t (setf (cadr r)(mapremopts (cadr r)))
	   r)))



(defun mapremopts (pat)
  (cond((atom pat) pat)
       (t
	(let ((h (ucons (car pat)(umapcar #'mapremopts (cdr pat)))))
	  (cond
	   ((hasoptionalp pat) ;; remove this Optional  and then look for more in results
	    (remopts h (car h)))
	   (t h))))))

(defun rulefix (lhs rhs  type)
  (let* ((visible (ulist type  lhs rhs))
	 (optim (gethash visible optimruleht)))
    (cond (optim optim)			; already have this rule
	  (t (let ((hidden (bindfix(fixopts visible))))
	       ;;(setdelayed1 hidden)
	       (setf (gethash visible optimruleht) optim) ;; store the optim rule here
	   ;;    (format t "~%hidden_rule=~s" hidden)
	       visible)))))

(defun |Rule| (a b)( rulefix a b '|Rule|))
(defun |RuleDelayed| (a b)( rulefix a b '|RuleDelayed|))




