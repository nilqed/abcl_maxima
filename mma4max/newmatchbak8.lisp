;;; -*- Mode:Common-Lisp; Package:mma; Base:10 -*-

;; not including some newer stuff, e.g. in version 6,0, specifically,
;; PatternSequence, Longest, Shortest, Repeated (? modified) 12/2010 RJF

;; note: I'm equivocating somewhat on the use of 'True and 'False or nil
;; as Boolean values in Mma. Problem: most lisp functions naturally
;; use nil for false, versus "anything else" but sometimes just 't
;; for true.  

;; Here's the decision: 
;;   Common   Our       WRI's             Meaning
;;    Lisp    mma       Mathematica (tm)
;; ----------------------------------
;; non-nil     ..True?   True            Boolean Truth
;;    nil     nil        False           Boolean False
;;    nil     Null       Null            Null value 
;;    nil    (List)     {} (i.e. List[]) Empty List

;; possible problem: setting t in lisp is a no-no. Can be averted in
;; the parser, I suppose, or we could allow the use of  atom  t as
;; a symbol in the :mma package, separate from  the
;; global name space.

;;(provide 'match)
;;(require "stack1")
(in-package :mma)

;;(declaim (optimize (speed 3)(safety 0)))
;;; definition: a patternobject is like an expression but it can
;;; have (Pattern ...) or PatternTest or PatternSequence or etc in it.
;;; Actually, any expression can be used as a pattern, but it would
;;; match only itself.  This is useful too.

;;; there is special attention given to a variety of situations.

;;; first of all, there are external conditions, sometimes.
;;; (x_->f[x])/; pred[x]
;;; is Condition[Rule[x_,f[x]] , pred[x]]
;;; which means that we must consider matching (and possibly failing/rematching)
;;; x subject to the condition pred[x].
;;; 

;; sic = subject to inherited conditions

;;; All that matches here
;;; (a) structurally identical matches. [sic]
;;; (b) (Blank) matches <anything> [sic]
;;;     (Blank foo) matches anything with a Head of foo [usually the car)
;;;     but also if foo is in {Integer, Rational, Complex, rat...?}
;;; matching rational or complex requires some special deconstruction of these.
;;; program is in ReplaceAll, but maybe should be somewhere else like Replace.

;;; (c) (Pattern x <patternobject>) matches whatever would
;;;   otherwise be matched by <patternobject> and also binds the value of
;;;   patternobject to the name x.  From that time onward during the match
;;;   x will only match identical items. [sic]
;;; (d) [must fix]
;;;    f[pat1, pat2, ...,patn] matches expression f[e1,e2, ..., en] {sic} if
;;; f matches f, and pat1 matches e1 subject to the conditions:
;;; {pat2, ...,patn} matches {e2, ...,en}  [sic]
;;; recursively. 
;;; {} matches {} [sic]
;;; This is only the simple case, since if p1 is a BlankSequence it can absorb
;;; one OR MORE of e1, e2 ...  If p1 is a BlankNullSequence it can absorb 0, one or more
;;; of e1, ...

;;; there is also a result of binding, that a variable
;;; can only match an expression consistently.
;;; f[x_,x_] will match f[3,3] but not f[3,4].
;;; f[x_,x_^2] will match f[a,a^2].  But not f[3,9]. (sorry).
;;; However, f[_,_]  matches f[3,4].

;;; check to see if the following is broken now.

;;; (d)  (BlankSequence ...) or __ inside (non-Orderless) functions. Note:
;;;  If f is "Flat" then f[a,x_] matches f[a,z,w]
;;;   with x-> f[z,w]. 
;;;  If f is "Flat" then f[b,x__] matches f[b,z,w]
;;;   with x-> Sequence[z,w] .  That's Sequence[z,w]. Look it up...

;;; (e) (BlankNullSequence ...) or ___ inside (non-Orderless) functions.

;;; (f) Orderless functions are matched only if they have a fixed
;;; number of arguments.

;;; (g) (PatternTest <patternobj> Test) matches whatever <patternobj> would
;;; match, but only if (Test <patternobj>) returns lisp t. 
;;;  (PatternTest x_ Test) is usually written x_?Test.
;;;Question:
;;; perhaps we should ask that (meval (list Test <patternobj>)) be True not t?
;;; we keep to t.

;;; g[x__,x__] matches g[1,2,3,1,2,3]  with x->(1,2,3)
;;; broken

;;; Some Flat functions are Plus, Times.  There are hardly any
;;; others.  They are also Orderless (see below) complicating
;;; the matching considerably.  

;;; Functions which are both Flat and Orderless functions are
;;; not handled by this version. yet.


;;; Orderless is handled...  (not, "If you can't eat it all, order less"  but
;;; "The universe tends toward a state without order -- orderless"..)

;;; if (say) Plus is orderless, then Plus[x_,Sin[x_]] matches
;;; Plus[3,Sin[3]] or Plus[Sin[3],3].

;;; Also allowed: x_Integer, x_foo,  or x:f[_] or x_?pred . 
;;;The form x_foo has the
;;; meaning that x's Head must be foo, or x must be of type foo.
;;;  x_foo parses into (Pattern x (Blank foo))
;;; x:f[_] parses into (Pattern x (f (Blank)))
;;; x_:pred parses into (PatternTest(Pattern x (Blank)) pred)

;; It is also possible to go into a pattern matching situation with a
;; condition specified externally, e.g. {note parens..}
;;  (x_-> f[x]) /;pred[x],  where this is parsed as
;; (Condition (Rule (Pattern x (Blank))(f x))
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
(defvar *matdebug* t)
(defmacro dformat(&rest x)`(if *matdebug* (format ,@ x)))
(defun deb()(setf *matdebug* (not *matdebug*)))

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


(defmacro docomb (iter &rest exp)
  (let ((ii (gensym)))
    
    `(let* ((,ii ,(cadr iter))
	    (,(car iter) ()))
       
       ;; try (docomb (kk '(1 2 3)) (print kk))
       ;; or  (docomb (kk '(1 2 3)) (print kk) (if (equal kk '(1 3)) (return 'gotcha)))
       ;; or  (docomb (kk '(1 2 3) 'default-return) (print kk) (if (equal kk '(1 7)) (return 'gotcha)))

       (block nil
	 (labels((mc (l c)
		     (cond ((null l) (let((,(car iter) c)) ,@exp))
			   (t (mc (cdr l) c)
			      (mc (cdr l) (cons (car l) c))))))
	   (mc 
	    (nreverse ,ii) 
	    nil))
	 ,(caddr iter)
	 )) ))




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
the Head that governs this (gh) so we can compute its attributes.

Hazardous.  Consider f[a,b,s] /.  f[x:(y:__),z___]->g[x,y,z]
need to bind x to whatever Sequence y matches.





|#

(defun trial(pat exp &optional (env (make-stack :size 20) wasitprovided))
  (spushframe env 'trialmatch);; make a frame to store results of match
  
 (let ((res (if  (m1 pat exp nil nil #'truth) ;; m1 is the matching program. env is global, result bindings.
     ;  (format t "~%The match succeeded. ~%Binding Stack is ~%~s" (env2alist env))
      (list 'success (env2alist env))
      (list 'failure)
      ;(format t "~%Match Failed~%")
      )))

  ;; reset ptr to former position if environment was provided
  ;; otherwise env will simply be removed via unbinding on return
       (if wasitprovided (spopframe env))
       res))

;; this next version shows how to return the answer by
;; adding to the conditions

(defun trial2(pat exp &optional (env (make-stack :size 20) wasitprovided))
  
  (spushframe env 'trialmatch) ;; make a frame to store results of match
  (let* ((ans nil)
	 (res  (m1 pat exp nil nil #'(lambda() 
				      (setf ans (env2alist env))
				      t))))

    ;; reset ptr to former position if environment was provided
    ;; otherwise env will simply be removed via unbinding on return
    (if wasitprovided (spopframe env))
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
;;(Rule  (Condition  (Pattern   w   (Blank))  (Comparison   w   Greater   3))
;;       aha)

;; w_?(#>3&)->hah is
'(Rule (PatternTest  (Pattern   w   (Blank)) (Function (Comparison   (slot 1)   Greater   3)))
       hah)


;;  ff[xx_] + rr_ :> gg[xx] + rr /; OddQ[xx]
'(RuleDelayed (Plus (ff (Pattern xx (Blank))) (Pattern rr (Blank)))
	      (Condition (Plus (gg xx) rr) (OddQ xx)))

;; this rule works to back up over the pattern match.
;; ff[1]+ff[2] /. %

;; 10 /. w_?((# > 3) & ) -> aha   returns aha
;; 10 /. w_?(# > 3 & ) -> aha     returns aha

;;; 12/2010
;;;  1/2011
(defun truth() 't)

;;; match is a top-level function which does not allow for  conditions on the overall match.
;;; Here is the Mathematica name
(defun |Match|(exp pat)(match pat exp))

;; I prefer the pattern to come first.
(defun match (pat exp)
  (spushframe env 'match)
  (m1 pat exp nil nil#'truth))

(defun matchall(pat exp &optional (condition #'truth))
  ;; return a list, the collection of all matches
  (let ((res nil))
    (m1 pat exp nil nil #'(lambda()(if (funcall condition)(push (env2alist env) res)) nil))
    res))

(defun matchtest(pat exp &optional (condition #'truth))
  ;; test for match, but do not leave bindings around
  (spushframe env 'match)
  (prog1  (m1 pat exp nil nil condition)
    (spopframe env))) ;;won't work for checking extra conditions. then use mlist.
  

(defun matchlist(pl el condition)  ;;alternative top level for simultaneous matches
  (spushframe env 'matchlist)
  ;;    pattern expression name condition
  (mlist pl el nil nil condition))



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
  
  
  ;; the Alternatives clause shows how backtracking can be done.
  ;; gh stands for governing head.
  ;; gh is the Head of the pattern that governs the attributes to make the pattern
  ;; e.g. in (f  (Pattern x ...) (Pattern y ...) z ...)
  ;; in the arguments of Pattern [x] and [y], as well as z, the governing head is f.
  ;; c is a function to be called, accumulated in the matching,
  ;; that must return t for the match to succeed. That is,
  ;; The success of m1 matching requires that (funcall c)
  ;; result in a non-nil value. As a side effect, the global variable env
  ;; will contain the bindings for pattern variables.
  ;; 
  ;;(print env)
  (let* ((hp (Head p))
	 (ha (and gh (cdr (Attributes gh)))) ;; if non-null gh, get attributes. could cache.
	 )
    (dformat t "~%ha=~s" ha) ;; attributes of head of p
    (cond 
     ;; first check for simple termination conditions in which
     ;; the pattern is exhausted and all that needs to be checked
     ;; is the functional condition that has been accumulated to this
     ;; point and is the 'c' function
     ((null p)(if (null e) (funcall c) nil)) ; check the conditions.
     ((atom p)(and (equal p e) (funcall c)))
     ((equal p e) (funcall c))

     ;; check for a Pattern.  This construction provides a NAME for
     ;; the result of the sub-expression matching.  That is, matching
     ;; (Pattern Q  <stuff>)  is exactly the same as matching <stuff>
     ;; but with the side consequence of making the binding of the name Q
     ;; to the expression matching <stuff>
     ((eq hp 'Pattern) 
      (dformat t "~% calling mpattern from m1 with  p=~s, e=~s gh=~s" p e gh)
      (mpattern  p 
		 e			;the expression
		 name
		 ;;nil
		 gh ;governing head, whose Attributes may be important
		 c)) ;condition, necessary for the rest of the pattern match

     ;; generally, the execution of this next clause signals a problem
     ;; in the composition of a pattern if gh has attribute Flat
     ((member hp '(Blank) :test 'eq)
      (dformat t "~% ~s match to expr ~s with name ~s" p e name)
      (mblanks p e name gh c) )
     ;; generally, the execution of this next clause signals a problem
     ;; in the composition of a pattern anytime.
     ;; the reason is, we need to have the context in the pattern -- what
     ;; else follows the X=blank[Null]sequence, to see how much X should
     ;; absorb.
      
     ((member hp '(BlankSequence BlankNullSequence :test 'eq))
      (dformat t "~% ~s match to expr ~s with name ~s" p e name)
      (mblanks p e name gh c))

     ;; in general, 'Plus/'Times is the operator, 0/1 is the identityk
     ;; e.g. we could implement this by setting Default[Plus]=0, Default[Times]=0, 
     ;; maybe Default[_]=Sequence[] ???
	
     ((eq hp 'Plus)(matchfol p e 'Plus 0 name gh c))
     ((eq hp 'Times) (matchfol p e 'Times 1 name gh c))

    ;; next, go through the cases
    ((eq hp 'Condition)
     (dformat t "~%processing Condition ~s "(caddr p))
     ;; transform the condition to be THIS condition AND whatever else.
     (m1 (cadr p)e name gh (let ((ccc (caddr p)))
			     #'(lambda()(and (meval ccc)(funcall c))))) )
    ((eq hp 'Alternatives) 
     ;; if we change Alternatives to be n-ary, 
     ;; this still works.
     (dolist (alt (cdr p) nil)		; range through alternatives.
       (dformat t "~% check alternative ~s" alt)
       (spushframe env 'Alternatives)
       (if (m1 alt e name gh c)
	   (return t)
	 (spopframe env) ; if this choice failed, try another til exhausted
	 )))
    ((eq hp 'Optional)
     (dformat t "~%Pattern ~s using Optional is probably misplaced" p)
     )
    ((member hp '(Repeated RepeatedNull Except))
     (error "Matching of Pattern objects with Head ~s not installed, used in ~s" hp p))
    ((eq hp 'PatternTest) ;;  a_?pred  for example
     (m1 (cadr p)e name gh
	 (let ((ccc (caddr p)))
	   #'(lambda()(and (meval (list ccc e))
			   (funcall c))))))
    ;; mlist takes care of the rest here..
    ;; if (Head p) is neither flat nor orderless, match in order.
    ;; Here, we match the heads and go through the rest via mlist.
    ;; simplified coding here
    ;; Note. (Head p) is NOT "Pattern" at this point in m1
     
    ;; now both p and e are expressions.
    ;; we match (recursively) their heads, and match,
    ;; in sequence, elements in their cdrs, by mlist.
    ;; in mlist we worry about attributes of phead, e.g. flat, orderless
    ((and (member 'Flat ha :test 'eq)
	  (member 'Orderless ha :test 'eq))
     (matchfol p e phead (Default phead) name phead c))

     ;;; THIS NEXT CLAUSE IS THE MAIN LOCUS OF ACTIVITY, usually
     
    ((and (not (member 'Flat ha :test 'eq))
	  (not (member 'Orderless ha :test 'eq)))
     ;; start matching in order.
     ;; (format t "~%in m1 should bind name ~s to ~s" name e)
     ;; #+ignore      (mpattern p e name gh c)
     (if name
	 (multiple-value-bind (val found)(sfind env name)
	   (cond (found (and (equal val e) ; if found and equal, don't change
			     (m1 hp 
				 (Head e)
				 nil
				 gh
				 #'(lambda()
				     (mlist (rest p)(rest e) nil  gh c)))))			      
		 (t  (spush env name e)
		     (if  (m1 hp 
			      (Head e)
			      nil
			      gh
			      #'(lambda()
				  (mlist (rest p)(rest e) nil  gh c)))
			 t
		       (let ()(spop env) nil)))))
       ;; if no name
       (m1 hp 
	   (Head e)
	   name
	   gh
	   #'(lambda()
	       ;; (mlist (rest p)(rest e) nil  gh c)
	       (mlist (rest p)(rest e) nil  hp c)
	       ) )
       ))
      
    ;;I think the comment below is not true..	  
    ;; if (Head p) is orderless, mlist will allow jumbling the args if necessary.
    ((not (member 'Flat ha :test 'eq)) ;; it is  Orderless though
					;  (print '**)
     (m1 hp 
	 (Head e)
	 name
	 gh
	 #'(lambda()(mlist (rest p)(rest e) hp  gh c)) ))
    ((atom e) nil) 
    ;; non-atom, non-blank p can't match atom?

    ((member 'Flat ha)
     ;; separate Flat and Orderless
     (if (member 'Orderless ha :test 'eq)
	 (dformat t "~% ran into unimplemented section of m1 with ~s" p)
       ;; merely Flat. If head is H,
       ;;this has the effect of sortof making
       ;;Blank into BlankSequence, except with head H.
	      
       (m1 hp (car e) name restp gh 
	   #'(lambda() (mlist (cdr p) (cdr e) hp gh c)))))
    (t					; anything left??
     (format t "~% ran off end of m1 with ~s" p)
     nil))))

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
  ;; note that if pat begins with BlankSequence or gh is Flat and/or Orderless
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
		 #'(lambda() (and (equal val (meval e))(funcall c)))))
	    (t ;; it doesn't already have a value
	     (spush env name e) 
	     (dformat t "~%tentatively assigning  ~s := ~s" name e)
	     ;; there is potentially a problem here.  If (third p) is a BlankSequence,
	     ;; it may match, but not against e.  [issue is for mlist, not m1 though]
	  
	     (if (m1   (third p) ; the subpattern X within (Pattern newname X)
		     e (second p);; the new name
		     gh 
		     #'(lambda()
			 (schange env name (sfind env (second p))) ;adjust this name too.
			 (funcall c)))
		 t ;; it matches, and all conditions are GO
	       ;; it does not match
	       (progn (spop env)
		      (dformat t "~%retracting last assignment to ~s"  name)
		      nil))	     
	     ))))) )


(defun mpatternlist (pl el name gh c)  ; pl is list of patterns, el is list of expressions
  ;; match the pattern p= (car pl) where p looks like (Pattern name ...)
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
  ;; note that if pat begins with BlankSequence or gh is Flat and/or Orderless
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
			  #'(lambda() (and (equal val (meval altname))(funcall c))))
		  t
		(spop env)		; get rid of altname
		)))
	    (t ;; it doesn't already have a value
	     (spush env name (car el)) 
	     (dformat t "~%tentatively assigning  ~s := ~s" name (car el))
	     ;;  If (third p) is a BlankSequence,
	     ;;  we have to iterate to try to suck up enough parts of el
	     ;; to make it match.
	     
	     (if (m1 (third p) ; the subpattern X within (Pattern newname X)
		     el 
		     (second p);; the new name
		     gh 
		     #'(lambda()
			 (schange env name (sfind env (second p))) ;adjust this name too.
			 (mlist (cdr pl) (cdr el) name gh c)))

		 t ;; it matches, and all conditions are GO
	       ;; it does not match
	       (progn (spop env)
		      (dformat t "~%retracting last assignment to ~s"  name)
		      nil))	     
	     ))))) ))

;; mblanks is called when m1 encounters a Blank* pattern.
;; it should not -- Blank* should be within mlist
;; but this may happen anyway, and should be made to work

;;MBLANKS
(defun mblanks   ;; we need this, it seems
  (p e name gh condition)
  (let ((phead (car p))(plist (cdr p)))
    (cond
     ((eq phead 'Blank)
      ;;(mblank1 plist e name gh condition)) ;do simple Blank matching
      (mblank1 plist e name gh condition)) ;do simple Blank matching
	      
     ;; __foo .. this next line is like (Pattern name (BlankSequence foo))
     ;; where name=nil means no name. foo is the "head" to match
     ;; this pattern matches one or more objects EACH of which has
     ;; head foo.
     ;; f[foo[3], foo[4]] /. f[z__foo] -> hah[z]  
     ;; binds z to Sequence[foo[3],foo[4]].
     ((eq phead 'BlankSequence)
      ;;    (print 'BBB)
         (mblanklist '((BlankSequence))(list e) name gh condition)
	 #+ignore
	 (mblank2 plist e name gh condition))
     ;; ___foo .. this next line is like (Pattern nil (BlankNullSequence foo))
     ;; where nil means no name. foo is the "head" to match
     ;; this pattern matches zero or more objects each of which has
     ;; head foo.
	      
     ((eq phead 'BlankNullSequence)
      #+ignore      (mblank3 plist e name gh condition)
      (mblank3list '((BlankNullSequence)) e ;(list e) 
		   name gh condition)
      )
	      
     ;; we get here in a case like  x:f[_] which parses to
     ;; (Pattern x (f (Blank)))
     ;; that is, we use the name x and bind it to f[something] if
     ;; we can match [something]

     (t (error "mblanks called on ~s " phead)))))


(defun patfree(p) 
   (cond ((atom p)t) ;; null or atomic has no Sequence
	(t nil)))

;; not used

#+ignore (defun mlistfol(patlist  explist  count max)
  (dformat t "~% mlistfol ~s  ~s ~s ~s" patlist explist count max)
  ;; there are count items in the expression list
  ;; for each item in the pattern list we can try matching it
  ;; against 0 of the expressions, 1 of the expressions,
  ;; up to count.
  ;; recursively we can reduce the number of patterns in patlist
  ;; and delete from the explist those items accounted for.
  
  ;; Do we need to know the Head? If c___ matches Sequence[], 
  ;; Plus[c] is 0, Times[c] is 1 ... 
  ;; notes 12/21/2010 RJF  

  )


(defun orderlessp(x)(member x '(ol Plus Times) :test #'eq))

(defun flatp (x)(member x '(flat Plus Times) :test #'eq))


;;*********************************

;; mblank1 matches a (Blank h) or (Blank). This works if the test

;; this checking plist requires that e.g. (Blank foo) matches (foo ...))

(defun mblank1			
    (plist e name gh condition)
  (cond (name  
	 (multiple-value-bind (val found)(sfind env name) ;; has the name a value?
	   
	   (cond ((and found   ;; it has a value
		       (progn (dformat "~% mblank1 found binding on variable  ~s  = ~s" name val)t)
		       (or (null plist)(eq (Head e) (car plist))) ;; the head matches
		       (equal val e)) ;; the value matches
		  
		  (return-from mblank1 (funcall condition)))
		 ;; here, there is a name but it does not have a previous value
		 ((or (null plist)(eq (Head e) (car plist))); head matches?
		  (spush env name e) ;; since no value yet, push it.
		  (dformat t "~% new mblank1 ~s=~s, condition= ~s" name e condition )
		  (cond ((funcall condition) t)(t (spop env) nil)))))) ;;if it fails, pop it.
	;; if there is no name, just match anything  with the right Head 
	(t  
	 (dformat t "~% plist=~s e=~s" plist e)
	 (and (or (null plist) (eq (Head e) (car plist)))
	      (funcall condition)))))


#+ignore
(defun mblank1 (p e name gh condition)
 (and (or(null (cdr p))			; no requirement on Head
	 (eq (Head e) (cadr p)))	; or Head matches
      (funcall condition)))


#+ignore
(defun mblank2 (p e name gh condition)
  ;; try to match 1, 2, ... (length e) pieces to p
  (let ((result nil))
    ;; hm, can we just do this recursively??
    ;;first try matching p to (BlankSequence *)
    ;; to e1, and test condition.
    ;; then to e1, and separately to e2, and test condition with p bound to 
    ;;  (gh e1 e2)
    ;; (schange env name (cons gh (cons (sfind env name))))
    
 (and (or(null (cdr p))			; no requirement on Head
	 (eq (Head e) (cadr p)))	; or Head matches
      (funcall condition))))


(defun mblankname(pat e name restp gh condition)
	   (multiple-value-bind (val found)(sfind env name) ;; has the name a value?
	   (cond
	    ((and found   ;; it has a value, continue to try match.
		   (m1 pat e name gh #'(lambda()
				    (spush env name e) 
				    
				(cond ((and (equal val (meval e))(funcall condition)) t)
				      (t (spop env) nil))))))
	    (t  
		(spush env name e) 
		(if (m1 pat e name gh condition) t (progn (spop env) nil))))))

		
	   

	   
;; start with a copy of mblank1. inadequate in general.

#+ignore
(defun mblank2(plist e name restp gh condition)
  (cond (name  
	 (multiple-value-bind (val found)(sfind env name) ;; has the name a value?
	   
	   (cond ((and found   ;; it has a value
		       (or (null plist)(eq (Head e) (car plist))) ;; the head matches
		       (equal val e)) ;; the value matches
		  (return-from mblank2 (funcall condition)))
		 ;; here, there is a name but it does not have a previous value
		 ((or (null plist)(eq (Head e) (car plist))); head matches?
		  (spush env name e) ;; since no value yet, push it.
		  (cond ((funcall condition) t)(t (spop env) nil)))))) ;;if it fails, pop it.
	;; if there is no name, just match anything  with the right Head 
	(t  (and (or (null plist) (eq (Head e) (car plist)))(funcall condition)))))


(defun blank1p(x)
  (and (consp x)
       (eq (car x) 'Blank)))

  (defun blank2p(x)
  (and (consp x)
       (eq (car x) 'BlankSequence)))

(defun blank3p(x)
  (and (consp x)
       (eq (car x) 'BlankNullSequence)))

;; check for case of Pattern[name,blank[]]   i.e. name_ or (Pattern x (Blank))

(defun patternnameblank(x)(and (patternp x)(blank1p (third x))))

(defun patternp(x)(and (consp x)(eq (car x) 'Pattern)))

;; mlist matches two lists, ordered. If the pl (pattern list)
;; contains a Blank (_), BlankSequence (_), or BlankNullSequence (___).
;; then special routines must be used.

;; If phead has attribute flat, (isflat is true), then Blank matching
;; operates the same as BlankSequence.  Why?  So
;; a+x_ matches a+b+c with x == b+c, instead of
;;                         x == Sequence[b,c]

;; k[r,s,t]/. k[r,x__]-> x  works, result is Sequence[s,t]
;; k[r,s,t]/. k[r,x__]-> g[x] bug. results in g[Sequence[s,t] 12/20/2010

;;(defun mlist-notfol-noxblanks(pl el name condition)  )


(defun simple-pat-or-constant (x)
  (or (patternnameblank x) ;; just a named pattern, e.g. x_
      (pattern-free x)))		; no pattern at all

(defun pattern-free (x)
  (cond ((atom x) (not (member x '(Pattern Blank Alternatives 
				   Condition Optional ;; do we need these?
				   ) :test 'eq)))
	(t (every #'pattern-free (cdr x)))))
  
;;MLIST
(defun mlist(pl el name gh condition)
  ;;   (mlist pl el name governing-head condition) matches (car pl) to some part of el,
  ;; and the remainder of pl gets matched to remainder of el.

  ;; The simple case is where (car pl) can match exactly one item (car el).
  ;; The complicated case is where (car pl) can match 0, 1, or more items from el.
  ;; This complicated case can occur when (car pl) looks like 
  ;; (BlankSequence head)  ;; one or more items x with Head[x]=head
  ;; (BlankNullSequence head) ;; zero or more items x with Head[x]=head
  ;; (Pattern x (BlankSequence head))  ;; one or more items x with Head[x]=head
  ;; (Pattern x (BlankNullSequence head))  ;; zero or more items x with Head[x]=head
  ;;  Other options? like Alternatives, Optional, Condition, PatternTest, ...  more messy stuff.
 (dformat t "~%mlist env=~s" (env2alist env))
  (cond ((null pl) 
	 (and(null el) 
	     (funcall condition)))
	#+ignore
	((atom (car pl)) ;; can't do this without checking orderless. see next clause
	 (m1 (car pl)(car el) ;; match the first elements
	     name
	     gh
	     #'(lambda()(and (mlist (cdr pl)(cdr el) nil  gh condition) ;then the rest
			     (funcall condition)))))

	;; sort of simple case, no sequences [BlankSequence, BlankNullSequence]
	;;#+ignore
	((patfree (car pl))
	 (if (not (member 'Orderless (Attributes gh)  :test 'eq))
	     ;; no sequences and in order. especially simple
	     (m1 (car pl)(car el) ;; match the first elements
		 name
		 gh
		 #'(lambda()(and (mlist (cdr pl)(cdr el) nil  gh condition) ;then the rest
				 (funcall condition))))  
	   ;; ELSE
	   ;; gh is ORDERLESS
	   ;; we have definitely got to match p1 to something.
	   ;; if gh is also FLAT, we could match sequences.  notprogrammed yet
	   (let ((p1 (car pl)))
	     ;; (format t "orderless ~s" p1)
	     (dolist (e1 el nil)
	       (if (m1 p1 e1 ;; match the chosen elements if possible
		       name gh
		       #'(lambda()(and (mlist 
					(remove p1 pl :count 1)
					(remove e1 el :count 1)
					name  gh condition) ;then the rest
				       (funcall condition))))
		   (return-from mlist t)
		 nil ))	    ; failed pattern match, go to next element
	     )))
	
	
	;; the next 3 clauses should not just call m1.

	((and (member 'Orderless (Attributes gh) :test 'eq)
	      (member 'Flat (Attributes gh) :test 'eq))
	 (format t "~%Oh dear, we have Orderless Flat matching of ~s to ~s" pl el)
	  ;; (matchfol pl el gh 0 nil gh condition)) ;;??
	(m1 (car pl)(car el) nil gh #'(lambda()(mlist (cdr pl)(cdr el) nil gh condition)))	)
	
	((member 'Orderless (Attributes gh) :test 'eq)
	 (dformat t "~%Oh dear, we have Orderless matching of ~s to ~s" pl el)
	 (m1 (car pl)(car el) nil gh #'(lambda()(mlist (cdr pl)(cdr el) nil gh condition))))
	
	#+ignore
	((member 'Flat (Attributes gh) :test 'eq)
	 (dformat t "~%Oh dear, we have Flat matching of ~s to ~s" pl el)
	 (m1 (car pl)(car el) nil gh #'(lambda()(mlist (cdr pl)(cdr el) nil gh condition))))
	;; first pattern in the list pl is not simple.
	;; say that pl looks like    (  (Pattern N (BlackNullSequence ...)) ...)
	;; we change pl to look like  (  (BlackNullSequence ...) ...)
	;; but with name N
	((eq (caar pl) 'Pattern)
	 ;; *** make tentative assignment for name and etc etc
	 (mpatternlist pl el name gh condition) ;; possibly correct
	 #+ignore
	 (mpatternlist pl el nil gh condition)
	 )

	((eq (caar pl) 'Blank)
	 (mblank1list pl el name gh condition))
	((eq (caar pl) 'BlankSequence)
	 (mblank2list pl el name gh condition))
	((eq (caar pl) 'BlankNullSequence)
	 (mblank3list pl el name gh condition))
	((eq (caar pl) 'Alternatives) 
	 (let ((p (car pl)))
	   (dolist (alt (cdr p) nil) ; range through alternatives. Exhausted? fail
	     (dformat t "~% check alternative ~s" alt)
	     (if (mlist (cons alt (cdr pl)) el nil gh condition)
		 (return t)
	       nil))))		  ; keep looping through alternatives.
	((eq (caar pl) 'Optional)
	 (mlist-option pl el name gh condition))
	(t (m1 (car pl)(car el) nil gh #'(lambda()(mlist (cdr pl)(cdr el) nil gh condition))) )))
	
	;;(t (format t "~% reached unimplemented MLIST section pl=~s el=~s alist=~s" pl el (env2alist env)) nil)


(defun mlist-option(pl el name gh condition)
	 
   ;; Apparently the valid forms are
   ;;(Optional (Pattern name (Blank)) value)
   ;;(Optional (Pattern name (Blank)) value)
   ;;(Optional (Blank) value)
   ;; where value may be omitted if there is a Default for this governing head and
   ;; argument position. Ugh, we haven't kept count of argument positions.
   ;; Anyway, consider
   ;; h[a, c] /. h[a_, x_: nox, c_] -> g[a, x, c]  returns g[a,nox,c]
   ;;h[a, b, c] /. h[a_, x_: nox, c_] -> g[a, x, c]  returns g[a,bc]

   ;; there are 2 ways to match an Optional, when
   ;; pl = (   (Optional (Pattern name (Blank <stuff>))default)  rest-of-pl )
   ;; One way is to to   remove Optional wrapper and thus match
   ;; ( (Pattern name (Blank <stuff>))  rest-of-pl ).
   ;; The second way is to bind name to default and  match <stuff> to el
	 
   (if (mlist (cons (second (car pl))(cdr pl)) el name gh condition)
       t	       ; first option worked, leaving pattern in there
     (let*((thepat (cadar pl))
	   (thename (cadr thepat))
	   (thestuff (caddr thepat))
	   (thedefault (caddar pl)))
       (format t "~% mlist-option env=~s, thepat=~s thestuff=~s, thedefault ~s" (env2alist env) thepat thestuff thedefault)
       (if (not thedefault) (format t "~% Optional needs default. Please set in ~s" thepat))
       (multiple-value-bind (val found)(sfind env name) ;; has the name a value?
	 (cond (found 
		;;  (print '*AA*)
		(and(equal (meval thedefault)(meval val)) ; meval to be safe?
		    (mlist (cdr pl) el nil gh condition)))
	       ;; no previous binding for this name
	       (t (cond ((mlist (cons thepat (cdr pl)) el thename gh condition)t)
			(t
			 (spush env thename thedefault)
			 (cond ((mlist (cdr pl) el nil gh condition)
				;;(mlist (cdr pl) el name gh condition)
				t)	;nil or name?
			       (t (spop env) ;unset the default
				  nil))))))))))


;;; f[a,b,d] /. f[x__,d]-> x   returns Sequence[a,b]  ;2blanks
;;; f[a,b,d] /. f[x___,d]-> x   returns Sequence[a,b] ;3blanks
;;; f[a,b,d] /. f[x_,d]-> x   returns f[a,b,d]  no match   ;1blank
;;; SetAttributes[ff,Flat]
;;; ff[a,b,d] /. ff[x_,d]-> x   returns f[a,b]            ;1blank and FLAT;;;


(defun mblank1list (pl el name gh condition)
  ;; we have pl=((Blank <head, maybe>) p2 p3 ...)
  ;; and   (e1 e2 ....)
  (dformat t "~%in mblank1list, env= ~s" (env2alist env))
  (let* ((p (car pl))
	 (headmatch (or (null(cdr p))(equal (Head(car el)) (cadr p)))))
;    (dformat t "~%headmatch =~s" headmatch)
  
    (cond ((null headmatch) nil) ; if required head doesn't match, return nil immediately
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
  ;; we have pl=((BlankSequence <head, maybe>) p2 p3 ...)
  ;; and   (e1 e2 ....)
  (let ((prevbind nil)
	(newbinding nil)
	(tempbind nil)
	(testevery nil)
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
			    ((not (eq (Head i)(cadr p))) (nreverse res)))
		      el))
			
    (dosublist 
     (ee shorterel 
	 (progn (if newbinding (spop env)) nil)) ;; successively, bind ee to () (e1) (e1 e2) ....
     (dformat t "~% in mblank2list pl=~s ee=~s env=~s" pl ee (env2alist env))
     (cond 
      ((null ee) nil) ;; 2 blanks requires at least one item. next iteration

      ((and
	;; tentatively set the value
	(schange env name (setf tempbind(if (cdr ee)(cons 'Sequence ee) (car ee))))
	    
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
  ;; we have pl=((BlankSequence <head, maybe>) p2 p3 ...)
  ;; and   (e1 e2 ....)
  (let ((prevbind nil)
	(newbinding nil)
	(tempbind nil)
	(testevery nil)
	(p (car pl))
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
			    ((or (null i)(not (equal (Head (car i))(cadr p))))
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
		   (cond ((null ee)'(Sequence))
			 ((null (cdr ee))  (car ee) ) ;;(cons 'Sequence(car ee)))
			 (t  (cons 'Sequence ee)) )))
	
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
#+ignore
(defun mlistfast(pl el condition)
  (cond((= (length pl)(length el)) ;;same length
	(map nil #'(lambda(p e)(if (simple-pat-or-constant p)
				   (if (null (m1 p e condition))
					     (return-from mlistfast nil))))
	     pl el)	;match simple ones first
	(every #'(lambda(p e)(if (not (patternnameblank p)) ;match the rest.
			       (m1 p e condition) t))
	       pl el)
	(meval-to-bool condition))))

#+ignore
(defun mlistfast(pl el condition &aux pp ee)
  (cond((= (length pl)(length el)) ;;same length
	(map nil #'(lambda(p e)(cond ((patternnameblank p)	;match simple ones first
				      (if (null (m1 p e condition))
					  (return-from mlistfast nil)))
				     (t (push p pp) ;;save the others.
					(push e ee))))
	     pl el)
	(dformat t "~%pp=~s, ee=~s~% env=~%~s" pp ee env)
	(if (every #'(lambda(p e)(m1 p e condition)) 
		   (nreverse pp)
		   ;;(nreverse (mapcar #'meval pp)) ;no good.
		   (nreverse ee))
	    (meval-to-bool condition)
	  nil))))

;;uu[vv_[w_^2,w_],w_]:=yes[vv,w]
;; uu[kk[r^2,r],r]   result yes[kk,r]. 


;; match patobj h against minmatch or more initial elements of el. Succeed only
;; if matching all of pl against the rest of el succeeds.
;; if name is non-nil, the matching of h will result in the
;; binding of name to the value matched.
;; As usual, el is an expression list, and pl is a list of pattern objs.
;; This is called to match BlankSequence with minmatch=1, and
;; to match BlankNullSequence with minmatch=0

#+ignore
(defun ml2(h pl el name minmatch condition &aux (ptr (stack-ptr env)))
  (cond ((null el)
	 ;; If there are no expressions left in the list then
	 ;; if it is acceptable for h to match "no" elements
	 ;; and if this is consistent with any previous assignments
	 ;; to "name", and the remaining patterns in the pattern list can
	 ;; also be matched to "no" elements, we will succeed here.
	 (let ((r (list phead)) )
	   (cond
	   ((and (= minmatch 0) (if name (mpat name r r) t))
	    (cond((mlist pl nil nil t) t)
		 (t (setf (stack-ptr env) ptr) nil)))
	   (t (setf (stack-ptr env) ptr)nil)) ))

;;remove the ;; below if you want to use the special end case.   
;;	((null pl)(ml2quick h pl el name))

	(t (let ((lel (length el)) )
	     ;; Starting with the minimum number (0, 1) of elements
	     ;; to match, try to match  h
	     (do ((k minmatch (1+ k))
		  (collect nil nil))
		 ;; termination with failure if we exhaust all
		 ;; the elements up to the length of el, and
		 ;; still haven't gotten a match. Reset the
		 ;; stack pointer if we've changed it, and
		 ;; return nil
		 ((> k lel) (setf (stack-ptr env) ptr) nil)
		 
		 ;; try to match h against 1st, then 1 thru 2, then 1 thru lel
		 ;; of the elements in el. This program can't skip any elements
		 ;; since it is not for "orderless"  heads.
		 ;;		 (dformat t "~%k=~s" k) ;debug

		 (do ((j el (cdr j)) 
		      ;; j is the list of expressions in el
		      ;; starting with el itself, and stepping down..
		      (count 1 (1+ count))
		      ;; the count mirrors j's length..
		      )
		     
		     ;;termination check: when we've looked at the first k els
		     ;; and haven't jumped out, we let the rest of the pattern
		     ;; have a chance.

		     ((> count k)
		      ;; note that phead will be a function
		      ;; head f if f is flat, else Sequence.
		      ;; q? should use uniq/ucons here
		      (setq collect (cons phead (reverse collect)))
;;		      (dformat t "~%Collected so far .. ~s" collect)
		      ;; if we've been provided a non-nil name, then
		      ;; check it against previous bindings if any;
		      ;; push the value for future checks.
		      (and (if name (mpat name collect collect) t)
		      ;; match the rest, if you can.
		      (cond((mlist pl j nil condition) 
			    (return-from ml2 t))
			   (t
			    ;; else un-assert this binding and try k:=k+1
			    (setf (stack-ptr env) ptr)
			    nil))))
		 
		     
		     ;; the do-loop body
;;		     (dformat t "~% j = ~s" j)
		     (cond ((mblank2 h (car j))
			    (setq collect (cons (car j) collect))
;;			    (dformat t "~% consed onto collect: ~s" collect)
			    ;; reset stack pointer after collection
			    (setf (stack-ptr env) ptr))
			   ;; it should be possible to expedite failure here.
			   ;; If p fails to match e[j+n], then p's predecessor
			   ;; should be advanced n places. But the control
			   ;; flow is too messy to contemplate for now.
			   ;;. (e.g. f[x__,p_pred])
			   ;; But, anyway, the predicate failed.
			   (t (setf (stack-ptr env) ptr)
			      (return-from ml2 nil)))))))))
  
;; special case in above..
;; if you have the last pattern element, you can be sure that either
;; it matches the rest of the expressions, or the pattern fails.
  
#+ignore
(defun ml2quick (h pl el name condition)	
  ;; the BlankSequence is at the end: try to match against
  ;; all the rest of the elements
  (let ((collect nil)
	(ptr (stack-ptr env)))
    (do ((j el (cdr j)))
	       
	;;termination check: when we've exhausted expr. list
	;; and haven't jumped out, we've absorbed all of el.
	((null j)
	 ;; note that phead will be a function
	 ;; head f if f is flat, else Sequence.
	 (setq collect (cons phead (nreverse collect)))
	 ;; if we've been provided a non-nil name, then
	 ;; check it against previous bindings if any;
	 ;; push the value for future checks. Return.
	 (if name (mpat name collect collect) (meval-to-bool condition)))
	       
	;; the do-loop body
	(cond ((mblank2 h (car j))
	       (setq collect (cons (car j) collect))
	       ;; reset stack pointer after collection
	       (setf (stack-ptr env) ptr))
	      (t (return nil))))))


;; match two lists, orderless, not flat or allowing BlankSequence
;; start with element i of expressionlist el, length of el is h.
;; This program is not directly applicable to Mma since BlankSequence
;; is allowed anywhere..

#+ignore
(defun mlistol (pl el i h condition)

  (cond ((null pl) (null el));; success if we run out of p and e together
	((= i h) nil);;exhausted all possibilities. fail
	(t (let ((p (car pl))
		 (ptr (stack-ptr env)))
	     (do ((count 0 (1+ count))
		  (index i (mod (1+ index) h)))
		 ((> count h)
		  ;; we never matched  p=(car pl), starting at (elt el i).
		  ;; try matching p again, but starting at (elt el (1+ i))

		  (mlistol pl el (1+ i) h condition))
		 
		 (cond ((m1 p (elt el index) condition)
			;; if success, we leave a binding for (car p) on env,
			;; remove (elt el index) from el, 
			;; and try next pattern.
			;;debug	(dformat t "~%matched ~s to ~s " p (elt el index))
			(cond ((mlistol
				(cdr pl)
				(remove nil el
					:test #'true2 :start index
					:count 1)
				0
				(1- h)
				condition)
			       (return (meval-to-bool condition))))))
		 ;; failure to match p to (elt el i)
		 ;; or perhaps failure at another level below -->
		 ;; reset bindings, restore el, and keep trying
		 (setf (stack-ptr env) ptr))))))


;; this one handles orderless and flat and blanks..
;; try to match (car pl) against 0 or more elements of el.
;; start with element i of expressionlist el, length of el is h.

;;this program is not structurally correct...
#+ignore ALSO
(defun mlistfol (pl el i h) ;; name condition

  (cond ((null pl) (null el));; success if we run out of p and e together
	((= i h) nil);;exhausted all possibilities. Can't match (car pl). Fail.
	
	(t (let ((p (car pl))
		 (ptr (stack-ptr env)))
	     
	     (cond 
	      ((patternp (car pl))
	       ;; pick out the name and call for a match
	       ;;               patobj                       name
	       (mlistolf2 (cons (caddr p) (cdr pl)) el i h  (cadr p)))
	      ;; undefined function mlistolf2

	      ((and (blank1p (car pl)) (not isflat))
	       ;; match exactly one expression, somewhere
	       (do ((count 0 (1+ count))
		    (index i (mod (1+ index) h)))
		   ((> count h)
		    ;; we never matched  p=(car pl), starting at (elt el i).
		    ;; try matching p again, but starting at (elt el (1+ i))

		    (mlistolf pl el (1+ i) h))
		 
		   (cond ((m1 p (elt el index) condition)
			  ;; if success, we leave a binding for (car p) on env,
			  ;; remove (elt el index) from el, 
			  ;; and try next pattern.
			  ;;debug	(dformat t "~%matched ~s to ~s " p (elt el index))
			  (cond ((mlistolf
				  (cdr pl)
				  (remove nil el
					  :test #'true2 :start index
					  :count 1)
				  0
				  (1- h))
				 (return t)))))
		   ;; failure to match p to (elt el i)
		   ;; or perhaps failure at another level below -->
		   ;; reset bindings, restore el, and keep trying
		   (setf (stack-ptr env) ptr)))
	      
	      ((or (blank1p p);; and isflat
		   (blank2p p))
	       ;; ??
	     ;;  (let ((collect trialval)
	;;	     (ptr (stack-ptr env)))	       
	       (do ((count 0 (1+ count))
		    (index i (1+ index)))
		   ((> count h)
		    ;; we never matched  p=(car pl), starting at (elt el i).
		    ;; try matching p again, but starting at (elt el (1+ i))
		    
		    (mlistolf pl el (1+ i) h name))
		 
		 (cond ((m1 p (elt el index) condition)
		 ;; if success, we place the binding for (car p) 
		 ;; on the list collect.
		 ;; remove (elt el index) from el, 
		 ;; and try next pattern.
		 ;;debug	(dformat t "~%matched ~s to ~s " p (elt el index))
			(setq collect (cons (elt el index) collect))
			(setf (stack-ptr env) ptr)
			(setq trialval (cons phead (reverse collect)))
			(cond ((or (null name) (mpat name trialval trialval))
			       (cond ((mlistolf
				       (cdr pl)
				       (remove nil el
					       :test #'true2 :start index
					       :count 1)
				       0
				       (1- h) nil)
				      (return t)))))
			;; failure to match p to (elt el i)
			;; or perhaps failure at another level below -->
			;; reset bindings, restore el, and keep trying.  Two
		 ;; ways to keep trying, though.
		 ;; (a) retract p, or
		 ;; (b) extend p to more terms so that the lower level
		 ;; will match. We do (a) first, and then (b).
		 ;;; GOTTA PUT THIS IN HERE. HOW?
			(setf (stack-ptr env) ptr)))))
	  
	      (t
	   ;; regular case: match a constant pattern subexpression,
	       ;; more or less, against (some) constant expression
	       (error "left over clause in mlistfol p=~s  el=~s" p el)   
				 
	       ))))))
	     		 
(defun true2 (x y) t)		
	       
;; match a pattern and an expression
;; Changed from version 1 to allow for pattern vars to appear repeated
;; Changed from version 2 and 3 to deal with a:f[_]  etc.

				
     
;; if x is atomic, if typ is not a type, or x is not of type typ, return nil.
;; in case x is a cons, see if its head is eq to typ.

;; extra cases to consider: 

;;  x_.
;;    = (Optional (Pattern x (Blank))) 

;; Also, Orderless + Flat.  Any other Attributes?  How about Defaults?,
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
;; p is a pattern with head Plus.  That is, for x_+y_,  
;; p would be (Plus (Pattern x (Blank))(Pattern y (Blank)))

;; e is either an expression with head Plus, or not.
;; That is, it might be something like (Plus a b c), or
;; perhaps something else like (Sin q) or (Times a b).
#| 
(trial '(Plus (Pattern x (Blank)) a) 'a) ;; x is 0 ; not the case now?
(trial '(Plus (Pattern x (Blank)) a)'(Plus xx a))
|#

#+ignore
(defun matchplus(p e name gh c)
  (matchfol p e 'Plus 0 name gh c))

#+ignore
(defun matchtimes(p e name gh c)
  (matchfol p e 'Times 1 name gh c))

;;FOL
(defun matchfol (p e op ident name gh c)
  ;;pattern, expression, operation (Plus or Times), identity (0 or 1),
  ;; expression may be e=nil, empty
  ;; governing head, and condition c. 
  ;; I suspect gh is always op.
  ;; comments are pertinent to Plus, since we wrote that first.
  (let ((pl (cdr p)) ;; list of pattern inside Plus
	(el nil)
	(pk nil))
    
    (cond
         ;; nothing left to match?, we hope expression e is zero
     ((null pl) (return-from matchfol (and (or (null e)(equal ident e)); expression empty
				   (funcall c))))
     ;; only one pattern item term remaining to be matched?
     ((null (cdr pl))
      (dformat t "~% singleton ~s to match against ~s" (car pl) e)
      (dformat t" ~% args to m1 are ~s"(list  (car pl) (if (and (consp e)(eq (car e) 'Plus)(null(cddr e))) (cadr e) e) name op c))
      
       (return-from matchfol  ;;
	 (m1 (car pl) (if (and (consp e)(eq (car e) 'Plus)(null(cddr e)))
			  (cadr e) e)
	     name op c)))     #+ignore
     ((null e) ;; empty expression...
      (dformat t "~%expression is empty, must match pl=~s to nothing" pl)
      (setf pk (car pl))		;
      ;; pk is either (Pattern ..) or (Blank* ..) or (Optional ...) or .. none of those...
      ;; must match pk against nil or perhaps ident, and also match
      ;; (cdr pl) elements EACH against nil or ident.
      ;;		;;(p e op ident name gh c)
      (cond 
       ((atom pk) nil)
	(t    
	 (m1 pk 
	     (if (or (eq (car pk) 'Blank)  ;; insert nil or 0 [ident] for expression for matching
			 (and (eq (car pk) 'Pattern)
			      (eq (caaddr pk) 'Blank)))
		     ident nil)
	     name op
	     #'(lambda()
		 (matchfol (cons op (cdr pl))
			    el    ; every else matches to empty expression
			   op ident name gh c))))))
     ;; whew
      
     ;; more than one pattern term remaing to be matched. p1+p2+..

     ;; case one:  e is not a Plus.
     ((not(eq (Head e) op))
      ;; must match one of p1, p2, say pk ... against e, 
      ;; and the others of p1, ... against 0;  say  pj_.  which would be pj_.0 given
      ;; identity for Plus is 0.
      ;; pj__ or pj___ are bad form, though mma doesn't forbid them, they don't seem to
      ;; really work, viz:    x /. x+c_.-> f[c] is ok. x /. x+c__->f[c]  returns x.
      ;; yet x+y/. x+c__->f[c] returns f[]
      (dolist (pk pl nil) ;; go through items in pl. None fit, return nil.
	(dformat t "~%matching pattern ~s in expr ~s" pk e)
	(if (m1 pk e nil op #'(lambda() (matchfol (remove pk p)
						  nil
						  ;; e
						  ;;(remove pk e) ;;
						  op ident name gh c)))
	    (return t)))) ;return out of dolist
     

     #+ignore  
     (t (mlist pl(cdr e) nil gh c))
     
     ;; case two:  e IS a Plus.
     (t
      (setf el (cdr e));  remove the Plus
      (dformat t "~%p is a ~s of ~s and e is a(n)  ~s of  ~s" op pl op e)
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
				(if (null (cddr k)) (cadr k) k)) ;; (Plus b) -> b
			      op ident name gh c))
		   (t nil)))		; pattern element is atom and not found
	    ;;
	    
	    ;; pattern element is NOT an atom
	    ;; try to match it.
	    ;;
	    ;; this is inner loop over combinations of parts of expressions
	    (t  
	     (dformat t "~% run through combinations from expression e=~s" e)
	     (docomb 
	      (e1 el nil) ;; go through all combinations. If nothing matches, return nil
	      (dformat t "~%matching pattern ~s to expr e1= ~s"
		       pk e1)
	      (cond
	       ;((null e1) nil)		;skip empty ?? should check pk for Blank vs Blank* ??
		    ;; if the chosen pattern part matches
		    ((m1 pk 
			 (if (null (cdr e1)) (car e1) (cons 'Plus e1)) ;; singleton plus is simpler
			 op op  ;;name, gh
			 (let* ((pp (remove pk p :count 1))
				(ee e)
			       (eex (dolist (i e1 ee)(setf ee (remove e1 ee :count 1)))))
			   #'(lambda()(matchfol	;; and the rest matches too,
				       (cons op pp)
				       (cons op  eex) ;; need to simplify?? need ucons not cons?
				       op ident name gh  c))))
		     (dformat t "matched p=~s to e=~s" p e)
		     (return  t))))))))))	;out of loop

			  
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
		 
(defun IntegerQ (x)(integerp x))
(defun EvenQ (x)(and (integerp x)(evenp x)))
(defun OddQ (x)(and (integerp x)(oddp x)))
(defun NumberQ(x)(numberp x))

;; have not defined PrimeQ PolynomialQ VectorQ MatrixQ ValueQ OrderedQ UnSameQ

;; SameQ is NOT consistent with Mathematica's program, exactly,
;; since 2 numbers of different precision may be SameQ in mma.

(defun SameQ(x y)(if(equal x y) 'True))

;; this is really cute:  if x matches an element of l, return true
;; e.g. if pat = (m1 x l) (PatternTest (Pattern x (Blank)) IntegerQ) 
;; then (MemberQ '(a b 3 z) pat)  will return True
;; example  MemberQ[{2,3},_?OddQ]  returns True.
;; example  MemberQ[{2,4},_?OddQ]  returns nil.

(defun MemberQ(l x)(if (member x l :test #'match) 'True)) 

(defun FreeQ (l x) 
  (labels((freeqx (h)(FreeQ h x))
	  (dependsx (h)(null (FreeQ h x))))  ;;returns t or nil
	 (cond ((MatchQ l x) nil)
	       ((consp l)(if (some #'dependsx (cdr l))
			     nil 'True))
	       (t 'True))))
	       
(defun MatchQ(l x)(if (match x l) 'True nil))

(defun AtomQ(x)(if(atom x) 'True))

(defun Greater(x y)(cond ((and (numberp x)(numberp y))
			  (and (> x y) 'True))
			 (t `(Inequality ,x Greater ,y))))
(defun Less (x y)(cond ((and (numberp x)(numberp y))
			  (and (< x y) 'True))
			 (t `(Inequality ,x Less ,y))))
(defun |Equal|(x y)(cond 
		  ((member x '(Indeterminate Infinity) :test #'eq)
		   `(Inequality ,x Equal ,y))
		  ((equalp x y)  'True) ;; handles numbers too, if equal
		  ((and(numberp x)(numberp y)) nil)		  ;; for now, we dump anything we can't prove into Inequality
		  (t `(Inequality ,x Equal ,y))))

(defun Comparison(&rest h)
  ;;  (print h)
  (cond ((null h) 'True)
	(t
	 (cond ((null (cdddr h))	; no 4th op.  Comparison[a,Op,b]
		(let ((x (car h))
		      (op (cadr h))
		      (y (caddr h)))
		 (if (and (numberp x)(numberp y))
		     (apply op (list x y)))))
	       (t (cons 'Comparison h))))))

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

a. RuleDelayed[w_,Condition[aha,Great3[w]]]
b. Rule       [w_,Condition[aha,Great3[w]]]

When defining a rule by version a., the condition is not evaluated until after w
is bound. this is probably what you want to do.

When defining by version b, Great3[w] is never evaluated at all ??
|#
;; works.


;;(trial '(Pattern ww (Blank)) '(f x y))


#+ignore(trial '(f (Pattern a (Blank)) (Condition (Pattern b (Blank)) (Comparison b Greater a)))
       '(f 5 4))  ; correct, fails

#+ignore(trial '(f (Pattern a (Blank)) (Condition (Pattern b (Blank)) (Comparison b Greater a)))
       '(f  4 5)) ; correct, success.

(defun runtest(h)
  (let ((yescount 0)(nocount 0)(ans nil))
    (labels ((runtest1(h)
	       (cond  ((null h)(return-from runtest (list 'Done 'yes yescount 'no nocount)))
		      ((equal (setf ans (eval (car h)))(cadadr h)) (incf yescount))
		      (t (format t "~% ~s: problem with ~s ~%expected ~s, got ~s" yescount (car h)(cadr h) ans)
			 (incf nocount)))
	       (runtest1 (cddr h) )))
      (runtest1 h))))

(defun showtest(h)
  (let ((yescount 0)(nocount 0)(ans nil))
    (labels ((runtest1(h)
	       (cond  ((null h)(return-from showtest 'Done))
		      (t 
		       (disp(BuildFormat (cadar(cdar h)))))
			 )
	       (runtest1 (cddr h) )))
	     (runtest1 h))))

(defparameter
  tests 
  '(
    (trial '(f (Alternatives a b) b)        '(f a b)) '(success nil)
    (trial '(f (Alternatives a b) b)        '(f b b)) '(success nil)
    ;; 3rd arg is equal to the first or second arg
    (trial '(f (Pattern a (Blank)) (Pattern b (Blank)) 
	       (Alternatives (Pattern a (Blank))  (Pattern b (Blank))))
	   '(f x y y)) '(success ((b . y) (a . x)))
    (trial '(f (Pattern a (Blank)) (Pattern b (Blank)) 
	       (Alternatives (Pattern a (Blank))  (Pattern b (Blank))))
	   '(f x y x)) '(success ((b . y) (a . x)))
    (trial '(f 
	     (Alternatives (Pattern a (Blank))  (Pattern b (Blank)))
	     (Pattern a (Blank))
	     (Pattern b (Blank)))
	   '(f x x y)) '(success ((b . y) (a . x)))
    (trial '(f (Blank) (Blank)) '(f 1 2))  '(success nil)
    (trial '(f (Pattern ww (Blank)) y) '(f x y)) '(success ((ww . x)))
    (trial '(f (Blank Integer) (Blank)) '(f 1 2)) '(success nil)
    (trial '(f (Blank Symbol) (Blank)) '(f x 2))  '(success nil)
    (trial '(f (Pattern x (BlankSequence))) '(f a b c)) '(success ((x Sequence a b c)))
    (trial '(f (Pattern x (BlankNullSequence Symbol))) '(f x y)) '(success ((x Sequence x y)))
    (trial '(Pattern a  (Pattern b (Blank))) 'xxx) '(success ((b . xxx) (a . xxx)))
    (trial '(f (Pattern x (BlankNullSequence Symbol))) '(f x 2))  '(failure)
    (trial '(f (Pattern x (BlankNullSequence Integer) )2) '(f 1 2)) '(success ((x .  1)))
    (trial '(f (Pattern x (BlankNullSequence))) '(f 1 y)) '(success ((x . (Sequence 1 y))))

    ;;   (trial '(f (Pattern x (BlankSequence Symbol))) '(f x y)) ;matches with x= Sequence[x,y]

    (trial '(f (Pattern a (BlankSequence))) '(f 1 2)) '(success ((a Sequence 1 2)))

    (trial '(f (Pattern a (BlankSequence)) (Pattern b (BlankSequence))) '(f 1 2)) '(success ((b . 2) (a . 1)))

    (trial '(f (Pattern a (BlankNullSequence)) (Pattern b (BlankSequence))) '(f 1 2)) '(success ((b Sequence 1 2) (a Sequence)))

    (trial '(P (Pattern a (Blank)) (Pattern b (Blank Sin))) '(P x (Sin y))) '(success ((b Sin y) (a . x)))
    (trial '(Pattern a  (f (Pattern b (Blank)))) '(f c))    '(success ((b . c) (a f c)))
    ;;none of the examples above deal with Orderless.


    ;;   (tl) SetAttribute[g,Orderless]
    ;;   SetAttribute[h,Flat]

    ;; (trial '(Plus a b) '(Plus b a)) '(success nil)
  ;;   g is Orderless
    
    (trial '(g (Pattern a (Blank)) (Pattern b (Blank Sin))) '(g x (Sin y))) '(success ((b Sin y) (a . x)))
    
    ;; 
    (trial '(f (Pattern x (Blank))) '(f a b c)) '(failure) ;; if f is not flat
    (trial '(f (Pattern x (BlankSequence))) '(f a b c)) '(success ((x Sequence a b c)))
    (trial '(f (Pattern x (BlankNullSequence))) '(f a b c)) '(success ((x . (Sequence a b c))))
      
    (trial '(g (Pattern a (Blank)) (Pattern b (Blank Sin))) '(g  (Sin y) x)) '(failure) ; should fail
    (trial '(f 
	     (Alternatives (Pattern a (Blank))  (Pattern b (Blank)))
	     (Pattern a (Blank))
	     (Pattern b (Blank)))
	   '(f y x y)) 
    '(success ((a . x) (b . y)))
    (trial '(f 
	     (Alternatives (Pattern a (Blank))  (Pattern b (Blank)))
	     (Pattern a (Blank))
	     (Pattern b (Blank)))
	   '(f x x y))
    '(success ((b . y) (a . x)))

    ;; also, if h is Flat, then (Pattern a (Blank)) operates like BlankSequence
    (trial '(q (Pattern a (Blank)) (Pattern b (Blank))) '(q 1 2)) '(success ((b . 2) (a . 1)))
    (trial '(q (Pattern a (Blank)) (Pattern a (Blank))) '(q 1 2)) '(failure) ; should fail
    ;; sticky assignment below
    (trial '(Pattern x (f (Pattern y (Blank z)))) '(f(z 3)))'(success ((y z 3) (x f (z 3))))


    (trial '(f (Optional (Pattern x(Blank))0 ) a)'(f xx a)) '(success ((x . xx)))
    (trial '(h (Pattern a (BlankSequence))) '(h x (Sin y))) '(success ((a Sequence x (Sin y))))
    (trial '(P (Pattern a (BlankSequence))) '(P x (Sin y))) '(success ((a Sequence x (Sin y))))
    (trial '(P (Pattern a (BlankNullSequence))) '(P x y z)) '(success ((a Sequence x y z)))
    (trial '(f (Pattern a (BlankSequence)) (Pattern b (BlankSequence))) '(f 1 2 3 4))
    '(success ((b Sequence 2 3 4) (a . 1)))

    (trial '(f (Pattern a (BlankSequence)) (Pattern b (BlankSequence))) '(f 1 2)) '(success ((b .  2) (a . 1)))
    (trial '(q (Pattern a (BlankSequence)) (Pattern a (BlankSequence))) '(q 1 2))  '(failure)
    ;; g is Orderless
    (trial '(g a b)'(g a b)) '(success nil)
    (trial '(g (Pattern x (Blank)) a)  '(g  b a)) '(success ((x . b)))
    (trial '(h a b (Pattern x (Blank))) '(h a c b )) '(failure)
    
    ;
    
    (trial '(f a  (Pattern x (Blank))b ) '(f a c b )) '(success ((x . c)))
    (trial '(ff (Pattern x (BlankSequence)) b)  '(ff  b a)) '(failure)
    (trial '(ff (Pattern x (BlankSequence)) b)  '(ff  a b)) '(success ((x .  a))) 
    (trial '(ff (Pattern x (BlankSequence)) b)  '(ff  a c b)) '(success ((x Sequence a c)))
    (trial '(ff  a (Pattern x (BlankSequence)) b)  '(ff  a c b)) '(success ((x . c)))
    (trial '(ff  a (Pattern x (BlankSequence)) (Pattern x (BlankSequence)) b)  '(ff  a c d c d b)) '(success ((x Sequence c d)))
    (trial '(f (Pattern a (BlankNullSequence)) (Pattern b (BlankSequence))) '(f 1 2 3 4))
    '(success ((b Sequence 1 2 3 4) (a . (Sequence ))))
    (trial '(r (Pattern x (Blank)) (s (Pattern x (Blank)))) '(r a ( s a))) '(success ((x . a)))
    (trial '(r (Pattern x (BlankSequence)) (s (Pattern x (BlankSequence)))) '(r a b ( s a b)))
    '(success ((x Sequence a b)))
    (trial '(r (Pattern x (BlankNullSequence)) (s (Pattern x (BlankNullSequence)))) '(r  ( s )))
    '(success ((x Sequence)))
    (trial '(f (Optional (Pattern x(Blank))0 ) a)'(f  a)) '(success ((x . 0)))
    (trial '(g (Pattern x (Blank)) (f (Pattern x (Blank)))) '(g a (f a))) '(success ((x . a)))  
    ))

;; these fail
(defparameter ftests
  '(
    (trial '(h (Pattern x (Blank))) '(h a b c)) '(success (x . (h a b c))) ;; if h is flat
    ;; work on this one ... currently returns (success ((b Sequence 2 3 4) (a 1)))
    (trial '(g (Pattern x (Blank)) b)  '(g  b a)) '(success ((x . a)))
    (trial '(g a b)'(g  b a))'(success nil) ;g is orderless
    (trial '(g a b (Pattern x (Blank))) '(g a c b )) '(success ((x . c)))
    ))

(defparameter tests2
    '(
    (trial '(Plus a (Pattern x (Blank))) '(Plus  a b)) '(success ((x . b)))
    (trial '(Plus a (Pattern x (Blank))) '(Plus  a b c )) '(success ((x Plus b c)))
    (trial '(Plus b (Pattern x (Blank))) '(Plus  a b c )) '(success ((x Plus a c)))
    ;; LLL    
    ;; (trial '(Plus a b (Pattern z (BlankNullSequence))) '(Plus a b)) ;?WRONG should be '(success((z Sequence)))
    
    ;; (trial '(Plus a (Pattern x (Blank))) '  a)  ;; FIX THIS!!
    ;;    (trial '(ff  (Pattern x (BlankSequence)) (Pattern y (BlankSequence))  (Pattern x (BlankSequence))	     )  '(ff  a a b b a a))
    ;; should be '(success((x . a) (y . (Sequence a b b a))))
    ;; now is just '(success((x . a)))  or '(failure)
    
    (trial '(Plus a b (Pattern z (BlankNullSequence))) '(Plus a b)) '(success ((z Sequence)))
    (trial '(Plus (Pattern x1 (BlankNullSequence)) d e) '(Plus a b c d e))'(success ((x1 Sequence a b c)))
    (trial '(Plus (Pattern x1 (BlankNullSequence)) c d (Pattern x2 (BlankNullSequence))) '(Plus a b c d e))
    '(success ((x2 . e) (x1 Sequence a b)))
    
    ;;  (trial '(Plus a (Pattern z (Blank))) '(Plus a  ))  should be z . 0
    ;; (trial '(Plus (Pattern x1 (BlankNullSequence)) b d) '(Plus a b c d e))
    
    

    (trial '(Plus  (Pattern x (Blank))  (Sin (Pattern x (Blank))))  '(Plus  a  (Sin a))) '(success ((x . a))) 
    ;; Plus is Flat and Orderless.
    ;; (trial '(Plus  (Pattern x (Blank))  (Sin (Pattern x (Blank))))  '(Plus  (Sin a) a))
    
  ;;  (trial '(ff  (Pattern x (BlankSequence))  (Pattern y (BlankNullSequence)) (Pattern x (BlankSequence)) 	     )  '(ff  a  a))'(success ((y Sequence) (x . a)))

    (trial '(ff  (Pattern x (BlankSequence))  (Pattern y (BlankSequence)) (Pattern x (BlankSequence)) 	     )  '(ff  a  b  a)) '(success ((y .  b) (x . a)))
    ;;
    (trial '(Plus  (Pattern x (BlankSequence))  (Pattern y (BlankSequence)) (Pattern x (BlankSequence)) 	     )  '(Plus  a  b  a))
    '(success ((y . b) (x . a)))
    
    (trial '(Plus  (Pattern x (Blank))  (Pattern y (Blank)) (Pattern x (Blank)) 	     )  '(Plus  a  b  a)) '(success ((y . b) (x . a)))
    (trial '(Plus  (Pattern x (Blank))  (Pattern y (BlankSequence))  )  '(Plus  a  b  a)) '(success ((y Sequence b a) (x . a)))

    (trial '(Plus  (Pattern x (Blank))  (Pattern y (Blank)) (Pattern x (Blank)) 	     )  '(Plus  a  b  a))
    '(success ((y . b) (x . a)))
    
    (trial '(Plus  (Pattern x (Blank))  (Pattern y (BlankSequence)) (Pattern x (Blank)) 	     )  '(Plus  a  b b b a))
    '(success ((y Sequence b b b) (x . a)))
    (trial '(Plus  (Pattern x (Blank))  (Pattern y (BlankSequence)) (Pattern x (Blank)) 	     )  '(Plus  a  b b b a c))
    '(failure)
    (trial '(Plus  (Pattern x (BlankSequence))  (Pattern y (BlankSequence)) (Pattern x (BlankSequence)) 	     )  '(Plus  a  b c a b))
    '(success ((y . c) (x Sequence a b)))

    ))
;; stubs.  If we need these, I suspect we are in trouble.
;; also for mblanks..
;(defun mblank3(&rest x) t)
(defun mblank2(&rest x) t)
(defun Default(h)(case h (Plus 0)(Times 1)(otherwise '(Sequence ))))