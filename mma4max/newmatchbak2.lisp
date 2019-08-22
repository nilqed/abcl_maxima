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

;;; definition: a patternobject is like an expression but it can
;;; have (Pattern ...) or PatternTest or PatternSequence in it.
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

;;; All that matches in version 17
;;; (a) structurally identical matches. [sic]
;;; (b) (Blank) matches <anything> [sic]
;;;     (Blank foo) matches anything with a Head of foo [usually the car)
;;;     but also if foo is in {Integer, Rational, Complex, rat...?}
;;; matching rational or complex requires some special deconstruction of these.
;;; program is in ReplaceAll, but maybe should be somewhere else like Replace.

;;; (c) (Pattern x <patternobject>) matches whatever would
;;;   otherwise be matched by <patternobject> and also  binds the value of
;;;   patternobject to the name x.  From that time onward during the match
;;;   x will only match identical items. [sic]
;;; (d) [must fix]
;;;    f[pat1, pat2, ...,patn] matches expression f[e1,e2, ..., en] {sic} if
;;; {pat2, ...,patn} matches {e2, ...,en} subject to
;;;         (pat1 matches e1) AND sic.... which is, recursively...

;;; {pat3...} matches {e3 ...} subject to
;;;          (pat2 matches e2)  AND (pat1 matches e1)AND  sic)... etc.

;;these are evaluated kind of inside out???  wrong..


;;; That is, f[x_,x_] will match f[3,3] but not f[3,4].
;;; f[x_,x_^2] will match f[a,a^2].  But not f[3,9]. (sorry).
;;; However, f[_,_]  matches f[3,4].

;;; the following is broken now.


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
;;; others.  Plus is also orderless (see below) complicating
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
;;; x:f[_] parses into (Pattern x (f (Blank)))
;;; x_:pred parses into (PatternTest(Pattern x (Blank)) pred)

;; it is also possible to go into a pattern matching situation with a
;; condition specified externally, e.g. {note parens..}
;;  (x_-> f[x]) /;pred[x],  where 
;; (Condition (Rule (pattern x (blank))(f x))
;;            (pred x))

;; note there can be rule { ->} and ruledelayed  { :> }

;;; Return value for a match is  nil or non-nil.
;;; If the value is non-nil, the stack structure env
;;; will have a set
;;; of bindings of all the pattern-variable bindings. If the return value
;;; is nil, env will be unchanged.

;; define match stack, using up to 100 pattern variables if no-one else
;; has done it.

(defvar env (make-stack :size 100))

;; match provides a top level match of pattern to expression.
;; both pattern and expression are assumed to be Lisp lists
;; from the parser (lmath:parser)

;; Usually match won't be used by a program ... rather, it will use m1.
;; Typical usage would then be as follows...

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

#|note.  I think that many, if not all, of the functions must carry
a condition which may be a function.  The idea is that if you are
trying to match {a_,b_,c, ...}  against {e1,e2, e3...} subject to test[a,b,c]
  then you match a to e1 subject to 
     matching {b_,c_, ...} to {e2, e3, ...}
      AND
     test[a,b,c]  evaluated with a=e1
which operates recursively for matching down the list.

do the functions also have to carry flags from attributes like Flat and Orderless?
I think that would be good. One way would be to just transmit the Head, and know
that (flatp thehead) ... etc could be computed



|#

(defun trial(pat exp &optional (env (make-stack :size 20) wasitprovided))
  (spushframe env 'trialmatch);; make a frame to store results of match
  
  (if  (m1 pat exp #'truth) ;; m1 is the matching program. env is global, result bindings.
      (format t "~%The match succeeded. ~%Binding Stack is ~%~s" env)
    (format t "~%Match Failed~%"))

  ;; reset ptr to former position if environment was provided
  ;; otherwise env will simply be removed via unbinding on return
  (if wasitprovided (spopframe env)))

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
  (m1 pat exp #'truth))

(defun matchall(pat exp &optional (condition #'truth))
  ;; return a list, the collection of all matches
  (let ((res nil))
    (m1 pat exp #'(lambda()(if (funcall condition)(push (env2alist env) res)) nil))
    res))

(defun matchtest(pat exp &optional (condition #'truth))
  ;; test for match, but do not leave bindings around
  (spushframe env 'match)
  (prog1  (m1 pat exp condition)
    (spopframe env))) ;;won't work for checking extra conditions. then use mlist.
  

(defun matchlist(pl el condition)  ;;alternative top level for simultaneous matches
  (spushframe env 'matchlist)
  ;;    pattern expression name condition
  (mlist pl el nil condition))

(defvar phead nil)
;; mblanks matches blankX to expression e.
;; phead is the pattern head and is either Blank, BlankSequence or BlankNullSequence
;; plist is the pattern list part, after (Blank* )
;; e is the expression.
;; name is the variable to bind to the expression. Nil name means it was unnamed _.
;; condition is inherited constraint
(defun mblanks (phead plist e name condition)
	      (cond
	      ;; _foo .. this next line is like (Pattern name (Blank foo))
	      ;; where name=nil means no name. blankshead would be like the foo above.
	      ((eq phead 'Blank)
	       (mblank1 plist e name condition)) ;do simple Blank matching
	      
	      ;; __foo .. this next line is like (Pattern name (BlankSequence foo))
	      ;; where name=nil means no name. foo is the "head" to match
	      ;; this pattern matches one or more objects EACH of which has
	      ;; head foo.
	      ;; f[foo[3], foo[4]] /. f[z__foo] -> hah[z]  
	      ;; binds z to Sequence[foo[3],foo[4]].
	      ((eq phead 'BlankSequence)
	       (mblank2 plist e name condition))
	      ;; ___foo .. this next line is like (Pattern nil (BlankNullSequence foo))
	      ;; where nil means no name. foo is the "head" to match
	      ;; this pattern matches zero or more objects each of which has
	      ;; head foo.
	      
      	      ((eq phead 'BlankNullSequence)
	       (mblank2 plist e name condition))
	      
	      (t (error "mblanks called on ~s " phead))))

;;; M1 main pattern matcher
(defun m1 (p e gh &optional (c #'truth))
  ;; p is pattern, e is expression, gh is governing head
  ;; gh is the Head of the pattern that governs the attributes.
  ;; e.g. in (f  (Pattern x ...) (Pattern y ...) ...)
  ;; in the 2nd argument [y], the governing head is f, not Pattern.
  ;;(print env)
  (let* ((hp (Head p))
	 ;;(ha (cdr (Attributes hp)))
	 ;; empty attribute list would be (List )
    ;;; hold it, if hp = Pattern, the attributes are from the parent..
    ;;; how to do it? 1/13/11
	 (ha (and gh (cdr (Attributes gh)))) ;; if non-null gh, get attributes
	 
	 )
    (cond ((atom p)(and (equal p e) (funcall c)			))
	  ((eq p e) (funcall c)		    )
	  ((equal p e) (funcall c)) ;; remove this if it isnt saving time..
	  ;; check for a pattern
	  ((eq hp 'Pattern)	    ;; dissect it for mblanks
	   (mblanks (car (third p)) ;; Blank, Blanksequence etc
		    (cdr (third p)) ; the part after (blank*  ...)
		    e		    ;the expression
		    (second p)		;the name
		    gh
		    c))
	
	  ((eq hp 'Plus)(matchpluspe p e c))
	  ((eq hp 'Times)(matchtimespe p e c))
	  ;; the above 2 clauses could be instances of (matchflatorderlesspe p e head identity c)
	  ;; next, go through the cases
	  ((eq hp 'Condition)
	   (format t "~%processing Condition ~s "(caddr p))
	   (m1 (cadr p)e (let ((ccc (caddr p)))
			   #'(lambda()(and (meval ccc)(funcall c))))) )
	  ((eq hp 'Alternatives) 
	   ;; if we change Alternatives to be n-ary, 
	   ;; this still works.
	   (dolist (alt (cdr p) nil)	; range through alternatives.
	     (format t "~% check alternative ~s" alt)
	     (if (m1 alt e gh c)(return t))))
  	  ;; p is a pattern.
	  ;; this is handled in mblanks.
	  ;; This first test is for a "naked" unnamed blank etc.
	  ;; otherwise the head is "Pattern".
	  ;; encountering a BlankSequence or BlankNullSequence
	  ;; at this point in the code is likely to indicate the
	  ;; programmer is mis-using them, since they really can't
	  ;; match a sequence.

	  ((eq hp 'Blank)
	   (format t "~% naked ~s match to expr ~s" hp e)
	   (mblanks hp (cdr p) e nil gh c))
	  
	   ((member hp '(BlankSequence BlankNullSequence :test 'eq))
	    (format t "~% matching (probably misused) ~s  to expr ~s, treated as Blank" hp e)
	    (mblanks 'Blank (cdr p) e nil gh c))
	   ((member hp '(Repeated RepeatedNull Except))
	    (error "Matching of Pattern objects with Head ~s not installed, used in ~s" hp p))
	   ((eq hp 'PatternTest) ;;  a_?pred  for example
	    (m1 (cadr p)e gh
		(let ((ccc (caddr p))(eee e))
		  #'(lambda()(and (meval (list ccc eee))
				  (funcall c))))))
	   ;; mlist takes care of the rest here..
	   ;; if (Head p) is neither flat nor orderless, match in order.
	   ;; if (Head p) is orderless, jumble the args if necessary.
	   ;; Here, we match the heads and go through the rest via mlist.
	   ;; simplified coding here
	   ;; Note. (Head p) is NOT "Pattern"
	   ((not (member 'Flat ha :test 'eq)) ;; possibly Orderless..
	    (m1 hp (Head e)
		   (let ((pp (rest p))(ee (rest e)))
		     #'(lambda()
			 (and (mlist pp ee hp gh c) (funcall c))))))
	  
	   ((atom e) nil) 
	  ;; non-atom, non-blank p can't match atom?
	  ;; now both p and e are expressions.
		   
	  ;; we match (recursively) their heads, and match,
	  ;; in sequence, elements in their cdrs, by mlist.
	   ;; in mlist we worry about attributes of phead, e.g. flat, orderless

	   ((member 'Flat ha)
	    ;; separate Flat and Orderless
	    (if (member 'Orderless ha :test 'eq)
		(format t "~% ran into unimplemented section of m1 with ~s" p)
	      ;; merely Flat. If head is H,
	      ;;this has the effect of sortof making
	      ;;Blank into BlankSequence, except with head H.
	      
	      (m1 hp (car e) 
		  #'(lambda()(and (mlist (cdr p) (cdr e) hp gh c)
				  (funcall c ))))))
	   (t				; anything left??
	    (format t "~% ran off end of m1 with ~s" p)
	     nil))))

;; I know the following guy fails to backs up right..
;;;*****
#+ignore

(defun everym1 (pp ee gh)(cond ((null pp)(null ee)) ;;same length, all matching
			    ((null ee) nil) ; not same length
			    (t (and (m1 (car pp)(car ee) gh)
				    (everym1 (cdr pp)(cdr ee))))))

(defun everym1(pp ee gh) (mlist .... what args))  ;; should do it.

(defun patfreesequence(p)
  ;; returns t if p is a pattern that has no BlankSequence or BlankNullSequence
  ;; on its top level or within (Pattern...)
  (cond ((atom p)t) ;; null or atomic has no Sequence
	((member (car p) '(BlankSequence BlankNullSequence) :test 'eq) nil)
	((consp (car p))
	 (and
	  (eq (caar p) 'Pattern)  ;; (Pattern name (BlankSequence ...))
	  (member (car (third (car p)))
		  '(BlankSequence BlankNullSequence) :test 'eq))
	 t)
	(t (patfreesequence(cdr p)))))

	
#+ignore
(defun m1 (p e condition)
  (cond ((and (atom p)(equal p e)) (meval-to-bool condition))
	((or (eq p e)(equal p e)) (meval-to-bool condition) )
	(t (let ((phead (car p))) ;;phead is the pattern's head.
	     ;;(format t "~%phead = ~s" phead)
	     (cond  
	      ((member phead '(Blank BlankSequence BlankNullSequence))
	       (mblanks phead (cdr p) e nil condition )) ;nil means no name
	      
	      ((eq phead 'Pattern)
	       (mblanks (car (third p)) ;; blank, blanksequence etc
			(cdr (third p))	; the part after (blank*  ...)
			e ;the expression
			(second p)	;the name
			condition))
	      ((eq phead 'PatternTest)
		    (let ((result (m1 (cadr p) e condition)))
		      (cond (result
			     (funcall (caddr p)(meval e)))
			    (t nil))))
	      ((eq phead 'Condition)
		    (let ((result (m1 (cadr p) e condition))); check local condition
		      (cond ((and result 
			      (meval (caddr p))) t) ;check added condition
			    (t nil))))
		   ;; try this.. 12/20/2010 RJF
		   ((eq phead 'Alternatives) 
		    ;; if we change Alternatives to be n-ary, 
		    ;; this still works.
		    (dolist (alt (cdr p) nil) ; range through alternatives.
		      (if (and (m1 alt e condition)(meval (caddr p))) (return t))))
		   ((member phead '(Repeated RepeatedNull Except))
		    (error "we didn't implement rule with ~s in  ~s" phead p))
		   ((atom e) nil) 
		   ;; non-atom, non-blank p can't match atom?
		   ;; now both p and e are expressions.
		   
		   ;; we match (recursively) their heads, and match,
		   ;; in sequence, elements in their cdrs, by mlist.
		   
		   ;; in mlist we worry about attributes of phead, e.g. flat, orderless
		   
		   (t(m1 phead (car e) 
			`(mlist (m2 ,(cdr p) ,(cdr e) ,condition ,phead)))
					;first check that heads match, then the rest
					;then condition.
		     ))))))
		    
;******


(defun mlistfol(patlist  explist  count max)
  (format t "~% mlistfol ~s  ~s ~s ~s" patlist explist count max)
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

;; mblank matches a (Blank h) or (Blank). This works if the test
;; headtest = (h) is nil, 
;; or if the head of e is (car headtest)
;;   (i.e. (Blank foo) matches (foo ...))
;; or if headtest is the "type" of e.  
;;   (i.e. (Blank Integer) matches 3.

(defun mblank(headtest e name condition)
  (and (if headtest (mtypep e (car headtest)) t)
       (if name  
	   (multiple-value-bind (val found)(sfind env name)
	     (cond (found (equal val e))
		   (t (spush env name e))))
	 t)
       
       (funcall condition)))

(defun mblank1(plist e name condition)
       (cond (name  
	      (multiple-value-bind (val found)(sfind env name) ;; has the name a value?
		(cond (found 
		       (or (equal val e) ;; if it is equal, continue.
			   (return-from mblank1 nil))) ;; is it equal to expression?
		      (t (spush env name e))));; if no value yet, push it.
	      ;; now check the list following the Blank. Does it specify a particular Head?
	     (and (or (null plist) (eq (Head e) (car plist))) (funcall condition)))
	     ;; if there is no name, just match anything  with the right Head     
	     (t  (and (or (null plist) (eq (Head e) (car plist)))(funcall condition))
		      
		 )))
;; this is just a copy of mblank1. inadequate in general.
(defun mblank2(plist e name condition)
       (cond (name  
	      (multiple-value-bind (val found)(sfind env name) ;; has the name a value?
		(cond (found 
		       (or (equal val e) ;; if it is equal, continue.
			   (return-from mblank1 nil))) ;; is it equal to expression?
		      (t (spush env name e))));; if no value yet, push it.
	      ;; now check the list following the Blank. Does it specify a particular Head?
	     (and (or (null plist) (eq (Head e) (car plist))) (funcall condition)))
	     ;; if there is no name, just match anything  with the right Head     
	     (t  (and (or (null plist) (eq (Head e) (car plist)))(funcall condition))
		      
		 )))

#+ignore
(defun mblank2 (headtest e condition) ;; hardly enough for _ _ or _ _ _.
  (if headtest (mtypep e (car headtest)) (meval-to-bool condition)))

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
(defun mlist(pl el name attrs condition)
  ;;; need to majorly revise this.
  
;;   (mlist pl el name attributes condition) matches (car pl) to some part of el,
;;and the remainder of pl gets matched to remainder of el.

;; the simple case is where (car pl) can match exactly one item (car el)
;; the complicated case is where (car pl) can match 0, 1, or more items from el.
;; this occurs when (car pl) looks like 
;; (BlankSequence head)  ;; one or more items x with Head[x]=head
;; (BlankNullSequence head) ;; zero or more items x with Head[x]=head
;; (Pattern x (BlankSequence head))  ;; one or more items x with Head[x]=head
;; (Pattern x (BlankNullSequence head))  ;; zero or more items x with Head[x]=head
;;  Other options? like Alternatives, Condition, PatternTest, ...  ugly stuff.


  (cond ((null pl) 
	 (and(null el) 
					;(print '**)
	     (funcall condition)))
	;; sort of simple case, no sequences
	((patfreesequence (car pl))
	 (if (not (member 'Orderless attrs :test 'eq))
	     ;; no sequences and in order. especially simple
	 (m1 (car pl)(car el) ;; match the first elements
	     #'(lambda()(and (mlist (cdr pl)(cdr el) name attrs condition) ;then the rest
			     (funcall condition))))  
	 ;; free of sequence, but Orderless. less simple.
	 
	 (dolist (p1 pl nil)
	   (dolist (e1 el nil)
	   (if (m1 p1 e1 ;; match the chosen elements if possible
	       #'(lambda()(and (mlist 
				(remove p1 pl :count 1)
				(remove e1 el :count 1)
			 name attrs condition) ;then the rest
			       (funcall condition))))
	       (return-from mlist t)
	     (spop env)))) ; remove failed pattern match
	 ))
	;; first pattern in the list pl is not simple.
	
	((eq (caar pl) 'BlankNullSequence) ;; propagate these changes lower.
	 (if (member 'Orderless attrs :test 'eq)
	 (docomb (ee el nil) ;; pick some subset
		    (cond  ((and (every #'(lambda(ex)(m1 (caar pl) ex condition)) ee)
				 (mlist (cdr pl)(set-difference el ee) name attrs condition))
			    t)
			   ;; bind <nothing> to ex here..
			   ;; if the pattern has a [new] name, bind it to `(,name ,@ee)
			   ;; etc.  done with mblank1 or something similar.
			   ))
	 (dosublist (ee el nil) ;; pick some subset starting from the front of the elements
		    (cond  ((and (every #'(lambda(ex)(m1 (caar pl) ex condition)) ee)
				 (mlist (cdr pl)(set-minus el ee) name attrs condition))
			    t)
			   ;; bind <nothing> to ex here..
			   ;; if the pattern has a [new] name, bind it to `(,name ,@ee)
			   ;; etc.  done with mblank1 or something similar.
			   ))))
	
	((eq (caar pl) 'BlankSequence)
	 (dosublist (ee el nil) ;; pick some subset starting from the front of the elements
		    (cond  ((null ee) nil) ;must be at least 1
			   #+ignore
			   ((and (every #'(lambda(ex)(m1 (caar pl) ex condition)) ee)
				 (mlist (cdr pl)(set-minus el ee) name attrs condition))   t)
			   
			   ((and (every 
				  #'(lambda(ex)
				      (m1 (caar pl) ex 
					  (let ((newel (set-minus el ee))
						(newpl (cdr pl)))
					    
					    #'(lambda()(and 
							(mlist newpl newel name attrs condition)
							(funcall condition))))))))
			    (return t))
						     
							   )))
			   ;; bind <nothing> to ex here..
			   ;; if the pattern has a [new] name, bind it to `(,name ,@ee)
			   ;; etc.  done with mblank1 or something similar.

	
	((and (eq (caar pl) 'Pattern)(eq (car (third (car pl))) 'BlankNullSequence))
	 (if (member 'Orderless attrs :test 'eq)
	     	 (docomb (ee el nil) ;; pick some subset starting from the front of the elements
		    (cond ; ((null ee) nil)
		     ((and (every #'(lambda(ex)(m1 
						(car pl)
						#+ignore
						(cons 'Blank (cdr (third (car pl))))
							 
							 ex condition)) ee)
				 (spush env (cadar pl)(if (or (null ee) (cdr ee))(cons 'Sequence ee)
							(car ee))) ;; try this binding
				 (if  (mlist (cdr pl)(set-difference el ee) name attrs condition)
				     (return t) ;; if the rest matches, we win
				   (spop env)) ;remove binding for name and keep going.
				 ))
			   (t nil)))
	   
	     
	 (dosublist (ee el nil) ;; pick some subset starting from the front of the elements
		    (cond ; ((null ee) nil)
		     ((and (every #'(lambda(ex)(m1 
						(car pl)
						#+ignore
						(cons 'Blank
							       (cdr (third (car pl))))
							 ex condition)) ee)
				 (spush env (cadar pl)(if (or (null ee) (cdr ee))(cons 'Sequence ee)
							(car ee))) ;; try this binding
				 (if  (mlist (cdr pl)(set-minus el ee) name attrs condition)
				     (return t) ;; if the rest matches, we win
				   (spop env)) ;remove binding for name and keep going.
				 ))
			   (t nil)))))
	((and (eq (caar pl) 'Pattern)(eq (car (third (car pl))) 'BlankSequence))
	 (dosublist (ee el nil) ;; pick some subset starting from the front of the elements
		    (cond  ((null ee) nil)
			   ((and (every #'(lambda(ex)
					    (m1 (cons 'Blank
						      (cdr (third (car pl))))
						ex condition)) ee)
				 (spush env (cadar pl)(if (or(null ee)(cdr ee))(cons 'Sequence ee)
							(car ee))) ;; try this binding
				 (if  (mlist (cdr pl)(set-minus el ee) name attrs condition)
				     (return t) ;; if the rest matches, we win
				   (spop env)) ;remove binding for name and keep going.
				 ))
			   (t nil))))
	(t (format t "~% reached unimplemented MLIST section") nil)))
	
	
	 

#+ignore
(cond ;; both must end at the same time to match
	;; could check for entries that are simply names like foo_, easily done
	;; if phead is NOT flat 
;;	#+ignore
	((and (not isflat)(not isorderless) 
	      (some #'simple-pat-or-constant pl)) ;; faster case.
	 (mlistfast pl el condition))
	
	((patternp (car pl)) 
	 ;; must to assign the matched stuff to
	 ;; (cadar pl), the name..
	 ;; might try to avoid this cons...
	 (mlist (cons (caddar pl)(cdr pl)) el (cadar pl) condition ))
	
	((blank2p (car pl))  ;; the pattern is __   two blanks
	 ;;since this is NOT orderless, we must match these suckers in
	 ;;order, or not at all. We look for the shortest sequence
	 ;;of length ONE or more that matches.
	 
	 ;; Mma semantics requires the following glitch..
	 (if isflat (setq phead 'Sequence))

	 (ml2 (cadar pl) (cdr pl) el name 1 condition)
)
	
	((blank3p (car pl))
	 ;;this one, BlankNullSequence (_ _ _) requires
	 ;; the shortest sequence of ZERO or more elements.
	 (ml2 (cadar pl) (cdr pl) el name 0 condition))	   	 
	
	((and isflat (blank1p (car pl)))
	 ;; for a flat operator, treat f[...,x_,...] like f[...,x__,...].
	 ;; So says Mma semantics.
	 (ml2 (cadar pl) (cdr pl) el name 1 condition))
	(name (and(mpat name (car pl)(car el))
		  (mlist (cdr pl)(cdr el) nil condition)))
	((m1 (car pl)(car el) condition)
	 ;; if the cars match, so must the elements of the cdrs
	 (mlist (cdr pl)(cdr el) nil condition)))

#+ignore

(defun mlist(pl el name condition)
  
  
  ;;; need to majorly revise this.
  
#|   (mlist pl el name condition) matches (car pl) to some part of el,
      and the remainder of pl gets matched to remainder of el, 
===  (and (setf e0 (match p0 el name
	    (and (mlist (cdr pl) (set-minus el e0)) condition)  )) ;; note new condition
|#
  (cond ((null pl) 
	 (and(null el) (funcall condition)));; both must end at the same time to match
	;; could check for entries that are simply names like foo_, easily done
	;; if phead is NOT flat 
;;	#+ignore
	((and (not isflat)(not isorderless) 
	      (some #'simple-pat-or-constant pl)) ;; faster case.
	 (mlistfast pl el condition))
	
	((patternp (car pl)) 
	 ;; must to assign the matched stuff to
	 ;; (cadar pl), the name..
	 ;; might try to avoid this cons...
	 (mlist (cons (caddar pl)(cdr pl)) el (cadar pl) condition ))
	
	((blank2p (car pl))  ;; the pattern is __   two blanks
	 ;;since this is NOT orderless, we must match these suckers in
	 ;;order, or not at all. We look for the shortest sequence
	 ;;of length ONE or more that matches.
	 
	 ;; Mma semantics requires the following glitch..
	 (if isflat (setq phead 'Sequence))

	 (ml2 (cadar pl) (cdr pl) el name 1 condition)
)
	
	((blank3p (car pl))
	 ;;this one, BlankNullSequence (_ _ _) requires
	 ;; the shortest sequence of ZERO or more elements.
	 (ml2 (cadar pl) (cdr pl) el name 0 condition))	   	 
	
	((and isflat (blank1p (car pl)))
	 ;; for a flat operator, treat f[...,x_,...] like f[...,x__,...].
	 ;; So says Mma semantics.
	 (ml2 (cadar pl) (cdr pl) el name 1 condition))
	(name (and(mpat name (car pl)(car el))
		  (mlist (cdr pl)(cdr el) nil condition)))
	((m1 (car pl)(car el) condition)
	 ;; if the cars match, so must the elements of the cdrs
	 (mlist (cdr pl)(cdr el) nil condition))))


#+ignore
(defun mlistfast(pl el condition)
  (and(= (length pl)(length el)) ;;same length
      (everym1 pl el) ;;each pattern matches 
      (meval-to-bool condition)		;condition is true
      ))

;; another version, where we look for the simple bindings first.
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

(defun mlistfast(pl el condition &aux pp ee)
  (cond((= (length pl)(length el)) ;;same length
	(map nil #'(lambda(p e)(cond ((patternnameblank p)	;match simple ones first
				      (if (null (m1 p e condition))
					  (return-from mlistfast nil)))
				     (t (push p pp) ;;save the others.
					(push e ee))))
	     pl el)
	(format t "~%pp=~s, ee=~s~% env=~%~s" pp ee env)
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
		 ;;		 (format t "~%k=~s" k) ;debug

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
;;		      (format t "~%Collected so far .. ~s" collect)
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
;;		     (format t "~% j = ~s" j)
		     (cond ((mblank2 h (car j))
			    (setq collect (cons (car j) collect))
;;			    (format t "~% consed onto collect: ~s" collect)
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
			;;debug	(format t "~%matched ~s to ~s " p (elt el index))
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
#+ignore
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
			  ;;debug	(format t "~%matched ~s to ~s " p (elt el index))
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
		 ;;debug	(format t "~%matched ~s to ~s " p (elt el index))
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


(defun mpat (name patobj e condition)
  ;; to match (Pattern name patobj) to e, 
  ;; first see if name has a value - if so, test for equality of value to e.
  ;; we assume the user has not done f[x_foo,x_bar] (different tests...)
  ;; Otherwise just bind name to e
  

  (multiple-value-bind (val found)(sfind env name)
		       (cond (found (equal val e)); if found, don't change
			     (t (m1 patobj e #'(lambda(env)
					        (spush env name e)
						  (funcall condition)))))))
				
     
;; if x is atomic, if typ is not a type, or x is not of type typ, return nil.
;; in case x is a cons, see if its head is eq to typ.

(defun mtypep(x typ)(if (atom x)
			(multiple-value-bind 
			 (isnoerr val)
			 (errorset  ;;excl returns nil if no err, value of form
			  (typep x typ))
			 (if isnoerr val nil))
		      (eq (car x) typ)))

;;these cause problems if Integer and integer are the same as INTEGER

#-kcl (deftype |Integer|() 'integer)
#-kcl (deftype |Rational|() 'rational)
;;; etc could do single-float, double-float, complex, number, cons, ...

;; extra cases to consider: 

;;  x_.
;;    = (Optional (Pattern x (Blank))) 

;; Also, Orderless + Flat.  Any other Attributes?  How about Defaults?,
;; repeated, what else?

;; also f[x_]:=g[x]  /; x>0

;;; some sample programs for combinatorial matching.
;;; mapcomb applies the function f to each combination of
;;; elements in l. Non-destructive.  Try (mapcomb 'print '(1 2 3))

(defun mapcomb(f l)
  (labels((mc (l c)
	      (cond ((null l) (funcall f c))
		    (t (mc (cdr l) c)
		       (mc (cdr l) (cons (car l) c))))))
    (mc 
     (nreverse l) 
     nil)))

(defun mapcombl(f l &aux ans)
  (labels((mc (l c)
	      (cond ((null l) (if(funcall f c) (push c ans)))
		    (t (mc (cdr l) c)
		       (mc (cdr l) (cons (car l) c))))))
    (mc 
     (nreverse l) 
     nil))
  ans)

;; (mapcombl #'(lambda(r)(every #'oddp r)) '(1 2 3))
;; computes a list of all subsets of (1 2 3 that are all odd.
;; returns ((1 3)(3)(1)())

;; map over all combinations of l, and return the first combination c
;; for which (f c) is true.
;; try  (mapcomb-if #'(lambda(r)(> (length r) 3)) '(1 2 3 4 5 ))

(defun mapcomb-if(f l)
  (labels((mc (l c)
	      (cond ((null l)
		     (if (funcall f c) (return-from mapcomb-if c)))
		    (t (mc (cdr l) c)
		       (mc (cdr l) (cons (car l) c))))))
    (mc 
      (reverse l)
      nil)))



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

;; dosublist iterate over all sublists of items
;; try (dosublist (kk '(1 2 3)) (print kk))

(defmacro dosublist (iter &rest exp)
  (let ((ii (gensym))
	(len (gensym))
	(count (gensym)))
    
    `(let* ((,ii ,(cadr iter))
	    (,len (length ,ii))	 )
       
       ;; we can return from the middle of this..
       ;; try (dosublist (kk '(1 2 3 5 6 7)) (print kk) (if (and kk (> (car(last kk)) 5))(return 'toobig )))
       
       (do ((,count 0 (1+ ,count)))
	   ((> ,count ,len) ,(caddr iter))
	 (setf ,(car iter) (subseq ,(cadr iter) 0 ,count))
	 ,@ exp))))


(defun set-minus( r s) 
  ;; tail of r that remains after its prefix s is removed.
  (nthcdr (length s) r))


;; debug tool. wipe stack
(defun clrs()(setf env (make-stack)))

;; really slow pattern match for +, perhaps more general than mma? or just the same
;; p is a pattern with head Plus.  That is, for x_+y_,  
;; p would be (Plus (Pattern x (Blank))(Pattern y (Blank)))

;; e is either an expression with head Plus, or not.
;; That is, it might be something like (Plus a b c), or
;; perhaps something else like (Sin q) or (Times a b).

(defun matchpluspe(p e &optional (Condition '#'truth))
  (let ((pl (cdr p))) ;; list of pattern inside Plus
        ;; nothing left to match?, we hope expression e is zero
      (if (null pl)
	   (return-from matchpluspe (if (and (= 0 e)
					      (funcall Condition))
					'True )))
      (cond
       ;; only one pattern item term remaining to be matched?
       ((null (cdr pl))  (return-from matchpluspe
			   (m1 (car pl) e Condition)))
      
       ;; more than one pattern term remaing to be matched. p1+p2+..

       ;; case one:  e is not a Plus.
       ((not(eq (Head e) 'Plus))
	;; must match one of p1, p2, say pk ... against e, 
	;; and the others of p1, ... against 0;  say  pj_.  which would be pj_.0 given
	;; identity for Plus is 0.
	;; pj__ or pj___ are bad form, though mma doesn't forbid them, they don't seem to
	;; really work, viz:    x /. x+c_.-> f[c] is ok. x /. x+c__->f[c]  returns x.
	;; yet x+y/. x+c__->f[c] returns f[]
        (dolist (pk (cdr pl) (funcall Condition) ;; go through items in pl.
	  (format t "~%matching pattern ~s in expr ~s" pk e)
	  (if  (m1 pk e #'(lambda(env) (and (matchpluspe (remove pk p) 0 t)
					       (funcall Condition))))
	      (return t)))))
       ;; case two:  e IS a Plus.
       (t
	(dolist (pk (cdr pl) (funcall Condition) ;; go through items in pl.
	  (format t "~%matching pattern ~s in expr ~s" pk e)
	  (if (atom pk)
	      (cond ((find pk (cdr e))
		     (setf (cdr e) (remove pk (cdr e) :count 1))
					; remove pk and go to next pattern
		     (if (null (cdr e)) (return (funcall Condition))); all done here
		     ))
		(return-from matchpluspe nil)) ; pattern element is atom and not found
	    ;; pattern element is NOT an atom
	    ;; try to match it.
	    (let ((ans (docomb 
			(e1 (cdr e) nil) ;; go through all combinations
			(if (m1 pk (cons 'Plus e1)
				#'(lambda(env)
				    (and (matchpluspe 
					  (remove pk k :count 1)
					  (meval (cons 'Plus 
						       (set-minus (cdr e) e1)))
					  t)
					 (funcall Condition))))))
		       ))
	      (if ans (return t)))))))))

#+ignore ;; older version
(defun matchplus(p e &optional (Condition t))
  (if (null p)(return-from matchplus (if (and (null e)
					      (funcall Condition))
					 'True )))
  ;; check for singleton p
  (if (null (cdr p)) (return-from matchplus (match p (cons 'Plus e) ;Condition
						   )))
  
  (let* ((pp (car p)) ;; pp is the first of the patterns to match
	 (combs nil)
	 (exp nil))
    (mapcomb #'(lambda(ex)
		      (if
			  (matchtest pp
				(setf exp (if (cdr ex)(cons 'Plus ex) (car ex))))
			  ;;(push exp combs)
			  (push ex combs)
			))
				 e) ;;really this should be a stream

    (format t "~%Candidates to match ~s are ~%~s" pp combs)
    
    ;; possible glitch if generalized to all flat, orderless:
    ;; set difference assumes no duplicate elements.
    (dolist (e1 combs nil) 
      (spushframe env 'matchplus)
	 ;; if we ran through them all without succeeding, it failed.
      (if (and (m1 pp ;; e1
		   (if (cdr e1)(cons 'Plus e1) (car e1)) 
		   condition
		   ) ;; we know this matches. bind it
	     (matchplus (cdr p) 
			(set-minus e 
					e1
					;;(if (atom e1) (list e1) e1)
					))) ;; we try this match
	     (return-from matchplus 'True)
	   (spopframe env)
	   ))))

;; the above program, if debugged, would work. A better version
;; would not generate all of combs and rems first :)
;; need to do better blanksequence etc matching.

;; 12/27/2010 RJF

	     




	


	     


;; list of permutations. 

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

;; these are not part of the matcher, but they are often used in
;; the matching process.  For the moment, we'll keep them in the
;; same file.
		 
(defun IntegerQ (x)(integerp x))
(defun EvenQ (x)(and (integerp x)(evenp x)))
(defun OddQ (x)(and (integerp x)(oddp x)))
(defun NumberQ(x)(numberp x))

;; have not defined PrimeQ PolynomialQ VectorQ MatrixQ ValueQ OrderedQ UnSameQ

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

;; this is NOT consistent with Mathematica's program, exactly,
;; since 2 numbers of different precision may be SameQ in mma.

;;(defun SameQ(x y) (equal x y))

#+ignore
(defun meval-to-bool(x)(cond((eq x t) t)
			    ((null x) nil)
			    ((eq (meval x) t) t)
			    (t nil) ;; that is, anything not t is untrue
			    ))

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
	  (t    (push  (cons (aref (stack-vars a) i)
	     (aref (stack-vals a) i)) ans))
))     
    ))
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

;; works.

(trial '(Plus a b) '(Plus b a))
(trial '(Pattern ww (Blank)) '(f x y))
(trial '(f (Pattern ww (Blank)) y) '(f x y))


(trial '(f (Pattern a (Blank)) (Condition (Pattern b (Blank)) (Comparison b Greater a)))
       '(f 5 4))  ; correct, fails

(trial '(f (Pattern a (Blank)) (Condition (Pattern b (Blank)) (Comparison b Greater a)))
       '(f  4 5)) ; correct, success.



(trial '(f (Alternatives a b) b) 
       '(f a b))
(trial '(f (Alternatives a b) b) 
       '(f b b))

;; 3rd arg is equal to the first or second arg
(trial '(f (Pattern a (Blank)) (Pattern b (Blank)) 
	 (Alternatives (Pattern a (Blank))  (Pattern b (Blank))))
       '(f x y y))

(trial '(f (Pattern a (Blank)) (Pattern b (Blank)) 
	 (Alternatives (Pattern a (Blank))  (Pattern b (Blank))))
       '(f x y x))

(trial '(f 
	 (Alternatives (Pattern a (Blank))  (Pattern b (Blank)))
	 (Pattern a (Blank))
	 (Pattern b (Blank)))
       '(f x x y)) ;succeeds, but by accident





(trial '(f (Blank) (Blank)) '(f 1 2)) 

(trial '(f (Blank Integer) (Blank)) '(f 1 2)) 

(trial '(f (Blank Symbol) (Blank)) '(f x 2)) 
(trial '(f (Pattern x (BlankNullSequence Symbol))) '(f x y)) ;matches with x= Sequence[x,y]
(trial '(f (Pattern x (BlankNullSequence Symbol))) '(f x 2)) ;fails as it should















(trial '(f (Pattern x (BlankNullSequence Integer) )2) '(f 1 2)) ;x= 1
(trial '(f (Pattern x (BlankNullSequence))) '(f 1 y)) ;matches with x= Sequence[1,y]

(trial '(f (Pattern x (BlankSequence Symbol))) '(f x y)) ;matches with x= Sequence[x,y]

(trial '(f (Pattern a (BlankSequence))) '(f 1 2)) 

(trial '(f (Pattern a (BlankSequence)) (Pattern b (BlankSequence))) '(f 1 2)) 
(trial '(f (Pattern a (BlankNullSequence)) (Pattern b (BlankSequence))) '(f 1 2)) 
(trial '(f (Pattern a (BlankNullSequence)) (Pattern b (BlankSequence))) '(f 1 2)) ; a=Sequence[] b=Sequence[1,2].  

(trial '(P (Pattern a (Blank)) (Pattern b (Blank Sin))) '(P x (Sin y)))
;;none of the examples above deal with Orderless.


(tl) SetAttribute[g,Orderless]
     SetAttribute[h,Flat]

(trial '(g (Pattern a (Blank)) (Pattern b (Blank Sin))) '(g x (Sin y)))
(trial '(g (Pattern a (Blank)) (Pattern b (Blank Sin))) '(g  (Sin y) x))

;; also, if h is Flat, then (Pattern a (Blank)) operates like BlankSequence

;; not yet working

(trial '(h (Pattern a (BlankSequence))) '(h x (Sin y)))
(trial '(P (Pattern a (BlankNullSequence))) '(P x (Sin y)))


(trial '(g (Pattern a (BlankSequence)) (Pattern b (BlankSequence))) '(g 1 2)) 

(trial '(f 
	 (Alternatives (Pattern a (Blank))  (Pattern b (Blank)))
	 (Pattern a (Blank))
	 (Pattern b (Blank)))
       '(f y x y)) ;fails


|#



