;;; -*- Mode:Common-Lisp; Package:mma; Base:10 -*-
;;; version 15 does not include
;;; flat+orderless. or Optional. or Condition-on-replacement

;; not including some newer stuff, e.g. in version 6,0, specifically,
;; PatternSequence, Longest, Shortest, Repeated (? modified) 12/2010 RJF

;; note: I'm equivocating somewhat on the use of 'True and 'False
;; as Boolean values in Mma. Problem: most lisp functions naturally
;; use nil for false, and "anything else" but sometimes just 't
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
;; the parser, I suppose, or maybe abandon the use of lisp atom space
;; for global name space.

;;(provide 'match)
;;(require "stack1")
(in-package :mma)

;;; definition: a patternobject is like an expression but it can
;;; have (Pattern ...) or (Blank ..) subexpressions in it.

;;; All that matches in version 16
;;; (a) structurally identical matches.
;;; (b) (Blank) matches <anything>
;;;     (Blank foo) matches anything with a car of foo or
;;;     in the case of a match to an atom x, it will match
;;;     if (typep x foo) is true, foo is in {Integer, Rational ..?}

;;; (c) (Pattern x <patternobject>) matches whatever would
;;;   otherwise be matched by <patternobject> but  binds the value of
;;;   patternobject to the name x.  From that time onward during the match
;;;   x will only match identical items.

;;; That is, f[x_,x_] will match f[3,3] but not f[3,4].
;;; f[x_,x_^2] will match f[a,a^2].  But not f[3,9]. (sorry).

;;; (d)  (BlankSequence ...) or __ inside (non-Orderless) functions. Note:
;;;  If f is "Flat" then f[a,x_] matches f[a,z,w]
;;;   with x-> f[z,w]. 
;;;  If f is "Flat" then f[b,x__] matches f[b,z,w]
;;;   with x-> (z,w) .  That's Sequence[z,w]. Look it up...

;;; (e) (BlankNullSequence ...) or ___ inside (non-Orderless) functions.

;;; (f) Orderless functions are matched only if they have a fixed
;;; number of arguments.

;;; (g) (PatternTest <patternobj> Test) matches whatever <patternobj> would
;;; match, but only if (Test <patternobj>) returns lisp t.  Question:
;;; perhaps we should ask that (meval (list Test <patternobj>)) be True not t?
;;; we keep to t.

;;; g[x__,x__] matches g[1,2,3,1,2,3]  with x->(1,2,3)

;;; Some Flat functions are Plus, Times.  There are hardly any
;;; others.  Plus is also orderless (see below) complicating
;;; the matching considerably.  

;;; Functions which are both Flat and Orderless functions are
;;; not handled by this version.


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

(defun trial(pat exp &optional (env (make-stack :size 20) wasitprovided))
  (spushframe env 'trialmatch);; make a frame to store results of match
  
  (if  (m1 pat exp) ;; m1 is the matching program. env is global, result bindings.
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

;;note that  w_ /;  > 3 -> aha is something like
;;(Rule  (Condition  (Pattern   w   (Blank))  (Comparison   w   Greater   3))
;;       aha)

;; w_?(w>3)->hah is
;;(Rule (PatternTest  (Pattern   w   (Blank))  (Comparison   w   Greater   3))
;;           hah)


;;  ff[xx_] + rr_ :> gg[xx] + rr /; OddQ[xx]
;;(RuleDelayed (Plus (ff (Pattern xx (Blank))) (Pattern rr (Blank)))
;;   (Condition (Plus (gg xx) rr) nil))

;; this rule works to back up over the pattern match.


;;; 12/2010

(defun match (pat exp)
  (spushframe env 'match)
  (m1 pat exp)) ;;won't work for checking extra conditions. then use mlist.

(defun matchtest(pat exp) ;; test for match, but do not leave bindings around
  (spushframe env 'match)
  (prog1  (m1 pat exp)
    (spopframe env))) ;;won't work for checking extra conditions. then use mlist.
  

(defun matchlist(pl el)  ;;alternative top level for simultaneous matches
  (spushframe env 'matchlist)
  (mlist pl el nil t))

(defvar phead nil)

(defun m1 (p e)
  (cond ((atom p)(equal p e)) 
	((eq p e) t)
	((equal p e) t) ;; remove this if it isnt saving time..
	(t (let ((phead (car p)))
	     (declare (special phead))
	     ;;(format t "~%phead = ~s" phead)
	     (cond ((eq phead 'Blank)(mblank (cdr p) e)) ;do Blank matching
		   ((eq phead 'BlankSequence)
						    ;(print '*)
		    (mblank (cdr p) e))
		   
		   ((eq phead 'Pattern)

		    ;; (cadr p) is the name to be used for the
		    ;; pattern variable if the pattern object (caddr p)
		    ;; matches the expression e.
		    (mpat (cadr p) (caddr p) e))
		   ((eq phead 'PatternTest)
		    (let ((result (m1 (cadr p) e)))
		      (cond (result
			     (funcall (caddr p)(meval e)))
			    (t nil))))
		   ((eq phead 'Condition)
		    (let ((result (m1 (cadr p) e)))
		      (cond ((and result 
			      (meval (caddr p))) t)
			    (t nil))))
		   ;; try this.. 12/20/2010 RJF
		   ((eq phead 'Alternatives) 
		    ;; if we change Alternatives to be n-ary, 
		    ;; this still works.
		    (dolist (alt (cdr p) nil) ; range through alternatives.
		      (if (and (m1 alt e)(meval (caddr p))) (return t))))

		   ;;
		   ;;              f[a, b, a] /. f[x_ | y_, x_ | y_, y_] -> aha ; yes.

		   ((member phead '(Repeated RepeatedNull))
		    (error "we didn't implement rule with ~s in  ~s" phead p))
		   ((atom e) nil) ; non-atom, non-blank p can't match atom.
		   ;; now both p and e are expressions.
		   ;; we match (recursively) their heads, and match,
		   ;; in sequence, elements in their cdrs, by mlist.
		   ;; Before calling mlist, we bind a spec variable phead
		   ;; to phead and note properties (Flat, Orderless) of it
		   ;; on spec variables isflat, isorderless, isfol.
		   
		   ((m1 phead (car e)) ;first check that heads match
		    (let* ((isflat (flatp phead))
			   (isorderless (orderlessp phead))
			   (isfol (and isflat isorderless)))
		      ;; if phead is not flat, it is convenient to
		      ;; set it to Sequence when matching __.
		      (if isflat nil (setq phead 'Sequence))
		      
		      (cond(isfol ;;Flat AND Orderless

			    

			    ;; this is not written yet
			    ;; needed for Plus, Times, but not much
			    ;; else other than these important cases.
			    ;; (maybe "Bag"?)
			    (mlistfol (cdr p)(cdr e) 0 (1-(length e))))
			   
			   (isorderless ;; Orderless, but not Flat
			    ;; There are lots of commutative operators,
			    ;; including symmetric ones (e.g. x=y).
			    ;; this version is not quite right. see defun.
			    (mlistol (cdr p)(cdr e) 0 (1-(length e)) condition))
			   
			   (t ;; Flat or not, ordered.
			    ;; we must match in sequence all elements.
			    (mlist (cdr p)(cdr e) nil t)) ;;condition t??
			   ))))))))

;;******makeshift for now..********

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

(defun mblank(headtest e)
  (if headtest (mtypep e (car headtest)) t))
     
(defun mblank2 (headtest e) ;; hardly enough for _ _ or _ _ _.
  (if headtest (mtypep e (car headtest)) t))

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

;;(defum mlist-notfol-noxblanks(pl el name condition)  )


(defun simple-pat-or-constant (x)
  (or (patternnameblank x) ;; just a named pattern, e.g. x_
      (pattern-free x)))		; no pattern at all

(defun pattern-free (x)
  (cond ((atom x) (not (member x '(Pattern Blank Alternatives 
				   Condition Optional ;; do we need these?
				   ) :test 'eq)))
	(t (every #'pattern-free (cdr x)))))
  

(defun mlist(pl el name &optional (condition #'truth))
  (cond ((null pl) 
	 (and(null el) (meval-to-bool condition)));; both must end at the same time to match
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
	((m1 (car pl)(car el))
	 ;; if the cars match, so must the elements of the cdrs
	 (mlist (cdr pl)(cdr el) nil condition))))


#+ignore
(defun mlistfast(pl el condition)
  (and(= (length pl)(length el)) ;;same length
      (every #'m1 pl el) ;;each pattern matches 
      (meval-to-bool condition)		;condition is true
      ))

;; another version, where we look for the simple bindings first.
#+ignore
(defun mlistfast(pl el condition)
  (cond((= (length pl)(length el)) ;;same length
	(map nil #'(lambda(p e)(if (simple-pat-or-constant p)
				   (if (null (m1 p e))
					     (return-from mlistfast nil))))
	     pl el)	;match simple ones first
	(every #'(lambda(p e)(if (not (patternnameblank p)) ;match the rest.
			       (m1 p e) t))
	       pl el)
	(meval-to-bool condition))))

(defun mlistfast(pl el condition &aux pp ee)
  (cond((= (length pl)(length el)) ;;same length
	(map nil #'(lambda(p e)(cond ((patternnameblank p)	;match simple ones first
				      (if (null (m1 p e))
					  (return-from mlistfast nil)))
				     (t (push p pp) ;;save the others.
					(push e ee))))
	     pl el)
	(format t "~%pp=~s, ee=~s~% env=~%~s" pp ee env)
	(if (every #'(lambda(p e)(m1 p e)) 
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
		 
		 (cond ((m1 p (elt el index))
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
		 
		   (cond ((m1 p (elt el index))
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
		 
		 (cond ((m1 p (elt el index))
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


(defun mpat (name patobj e)
  ;; to match (Pattern name patobj) to e, 
  ;; first see if name has a value - if so, test for equality of value to e.
  ;; we assume the user has not done f[x_foo,x_bar] (different tests...)
  ;; Otherwise just bind name to e
  

  (multiple-value-bind (val found)(sfind env name)
		       (cond (found (equal val e))
			     ((m1 patobj e)
					;(spush env name (cadr e))
					;(spush env name (car e))
			      (spush env name e)
					   t)
			     (t nil))))
     
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
					      (meval-to-bool Condition))
					  'True )))
     ;; only one pattern item term remaining to be matched?
     ((null (cdr pl))  (return-from matchpluspe
			 (match (car pl) e Condition)))
      
      ;; more than one pattern term remaing to be matched. p1+p2+..

      (cond 
       ;; case one:  e is not a Plus.
       ((not(eq (Head e) 'Plus))
       ;; must match one of p1, p2, say pk ... against e, 
       ;; and the others of p1, ... against 0;  say  pj_.  which would be pj_.0 given
       ;; identity for Plus is 0.
       ;; pj__ or pj___ are bad form, though mma doesn't forbid them, they don't seem to
       ;; really work, viz:    x /. x+c_.-> f[c] is ok. x /. x+c__->f[c]  returns x.
       ;; yet x+y/. x+c__->f[c] returns f[]
        (dolist (pk (cdr pl) nil) ;; go through items in pl.
	  (if  (match pk e #'(lambda(env) (and (matchpluspe (remove pk p) 0 t)
					       (meval-to-bool Condition))))
		t)))
       ;; case two:  e IS a Plus.
       (t
	(dolist (pk (cdr pl) nil) ;; go through items in pl.
	  
	  (let ((ans (docomb 
		      (e1 (cdr e) nil) ;; go through all combinations
		      (if (match pk (cons 'Plus e1)
				 #'(lambda(env)
				     (and (matchpluspe 
					   (remove pk k :count 1)
					   (meval (cons 'Plus 
							(set-difference (cdr e) e1)))
					   t)
					  (meval-to-bool Condition))))))
		     ))
	    (if ans (return t))))))))


;; need to do spop sometimes??      

(defun match (p e &optional (c #'truth))
  (let* ((hp (Head P))
	 (ha (Attributes hp)))
    
  (cond ((atom p)(equal p e)) 
	((eq p e) t)
	((equal p e) t) ;; remove this if it isnt saving time..
	((eq hp 'Plus)(matchpluspe p e c))
	((eq hp 'Times)(matchtimespe p e c))
	;; the above 2 clauses could be instances of (matchflatorderlesspe p e head identity c)
	;; next, go through the cases
	;; if (Head p) is neither flat nor orderless, match in order.
	;; That is, match the heads and go through the rest.
	((and (not (member 'Flat ha)(member 'Orderless ha)))
	 (match hp (Head e)
		#'(lambda(env)
		    (and (every #'match (rest p)(rest e))
			 (funcall c env))))

	 ;; p is a pattern variable already bound and equal to e
	 *****
	 ;; p is a pattern variable and not bound
	 ;; p is a Cons and (Head p) = (Head e)
	
	 ;; possibly bogus at this point unless called from a Flat head ..
	 ;; p is a blanksequence
	 ;; p is a blanknullsequence
	 ;; 
     
	 ))  ))


(defun matchplus(p e &optional (Condition t))
  (if (null p)(return-from matchplus (if (and (null e)
					      (meval-to-boolean Condition))
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
		   ) ;; we know this matches. bind it
	     (matchplus (cdr p) 
			(set-difference e 
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

(defun meval-to-bool(x)(funcall x env))



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
  