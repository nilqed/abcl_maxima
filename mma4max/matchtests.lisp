;;; -*- Mode:Common-Lisp; Package:mma; Base:10 -*-

;; tests for pattern matcher in newmatch.lisp 

(defun runtest(h)
  (let ((yescount 0)(nocount 0)(ans nil))
    (labels ((runtest1(h)
	       (cond  ((null h)
		       (return-from runtest (list 'Done 'yes yescount 'no nocount)))
		      ((equal (setf ans (eval (car h)))(cadadr h)) (format t "~s," (incf yescount)))
		      (t (format t "~% ~s: problem with ~s ~%expected ~s, got ~s" yescount (car h)(cadr h) ans)
			 (incf nocount)))
	       (runtest1 (cddr h) )))
      (runtest1 h))))(chash '|g|)
(chash 'g)
(chash '|h|)
(chash 'h)
(chash 'gh)
(|SetAttributes| '|g| '(|List| |Orderless| ))
(|SetAttributes| 'g '(|List| |Orderless| ))
(|SetAttributes| '|h| '(|List| |Flat| ))
(|SetAttributes| 'h '(|List| |Flat| ))
(|SetAttributes| 'gh '(|List| |Flat| |Orderless|))


(defparameter rubit ;; rubi test patterns
  '(
    (trial kp '(|Power| (|Plus| 3 (|Times| 4 (Sin x))) 5))
    '(success ((n . 5) (b . 4) (a . 3)))

    (trial kp '(|Power| (|Plus| 3 (Sin x)) 5))
    '(success ((n . 5) (b . 1) (a . 3)))

    (trial kp ' (|Plus| 3 (Sin x)) )
    '(success ((b . 1) (a . 3) (n . 1)))
    
    (trial kp '  (Sin x))
    '(success ((a . 0) (b . 1) (n . 1)))
    
    (trial kp '(|Power| (Sin x) 5))
    '(success ((n . 5) (a . 0) (b . 1)))

    (trial '(|Times|  (|Optional| (|Pattern| b (|Blank|))) (Sin x)) '(|Times| 4 (Sin x)))
    '(success ((b . 4)))
    (trial '(|Times|  (|Optional| (|Pattern| b (|Blank|))) (Sin x)) ' (Sin x))
    '(success ((b . 1)))
    (trial '(|Plus| (|Optional| (|Pattern| a (|Blank|))) x) '(|Plus| 4 x))
    '(success ((a . 4)))
    (trial '(|Plus| (|Optional| (|Pattern| a (|Blank|))) x) 'x)
    '(success ((a . 0)))

    (trial kq '(|Power| (|Times| (|Plus| 5 (|Times| 6 x)) (|Plus| 3 (|Times| 4 x)))   -1))
    '(success ((d . 4) (c . 3) (b . 6) (a . 5)))
    
    (trial kq '(|Power| (|Times|  x  (|Plus| 3 (|Times| 4 x)))    -1))
    '(success ((d . 4) (c . 3) (a . 0) (b . 1)))

    (trial kq '(|Power| (|Times|  x  (|Plus| 3 x))    -1))
    '(success ((d . 1) (c . 3) (a . 0) (b . 1)))
    ;; the pattern u_.*(a_.+b_.*x_^m_.)^p_.*(c_.+d_.*x_^n_.)^q_.
    (trial '(|Times| (|Optional| (|Pattern| u (|Blank|)))
		   (|Power| (|Plus| (|Optional| (|Pattern| a (|Blank|)))
				(|Times| (|Optional| (|Pattern| b (|Blank|)))
				       (|Power| (|Pattern| x (|Blank|))
					      (|Optional| (|Pattern| m (|Blank|))))))
			  (|Optional| (|Pattern| p (|Blank|))))
		   (|Power| (|Plus| (|Optional| (|Pattern| c (|Blank|)))
				(|Times| (|Optional| (|Pattern| d (|Blank|)))
				       (|Power| (|Pattern| x (|Blank|))
					      (|Optional| (|Pattern| n (|Blank|))))))
			  (|Optional| (|Pattern| q (|Blank|))))) 
     ;;  the expression  4*(5+6*x^(-2))^3*(9+10*x^2)^(-3)
	   '(|Times| 4 (|Power| (|Plus| 5 (|Times| 6 (|Power| x -2))) 3)
		   (|Power| (|Plus| 9 (|Times| 10 (|Power| x 2))) -3)))
    '(success
      ((q . -3) (n . 2) (d . 10) (c . 9) (p . 3) (m . -2) (x . x) (b . 6) (a . 5) (u . 4)))
    ))

(defun showtest(h)
  (let ()
    (labels ((runtest1(h)
	       (cond  ((null h)(return-from showtest 'Done))
		 		 
		      
		      (t   (dispb (list '|List| (list '|Matchpat| (cadar(cdar h))
						    (cadr(cadr(cdar h))))  
					;(format nil " --> ~s"  (eval (car h)))
					))))
	       (runtest1 (cddr h) )))
      (runtest1 h))))

(defparameter
  tests 
  '(
    (trial '(f (|Alternatives| a b) b)  '(f a b)) '(success nil)
    (trial '(f (|Alternatives| a b) b)  '(f b b)) '(success nil)
    ;; 3rd arg is equal to the first or second arg
    (trial '(f (|Pattern| a (|Blank|)) (|Pattern| b (|Blank|)) 
	       (|Alternatives| (|Pattern| a (|Blank|)) (|Pattern| b (|Blank|))))
	   '(f x y y)) '(success ((b . y) (a . x)))
    (trial '(f (|Pattern| a (|Blank|)) (|Pattern| b (|Blank|)) 
	       (|Alternatives| (|Pattern| a (|Blank|)) (|Pattern| b (|Blank|))))
	   '(f x y x)) '(success ((b . y) (a . x)))
    (trial '(f 
	     (|Alternatives| (|Pattern| a (|Blank|)) (|Pattern| b (|Blank|)))
	     (|Pattern| a (|Blank|))
	     (|Pattern| b (|Blank|)))
	   '(f x x y)) '(success ((b . y) (a . x)))
    (trial '(f (|Blank|) (|Blank|)) '(f 1 2)) '(success nil)
    (trial '(f (|Pattern| ww (|Blank|)) y) '(f x y)) '(success ((ww . x)))
    (trial '(f (|Blank| |Integer|) (|Blank|)) '(f 1 2)) '(success nil)
    (trial '(f (|Blank| |Symbol|) (|Blank|)) '(f x 2)) '(success nil)
    (trial '(f (|Pattern| x (|BlankSequence|))) '(f a b c)) '(success ((x |Sequence| a b c)))
    (trial '(f (|Pattern| x (|BlankNullSequence| |Symbol|))) '(f x y)) '(success ((x |Sequence| x y)))
    (trial '(|Pattern| a (|Pattern| b (|Blank|))) 'xxx) '(success ((b . xxx) (a . xxx)))
    (trial '(f (|Pattern| x (|BlankNullSequence| |Symbol|))) '(f x 2)) '(failure)
    (trial '(f (|Pattern| x (|BlankNullSequence| |Integer|) )2) '(f 1 2)) '(success ((x . 1)))
    (trial '(f (|Pattern| x (|BlankNullSequence|))) '(f 1 y)) '(success ((x . (|Sequence| 1 y))))

    ;; (trial '(f (|Pattern| x (|BlankSequence| |Symbol|))) '(f x y)) ;matches with x= Sequence[x,y]

    (trial '(f (|Pattern| a (|BlankSequence|))) '(f 1 2)) '(success ((a |Sequence| 1 2)))

    (trial '(f (|Pattern| a (|BlankSequence|)) (|Pattern| b (|BlankSequence|))) '(f 1 2)) '(success ((b . 2) (a . 1)))

    (trial '(f (|Pattern| a (|BlankNullSequence|)) (|Pattern| b (|BlankSequence|))) '(f 1 2)) '(success ((b |Sequence| 1 2) (a |Sequence|)))

    (trial '(P (|Pattern| a (|Blank|)) (|Pattern| b (|Blank| Sin))) '(P x (Sin y))) '(success ((b Sin y) (a . x)))
    (trial '(|Pattern| a (f (|Pattern| b (|Blank|)))) '(f c)) '(success ((b . c) (a f c)))
    ;;none of the examples above deal with Orderless.

    ;; (trial '(|Plus| a b) '(|Plus| b a)) '(success nil)
    ;; g is Orderless
 
    (trial '(w (|Pattern| a (|Blank|)) (|Pattern| b (|Blank| Sin))) '(w x (Sin y))) '(success ((b Sin y) (a . x)))
    (trial '(f (|Pattern| x (|Blank|))) '(f a b c)) '(failure) ;; if f is not flat
    (trial '(f (|Pattern| x (|BlankSequence|))) '(f a b c)) '(success ((x |Sequence| a b c)))
    (trial '(f (|Pattern| x (|BlankNullSequence|))) '(f a b c)) '(success ((x . (|Sequence| a b c))))
  
    (trial '(gh (|Pattern| a (|Blank|)) (|Pattern| b (|Blank| Sin))) '(gh (Sin y) x)) '(success ((b Sin y) (a . x )))
    (trial '(gh (|Pattern| a (|BlankNullSequence|)) (|Pattern| b (|Blank| Sin))) '(gh (Sin y))) ; gh is flat and orderless
    '(success ((b Sin y) (a |Sequence|))) ;; maybe should be (success ((b Sin y) (a gh))) ?
    (trial '(f 
	     (|Alternatives| (|Pattern| a (|Blank|)) (|Pattern| b (|Blank|)))
	     (|Pattern| a (|Blank|))
	     (|Pattern| b (|Blank|)))
	   '(f y x y)) 
    '(success ((a . x) (b . y)))
    (trial '(f 
	     (|Alternatives| (|Pattern| a (|Blank|)) (|Pattern| b (|Blank|)))
	     (|Pattern| a (|Blank|))
	     (|Pattern| b (|Blank|)))
	   '(f x x y))
    '(success ((b . y) (a . x)))

    ;; also, if h is Flat, then (|Pattern| a (|Blank|)) operates like |BlankSequence|
    (trial '(q (|Pattern| a (|Blank|)) (|Pattern| b (|Blank|))) '(q 1 2)) '(success ((b . 2) (a . 1)))
    (trial '(q (|Pattern| a (|Blank|)) (|Pattern| a (|Blank|))) '(q 1 2)) '(failure) ; should fail
    ;; sticky assignment below
    (trial '(|Pattern| x (f (|Pattern| y (|Blank| z)))) '(f(z 3)))'(success ((y z 3) (x f (z 3))))
    (trial '(f a (|Optional| (|Pattern| x (|Blank|)) 0)) '(f a)) '(success ((x . 0)))
    (trial '(f  (|Optional| (|Pattern| x (|Blank|)) 0) a) '(f a))'(success ((x . 0)))

    (trial '(f (|Optional| (|Pattern| x(|Blank|))0 ) a)'(f xx a)) '(success ((x . xx)))
    (trial '(w (|Pattern| a (|BlankSequence|))) '(w x (Sin y))) '(success ((a |Sequence| x (Sin y))))
    (trial '(P (|Pattern| a (|BlankSequence|))) '(P x (Sin y))) '(success ((a |Sequence| x (Sin y))))
    (trial '(P (|Pattern| a (|BlankNullSequence|))) '(P x y z)) '(success ((a |Sequence| x y z)))
    (trial '(f (|Pattern| a (|BlankSequence|)) (|Pattern| b (|BlankSequence|))) '(f 1 2 3 4))
    '(success ((b |Sequence| 2 3 4) (a . 1)))

    (trial '(f (|Pattern| a (|BlankSequence|)) (|Pattern| b (|BlankSequence|))) '(f 1 2)) '(success ((b . 2) (a . 1)))
    (trial '(q (|Pattern| a (|BlankSequence|)) (|Pattern| a (|BlankSequence|))) '(q 1 2)) '(failure)
    ;; g is Orderless
    (trial '(g a b)'(g a b)) '(success nil)
    ;;(trial '(g (|Pattern| x (|Blank|)) a) '(g b a)) '(success ((x . b)))
    (trial '(w (|Pattern| x (|Blank|)) a) '(w b a)) '(success ((x . b)))
    (trial '(g a b (|Pattern| x (|Blank|))) '(g a c b )) '(success ((x . c))) ; g is orderless
    (trial '(gh a b (|Pattern| x (|Blank|))) '(gh a c b d)) '(success ((x gh c d)))
 
    (trial '(f a (|Pattern| x (|Blank|))b ) '(f a c b )) '(success ((x . c)))
    (trial '(ff (|Pattern| x (|BlankSequence|)) b) '(ff b a)) '(failure)
    (trial '(ff (|Pattern| x (|BlankSequence|)) b) '(ff a b)) '(success ((x . a))) 
    (trial '(ff (|Pattern| x (|BlankSequence|)) b) '(ff a c b)) '(success ((x |Sequence| a c)))
    (trial '(ff a (|Pattern| x (|BlankSequence|)) b) '(ff a c b)) '(success ((x . c)))
    (trial '(ff a (|Pattern| x (|BlankSequence|)) (|Pattern| x (|BlankSequence|)) b) '(ff a c d c d b)) '(success ((x |Sequence| c d)))
    (trial '(f (|Pattern| a (|BlankNullSequence|)) (|Pattern| b (|BlankSequence|))) '(f 1 2 3 4))
    '(success ((b |Sequence| 1 2 3 4) (a . (|Sequence| ))))
    (trial '(r (|Pattern| x (|Blank|)) (s (|Pattern| x (|Blank|)))) '(r a ( s a))) '(success ((x . a)))
    (trial '(r (|Pattern| x (|BlankSequence|)) (s (|Pattern| x (|BlankSequence|)))) '(r a b ( s a b)))
    '(success ((x |Sequence| a b)))
    (trial '(r (|Pattern| x (|BlankNullSequence|)) (s (|Pattern| x (|BlankNullSequence|)))) '(r ( s )))
    '(success ((x |Sequence|)))
    (trial '(f (|Optional| (|Pattern| x(|Blank|))0 ) a)'(f a)) '(success ((x . 0)))
    ;; (trial '(g (|Pattern| x (|Blank|)) (f (|Pattern| x (|Blank|)))) '(g a (f a))) '(success ((x . a))) 
    (trial '(w (|Pattern| x (|Blank|)) (f (|Pattern| x (|Blank|)))) '(w a (f a))) '(success ((x . a))) 
    ;;(trial '(g a b)'(g b a))'(success nil) ;g is orderless
    (trial '(w a b)'(w b a))'(failure)
    (trial '(w (|Except| b)) '(w a)) '(success nil)
    (trial '(w (|Except| b)) '(w b)) '(failure )
    (trial '(|Times| a b)'(|Times| b a)) '(success nil)
    (trial '(g a b (|Pattern| x (|Blank|))) '(g a c b )) '(success ((x . c)))
    (trial '(|Times| a b c (|Pattern| x (|Blank|))) '(|Times| a c b )) '(success ((x . 1)))
    (trial '(|Plus| a b c (|Pattern| x (|Blank|))) '(|Plus| a c b )) '(success ((x . 0)))
    (trial '(|Plus| a b c (|Times| (|Pattern| x (|Blank|)))) '(|Plus| a c b (|Times| 3 z))) '(success ((x |Times| 3 z))) (trial '(|Plus| a b c (|Times| d e (|Pattern| x (|Blank|)))) '(|Plus| a c b (|Times| d e)))
    '(success ((x . 1)))
    (trial '(|Plus| a b c (|Times| d e (|Pattern| x (|Blank|)))) '(|Plus| a c b (|Times| d e f g)))
    '(success ((x |Times| f g)))
    (trial '(|Times| (|Plus| d YY) (|Plus| e ZZ)) '(|Times|(|Plus| d YY) (|Plus| e ZZ))) '(success nil)
 
    #+ignore (trial '(|Times| 
		      (|Pattern| x1 (|Plus| d (|Pattern| y (|Blank|))))
		      (|Pattern| x2 (|Plus| e (|Pattern| z (|Blank|)))))
		    '(|Times| (|Plus| d YY)(|Plus| e ZZ)))
    ;; should be    '(success ((z . ZZ) (x2 . (|Plus| e ZZ)) (y . YY) (x1 . (|Plus| d YY))))
    ;; rather than  '(success ((z . ZZ) (x2 . ZZ) (y . YY) (x1 . YY))), now returned

    (trial '(|Times| 
	     (|Plus| d (|Pattern| y (|Blank|)))
	     (|Plus| e (|Pattern| z (|Blank|))))
	   '(|Times| (|Plus| d YY)(|Plus| e ZZ)))
    '(success ((z . ZZ) (y . YY)))
   
    (trial '(|Times| 
	     (|Plus| d YY)
	     (|Plus| e ZZ)) '(|Times| (|Plus|  YY d) (|Plus| e ZZ)))
    '(success nil)
 
					; (trial '(w (|Pattern| x1 A) B) '(w A C)) '(failure) ?? legal??
    (trial '(|Plus| a (Sin a)) '(|Plus| (Sin a) a)) '(success nil)
    (trial '(|Plus| (|Pattern| x (|Blank|)) (Sin a)) ' (Sin a)) '(success ((x . 0)))
    (trial '(g a b)'(g b a))'(success nil) ;g is orderless
    (trial '(|Plus| (|Pattern| x1 (|BlankSequence|)) ) '(|Plus| e f)) '(success ((x1 . (|Plus| e f))))
    (trial '(|Plus| a (|Pattern| x (|Blank|))) '(|Plus| a b)) '(success ((x . b)))
    (trial '(|Plus| a (|Pattern| x (|Blank|))) '(|Plus| a b c )) '(success ((x |Plus| b c)))
    (trial '(|Plus| b (|Pattern| x (|Blank|))) '(|Plus| a b c )) '(success ((x |Plus| a c)))
    (trial '(|Plus| a b (|Pattern| z (|Blank|))) '(|Plus| a b)) '(success ((z . 0)))
   
    (trial '(|Plus| a b (|Pattern| z (|BlankNullSequence|))) '(|Plus| a b)) '(success ((z |Sequence|)))
    (trial '(|Plus| a b (|Pattern| z (|BlankSequence|))) '(|Plus| a b)) '(failure)
    (trial '(|Plus| a (|Pattern| z (|Blank|))) '(|Plus| a)) '(success ((z . 0))) ;not simplified.
    (trial '(|Plus| a (|Pattern| x (|Blank|))) 'a) '(success ((x . 0))) ;;?? mma would fail. 
    (trial '(|Plus| a b (|Pattern| z (|BlankNullSequence|))) '(|Plus| a b)) '(success((z |Sequence|)))

 
    (trial '(|Plus| a b (|Pattern| z (|Blank|))) '(|Plus| a b )) '(success ((z . 0)))
    (trial '(ff (|Pattern| x (|BlankSequence|)) (|Pattern| y (|BlankSequence|)) (|Pattern| x (|BlankSequence|)) 	  ) '(ff a b a)) '(success ((y . b) (x . a)))
    (trial '(ff (|Pattern| x (|BlankSequence|)) (|Pattern| y (|BlankNullSequence|)) (|Pattern| x (|BlankSequence|)) 	  ) '(ff a a))'(success ((y |Sequence|) (x . a)))
    (trial '(ww (|Times| a c ) (|Times| b (|Pattern| y (|Blank|)))) '(ww (|Times| a c)(|Times| b d)))
    '(success ((y . d)))
    (trial '(ww (|Times| a (|Pattern| x (|Blank|))) (|Times| b (|Pattern| y (|Blank|)))) '(ww (|Times| a c)(|Times| b d))) '(success ((y . d) (x . c)))
    (trial '(ww (|Times| a (|Pattern| x (|Blank|))) (|Times| b (|Pattern| y (|Blank|)))) '(ww (|Times| a r s)(|Times| b t u)))  '(success ((y |Times| t u) (x |Times| r s)))
    (trial '(ww (|Times| a (|Pattern| x (|Blank|))) (|Times| b (|Pattern| y (|Blank|))) (|Pattern| z (|BlankNullSequence|))) '(ww (|Times| a r s)(|Times| b t u) 34 35))  '(success ((z |Sequence| 34 35) (y |Times| t u) (x |Times| r s)))
    ;; (trial '(|Plus| (|Times| a c ) (|Times| b (|Pattern| y (|Blank|)))) '(|Plus| (|Times| a c)(|Times| b d)))
    ;;(trial '(|Plus| (|Times| a (|Pattern| x (|Blank|))) (|Times| b (|Pattern| y (|Blank|)))) '(|Plus| (|Times| a r s)(|Times| b t u)))
   
    (trial '(|Plus|   (|Times| b (|Pattern| y (|Blank|))) a) '(|Plus| a (|Times| b d)))
    '(success ((y . d)))
    (trial '(|Plus|   (|Times| b (|Pattern| y (|Blank|)))    (|Pattern| x (|Blank|))) '(|Plus| a (|Times| b d)))
    ;; b*y_+x_   a+b*d
    '(success ((x . a) (y . d)))
    (trial '(|Plus|   (|Times| b (|Pattern| y (|Blank|)))    (|Pattern| x (|Blank|))) '(|Plus| (|Times| a c) (|Times| b d)))
    '(success ((x |Times| a c) (y . d)))
    (trial '(|Plus|   (|Times| b (|Pattern| y (|Blank|)))    (|Times| a (|Pattern| x (|Blank|)))) '(|Plus| (|Times| a c) (|Times| b d)))
    '(success ((x . c) (y . d)))
    (trial '(|Plus|   (|Times| b (|Pattern| x (|Blank|)))    (|Times| a (|Pattern| x (|Blank|)))) '(|Plus| (|Times| a c) (|Times| b d)))
    '(failure) ;; should fail, not '(success ((x . c) (x . d)))
    (trial '(|Plus| (|Pattern| x1 (|BlankNullSequence|)) d e) '(|Plus| a b c d e)) '(success ((x1 |Plus| a b c)))
    (trial '(|Plus| a (|Pattern| x1 (|BlankNullSequence|)) c) '(|Plus| a b c)) '(success ((x1 . b)))
   
    (trial '(|Plus| (|Pattern| x1 (|BlankSequence|)) c d
	     (|Pattern| x2 (|BlankNullSequence|)))
	   '(|Plus| a b c d e))
    '(success ((x2 |Plus| b e) (x1 . a)))
    ;; see bns instead of bs..
    (trial '(|Plus| (|Pattern| x1 (|BlankNullSequence|)) c d (|Pattern| x2 (|BlankNullSequence|))) '(|Plus| a b c d e))   '(success ((x2 |Plus| a b e) (x1 |Sequence|)))
   
    (trial '(g a b (|Pattern| x (|BlankSequence|))) '(g a c b ))'(success ((x . c)))
    (trial '(|Plus| (|Pattern| x1 (|BlankNullSequence|)) b d) '(|Plus| a b c d e)) '(success ((x1 |Plus| a c e)))
    (trial '(|Plus| (|Pattern| x (|Blank|)) (|Pattern| y (|Blank|)) (|Pattern| x (|Blank|)) 	  ) '(|Plus| a b a)) '(success ((y . b) (x . a)))
    (trial '(|Plus| (|Pattern| x (|Blank|)) (|Pattern| y (|BlankSequence|)) (|Pattern| x (|Blank|)) 	  ) '(|Plus| a b b b a))
    '(success ((y |Plus| b b b) (x . a)))
    (trial '(|Plus| (|Pattern| x (|Blank|)) (|Pattern| y (|BlankSequence|)) (|Pattern| x (|Blank|)) 	  ) '(|Plus| a b b b a c))
    '(success ((y |Plus| b b b c) (x . a)))
    ;;   (trial '(h (|Pattern| x (|Blank|))) '(h a b c)) '(success ((x . (h a b c)))) ;; if h is flat
    (trial '(gh (|Pattern| x (|Blank|))) '(gh a b c)) '(success ((x . (gh a b c)))) ;; if gh is flat & orderless
    (trial '(gh (|Pattern| x (|Blank|)) b) '(gh b a)) '(success ((x . a)))
    (trial '(|Plus| x (|PatternTest| (|Pattern| y (|Blank|)) |OddQ|))    '(|Plus| x 3))
    '(success ((y . 3)))
    (trial '(|Plus| x (|PatternTest| (|Pattern| y (|Blank|)) |OddQ|))    '(|Plus| x 4))
    '(failure)
   
    (trial '(|Plus|  (|PatternTest| (|Pattern| y (|Blank|)) |OddQ|) 
	     (|PatternTest| (|Pattern| z (|Blank|)) EvenQ))
	   '(|Plus|  3 4))
    '(success ((z . 4) (y . 3)))
   
    (trial '(|Plus|  (|Pattern| y (|Blank| |Integer|))
	     (|Pattern| z (|Blank|)))
	   '(|Plus|  3  x y ))
    '(success ((z |Plus| x y) (y . 3)))
    (trial '(|Plus| (|Pattern| x (|Blank|)) (Sin (|Pattern| x (|Blank|)))) '(|Plus| (Sin a) a))
    '(success ((x . a)))
    (trial '(|Plus| (|Pattern| x (|BlankSequence|)) (|Pattern| y (|BlankSequence|)) 
	     (|Pattern| x (|BlankSequence|)) )    '(|Plus| a b c a b))
    '(success ((y |Plus| b c b) (x . a)))

   
    (trial '(f a b (|Power| x (|Optional| (|Pattern| n (|Blank|)) 1)) c) '(f a b (|Power| x 4) c)) '(success ((n . 4)))
    (trial '(f a b (|Power| x (|Optional| (|Pattern| n (|Blank|)))) c) '(f a b (|Power| x 4) c)) '(success ((n . 4)))
    (trial '(|Times| (|Optional| (|Pattern| n (|Blank|))) q) 'q) '(success ((n . 1)))
    (trial '(|Times| (|Optional| (|Pattern| n (|Blank|)) 1) q r ) '(|Times| q r))'(success ((n . 1))) ;explicit default
    (trial '(|Times| (|Optional| (|Pattern| n (|Blank|))) q r ) '(|Times| q r))'(success ((n . 1))) ;implicit default
    (trial '(|Plus| (|Optional| (|Pattern| n (|Blank|)))  r ) 'r) '(success ((n . 0)))
    (trial '(f a b (|Power| x (|Optional| (|Pattern| n (|Blank|)) 1)) c) '(f a b x c)) '(success ((n . 1)))
    (trial '(|Plus| a b (|Power| x (|Optional| (|Pattern| n (|Blank|)) 1)) c) '(|Plus| a x c b)) '(success ((n . 1)))
    (trial '(|Plus| a b (|Power| x (|Optional| (|Pattern| n (|Blank|)) 1)) c) '(|Plus| a b (|Power| x 5)  c)) '(success ((n . 5)))
    (trial '(|Plus|   (|Times| b (|Pattern| y (|Blank|))(|Pattern| z (|Blank|)))    (|Times| a (|Pattern| x (|Blank|)))) '(|Plus| (|Times| a c) (|Times| b d gg)))
    '(success ((x . c) (z . gg) (y . d)))
    (trial '(|Plus| (|Pattern| x (|BlankSequence|)) (|Pattern| y (|BlankSequence|)) (|Pattern| x (|BlankSequence|)) 	  ) '(|Plus| a b a))
    '(success ((y . b) (x . a)))
    (trial '(|Plus| (|Pattern| x (|Blank|)) (|Pattern| y (|BlankSequence|)) ) '(|Plus| a b a)) '(success ((y |Plus| b a) (x . a)))
    (trial '(|Plus| (|Pattern| x (|Blank|)) (|Pattern| y (|Blank|)) (|Pattern| x (|Blank|)) 	  ) '(|Plus| a b a))
    '(success ((y . b) (x . a)))
    (trial '(w (Cos (|Pattern| x (|Blank|))) (Sin (|Pattern| x (|Blank|)))) '(w (Cos a) (Sin a) )) '(success (( x . a)))
   
    (trial '(|Plus| (Cos (|Pattern| x (|Blank|))) (Sin (|Pattern| x (|Blank|)))) '(|Plus| (Cos a) (Sin a) )) '(success ((x . a)))
   
    (trial '(w (|Pattern| x (|BlankSequence|))(|Pattern| x (|BlankSequence|))) '(w 1 2 3 1 2 3)) '(success ((x |Sequence| 1 2 3)))
    (trial '(gh (|Pattern| x (|BlankSequence|))(|Pattern| x (|BlankSequence|))) '(gh 1 2 3 1 2 3))'(success ((x gh 1 2 3)))
    (trial '(gh (|Pattern| x (|BlankSequence|))(|Pattern| x (|BlankSequence|))) '(gh 1 1 2 2 3 3))'(success ((x gh 1 2 3)))
   
    (trial '(|Pattern| p (|Blank| |Rational|)) 3/4) '(success ((p . 3/4)))
    (trial '(|Rational| (|Pattern| n (|Blank|)) (|Pattern| d (|Blank|))) 3/4) '(success ((d . 4) (n . 3)))
    (trial '(|Complex| (|Pattern| r (|Blank|)) (|Pattern| i (|Blank|))) #c(3 4)) '(success ((i . 4) (r . 3)))
   
    (trial '(|Pattern| ww (|Blank|)) '(f x y))
    '(success ((ww f x y)))

    (trial '(f (|Pattern| a (|Blank|)) (|Condition| (|Pattern| b (|Blank|)) (|Comparison| b |Greater| a)))
	   '(f 5 4))  '(failure)

    (trial '(f (|Pattern| a (|Blank|)) (|Condition| (|Pattern| b (|Blank|)) (|Comparison| b |Greater| a))) '(f 4 5))
    '(success ((b . 5) (a . 4)))

   
    ))
(defparameter tests2
  '(
    (trial '(ff (|Pattern| x (|BlankSequence|)) (|Pattern| y (|BlankSequence|)) (|Pattern| x (|BlankSequence|))	  ) '(ff a a b b a a))
    '(success((x . a) (y . (|Sequence| a b b a))))
    ;; now is just '(success((x . a))) or '(failure)

    ))
(defparameter ftests
 '(
   (trial '(ff (|Pattern| x (|BlankSequence|)) (|Pattern| y (|BlankNullSequence|)) (|Pattern| x (|BlankSequence|)) 	  )
	  '(ff a YY a)) '(success ((x . a)(y . YY)))
;;; well, (Plus a b a) is not simplified...
      (trial '(|Plus| (|Pattern| x (|Blank|)) (|Pattern| y (|BlankSequence|)) ) '(|Plus| a b a))   '(success ((y |Sequence| b a) (x . a)))

 
 ))