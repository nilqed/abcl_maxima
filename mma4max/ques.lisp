(defparameter ques
    '(List (List (f (Alternatives a b) b) (f a b)) (List (f (Alternatives a b) b) (f b b))
 (List
  (f (Pattern a (Blank)) (Pattern b (Blank))
   (Alternatives (Pattern a (Blank)) (Pattern b (Blank))))
  (f x y y))
 (List
  (f (Pattern a (Blank)) (Pattern b (Blank))
   (Alternatives (Pattern a (Blank)) (Pattern b (Blank))))
  (f x y x))
 (List
  (f (Alternatives (Pattern a (Blank)) (Pattern b (Blank))) (Pattern a (Blank))
   (Pattern b (Blank)))
  (f x x y))
 (List (f (Blank) (Blank)) (f 1 2)) (List (f (Pattern ww (Blank)) y) (f x y))
 (List (f (Blank Integer) (Blank)) (f 1 2)) (List (f (Blank Symbol) (Blank)) (f x 2))
 (List (f (Pattern x (BlankSequence))) (f a b c))
 (List (f (Pattern x (BlankNullSequence Symbol))) (f x y))
 (List (Pattern a (Pattern b (Blank))) xxx)
 (List (f (Pattern x (BlankNullSequence Symbol))) (f x 2))
 (List (f (Pattern x (BlankNullSequence Integer)) 2) (f 1 2))
 (List (f (Pattern x (BlankNullSequence))) (f 1 y))
 (List (f (Pattern a (BlankSequence))) (f 1 2))
 (List (f (Pattern a (BlankSequence)) (Pattern b (BlankSequence))) (f 1 2))
 (List (f (Pattern a (BlankNullSequence)) (Pattern b (BlankSequence))) (f 1 2))
 (List (P (Pattern a (Blank)) (Pattern b (Blank Sin))) (P x (Sin y)))
 (List (Pattern a (f (Pattern b (Blank)))) (f c))
 (List (w (Pattern a (Blank)) (Pattern b (Blank Sin))) (w x (Sin y)))
 (List (f (Pattern x (Blank))) (f a b c))
 (List (f (Pattern x (BlankSequence))) (f a b c))
 (List (f (Pattern x (BlankNullSequence))) (f a b c))
 (List (gh (Pattern a (Blank)) (Pattern b (Blank Sin))) (gh (Sin y) x))
 (List (gh (Pattern a (BlankNullSequence)) (Pattern b (Blank Sin))) (gh (Sin y)))
 (List
  (f (Alternatives (Pattern a (Blank)) (Pattern b (Blank))) (Pattern a (Blank))
   (Pattern b (Blank)))
  (f y x y))
 (List
  (f (Alternatives (Pattern a (Blank)) (Pattern b (Blank))) (Pattern a (Blank))
   (Pattern b (Blank)))
  (f x x y))
 (List (q (Pattern a (Blank)) (Pattern b (Blank))) (q 1 2))
 (List (q (Pattern a (Blank)) (Pattern a (Blank))) (q 1 2))
 (List (Pattern x (f (Pattern z (Blank)))) (f (z 3)))
 (List (f a (Optional (Pattern x (Blank)) 0)) (f a))
 (List (f (Optional (Pattern x (Blank)) 0) a) (f a))
 (List (f (Optional (Pattern x (Blank)) 0) a) (f xx a))
 (List (w (Pattern a (BlankSequence))) (w x (Sin y)))
 (List (P (Pattern a (BlankSequence))) (P x (Sin y)))
 (List (P (Pattern a (BlankNullSequence))) (P x y z))
 (List (f (Pattern a (BlankSequence)) (Pattern b (BlankSequence))) (f 1 2 3 4))
 (List (f (Pattern a (BlankSequence)) (Pattern b (BlankSequence))) (f 1 2))
 (List (q (Pattern a (BlankSequence)) (Pattern a (BlankSequence))) (q 1 2))
 (List (g a b) (g a b)) (List (w (Pattern x (Blank)) a) (w b a))
 (List (g a b (Pattern x (Blank))) (g a c b))
 (List (gh a b (Pattern x (Blank))) (gh a c b d))
 (List (f a (Pattern x (Blank)) b) (f a c b))
 (List (ff (Pattern x (BlankSequence)) b) (ff b a))
 (List (ff (Pattern x (BlankSequence)) b) (ff a b))
 (List (ff (Pattern x (BlankSequence)) b) (ff a c b))
 (List (ff a (Pattern x (BlankSequence)) b) (ff a c b))
 (List (ff a (Pattern x (BlankSequence)) (Pattern x (BlankSequence)) b)
  (ff a c d c d b))
 (List (f (Pattern a (BlankNullSequence)) (Pattern b (BlankSequence))) (f 1 2 3 4))
 (List (r (Pattern x (Blank)) (s (Pattern x (Blank)))) (r a (s a)))
 (List (r (Pattern x (BlankSequence)) (s (Pattern x (BlankSequence))))
  (r a b (s a b)))
 (List (r (Pattern x (BlankNullSequence)) (s (Pattern x (BlankNullSequence))))
  (r (s nil)))
 (List (f (Optional (Pattern x (Blank)) 0) a) (f a))
 (List (w (Pattern x (Blank)) (f (Pattern x (Blank)))) (w a (f a)))
 (List (w a b) (w b a)) (List (w (Except b)) (w a)) (List (w (Except b)) (w b))
 (List (Times a b) (Times b a)) (List (g a b (Pattern x (Blank))) (g a c b))
 (List (Times a b c (Pattern x (Blank))) (Times a c b))
 (List (Plus a b c (Pattern x (Blank))) (Plus a c b))
 (List (Plus a b c (Pattern x (Blank))) (Plus a c b (Times 3 z)))
 (List (Plus a b c (Times d e (Pattern x (Blank)))) (Plus a c b (Times d e)))
 (List (Plus a b c (Times d e (Pattern x (Blank)))) (Plus a c b (Times d e f g)))
 (List (Times (Plus d YY) (Plus e ZZ)) (Times (Plus d YY) (Plus e ZZ)))
 (List (Times (Plus d (Pattern y (Blank))) (Plus e (Pattern z (Blank))))
  (Times (Plus d YY) (Plus e ZZ)))
 (List (Times (Plus d YY) (Plus e ZZ)) (Times (Plus YY d) (Plus e ZZ)))
 (List (Plus a (Sin a)) (Plus (Sin a) a))
 (List (Plus (Pattern x (Blank)) (Sin a)) (Sin a)) (List (g a b) (g b a))
 (List (Pattern x1 (BlankSequence)) (Plus e f))
 (List (Plus a (Pattern x (Blank))) (Plus a b))
 (List (Plus a (Pattern x (Blank))) (Plus a b c))
 (List (Plus b (Pattern x (Blank))) (Plus a b c))
 (List (Plus a b (Pattern z (Blank))) (Plus a b))
 (List (Plus a b (Pattern z (BlankNullSequence))) (Plus a b))
 (List (Plus a b (Pattern z (BlankSequence))) (Plus a b))
 (List (Plus a (Pattern z (Blank))) a) (List (Plus a (Pattern x (Blank))) a)
 (List (Plus a b (Pattern z (BlankNullSequence))) (Plus a b))
 (List (Plus a b (Pattern z (Blank))) (Plus a b))
 (List
  (ff (Pattern x (BlankSequence)) (Pattern y (BlankSequence))
   (Pattern x (BlankSequence)))
  (ff a b a))
 (List
  (ff (Pattern x (BlankSequence)) (Pattern y (BlankNullSequence))
   (Pattern x (BlankSequence)))
  (ff a a))
 (List (ww (Times a c) (Times b (Pattern y (Blank)))) (ww (Times a c) (Times b d)))
 (List (ww (Times a (Pattern x (Blank))) (Times b (Pattern y (Blank))))
  (ww (Times a c) (Times b d)))
 (List (ww (Times a (Pattern x (Blank))) (Times b (Pattern y (Blank))))
  (ww (Times a r s) (Times b t u)))
 (List
  (ww (Times a (Pattern x (Blank))) (Times b (Pattern y (Blank)))
   (Pattern z (BlankNullSequence)))
  (ww (Times a r s) (Times b t u) 34 35))
 (List (Plus (Times b (Pattern y (Blank))) a) (Plus a (Times b d)))
 (List (Plus (Times b (Pattern y (Blank))) (Pattern x (Blank))) (Plus a (Times b d)))
 (List (Plus (Times b (Pattern y (Blank))) (Pattern x (Blank)))
  (Plus (Times a c) (Times b d)))
 (List (Plus (Times b (Pattern y (Blank))) (Times a (Pattern x (Blank))))
  (Plus (Times a c) (Times b d)))
 (List (Plus (Times b (Pattern x (Blank))) (Times a (Pattern x (Blank))))
  (Plus (Times a c) (Times b d)))
 (List (Plus (Pattern x1 (BlankNullSequence)) d e) (Plus a b c d e))
 (List (Plus a (Pattern x1 (BlankNullSequence)) c) (Plus a b c))
 (List (Plus (Pattern x1 (BlankSequence)) c d (Pattern x2 (BlankNullSequence)))
  (Plus a b c d e))
 (List (Plus (Pattern x1 (BlankNullSequence)) c d (Pattern x2 (BlankNullSequence)))
  (Plus a b c d e))
 (List (g a b (Pattern x (BlankSequence))) (g a c b))
 (List (Plus (Pattern x1 (BlankNullSequence)) b d) (Plus a b c d e))
 (List (Plus (Pattern x (Blank)) (Pattern y (Blank)) (Pattern x (Blank)))
  (Plus a b a))
 (List (Plus (Pattern x (Blank)) (Pattern y (BlankSequence)) (Pattern x (Blank)))
  (Plus a b b b a))
 (List (Plus (Pattern x (Blank)) (Pattern y (BlankSequence)) (Pattern x (Blank)))
  (Plus a b b b a c))
 (List (gh (Pattern x (Blank))) (gh a b c))
 (List (gh (Pattern x (Blank)) b) (gh b a))
 (List (Plus x (PatternTest (Pattern y (Blank)) OddQ)) (Plus 3 x))
 (List (Plus x (PatternTest (Pattern y (Blank)) OddQ)) (Plus 4 x))
 (List
  (Plus (PatternTest (Pattern y (Blank)) OddQ)
        (PatternTest (Pattern z (Blank)) EvenQ))
  7)
 (List (Plus (Pattern y (Blank Integer)) (Pattern z (Blank))) (Plus 3 x y))
 (List (Plus (Pattern x (Blank)) (Sin (Pattern x (Blank)))) (Plus (Sin a) a))
 (List
  (Plus (Pattern x (BlankSequence)) (Pattern y (BlankSequence))
        (Pattern x (BlankSequence)))
  (Plus a b c a b))
 (List (f a b (Power x (Optional (Pattern n (Blank)) 1)) c) (f a b x c))
 (List (f a b (Power x (Optional (Pattern n (Blank)) 1)) c) (f a b (Power x 4) c))
 (List (Times (Optional (Pattern n (Blank))) q) q)
 (List (Times (Optional (Pattern n (Blank)) 1) q r) (Times q r))
 (List (Times (Optional (Pattern n (Blank))) q r) (Times q r))
 (List (Plus (Optional (Pattern n (Blank))) r) r)
 (List (f a b (Power x (Optional (Pattern n (Blank)) 1)) c) (f a b x c))
 (List (Plus a b (Power x (Optional (Pattern n (Blank)) 1)) c) (Plus a x c b))
 (List (Plus a b (Power x (Optional (Pattern n (Blank)) 1)) c)
  (Plus a b (Power x 5) c))
 (List
  (Plus (Times b (Pattern y (Blank)) (Pattern z (Blank)))
        (Times a (Pattern x (Blank))))
  (Plus (Times a c) (Times b d gg)))
 (List
  (Plus (Pattern x (BlankSequence)) (Pattern y (BlankSequence))
        (Pattern x (BlankSequence)))
  (Plus a b a))
 (List (Plus (Pattern x (Blank)) (Pattern y (BlankSequence))) (Plus a b a))
 (List (Plus (Pattern x (Blank)) (Pattern y (Blank)) (Pattern x (Blank)))
  (Plus a b a))
 (List (w (Cos (Pattern x (Blank))) (Sin (Pattern x (Blank)))) (w (Cos a) (Sin a)))
 (List (Plus (Cos (Pattern x (Blank))) (Sin (Pattern x (Blank))))
  (Plus (Cos a) (Sin a)))
 (List (w (Pattern x (BlankSequence)) (Pattern x (BlankSequence))) (w 1 2 3 1 2 3))
 (List (gh (Pattern x (BlankSequence)) (Pattern x (BlankSequence))) (gh 1 2 3 1 2 3))
 (List (gh (Pattern x (BlankSequence)) (Pattern x (BlankSequence))) (gh 1 1 2 2 3 3))))

