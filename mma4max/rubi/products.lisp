(in-package :mma)

(defparameter
  defs
  '(
    (SetDelayed (Int (Times 1
			    (Power (Plus
				    (Pattern a (Blank))
				    (Times
				     (Optional (Pattern b (Blank)))
				     (Cos
				      (Plus
				       (Optional (Pattern c (Blank)))
				       (Times
					(Optional (Pattern d (Blank)))
					(Pattern x (Blank)))))))
				   -1))
		     (Pattern x (Blank Symbol)))
		(Condition (Plus (Times x
					(Power
					 (Rt
					  (Plus
					   (Power a 2)
					   (Times -1 (Power b 2)))
					  2)
					 -1))
				 (Times -1
					(Times
					 2
					 (Power
					  (Times
					   d
					   (Rt
					    (Plus
					     (Power a 2)
					     (Times -1 (Power b 2)))
					    2))
					  -1)
					 (ArcTan
					  (Sim
					   (Times
					    b
					    (Sin (Plus c (Times d x)))
					    (Power
					     (Plus
					      a
					      (Rt
					       (Plus
						(Power a 2)
						(Times -1 (Power b 2)))
					       2)
					      (Times
					       b
					       (Cos (Plus c (Times d x)))))
					     -1)))))))
			   (And (FreeQ (List a b c d) x)
				(PositiveQ
				 (Plus (Power a 2)
				       (Times -1 (Power b 2))))))) 
     
     
    (SetDelayed (Int (Times 1
			    (Power (Plus
				    (Pattern a (Blank))
				    (Times
				     (Optional (Pattern b (Blank)))
				     (Cos
				      (Plus
				       (Optional (Pattern c (Blank)))
				       (Times
					(Optional (Pattern d (Blank)))
					(Pattern x (Blank)))))))
				   -1))
		     (Pattern x (Blank Symbol)))
		(Condition (Times 2
				  (ArcTan
				   (Times
				    (Plus a (Times -1 b))
				    (Tan (Times (Plus c (Times d x)) 1/2))
				    (Power
				     (Rt
				      (Plus
				       (Power a 2)
				       (Times -1 (Power b 2)))
				      2)
				     -1)))
				  (Power (Times
					  d
					  (Rt
					   (Plus
					    (Power a 2)
					    (Times -1 (Power b 2)))
					   2))
					 -1))
			   (And (FreeQ (List a b c d) x)
				(PosQ
				 (Plus (Power a 2)
				       (Times -1 (Power b 2))))))) 
     
     
     
    (SetDelayed (Int (Times 1
			    (Power (Plus
				    (Pattern a (Blank))
				    (Times
				     (Optional (Pattern b (Blank)))
				     (Cos
				      (Plus
				       (Optional (Pattern c (Blank)))
				       (Times
					(Optional (Pattern d (Blank)))
					(Pattern x (Blank)))))))
				   -1))
		     (Pattern x (Blank Symbol)))
		(Condition (Times -2
				  (ArcTanh
				   (Times
				    (Plus a (Times -1 b))
				    (Tan (Times (Plus c (Times d x)) 1/2))
				    (Power
				     (Rt
				      (Plus
				       (Power b 2)
				       (Times -1 (Power a 2)))
				      2)
				     -1)))
				  (Power (Times
					  d
					  (Rt
					   (Plus
					    (Power b 2)
					    (Times -1 (Power a 2)))
					   2))
					 -1))
			   (And (FreeQ (List a b c d) x)
				(NegQ
				 (Plus (Power a 2)
				       (Times -1 (Power b 2))))))) 
     
     
     
    (SetDelayed (Int (Times 1
			    (Power (Sqrt
				    (Plus
				     (Optional (Pattern a (Blank)))
				     (Times
				      (Optional (Pattern b (Blank)))
				      (Cos
				       (Plus
					(Optional (Pattern c (Blank)))
					(Times
					 (Optional (Pattern d (Blank)))
					 (Pattern x (Blank))))))))
				   -1))
		     (Pattern x (Blank Symbol)))
		(Condition (Times 2
				  (EllipticF
				   (Times (Plus c (Times d x)) 1/2)
				   (Sim (Times 2 b (Power (Plus a b) -1))))
				  (Power (Times d (Sqrt (Plus a b))) -1))
			   (And (FreeQ (List a b c d) x)
				(NonzeroQ
				 (Plus (Power a 2) (Times -1 (Power b 2))))
				(PositiveQ (Plus a b))))) 
     
     
     
    (SetDelayed (Int (Times 1
			    (Power (Sqrt
				    (Plus
				     (Optional (Pattern a (Blank)))
				     (Times
				      (Optional (Pattern b (Blank)))
				      (Cos
				       (Plus
					(Optional (Pattern c (Blank)))
					(Times
					 (Optional (Pattern d (Blank)))
					 (Pattern x (Blank))))))))
				   -1))
		     (Pattern x (Blank Symbol)))
		(Condition (Times (Sqrt (Times
					 (Plus
					  a
					  (Times
					   b
					   (Cos (Plus c (Times d x)))))
					 (Power (Plus a b) -1)))
				  (Power (Sqrt
					  (Plus
					   a
					   (Times
					    b
					    (Cos (Plus c (Times d x))))))
					 -1)
				  (Int (Times
					1
					(Power
					 (Sqrt
					  (Plus
					   (Times a (Power (Plus a b) -1))
					   (Times
					    b
					    (Power (Plus a b) -1)
					    (Cos (Plus c (Times d x))))))
					 -1))
				       x))
			   (And (FreeQ (List a b c d) x)
				(NonzeroQ
				 (Plus (Power a 2) (Times -1 (Power b 2))))
				(Not (PositiveQ (Plus a b)))))) 
     
     
     
    (SetDelayed (Int (Sqrt (Plus (Optional (Pattern a (Blank)))
				 (Times (Optional (Pattern b (Blank)))
					(Cos
					 (Plus
					  (Optional (Pattern c (Blank)))
					  (Times
					   (Optional (Pattern d (Blank)))
					   (Pattern x (Blank))))))))
		     (Pattern x (Blank Symbol)))
		(Condition (Times 2 (Sqrt (Plus a b))
				  (EllipticE
				   (Times (Plus c (Times d x)) 1/2)
				   (Sim (Times 2 b (Power (Plus a b) -1))))
				  (Power d -1))
			   (And (FreeQ (List a b c d) x)
				(NonzeroQ
				 (Plus (Power a 2) (Times -1 (Power b 2))))
				(PositiveQ (Plus a b))))) 
     
     
     
    (SetDelayed (Int (Sqrt (Plus (Optional (Pattern a (Blank)))
				 (Times (Optional (Pattern b (Blank)))
					(Cos
					 (Plus
					  (Optional (Pattern c (Blank)))
					  (Times
					   (Optional (Pattern d (Blank)))
					   (Pattern x (Blank))))))))
		     (Pattern x (Blank Symbol)))
		(Condition (Times (Sqrt (Plus
					 a
					 (Times
					  b
					  (Cos (Plus c (Times d x))))))
				  (Power (Sqrt
					  (Times
					   (Plus
					    a
					    (Times
					     b
					     (Cos (Plus c (Times d x)))))
					   (Power (Plus a b) -1)))
					 -1)
				  (Int (Sqrt
					(Plus
					 (Times a (Power (Plus a b) -1))
					 (Times
					  b
					  (Power (Plus a b) -1)
					  (Cos (Plus c (Times d x))))))
				       x))
			   (And (FreeQ (List a b c d) x)
				(NonzeroQ
				 (Plus (Power a 2) (Times -1 (Power b 2))))
				(Not (PositiveQ (Plus a b)))))) 
     
     
     
    (SetDelayed (Int (Times (Sqrt (Cos (Plus
					(Optional (Pattern c (Blank)))
					(Times
					 (Optional (Pattern d (Blank)))
					 (Pattern x (Blank))))))
			    (Power (Plus
				    (Pattern a (Blank))
				    (Times
				     (Optional (Pattern b (Blank)))
				     (Cos
				      (Plus
				       (Optional (Pattern c (Blank)))
				       (Times
					(Optional (Pattern d (Blank)))
					(Pattern x (Blank)))))))
				   -1))
		     (Pattern x (Blank Symbol)))
		(Condition (Plus (Dist (Times 1 (Power b -1))
				       (Int (Times
					     1
					     (Power
					      (Sqrt (Cos (Plus c (Times d x))))
					      -1))
					    x))
				 (Times -1
					(Dist
					 (Times a (Power b -1))
					 (Int
					  (Times
					   1
					   (Power
					    (Times
					     (Sqrt
					      (Cos (Plus c (Times d x))))
					     (Plus
					      a
					      (Times
					       b
					       (Cos
						(Plus c (Times d x))))))
					    -1))
					  x))))
			   (And (FreeQ (List a b c d) x)
				(NonzeroQ
				 (Plus (Power a 2)
				       (Times -1 (Power b 2))))))) 
     
     
     
    (SetDelayed (Int (Times (Sqrt (Cos (Plus
					(Optional (Pattern c (Blank)))
					(Times
					 (Optional (Pattern d (Blank)))
					 (Pattern x (Blank))))))
			    (Power (Sqrt
				    (Plus
				     (Pattern a (Blank))
				     (Times
				      (Optional (Pattern b (Blank)))
				      (Cos
				       (Plus
					(Optional (Pattern c (Blank)))
					(Times
					 (Optional (Pattern d (Blank)))
					 (Pattern x (Blank))))))))
				   -1))
		     (Pattern x (Blank Symbol)))
		(Condition (Plus (Int (Times
				       (Plus 1 (Cos (Plus c (Times d x))))
				       (Power
					(Times
					 (Sqrt (Cos (Plus c (Times d x))))
					 (Sqrt
					  (Plus
					   a
					   (Times
					    b
					    (Cos (Plus c (Times d x)))))))
					-1))
				      x)
				 (Times -1
					(Int
					 (Times
					  1
					  (Power
					   (Times
					    (Sqrt
					     (Cos (Plus c (Times d x))))
					    (Sqrt
					     (Plus
					      a
					      (Times
					       b
					       (Cos
						(Plus c (Times d x)))))))
					   -1))
					 x)))
			   (And (FreeQ (List a b c d) x)
				(NonzeroQ
				 (Plus (Power a 2)
				       (Times -1 (Power b 2))))))) 
     
     
     
    (SetDelayed (Int (Times (Plus (Pattern A (Blank))
				  (Times (Optional (Pattern B (Blank)))
					 (Cos
					  (Plus
					   (Optional (Pattern c (Blank)))
					   (Times
					    (Optional (Pattern d (Blank)))
					    (Pattern x (Blank)))))))
			    (Power (Times
				    (Sqrt
				     (Cos
				      (Plus
				       (Optional (Pattern c (Blank)))
				       (Times
					(Optional (Pattern d (Blank)))
					(Pattern x (Blank))))))
				    (Sqrt
				     (Plus
				      (Pattern a (Blank))
				      (Times
				       (Optional (Pattern b (Blank)))
				       (Cos
					(Plus
					 (Optional (Pattern c (Blank)))
					 (Times
					  (Optional (Pattern d (Blank)))
					  (Pattern x (Blank)))))))))
				   -1))
		     (Pattern x (Blank Symbol)))
		(Condition (Times 4 A
				  (Power (Times d (Sqrt (Plus a b))) -1)
				  (EllipticPi -1
					      (ArcSin
					       (Tan (Times (Plus c (Times d x)) 1/2)))
					      (Times
					       -1
					       (Sim
						(Times
						 (Plus a (Times -1 b))
						 (Power (Plus a b) -1))))))
			   (And (FreeQ (List a b c d A B) x)
				(ZeroQ (Plus A (Times -1 B))) (PositiveQ b)
				(PositiveQ
				 (Plus (Power b 2)
				       (Times -1 (Power a 2))))))) 
     
     
     
    (SetDelayed (Int (Times (Plus (Pattern A (Blank))
				  (Times (Optional (Pattern B (Blank)))
					 (Cos
					  (Plus
					   (Optional (Pattern c (Blank)))
					   (Times
					    (Optional (Pattern d (Blank)))
					    (Pattern x (Blank)))))))
			    (Power (Times
				    (Sqrt
				     (Cos
				      (Plus
				       (Optional (Pattern c (Blank)))
				       (Times
					(Optional (Pattern d (Blank)))
					(Pattern x (Blank))))))
				    (Sqrt
				     (Plus
				      (Pattern a (Blank))
				      (Times
				       (Optional (Pattern b (Blank)))
				       (Cos
					(Plus
					 (Optional (Pattern c (Blank)))
					 (Times
					  (Optional (Pattern d (Blank)))
					  (Pattern x (Blank)))))))))
				   -1))
		     (Pattern x (Blank Symbol)))
		(Condition (Times (Sqrt (Times
					 -1
					 (Cos (Plus c (Times d x)))))
				  (Power (Sqrt (Cos (Plus c (Times d x))))
					 -1)
				  (Int (Times
					(Plus
					 A
					 (Times
					  A
					  (Cos (Plus c (Times d x)))))
					(Power
					 (Times
					  (Sqrt
					   (Times
					    -1
					    (Cos (Plus c (Times d x)))))
					  (Sqrt
					   (Plus
					    a
					    (Times
					     b
					     (Cos (Plus c (Times d x)))))))
					 -1))
				       x))
			   (And (FreeQ (List a b c d A B) x)
				(ZeroQ (Plus A B)) (NegativeQ b)
				(PositiveQ
				 (Plus (Power b 2)
				       (Times -1 (Power a 2))))))) 
     
     
     
    (SetDelayed (Int (Times (Plus (Pattern A (Blank))
				  (Times (Optional (Pattern B (Blank)))
					 (Cos
					  (Plus
					   (Optional (Pattern c (Blank)))
					   (Times
					    (Optional (Pattern d (Blank)))
					    (Pattern x (Blank)))))))
			    (Power (Times
				    (Sqrt
				     (Cos
				      (Plus
				       (Optional (Pattern c (Blank)))
				       (Times
					(Optional (Pattern d (Blank)))
					(Pattern x (Blank))))))
				    (Sqrt
				     (Plus
				      (Pattern a (Blank))
				      (Times
				       (Optional (Pattern b (Blank)))
				       (Cos
					(Plus
					 (Optional (Pattern c (Blank)))
					 (Times
					  (Optional (Pattern d (Blank)))
					  (Pattern x (Blank)))))))))
				   -1))
		     (Pattern x (Blank Symbol)))
		(Condition (Times 4 A
				  (Sqrt (Plus
					 1
					 (Cos (Plus c (Times d x)))))
				  (Power (Times
					  d
					  (Sqrt
					   (Plus
					    a
					    (Times
					     b
					     (Cos (Plus c (Times d x)))))))
					 -1)
				  (Sqrt (Times
					 (Plus
					  a
					  (Times
					   b
					   (Cos (Plus c (Times d x)))))
					 (Power
					  (Times
					   (Plus a b)
					   (Plus
					    1
					    (Cos (Plus c (Times d x)))))
					  -1)))
				  (EllipticPi -1
					      (ArcSin
					       (Tan (Times (Plus c (Times d x)) 1/2)))
					      (Times
					       -1
					       (Sim
						(Times
						 (Plus a (Times -1 b))
						 (Power (Plus a b) -1))))))
			   (And (FreeQ (List a b c d A B) x)
				(ZeroQ (Plus A (Times -1 B)))
				(NonzeroQ
				 (Plus (Power a 2)
				       (Times -1 (Power b 2))))))) 
     
     
     
    (SetDelayed (Int (Times (Sqrt (Times -1
					 (Cos
					  (Plus
					   (Optional (Pattern c (Blank)))
					   (Times
					    (Optional (Pattern d (Blank)))
					    (Pattern x (Blank)))))))
			    (Power (Sqrt
				    (Plus
				     (Pattern a (Blank))
				     (Times
				      (Optional (Pattern b (Blank)))
				      (Cos
				       (Plus
					(Optional (Pattern c (Blank)))
					(Times
					 (Optional (Pattern d (Blank)))
					 (Pattern x (Blank))))))))
				   -1))
		     (Pattern x (Blank Symbol)))
		(Condition (Plus (Int (Times
				       (Plus
					1
					(Times
					 -1
					 (Cos (Plus c (Times d x)))))
				       (Power
					(Times
					 (Sqrt
					  (Times
					   -1
					   (Cos (Plus c (Times d x)))))
					 (Sqrt
					  (Plus
					   a
					   (Times
					    b
					    (Cos (Plus c (Times d x)))))))
					-1))
				      x)
				 (Times -1
					(Int
					 (Times
					  1
					  (Power
					   (Times
					    (Sqrt
					     (Times
					      -1
					      (Cos (Plus c (Times d x)))))
					    (Sqrt
					     (Plus
					      a
					      (Times
					       b
					       (Cos
						(Plus c (Times d x)))))))
					   -1))
					 x)))
			   (And (FreeQ (List a b c d) x)
				(NonzeroQ
				 (Plus (Power a 2)
				       (Times -1 (Power b 2))))))) 
     
     
     
    (SetDelayed (Int (Times (Plus (Pattern A (Blank))
				  (Times (Optional (Pattern B (Blank)))
					 (Cos
					  (Plus
					   (Optional (Pattern c (Blank)))
					   (Times
					    (Optional (Pattern d (Blank)))
					    (Pattern x (Blank)))))))
			    (Power (Times
				    (Sqrt
				     (Times
				      -1
				      (Cos
				       (Plus
					(Optional (Pattern c (Blank)))
					(Times
					 (Optional (Pattern d (Blank)))
					 (Pattern x (Blank)))))))
				    (Sqrt
				     (Plus
				      (Pattern a (Blank))
				      (Times
				       (Optional (Pattern b (Blank)))
				       (Cos
					(Plus
					 (Optional (Pattern c (Blank)))
					 (Times
					  (Optional (Pattern d (Blank)))
					  (Pattern x (Blank)))))))))
				   -1))
		     (Pattern x (Blank Symbol)))
		(Condition (Times -4 A
				  (Power (Times
					  d
					  (Sqrt (Plus a (Times -1 b))))
					 -1)
				  (EllipticPi -1
					      (ArcSin
					       (Cot (Times (Plus c (Times d x)) 1/2)))
					      (Times
					       -1
					       (Sim
						(Times
						 (Plus a b)
						 (Power (Plus a (Times -1 b)) -1))))))
			   (And (FreeQ (List a b c d A B) x)
				(ZeroQ (Plus A B)) (NegativeQ b)
				(PositiveQ
				 (Plus (Power b 2)
				       (Times -1 (Power a 2))))))) 
     
     
     
    (SetDelayed (Int (Times (Plus (Pattern A (Blank))
				  (Times (Optional (Pattern B (Blank)))
					 (Cos
					  (Plus
					   (Optional (Pattern c (Blank)))
					   (Times
					    (Optional (Pattern d (Blank)))
					    (Pattern x (Blank)))))))
			    (Power (Times
				    (Sqrt
				     (Times
				      -1
				      (Cos
				       (Plus
					(Optional (Pattern c (Blank)))
					(Times
					 (Optional (Pattern d (Blank)))
					 (Pattern x (Blank)))))))
				    (Sqrt
				     (Plus
				      (Pattern a (Blank))
				      (Times
				       (Optional (Pattern b (Blank)))
				       (Cos
					(Plus
					 (Optional (Pattern c (Blank)))
					 (Times
					  (Optional (Pattern d (Blank)))
					  (Pattern x (Blank)))))))))
				   -1))
		     (Pattern x (Blank Symbol)))
		(Condition (Times (Sqrt (Cos (Plus c (Times d x))))
				  (Power (Sqrt
					  (Times
					   -1
					   (Cos (Plus c (Times d x)))))
					 -1)
				  (Int (Times
					1
					(Power
					 (Times
					  (Sqrt (Cos (Plus c (Times d x))))
					  (Sqrt
					   (Plus
					    a
					    (Times
					     b
					     (Cos (Plus c (Times d x)))))))
					 -1))
				       x))
			   (And (FreeQ (List a b c d A B) x)
				(ZeroQ (Plus A (Times -1 B))) (PositiveQ b)
				(PositiveQ
				 (Plus (Power b 2)
				       (Times -1 (Power a 2))))))) 
     
     
    (SetDelayed (Int (Times (Plus (Pattern A (Blank))
				  (Times (Optional (Pattern B (Blank)))
					 (Cos
					  (Plus
					   (Optional (Pattern c (Blank)))
					   (Times
					    (Optional (Pattern d (Blank)))
					    (Pattern x (Blank)))))))
			    (Power (Times
				    (Sqrt
				     (Times
				      -1
				      (Cos
				       (Plus
					(Optional (Pattern c (Blank)))
					(Times
					 (Optional (Pattern d (Blank)))
					 (Pattern x (Blank)))))))
				    (Sqrt
				     (Plus
				      (Pattern a (Blank))
				      (Times
				       (Optional (Pattern b (Blank)))
				       (Cos
					(Plus
					 (Optional (Pattern c (Blank)))
					 (Times
					  (Optional (Pattern d (Blank)))
					  (Pattern x (Blank)))))))))
				   -1))
		     (Pattern x (Blank Symbol)))
		(Condition (Times -4 A
				  (Sqrt (Plus
					 1
					 (Times
					  -1
					  (Cos (Plus c (Times d x))))))
				  (Power (Times
					  d
					  (Sqrt
					   (Plus
					    a
					    (Times
					     b
					     (Cos (Plus c (Times d x)))))))
					 -1)
				  (Sqrt (Times
					 (Plus
					  a
					  (Times
					   b
					   (Cos (Plus c (Times d x)))))
					 (Power
					  (Times
					   (Plus a (Times -1 b))
					   (Plus
					    1
					    (Times
					     -1
					     (Cos (Plus c (Times d x))))))
					  -1)))
				  (EllipticPi -1
					      (ArcSin
					       (Cot (Times (Plus c (Times d x)) 1/2)))
					      (Times
					       -1
					       (Sim
						(Times
						 (Plus a b)
						 (Power (Plus a (Times -1 b)) -1))))))
			   (And (FreeQ (List a b c d A B) x)
				(ZeroQ (Plus A B))
				(NonzeroQ
				 (Plus (Power a 2)
				       (Times -1 (Power b 2))))))) 
     
     
     
    (SetDelayed (Int (Times (Sqrt (Cos (Plus
					(Optional (Pattern c (Blank)))
					(Times
					 (Optional (Pattern d (Blank)))
					 (Pattern x (Blank))))))
			    (Sqrt (Plus (Pattern a (Blank))
					(Times
					 (Optional (Pattern b (Blank)))
					 (Cos
					  (Plus
					   (Optional (Pattern c (Blank)))
					   (Times
					    (Optional (Pattern d (Blank)))
					    (Pattern x (Blank)))))))))
		     (Pattern x (Blank Symbol)))
		(Condition (Plus (Times (Sqrt (Cos (Plus c (Times d x))))
					(Sqrt
					 (Plus
					  a
					  (Times
					   b
					   (Cos (Plus c (Times d x))))))
					(Tan
					 (Times 1/2 (Plus c (Times d x))))
					(Power d -1))
				 (Int (Times
				       (Sqrt
					(Plus
					 a
					 (Times
					  b
					  (Cos (Plus c (Times d x))))))
				       (Power
					(Times
					 (Sqrt (Cos (Plus c (Times d x))))
					 (Plus
					  1
					  (Cos (Plus c (Times d x)))))
					-1))
				      x)
				 (Times -1
					(Dist
					 (Times a 1/2)
					 (Int
					  (Times
					   (Plus
					    1
					    (Times
					     -1
					     (Cos (Plus c (Times d x)))))
					   (Power
					    (Times
					     (Sqrt
					      (Cos (Plus c (Times d x))))
					     (Sqrt
					      (Plus
					       a
					       (Times
						b
						(Cos
						 (Plus c (Times d x)))))))
					    -1))
					  x))))
			   (And (FreeQ (List a b c d) x)
				(NonzeroQ
				 (Plus (Power a 2)
				       (Times -1 (Power b 2))))))) 
     
     
     
    (SetDelayed (Int (Times 1
			    (Power (Times
				    (Sqrt
				     (Cos
				      (Plus
				       (Optional (Pattern c (Blank)))
				       (Times
					(Optional (Pattern d (Blank)))
					(Pattern x (Blank))))))
				    (Plus
				     (Pattern a (Blank))
				     (Times
				      (Optional (Pattern b (Blank)))
				      (Cos
				       (Plus
					(Optional (Pattern c (Blank)))
					(Times
					 (Optional (Pattern d (Blank)))
					 (Pattern x (Blank))))))))
				   -1))
		     (Pattern x (Blank Symbol)))
		(Condition (Times 2 (Power (Times d (Plus a b)) -1)
				  (EllipticPi
				   (Sim (Times 2 b (Power (Plus a b) -1)))
				   (Times (Plus c (Times d x)) 1/2) 2))
			   (And (FreeQ (List a b c d) x)
				(NonzeroQ
				 (Plus (Power a 2)
				       (Times -1 (Power b 2))))))) 
     
     
     
    (SetDelayed (Int (Times 1
			    (Power (Times
				    (Sqrt
				     (Cos
				      (Plus
				       (Optional (Pattern c (Blank)))
				       (Times
					(Optional (Pattern d (Blank)))
					(Pattern x (Blank))))))
				    (Sqrt
				     (Plus
				      (Optional (Pattern a (Blank)))
				      (Times
				       (Optional (Pattern b (Blank)))
				       (Cos
					(Plus
					 (Optional (Pattern c (Blank)))
					 (Times
					  (Optional (Pattern d (Blank)))
					  (Pattern x (Blank)))))))))
				   -1))
		     (Pattern x (Blank Symbol)))
		(Condition (Times 2 (Power (Times d (Sqrt (Plus a b))) -1)
				  (EllipticF
				   (ArcSin
				    (Tan (Times (Plus c (Times d x)) 1/2)))
				   (Times
				    -1
				    (Sim
				     (Times
				      (Plus a (Times -1 b))
				      (Power (Plus a b) -1))))))
			   (And (FreeQ (List a b c d) x) (PositiveQ b)
				(PositiveQ
				 (Plus (Power b 2)
				       (Times -1 (Power a 2))))))) 
     
     
     
    (SetDelayed (Int (Times 1
			    (Power (Times
				    (Sqrt
				     (Cos
				      (Plus
				       (Optional (Pattern c (Blank)))
				       (Times
					(Optional (Pattern d (Blank)))
					(Pattern x (Blank))))))
				    (Sqrt
				     (Plus
				      (Pattern a (Blank))
				      (Times
				       (Optional (Pattern b (Blank)))
				       (Cos
					(Plus
					 (Optional (Pattern c (Blank)))
					 (Times
					  (Optional (Pattern d (Blank)))
					  (Pattern x (Blank)))))))))
				   -1))
		     (Pattern x (Blank Symbol)))
		(Condition (Times (Sqrt (Times
					 -1
					 (Cos (Plus c (Times d x)))))
				  (Power (Sqrt (Cos (Plus c (Times d x))))
					 -1)
				  (Int (Times
					1
					(Power
					 (Times
					  (Sqrt
					   (Times
					    -1
					    (Cos (Plus c (Times d x)))))
					  (Sqrt
					   (Plus
					    a
					    (Times
					     b
					     (Cos (Plus c (Times d x)))))))
					 -1))
				       x))
			   (And (FreeQ (List a b c d) x) (NegativeQ b)
				(PositiveQ
				 (Plus (Power b 2)
				       (Times -1 (Power a 2))))))) 
     
     
     
    (SetDelayed (Int (Times 1
			    (Power (Times
				    (Sqrt
				     (Cos
				      (Plus
				       (Optional (Pattern c (Blank)))
				       (Times
					(Optional (Pattern d (Blank)))
					(Pattern x (Blank))))))
				    (Sqrt
				     (Plus
				      (Optional (Pattern a (Blank)))
				      (Times
				       (Optional (Pattern b (Blank)))
				       (Cos
					(Plus
					 (Optional (Pattern c (Blank)))
					 (Times
					  (Optional (Pattern d (Blank)))
					  (Pattern x (Blank)))))))))
				   -1))
		     (Pattern x (Blank Symbol)))
		(Condition (Times 2
				  (Sqrt (Plus
					 1
					 (Cos (Plus c (Times d x)))))
				  (Power (Times
					  d
					  (Sqrt
					   (Plus
					    a
					    (Times
					     b
					     (Cos (Plus c (Times d x)))))))
					 -1)
				  (Sqrt (Times
					 (Plus
					  a
					  (Times
					   b
					   (Cos (Plus c (Times d x)))))
					 (Power
					  (Times
					   (Plus a b)
					   (Plus
					    1
					    (Cos (Plus c (Times d x)))))
					  -1)))
				  (EllipticF
				   (ArcSin
				    (Tan (Times (Plus c (Times d x)) 1/2)))
				   (Times
				    -1
				    (Sim
				     (Times
				      (Plus a (Times -1 b))
				      (Power (Plus a b) -1))))))
			   (And (FreeQ (List a b c d) x)
				(NonzeroQ
				 (Plus (Power a 2)
				       (Times -1 (Power b 2))))))) 
     
     
     
    (SetDelayed (Int (Times 1
			    (Power (Times
				    (Sqrt
				     (Times
				      -1
				      (Cos
				       (Plus
					(Optional (Pattern c (Blank)))
					(Times
					 (Optional (Pattern d (Blank)))
					 (Pattern x (Blank)))))))
				    (Sqrt
				     (Plus
				      (Pattern a (Blank))
				      (Times
				       (Optional (Pattern b (Blank)))
				       (Cos
					(Plus
					 (Optional (Pattern c (Blank)))
					 (Times
					  (Optional (Pattern d (Blank)))
					  (Pattern x (Blank)))))))))
				   -1))
		     (Pattern x (Blank Symbol)))
		(Condition (Times -2
				  (Power (Times
					  d
					  (Sqrt (Plus a (Times -1 b))))
					 -1)
				  (EllipticF
				   (ArcSin
				    (Cot (Times (Plus c (Times d x)) 1/2)))
				   (Times
				    -1
				    (Sim
				     (Times
				      (Plus a b)
				      (Power (Plus a (Times -1 b)) -1))))))
			   (And (FreeQ (List a b c d) x) (NegativeQ b)
				(PositiveQ
				 (Plus (Power b 2)
				       (Times -1 (Power a 2))))))) 
     
     
     
    (SetDelayed (Int (Times 1
			    (Power (Times
				    (Sqrt
				     (Times
				      -1
				      (Cos
				       (Plus
					(Optional (Pattern c (Blank)))
					(Times
					 (Optional (Pattern d (Blank)))
					 (Pattern x (Blank)))))))
				    (Sqrt
				     (Plus
				      (Pattern a (Blank))
				      (Times
				       (Optional (Pattern b (Blank)))
				       (Cos
					(Plus
					 (Optional (Pattern c (Blank)))
					 (Times
					  (Optional (Pattern d (Blank)))
					  (Pattern x (Blank)))))))))
				   -1))
		     (Pattern x (Blank Symbol)))
		(Condition (Times (Sqrt (Cos (Plus c (Times d x))))
				  (Power (Sqrt
					  (Times
					   -1
					   (Cos (Plus c (Times d x)))))
					 -1)
				  (Int (Times
					1
					(Power
					 (Times
					  (Sqrt (Cos (Plus c (Times d x))))
					  (Sqrt
					   (Plus
					    a
					    (Times
					     b
					     (Cos (Plus c (Times d x)))))))
					 -1))
				       x))
			   (And (FreeQ (List a b c d) x) (PositiveQ b)
				(PositiveQ
				 (Plus (Power b 2)
				       (Times -1 (Power a 2))))))) 
     
     
     
    (SetDelayed (Int (Times 1
			    (Power (Times
				    (Sqrt
				     (Times
				      -1
				      (Cos
				       (Plus
					(Optional (Pattern c (Blank)))
					(Times
					 (Optional (Pattern d (Blank)))
					 (Pattern x (Blank)))))))
				    (Sqrt
				     (Plus
				      (Pattern a (Blank))
				      (Times
				       (Optional (Pattern b (Blank)))
				       (Cos
					(Plus
					 (Optional (Pattern c (Blank)))
					 (Times
					  (Optional (Pattern d (Blank)))
					  (Pattern x (Blank)))))))))
				   -1))
		     (Pattern x (Blank Symbol)))
		(Condition (Times -2
				  (Sqrt (Plus
					 1
					 (Times
					  -1
					  (Cos (Plus c (Times d x))))))
				  (Power (Times
					  d
					  (Sqrt
					   (Plus
					    a
					    (Times
					     b
					     (Cos (Plus c (Times d x)))))))
					 -1)
				  (Sqrt (Times
					 (Plus
					  a
					  (Times
					   b
					   (Cos (Plus c (Times d x)))))
					 (Power
					  (Times
					   (Plus a (Times -1 b))
					   (Plus
					    1
					    (Times
					     -1
					     (Cos (Plus c (Times d x))))))
					  -1)))
				  (EllipticF
				   (ArcSin
				    (Cot (Times (Plus c (Times d x)) 1/2)))
				   (Times
				    -1
				    (Sim
				     (Times
				      (Plus a b)
				      (Power (Plus a (Times -1 b)) -1))))))
			   (And (FreeQ (List a b c d) x)
				(NonzeroQ
				 (Plus (Power a 2)
				       (Times -1 (Power b 2))))))) 
     
     
     
    (SetDelayed (Int (Times (Sqrt (Plus (Pattern a (Blank))
					(Times
					 (Optional (Pattern b (Blank)))
					 (Cos
					  (Plus
					   (Optional (Pattern c (Blank)))
					   (Times
					    (Optional (Pattern d (Blank)))
					    (Pattern x (Blank))))))))
			    (Power (Sqrt
				    (Cos
				     (Plus
				      (Optional (Pattern c (Blank)))
				      (Times
				       (Optional (Pattern d (Blank)))
				       (Pattern x (Blank))))))
				   -1))
		     (Pattern x (Blank Symbol)))
		(Condition (Plus (Dist (Plus a (Times -1 b))
				       (Int (Times
					     1
					     (Power
					      (Times
					       (Sqrt (Cos (Plus c (Times d x))))
					       (Sqrt
						(Plus
						 a
						 (Times
						  b
						  (Cos (Plus c (Times d x)))))))
					      -1))
					    x))
				 (Dist b
				       (Int (Times
					     (Plus 1 (Cos (Plus c (Times d x))))
					     (Power
					      (Times
					       (Sqrt (Cos (Plus c (Times d x))))
					       (Sqrt
						(Plus
						 a
						 (Times
						  b
						  (Cos (Plus c (Times d x)))))))
					      -1))
					    x)))
			   (And (FreeQ (List a b c d) x)
				(NonzeroQ
				 (Plus (Power a 2)
				       (Times -1 (Power b 2))))))) 
     
     
     
    (SetDelayed (Int (Times (Sqrt (Plus (Pattern a (Blank))
					(Times
					 (Optional (Pattern b (Blank)))
					 (Cos
					  (Plus
					   (Optional (Pattern c (Blank)))
					   (Times
					    (Optional (Pattern d (Blank)))
					    (Pattern x (Blank))))))))
			    (Power (Times
				    (Sqrt
				     (Cos
				      (Plus
				       (Optional (Pattern c (Blank)))
				       (Times
					(Optional (Pattern d (Blank)))
					(Pattern x (Blank))))))
				    (Plus
				     (Pattern A (Blank))
				     (Times
				      (Optional (Pattern B (Blank)))
				      (Cos
				       (Plus
					(Optional (Pattern c (Blank)))
					(Times
					 (Optional (Pattern d (Blank)))
					 (Pattern x (Blank))))))))
				   -1))
		     (Pattern x (Blank Symbol)))
		(Condition (Times (Sqrt (Plus a b)) (Power (Times d A) -1)
				  (EllipticE
				   (ArcSin
				    (Tan (Times (Plus c (Times d x)) 1/2)))
				   (Times
				    -1
				    (Sim
				     (Times
				      (Plus a (Times -1 b))
				      (Power (Plus a b) -1))))))
			   (And (FreeQ (List a b c d A B) x)
				(ZeroQ (Plus A (Times -1 B))) (PositiveQ b)
				(PositiveQ
				 (Plus (Power b 2)
				       (Times -1 (Power a 2))))))) 
     
     
     
     
     
     
     
    (SetDelayed (Int (Times (Sqrt (Plus (Pattern a (Blank))
					(Times
					 (Optional (Pattern b (Blank)))
					 (Cos
					  (Plus
					   (Optional (Pattern c (Blank)))
					   (Times
					    (Optional (Pattern d (Blank)))
					    (Pattern x (Blank))))))))
			    (Power (Times
				    (Sqrt
				     (Cos
				      (Plus
				       (Optional (Pattern c (Blank)))
				       (Times
					(Optional (Pattern d (Blank)))
					(Pattern x (Blank))))))
				    (Plus
				     (Pattern A (Blank))
				     (Times
				      (Optional (Pattern B (Blank)))
				      (Cos
				       (Plus
					(Optional (Pattern c (Blank)))
					(Times
					 (Optional (Pattern d (Blank)))
					 (Pattern x (Blank))))))))
				   -1))
		     (Pattern x (Blank Symbol)))
		(Condition (Times (Sqrt (Plus
					 a
					 (Times
					  b
					  (Cos (Plus c (Times d x))))))
				  (Power (Times
					  d
					  A
					  (Sqrt
					   (Plus
					    1
					    (Cos (Plus c (Times d x)))))
					  (Sqrt
					   (Times
					    (Plus
					     a
					     (Times
					      b
					      (Cos (Plus c (Times d x)))))
					    (Power
					     (Times
					      (Plus a b)
					      (Plus
					       1
					       (Cos (Plus c (Times d x)))))
					     -1))))
					 -1)
				  (EllipticE
				   (ArcSin
				    (Tan (Times (Plus c (Times d x)) 1/2)))
				   (Times
				    -1
				    (Sim
				     (Times
				      (Plus a (Times -1 b))
				      (Power (Plus a b) -1))))))
			   (And (FreeQ (List a b c d A B) x)
				(ZeroQ (Plus A (Times -1 B)))
				(NonzeroQ
				 (Plus (Power a 2)
				       (Times -1 (Power b 2))))))) 
     
     
     
    (SetDelayed (Int (Times (Power (Cos
				    (Plus
				     (Optional (Pattern c (Blank)))
				     (Times
				      (Optional (Pattern d (Blank)))
				      (Pattern x (Blank)))))
				   3/2)
			    (Power (Plus
				    (Pattern a (Blank))
				    (Times
				     (Optional (Pattern b (Blank)))
				     (Cos
				      (Plus
				       (Optional (Pattern c (Blank)))
				       (Times
					(Optional (Pattern d (Blank)))
					(Pattern x (Blank)))))))
				   -1))
		     (Pattern x (Blank Symbol)))
		(Condition (Plus (Dist (Times 1 (Power b -1))
				       (Int (Sqrt (Cos (Plus c (Times d x))))
					    x))
				 (Times -1
					(Dist
					 (Times a (Power b -1))
					 (Int
					  (Times
					   (Sqrt
					    (Cos (Plus c (Times d x))))
					   (Power
					    (Plus
					     a
					     (Times
					      b
					      (Cos (Plus c (Times d x)))))
					    -1))
					  x))))
			   (And (FreeQ (List a b c d) x)
				(NonzeroQ
				 (Plus (Power a 2)
				       (Times -1 (Power b 2))))))) 
     
     
     
    (SetDelayed (Int (Times (Power (Cos
				    (Plus
				     (Optional (Pattern c (Blank)))
				     (Times
				      (Optional (Pattern d (Blank)))
				      (Pattern x (Blank)))))
				   3/2)
			    (Power (Sqrt
				    (Plus
				     (Pattern a (Blank))
				     (Times
				      (Optional (Pattern b (Blank)))
				      (Cos
				       (Plus
					(Optional (Pattern c (Blank)))
					(Times
					 (Optional (Pattern d (Blank)))
					 (Pattern x (Blank))))))))
				   -1))
		     (Pattern x (Blank Symbol)))
		(Condition (Plus (Times (Sqrt (Cos (Plus c (Times d x))))
					(Sqrt
					 (Plus
					  a
					  (Times
					   b
					   (Cos (Plus c (Times d x))))))
					(Tan
					 (Times (Plus c (Times d x)) 1/2))
					(Power (Times b d) -1))
				 (Dist (Times 1 (Power b -1))
				       (Int (Times
					     (Sqrt
					      (Plus
					       a
					       (Times
						b
						(Cos (Plus c (Times d x))))))
					     (Power
					      (Times
					       (Sqrt (Cos (Plus c (Times d x))))
					       (Plus
						1
						(Cos (Plus c (Times d x)))))
					      -1))
					    x))
				 (Times -1
					(Dist
					 (Times a (Power (Times 2 b) -1))
					 (Int
					  (Times
					   (Plus
					    1
					    (Cos (Plus c (Times d x))))
					   (Power
					    (Times
					     (Sqrt
					      (Cos (Plus c (Times d x))))
					     (Sqrt
					      (Plus
					       a
					       (Times
						b
						(Cos
						 (Plus c (Times d x)))))))
					    -1))
					  x))))
			   (And (FreeQ (List a b c d) x)
				(NonzeroQ
				 (Plus (Power a 2)
				       (Times -1 (Power b 2))))))) 
     
     
     
    (SetDelayed (Int (Cos (Plus (Optional (Pattern a (Blank)))
				(Times (Optional (Pattern b (Blank)))
				       (Pattern x (Blank)))))
		     (Pattern x (Blank Symbol)))
		(Condition (Times (Sin (Plus a (Times b x))) (Power b -1))
			   (FreeQ (List a b) x))) 
     
     
     
    (SetDelayed (Int (Power (Cos (Plus (Optional (Pattern a (Blank)))
				       (Times
					(Optional (Pattern b (Blank)))
					(Pattern x (Blank)))))
			    (Pattern n (Blank)))
		     (Pattern x (Blank Symbol)))
		(Condition (Dist (Times 1 (Power b -1))
				 (ReplaceAll (Int
					      (Expand
					       (Power
						(Plus 1 (Times -1 (Power x 2)))
						(Times (Plus n -1) 1/2))
					       x)
					      x)
					     (Rule
					      x
					      (Sin (Plus a (Times b x))))))
			   (And (FreeQ (List a b) x) (OddQ n)
				(Comparison n Greater 1)))) 
     
     
     
    (SetDelayed (Int (Power (Times (Optional (Pattern b (Blank)))
				   (Cos
				    (Plus
				     (Optional (Pattern c (Blank)))
				     (Times
				      (Optional (Pattern d (Blank)))
				      (Pattern x (Blank))))))
			    (Pattern n (Blank)))
		     (Pattern x (Blank Symbol)))
		(Condition (Plus (Times b (Sin (Plus c (Times d x)))
					(Power
					 (Times
					  b
					  (Cos (Plus c (Times d x))))
					 (Plus n -1))
					(Power (Times d n) -1))
				 (Dist
				  (Times (Plus n -1) (Power b 2)
					 (Power n -1))
				  (Int (Power
					(Times
					 b
					 (Cos (Plus c (Times d x))))
					(Plus n -2))
				       x)))
			   (And (FreeQ (List b c d) x) (RationalQ n)
				(Comparison n Greater 1) (Not (OddQ n))))) 
     
     
     
    (SetDelayed (Int (Power (Times (Optional (Pattern b (Blank)))
				   (Cos
				    (Plus
				     (Optional (Pattern c (Blank)))
				     (Times
				      (Optional (Pattern d (Blank)))
				      (Pattern x (Blank))))))
			    (Pattern n (Blank)))
		     (Pattern x (Blank Symbol)))
		(Condition (Plus (Times (Times
					 -1
					 (Sin (Plus c (Times d x))))
					(Power
					 (Times
					  b
					  (Cos (Plus c (Times d x))))
					 (Plus n 1))
					(Power (Times b d (Plus n 1)) -1))
				 (Dist
				  (Times (Plus n 2)
					 (Power
					  (Times (Plus n 1) (Power b 2))
					  -1))
				  (Int (Power
					(Times
					 b
					 (Cos (Plus c (Times d x))))
					(Plus n 2))
				       x)))
			   (And (FreeQ (List b c d) x) (RationalQ n)
				(Comparison n Less -1)))) 
     
     
     
    (SetDelayed (Int (Power (Plus (Pattern a (Blank))
				  (Times (Optional (Pattern b (Blank)))
					 (Cos
					  (Plus
					   (Optional (Pattern c (Blank)))
					   (Times
					    (Optional (Pattern d (Blank)))
					    (Pattern x (Blank)))))))
			    (Pattern n (Blank)))
		     (Pattern x (Blank Symbol)))
		(Condition (Plus (Times b (Sin (Plus c (Times d x)))
					(Power
					 (Plus
					  a
					  (Times
					   b
					   (Cos (Plus c (Times d x)))))
					 (Plus n 1))
					(Power
					 (Times
					  d
					  (Plus n 1)
					  (Plus
					   (Power a 2)
					   (Times -1 (Power b 2))))
					 -1))
				 (Dist
				  (Times 1
					 (Power
					  (Times
					   (Plus n 1)
					   (Plus
					    (Power a 2)
					    (Times -1 (Power b 2))))
					  -1))
				  (Int (Times
					(Plus
					 (Times a (Plus n 1))
					 (Times
					  -1
					  (Times
					   b
					   (Plus n 2)
					   (Cos (Plus c (Times d x))))))
					(Power
					 (Plus
					  a
					  (Times
					   b
					   (Cos (Plus c (Times d x)))))
					 (Plus n 1)))
				       x)))
			   (And (FreeQ (List a b c d) x)
				(NonzeroQ
				 (Plus (Power a 2) (Times -1 (Power b 2))))
				(RationalQ n) (Comparison n Less -1)))) 
     
     
     
    (SetDelayed (Int (Power (Plus (Pattern a (Blank))
				  (Times (Optional (Pattern b (Blank)))
					 (Cos
					  (Plus
					   (Optional (Pattern c (Blank)))
					   (Times
					    (Optional (Pattern d (Blank)))
					    (Pattern x (Blank)))))))
			    2)
		     (Pattern x (Blank Symbol)))
		(Condition (Plus (Times (Plus
					 (Times 2 (Power a 2))
					 (Power b 2))
					x 1/2)
				 (Times 2 a b (Sin (Plus c (Times d x)))
					(Power d -1))
				 (Times (Power b 2)
					(Cos (Plus c (Times d x)))
					(Sin (Plus c (Times d x)))
					(Power (Times 2 d) -1)))
			   (FreeQ (List a b c d) x))) 
     
     
     
    (SetDelayed (Int (Power (Plus (Pattern a (Blank))
				  (Times (Optional (Pattern b (Blank)))
					 (Cos
					  (Plus
					   (Optional (Pattern c (Blank)))
					   (Times
					    (Optional (Pattern d (Blank)))
					    (Pattern x (Blank)))))))
			    (Pattern n (Blank)))
		     (Pattern x (Blank Symbol)))
		(Condition (Plus (Times b (Sin (Plus c (Times d x)))
					(Power
					 (Plus
					  a
					  (Times
					   b
					   (Cos (Plus c (Times d x)))))
					 (Plus n -1))
					(Power (Times d n) -1))
				 (Dist (Times 1 (Power n -1))
				       (Int (Times
					     (Sim
					      (Plus
					       (Times (Power a 2) n)
					       (Times (Power b 2) (Plus n -1))
					       (Times
						a
						b
						(Plus (Times 2 n) -1)
						(Cos (Plus c (Times d x)))))
					      x)
					     (Power
					      (Plus
					       a
					       (Times
						b
						(Cos (Plus c (Times d x)))))
					      (Plus n -2)))
					    x)))
			   (And (FreeQ (List a b c d) x)
				(NonzeroQ
				 (Plus (Power a 2) (Times -1 (Power b 2))))
				(RationalQ n)
				(Comparison n GreaterEqual 3/2)
				(Comparison n Unequal 2)))) 
     
     
     
    (SetDelayed (Int (Times (Power (Cos
				    (Plus
				     (Optional (Pattern c (Blank)))
				     (Times
				      (Optional (Pattern d (Blank)))
				      (Pattern x (Blank)))))
				   (Pattern m (Blank)))
			    (Power (Plus
				    (Pattern a (Blank))
				    (Times
				     (Optional (Pattern b (Blank)))
				     (Cos
				      (Plus
				       (Optional (Pattern c (Blank)))
				       (Times
					(Optional (Pattern d (Blank)))
					(Pattern x (Blank)))))))
				   2))
		     (Pattern x (Blank Symbol)))
		(Condition (Plus (Times (Times -1 (Power a 2))
					(Sin (Plus c (Times d x)))
					(Power
					 (Cos (Plus c (Times d x)))
					 (Plus m 1))
					(Power (Times d (Plus m 1)) -1))
				 (Dist (Times 1 (Power (Plus m 1) -1))
				       (Int (Times
					     (Power
					      (Cos (Plus c (Times d x)))
					      (Plus m 1))
					     (Plus
					      (Times 2 a b (Plus m 1))
					      (Times
					       (Plus
						(Times (Power a 2) (Plus m 2))
						(Times (Power b 2) (Plus m 1)))
					       (Cos (Plus c (Times d x))))))
					    x)))
			   (And (FreeQ (List a b c d) x)
				(NonzeroQ
				 (Plus (Power a 2) (Times -1 (Power b 2))))
				(RationalQ m) (Comparison m Less -1)))) 
     
     
     
    (SetDelayed (Int (Times (Power (Cos
				    (Plus
				     (Optional (Pattern c (Blank)))
				     (Times
				      (Optional (Pattern d (Blank)))
				      (Pattern x (Blank)))))
				   (Optional (Pattern m (Blank))))
			    (Power (Plus
				    (Pattern a (Blank))
				    (Times
				     (Optional (Pattern b (Blank)))
				     (Cos
				      (Plus
				       (Optional (Pattern c (Blank)))
				       (Times
					(Optional (Pattern d (Blank)))
					(Pattern x (Blank)))))))
				   2))
		     (Pattern x (Blank Symbol)))
		(Condition (Plus (Times (Power b 2)
					(Sin (Plus c (Times d x)))
					(Power
					 (Cos (Plus c (Times d x)))
					 (Plus m 1))
					(Power (Times d (Plus m 2)) -1))
				 (Dist (Times 1 (Power (Plus m 2) -1))
				       (Int (Times
					     (Power
					      (Cos (Plus c (Times d x)))
					      m)
					     (Plus
					      (Times (Power a 2) (Plus m 2))
					      (Times (Power b 2) (Plus m 1))
					      (Times
					       2
					       a
					       b
					       (Plus m 2)
					       (Cos (Plus c (Times d x))))))
					    x)))
			   (And (FreeQ (List a b c d) x)
				(NonzeroQ
				 (Plus (Power a 2) (Times -1 (Power b 2))))
				(RationalQ m)
				(Comparison m GreaterEqual -1)))) 
     
     
     
    (SetDelayed (Int (Times (Power (Cos
				    (Plus
				     (Optional (Pattern c (Blank)))
				     (Times
				      (Optional (Pattern d (Blank)))
				      (Pattern x (Blank)))))
				   (Pattern m (Blank)))
			    (Power (Plus
				    (Optional (Pattern a (Blank)))
				    (Times
				     (Optional (Pattern b (Blank)))
				     (Cos
				      (Plus
				       (Optional (Pattern c (Blank)))
				       (Times
					(Optional (Pattern d (Blank)))
					(Pattern x (Blank)))))))
				   (Pattern n (Blank))))
		     (Pattern x (Blank Symbol)))
		(Condition (Plus (Times (Power a 2)
					(Sin (Plus c (Times d x)))
					(Power
					 (Cos (Plus c (Times d x)))
					 (Plus m -2))
					(Power
					 (Plus
					  a
					  (Times
					   b
					   (Cos (Plus c (Times d x)))))
					 (Plus n 1))
					(Power
					 (Times
					  b
					  d
					  (Plus n 1)
					  (Plus
					   (Power a 2)
					   (Times -1 (Power b 2))))
					 -1))
				 (Dist
				  (Times 1
					 (Power
					  (Times
					   b
					   (Plus n 1)
					   (Plus
					    (Power a 2)
					    (Times -1 (Power b 2))))
					  -1))
				  (Int (Times
					(Power
					 (Cos (Plus c (Times d x)))
					 (Plus m -3))
					(Sim
					 (Plus
					  (Times (Power a 2) (Plus m -2))
					  (Times
					   a
					   b
					   (Plus n 1)
					   (Cos (Plus c (Times d x))))
					  (Times
					   -1
					   (Times
					    (Plus
					     (Times
					      (Power a 2)
					      (Plus m -1))
					     (Times
					      (Power b 2)
					      (Plus n 1)))
					    (Power
					     (Cos (Plus c (Times d x)))
					     2))))
					 x)
					(Power
					 (Plus
					  a
					  (Times
					   b
					   (Cos (Plus c (Times d x)))))
					 (Plus n 1)))
				       x)))
			   (And (FreeQ (List a b c d) x)
				(NonzeroQ
				 (Plus (Power a 2) (Times -1 (Power b 2))))
				(RationalQ (List m n))
				(Comparison m Greater 2)
				(Comparison n Less -1)))) 
     
     
     
    (SetDelayed (Int (Times (Power (Cos
				    (Plus
				     (Optional (Pattern c (Blank)))
				     (Times
				      (Optional (Pattern d (Blank)))
				      (Pattern x (Blank)))))
				   (Pattern m (Blank)))
			    (Power (Plus
				    (Optional (Pattern a (Blank)))
				    (Times
				     (Optional (Pattern b (Blank)))
				     (Cos
				      (Plus
				       (Optional (Pattern c (Blank)))
				       (Times
					(Optional (Pattern d (Blank)))
					(Pattern x (Blank)))))))
				   (Pattern n (Blank))))
		     (Pattern x (Blank Symbol)))
		(Condition (Plus (Times (Times -1 a)
					(Sin (Plus c (Times d x)))
					(Power
					 (Cos (Plus c (Times d x)))
					 (Plus m -1))
					(Power
					 (Plus
					  a
					  (Times
					   b
					   (Cos (Plus c (Times d x)))))
					 (Plus n 1))
					(Power
					 (Times
					  d
					  (Plus n 1)
					  (Plus
					   (Power a 2)
					   (Times -1 (Power b 2))))
					 -1))
				 (Dist
				  (Times 1
					 (Power
					  (Times
					   (Plus n 1)
					   (Plus
					    (Power a 2)
					    (Times -1 (Power b 2))))
					  -1))
				  (Int (Times
					(Power
					 (Cos (Plus c (Times d x)))
					 (Plus m -2))
					(Sim
					 (Plus
					  (Times (Times -1 a) (Plus m -1))
					  (Times
					   -1
					   (Times
					    b
					    (Plus n 1)
					    (Cos (Plus c (Times d x)))))
					  (Times
					   a
					   (Plus m n 1)
					   (Power
					    (Cos (Plus c (Times d x)))
					    2)))
					 x)
					(Power
					 (Plus
					  a
					  (Times
					   b
					   (Cos (Plus c (Times d x)))))
					 (Plus n 1)))
				       x)))
			   (And (FreeQ (List a b c d) x)
				(NonzeroQ
				 (Plus (Power a 2) (Times -1 (Power b 2))))
				(RationalQ (List m n))
				(Comparison 1 Less m Less 2)
				(Comparison n Less -1)))) 
     
     
     
    (SetDelayed (Int (Times (Power (Cos
				    (Plus
				     (Optional (Pattern c (Blank)))
				     (Times
				      (Optional (Pattern d (Blank)))
				      (Pattern x (Blank)))))
				   (Pattern m (Blank)))
			    (Power (Plus
				    (Optional (Pattern a (Blank)))
				    (Times
				     (Optional (Pattern b (Blank)))
				     (Cos
				      (Plus
				       (Optional (Pattern c (Blank)))
				       (Times
					(Optional (Pattern d (Blank)))
					(Pattern x (Blank)))))))
				   (Pattern n (Blank))))
		     (Pattern x (Blank Symbol)))
		(Condition (Plus (Times b (Sin (Plus c (Times d x)))
					(Power
					 (Cos (Plus c (Times d x)))
					 m)
					(Power
					 (Plus
					  a
					  (Times
					   b
					   (Cos (Plus c (Times d x)))))
					 (Plus n 1))
					(Power
					 (Times
					  d
					  (Plus n 1)
					  (Plus
					   (Power a 2)
					   (Times -1 (Power b 2))))
					 -1))
				 (Dist
				  (Times 1
					 (Power
					  (Times
					   (Plus n 1)
					   (Plus
					    (Power a 2)
					    (Times -1 (Power b 2))))
					  -1))
				  (Int (Times
					(Power
					 (Cos (Plus c (Times d x)))
					 (Plus m -1))
					(Sim
					 (Plus
					  (Times b m)
					  (Times
					   a
					   (Plus n 1)
					   (Cos (Plus c (Times d x))))
					  (Times
					   -1
					   (Times
					    b
					    (Plus m n 2)
					    (Power
					     (Cos (Plus c (Times d x)))
					     2))))
					 x)
					(Power
					 (Plus
					  a
					  (Times
					   b
					   (Cos (Plus c (Times d x)))))
					 (Plus n 1)))
				       x)))
			   (And (FreeQ (List a b c d) x)
				(NonzeroQ
				 (Plus (Power a 2) (Times -1 (Power b 2))))
				(RationalQ (List m n))
				(Comparison 0 Less m Less 1)
				(Comparison n Less -1)))) 
     
     
     
    (SetDelayed (Int (Times (Power (Cos
				    (Plus
				     (Optional (Pattern c (Blank)))
				     (Times
				      (Optional (Pattern d (Blank)))
				      (Pattern x (Blank)))))
				   (Pattern m (Blank)))
			    (Power (Plus
				    (Optional (Pattern a (Blank)))
				    (Times
				     (Optional (Pattern b (Blank)))
				     (Cos
				      (Plus
				       (Optional (Pattern c (Blank)))
				       (Times
					(Optional (Pattern d (Blank)))
					(Pattern x (Blank)))))))
				   (Pattern n (Blank))))
		     (Pattern x (Blank Symbol)))
		(Condition (Plus (Times (Sin (Plus c (Times d x)))
					(Power
					 (Cos (Plus c (Times d x)))
					 (Plus m -2))
					(Power
					 (Plus
					  a
					  (Times
					   b
					   (Cos (Plus c (Times d x)))))
					 (Plus n 1))
					(Power (Times b d (Plus m n)) -1))
				 (Dist
				  (Times 1 (Power (Times b (Plus m n)) -1))
				  (Int (Times
					(Power
					 (Cos (Plus c (Times d x)))
					 (Plus m -3))
					(Sim
					 (Plus
					  (Times a (Plus m -2))
					  (Times
					   b
					   (Plus m n -1)
					   (Cos (Plus c (Times d x))))
					  (Times
					   -1
					   (Times
					    a
					    (Plus m -1)
					    (Power
					     (Cos (Plus c (Times d x)))
					     2))))
					 x)
					(Power
					 (Plus
					  a
					  (Times
					   b
					   (Cos (Plus c (Times d x)))))
					 n))
				       x)))
			   (And (FreeQ (List a b c d) x)
				(NonzeroQ
				 (Plus (Power a 2) (Times -1 (Power b 2))))
				(RationalQ (List m n))
				(Comparison m Greater 2)
				(Comparison -1 LessEqual n Less 0)))) 
     
     
     
    (SetDelayed (Int (Times (Power (Cos
				    (Plus
				     (Optional (Pattern c (Blank)))
				     (Times
				      (Optional (Pattern d (Blank)))
				      (Pattern x (Blank)))))
				   (Pattern m (Blank)))
			    (Power (Plus
				    (Optional (Pattern a (Blank)))
				    (Times
				     (Optional (Pattern b (Blank)))
				     (Cos
				      (Plus
				       (Optional (Pattern c (Blank)))
				       (Times
					(Optional (Pattern d (Blank)))
					(Pattern x (Blank)))))))
				   (Pattern n (Blank))))
		     (Pattern x (Blank Symbol)))
		(Condition (Plus (Times (Sin (Plus c (Times d x)))
					(Power
					 (Cos (Plus c (Times d x)))
					 (Plus m -1))
					(Power
					 (Plus
					  a
					  (Times
					   b
					   (Cos (Plus c (Times d x)))))
					 n)
					(Power (Times d (Plus m n)) -1))
				 (Dist (Times 1 (Power (Plus m n) -1))
				       (Int (Times
					     (Power
					      (Cos (Plus c (Times d x)))
					      (Plus m -2))
					     (Sim
					      (Plus
					       (Times a (Plus m -1))
					       (Times
						b
						(Plus m n -1)
						(Cos (Plus c (Times d x))))
					       (Times
						a
						n
						(Power
						 (Cos (Plus c (Times d x)))
						 2)))
					      x)
					     (Power
					      (Plus
					       a
					       (Times
						b
						(Cos (Plus c (Times d x)))))
					      (Plus n -1)))
					    x)))
			   (And (FreeQ (List a b c d) x)
				(NonzeroQ
				 (Plus (Power a 2) (Times -1 (Power b 2))))
				(RationalQ (List m n))
				(Comparison m Greater 1)
				(Comparison m Unequal 2)
				(Comparison 0 Less n Less 1)))) 
     
     
     
    (SetDelayed (Int (Times (Power (Cos
				    (Plus
				     (Optional (Pattern c (Blank)))
				     (Times
				      (Optional (Pattern d (Blank)))
				      (Pattern x (Blank)))))
				   (Pattern m (Blank)))
			    (Power (Plus
				    (Optional (Pattern a (Blank)))
				    (Times
				     (Optional (Pattern b (Blank)))
				     (Cos
				      (Plus
				       (Optional (Pattern c (Blank)))
				       (Times
					(Optional (Pattern d (Blank)))
					(Pattern x (Blank)))))))
				   (Pattern n (Blank))))
		     (Pattern x (Blank Symbol)))
		(Condition (Plus (Times (Power b 2)
					(Sin (Plus c (Times d x)))
					(Power
					 (Cos (Plus c (Times d x)))
					 (Plus m 1))
					(Power
					 (Plus
					  a
					  (Times
					   b
					   (Cos (Plus c (Times d x)))))
					 (Plus n -2))
					(Power (Times d (Plus m n)) -1))
				 (Dist (Times 1 (Power (Plus m n) -1))
				       (Int (Times
					     (Power
					      (Cos (Plus c (Times d x)))
					      m)
					     (Sim
					      (Plus
					       (Times
						a
						(Plus
						 (Times (Power b 2) (Plus m 1))
						 (Times
						  (Power a 2)
						  (Plus m n))))
					       (Times
						b
						(Plus
						 (Times
						  3
						  (Power a 2)
						  (Plus m n))
						 (Times
						  (Power b 2)
						  (Plus m n -1)))
						(Cos (Plus c (Times d x))))
					       (Times
						a
						(Power b 2)
						(Plus
						 (Times 2 m)
						 (Times 3 n)
						 -2)
						(Power
						 (Cos (Plus c (Times d x)))
						 2)))
					      x)
					     (Power
					      (Plus
					       a
					       (Times
						b
						(Cos (Plus c (Times d x)))))
					      (Plus n -3)))
					    x)))
			   (And (FreeQ (List a b c d) x)
				(NonzeroQ
				 (Plus (Power a 2) (Times -1 (Power b 2))))
				(RationalQ (List m n))
				(Comparison m Greater -1)
				(Comparison m Unequal 2)
				(Comparison n Greater 2)))) 
     
     
     
    (SetDelayed (Int (Times (Power (Cos
				    (Plus
				     (Optional (Pattern c (Blank)))
				     (Times
				      (Optional (Pattern d (Blank)))
				      (Pattern x (Blank)))))
				   (Pattern m (Blank)))
			    (Power (Plus
				    (Optional (Pattern a (Blank)))
				    (Times
				     (Optional (Pattern b (Blank)))
				     (Cos
				      (Plus
				       (Optional (Pattern c (Blank)))
				       (Times
					(Optional (Pattern d (Blank)))
					(Pattern x (Blank)))))))
				   (Pattern n (Blank))))
		     (Pattern x (Blank Symbol)))
		(Condition (Plus (Times b (Sin (Plus c (Times d x)))
					(Power
					 (Cos (Plus c (Times d x)))
					 m)
					(Power
					 (Plus
					  a
					  (Times
					   b
					   (Cos (Plus c (Times d x)))))
					 (Plus n -1))
					(Power (Times d (Plus m n)) -1))
				 (Dist (Times 1 (Power (Plus m n) -1))
				       (Int (Times
					     (Power
					      (Cos (Plus c (Times d x)))
					      (Plus m -1))
					     (Sim
					      (Plus
					       (Times a b m)
					       (Times
						(Plus
						 (Times (Power a 2) (Plus m n))
						 (Times
						  (Power b 2)
						  (Plus m n -1)))
						(Cos (Plus c (Times d x))))
					       (Times
						(Plus
						 (Times a b (Plus m n))
						 (Times a b (Plus n -1)))
						(Power
						 (Cos (Plus c (Times d x)))
						 2)))
					      x)
					     (Power
					      (Plus
					       a
					       (Times
						b
						(Cos (Plus c (Times d x)))))
					      (Plus n -2)))
					    x)))
			   (And (FreeQ (List a b c d) x)
				(NonzeroQ
				 (Plus (Power a 2) (Times -1 (Power b 2))))
				(RationalQ (List m n))
				(Comparison m Greater 0)
				(Comparison m Unequal 2)
				(Comparison 1 Less n Less 2)))) 
     
     
     
    (SetDelayed (Int (Times (Power (Cos
				    (Plus
				     (Optional (Pattern c (Blank)))
				     (Times
				      (Optional (Pattern d (Blank)))
				      (Pattern x (Blank)))))
				   (Pattern m (Blank)))
			    (Power (Plus
				    (Optional (Pattern a (Blank)))
				    (Times
				     (Optional (Pattern b (Blank)))
				     (Cos
				      (Plus
				       (Optional (Pattern c (Blank)))
				       (Times
					(Optional (Pattern d (Blank)))
					(Pattern x (Blank)))))))
				   (Pattern n (Blank))))
		     (Pattern x (Blank Symbol)))
		(Condition (Plus (Times (Times -1 (Power a 2))
					(Sin (Plus c (Times d x)))
					(Power
					 (Cos (Plus c (Times d x)))
					 (Plus m 1))
					(Power
					 (Plus
					  a
					  (Times
					   b
					   (Cos (Plus c (Times d x)))))
					 (Plus n -2))
					(Power (Times d (Plus m 1)) -1))
				 (Dist (Times 1 (Power (Plus m 1) -1))
				       (Int (Times
					     (Power
					      (Cos (Plus c (Times d x)))
					      (Plus m 1))
					     (Plus
					      (Times
					       (Power a 2)
					       b
					       (Plus
						(Times 2 m)
						(Times -1 n)
						4))
					      (Times
					       a
					       (Plus
						(Times 3 (Power b 2) (Plus m 1))
						(Times (Power a 2) (Plus m 2)))
					       (Cos (Plus c (Times d x))))
					      (Times
					       b
					       (Plus
						(Times (Power b 2) (Plus m 1))
						(Times (Power a 2) (Plus m n)))
					       (Power
						(Cos (Plus c (Times d x)))
						2)))
					     (Power
					      (Plus
					       a
					       (Times
						b
						(Cos (Plus c (Times d x)))))
					      (Plus n -3)))
					    x)))
			   (And (FreeQ (List a b c d) x)
				(NonzeroQ
				 (Plus (Power a 2) (Times -1 (Power b 2))))
				(RationalQ (List m n))
				(Comparison m Less -1)
				(Comparison n Greater 2)))) 
     
     
     
    (SetDelayed (Int (Times (Power (Cos
				    (Plus
				     (Optional (Pattern c (Blank)))
				     (Times
				      (Optional (Pattern d (Blank)))
				      (Pattern x (Blank)))))
				   (Pattern m (Blank)))
			    (Power (Plus
				    (Optional (Pattern a (Blank)))
				    (Times
				     (Optional (Pattern b (Blank)))
				     (Cos
				      (Plus
				       (Optional (Pattern c (Blank)))
				       (Times
					(Optional (Pattern d (Blank)))
					(Pattern x (Blank)))))))
				   (Pattern n (Blank))))
		     (Pattern x (Blank Symbol)))
		(Condition (Plus (Times (Times -1 a)
					(Sin (Plus c (Times d x)))
					(Power
					 (Cos (Plus c (Times d x)))
					 (Plus m 1))
					(Power
					 (Plus
					  a
					  (Times
					   b
					   (Cos (Plus c (Times d x)))))
					 (Plus n -1))
					(Power (Times d (Plus m 1)) -1))
				 (Dist (Times 1 (Power (Plus m 1) -1))
				       (Int (Times
					     (Power
					      (Cos (Plus c (Times d x)))
					      (Plus m 1))
					     (Sim
					      (Plus
					       (Times
						a
						b
						(Plus m (Times -1 n) 2))
					       (Times
						(Plus
						 (Times (Power b 2) (Plus m 1))
						 (Times (Power a 2) (Plus m 2)))
						(Cos (Plus c (Times d x))))
					       (Times
						a
						b
						(Plus m n 1)
						(Power
						 (Cos (Plus c (Times d x)))
						 2)))
					      x)
					     (Power
					      (Plus
					       a
					       (Times
						b
						(Cos (Plus c (Times d x)))))
					      (Plus n -2)))
					    x)))
			   (And (FreeQ (List a b c d) x)
				(NonzeroQ
				 (Plus (Power a 2) (Times -1 (Power b 2))))
				(RationalQ (List m n))
				(Comparison m Less 0)
				(Comparison m Unequal -1)
				(Comparison 1 Less n Less 2)))) 
     
     
     
    (SetDelayed (Int (Times (Power (Cos
				    (Plus
				     (Optional (Pattern c (Blank)))
				     (Times
				      (Optional (Pattern d (Blank)))
				      (Pattern x (Blank)))))
				   (Pattern m (Blank)))
			    (Power (Plus
				    (Optional (Pattern a (Blank)))
				    (Times
				     (Optional (Pattern b (Blank)))
				     (Cos
				      (Plus
				       (Optional (Pattern c (Blank)))
				       (Times
					(Optional (Pattern d (Blank)))
					(Pattern x (Blank)))))))
				   (Pattern n (Blank))))
		     (Pattern x (Blank Symbol)))
		(Condition (Plus (Times (Times
					 -1
					 (Sin (Plus c (Times d x))))
					(Power
					 (Cos (Plus c (Times d x)))
					 (Plus m 1))
					(Power
					 (Plus
					  a
					  (Times
					   b
					   (Cos (Plus c (Times d x)))))
					 n)
					(Power (Times d (Plus m 1)) -1))
				 (Dist (Times 1 (Power (Plus m 1) -1))
				       (Int (Times
					     (Power
					      (Cos (Plus c (Times d x)))
					      (Plus m 1))
					     (Sim
					      (Plus
					       (Times (Times -1 b) n)
					       (Times
						a
						(Plus m 2)
						(Cos (Plus c (Times d x))))
					       (Times
						b
						(Plus m n 2)
						(Power
						 (Cos (Plus c (Times d x)))
						 2)))
					      x)
					     (Power
					      (Plus
					       a
					       (Times
						b
						(Cos (Plus c (Times d x)))))
					      (Plus n -1)))
					    x)))
			   (And (FreeQ (List a b c d) x)
				(NonzeroQ
				 (Plus (Power a 2) (Times -1 (Power b 2))))
				(RationalQ (List m n))
				(Comparison m Less -1)
				(Comparison 0 Less n Less 1)))) 
     
     
     
    (SetDelayed (Int (Times (Power (Cos
				    (Plus
				     (Optional (Pattern c (Blank)))
				     (Times
				      (Optional (Pattern d (Blank)))
				      (Pattern x (Blank)))))
				   (Pattern m (Blank)))
			    (Power (Plus
				    (Pattern a (Blank))
				    (Times
				     (Optional (Pattern b (Blank)))
				     (Cos
				      (Plus
				       (Optional (Pattern c (Blank)))
				       (Times
					(Optional (Pattern d (Blank)))
					(Pattern x (Blank)))))))
				   (Pattern n (Blank))))
		     (Pattern x (Blank Symbol)))
		(Condition (Plus (Times (Times
					 -1
					 (Sin (Plus c (Times d x))))
					(Power
					 (Cos (Plus c (Times d x)))
					 (Plus m 1))
					(Power
					 (Plus
					  a
					  (Times
					   b
					   (Cos (Plus c (Times d x)))))
					 (Plus n 1))
					(Power (Times a d (Plus m 1)) -1))
				 (Dist
				  (Times 1 (Power (Times a (Plus m 1)) -1))
				  (Int (Times
					(Power
					 (Cos (Plus c (Times d x)))
					 (Plus m 1))
					(Sim
					 (Plus
					  (Times (Times -1 b) (Plus m n 2))
					  (Times
					   a
					   (Plus m 2)
					   (Cos (Plus c (Times d x))))
					  (Times
					   b
					   (Plus m n 3)
					   (Power
					    (Cos (Plus c (Times d x)))
					    2)))
					 x)
					(Power
					 (Plus
					  a
					  (Times
					   b
					   (Cos (Plus c (Times d x)))))
					 n))
				       x)))
			   (And (FreeQ (List a b c d) x)
				(NonzeroQ
				 (Plus (Power a 2) (Times -1 (Power b 2))))
				(RationalQ (List m n))
				(Comparison m Less -1)
				(Comparison -1 LessEqual n Less 0)))) 
     
     
    (SetDelayed (Int (Times (Power (Cos
				    (Plus
				     (Optional (Pattern c (Blank)))
				     (Times
				      (Optional (Pattern d (Blank)))
				      (Pattern x (Blank)))))
				   (Pattern m (Blank)))
			    (Power (Plus
				    (Pattern a (Blank))
				    (Times
				     (Optional (Pattern b (Blank)))
				     (Cos
				      (Plus
				       (Optional (Pattern c (Blank)))
				       (Times
					(Optional (Pattern d (Blank)))
					(Pattern x (Blank)))))))
				   (Pattern n (Blank))))
		     (Pattern x (Blank Symbol)))
		(Condition (Plus (Times (Times -1 (Power b 2))
					(Sin (Plus c (Times d x)))
					(Power
					 (Cos (Plus c (Times d x)))
					 (Plus m 1))
					(Power
					 (Plus
					  a
					  (Times
					   b
					   (Cos (Plus c (Times d x)))))
					 (Plus n 1))
					(Power
					 (Times
					  a
					  d
					  (Plus n 1)
					  (Plus
					   (Power a 2)
					   (Times -1 (Power b 2))))
					 -1))
				 (Dist
				  (Times 1
					 (Power
					  (Times
					   a
					   (Plus n 1)
					   (Plus
					    (Power a 2)
					    (Times -1 (Power b 2))))
					  -1))
				  (Int (Times
					(Power
					 (Cos (Plus c (Times d x)))
					 m)
					(Sim
					 (Plus
					  (Times (Power a 2) (Plus n 1))
					  (Times
					   -1
					   (Times
					    (Power b 2)
					    (Plus m n 2)))
					  (Times
					   -1
					   (Times
					    a
					    b
					    (Plus n 1)
					    (Cos (Plus c (Times d x)))))
					  (Times
					   (Plus m n 3)
					   (Power b 2)
					   (Power
					    (Cos (Plus c (Times d x)))
					    2)))
					 x)
					(Power
					 (Plus
					  a
					  (Times
					   b
					   (Cos (Plus c (Times d x)))))
					 (Plus n 1)))
				       x)))
			   (And (FreeQ (List a b c d) x)
				(NonzeroQ
				 (Plus (Power a 2) (Times -1 (Power b 2))))
				(RationalQ (List m n))
				(Comparison m Less 0)
				(Comparison n Less -1)))) 
     
     
    (SetDelayed (Int (Times (Sqrt (Cos (Plus
					(Optional (Pattern c (Blank)))
					(Times
					 (Optional (Pattern d (Blank)))
					 (Pattern x (Blank))))))
			    (Plus (Pattern A (Blank))
				  (Times (Optional (Pattern B (Blank)))
					 (Cos
					  (Plus
					   (Optional (Pattern c (Blank)))
					   (Times
					    (Optional (Pattern d (Blank)))
					    (Pattern x (Blank))))))))
		     (Pattern x (Blank Symbol)))
		(Condition (Plus (Dist A
				       (Int (Sqrt (Cos (Plus c (Times d x))))
					    x))
				 (Dist B
				       (Int (Power
					     (Cos (Plus c (Times d x)))
					     3/2)
					    x)))
			   (FreeQ (List c d A B) x))) 
     
     
     
    (SetDelayed (Int (Times (Plus (Pattern A (Blank))
				  (Times (Optional (Pattern B (Blank)))
					 (Cos
					  (Plus
					   (Optional (Pattern c (Blank)))
					   (Times
					    (Optional (Pattern d (Blank)))
					    (Pattern x (Blank)))))))
			    (Power (Sqrt
				    (Cos
				     (Plus
				      (Optional (Pattern c (Blank)))
				      (Times
				       (Optional (Pattern d (Blank)))
				       (Pattern x (Blank))))))
				   -1))
		     (Pattern x (Blank Symbol)))
		(Condition (Plus (Dist A
				       (Int (Times
					     1
					     (Power
					      (Sqrt (Cos (Plus c (Times d x))))
					      -1))
					    x))
				 (Dist B
				       (Int (Sqrt (Cos (Plus c (Times d x))))
					    x)))
			   (FreeQ (List c d A B) x))) 
     
     
     
    (SetDelayed (Int (Times (Plus (Optional (Pattern A (Blank)))
				  (Times (Optional (Pattern B (Blank)))
					 (Cos
					  (Plus
					   (Optional (Pattern c (Blank)))
					   (Times
					    (Optional (Pattern d (Blank)))
					    (Pattern x (Blank)))))))
			    (Power (Plus
				    (Pattern a (Blank))
				    (Times
				     (Optional (Pattern b (Blank)))
				     (Cos
				      (Plus
				       (Optional (Pattern c (Blank)))
				       (Times
					(Optional (Pattern d (Blank)))
					(Pattern x (Blank)))))))
				   -1))
		     (Pattern x (Blank Symbol)))
		(Condition (Plus (Times B x (Power b -1))
				 (Dist
				  (Times (Plus
					  (Times b A)
					  (Times -1 (Times a B)))
					 (Power b -1))
				  (Int (Times
					1
					(Power
					 (Plus
					  a
					  (Times
					   b
					   (Cos (Plus c (Times d x)))))
					 -1))
				       x)))
			   (And (FreeQ (List a b c d A B) x)
				(NonzeroQ
				 (Plus (Times b A)
				       (Times -1 (Times a B))))))) 
     
     
     
    (SetDelayed (Int (Times (Plus (Optional (Pattern A (Blank)))
				  (Times (Optional (Pattern B (Blank)))
					 (Cos
					  (Plus
					   (Optional (Pattern c (Blank)))
					   (Times
					    (Optional (Pattern d (Blank)))
					    (Pattern x (Blank)))))))
			    (Power (Sqrt
				    (Plus
				     (Pattern a (Blank))
				     (Times
				      (Optional (Pattern b (Blank)))
				      (Cos
				       (Plus
					(Optional (Pattern c (Blank)))
					(Times
					 (Optional (Pattern d (Blank)))
					 (Pattern x (Blank))))))))
				   -1))
		     (Pattern x (Blank Symbol)))
		(Condition (Plus (Dist (Times B (Power b -1))
				       (Int (Sqrt
					     (Plus
					      a
					      (Times
					       b
					       (Cos (Plus c (Times d x))))))
					    x))
				 (Dist
				  (Times (Plus
					  (Times b A)
					  (Times -1 (Times a B)))
					 (Power b -1))
				  (Int (Times
					1
					(Power
					 (Sqrt
					  (Plus
					   a
					   (Times
					    b
					    (Cos (Plus c (Times d x))))))
					 -1))
				       x)))
			   (And (FreeQ (List a b c d A B) x)
				(NonzeroQ
				 (Plus (Times b A)
				       (Times -1 (Times a B))))))) 
     
     
     
    (SetDelayed (Int (Times (Sqrt (Cos (Plus
					(Optional (Pattern c (Blank)))
					(Times
					 (Optional (Pattern d (Blank)))
					 (Pattern x (Blank))))))
			    (Plus (Pattern A (Blank))
				  (Times (Optional (Pattern B (Blank)))
					 (Cos
					  (Plus
					   (Optional (Pattern c (Blank)))
					   (Times
					    (Optional (Pattern d (Blank)))
					    (Pattern x (Blank)))))))
			    (Power (Plus
				    (Pattern a (Blank))
				    (Times
				     (Optional (Pattern b (Blank)))
				     (Cos
				      (Plus
				       (Optional (Pattern c (Blank)))
				       (Times
					(Optional (Pattern d (Blank)))
					(Pattern x (Blank)))))))
				   -1))
		     (Pattern x (Blank Symbol)))
		(Condition (Plus (Dist (Times B (Power b -1))
				       (Int (Sqrt (Cos (Plus c (Times d x))))
					    x))
				 (Dist
				  (Times (Plus
					  (Times b A)
					  (Times -1 (Times a B)))
					 (Power b -1))
				  (Int (Times
					(Sqrt (Cos (Plus c (Times d x))))
					(Power
					 (Plus
					  a
					  (Times
					   b
					   (Cos (Plus c (Times d x)))))
					 -1))
				       x)))
			   (And (FreeQ (List a b c d A B) x)
				(NonzeroQ
				 (Plus (Power a 2) (Times -1 (Power b 2))))
				(NonzeroQ
				 (Plus (Times b A)
				       (Times -1 (Times a B))))))) 
     
     
     
    (SetDelayed (Int (Times (Sqrt (Cos (Plus
					(Optional (Pattern c (Blank)))
					(Times
					 (Optional (Pattern d (Blank)))
					 (Pattern x (Blank))))))
			    (Plus (Pattern A (Blank))
				  (Times (Optional (Pattern B (Blank)))
					 (Cos
					  (Plus
					   (Optional (Pattern c (Blank)))
					   (Times
					    (Optional (Pattern d (Blank)))
					    (Pattern x (Blank)))))))
			    (Power (Sqrt
				    (Plus
				     (Pattern a (Blank))
				     (Times
				      (Optional (Pattern b (Blank)))
				      (Cos
				       (Plus
					(Optional (Pattern c (Blank)))
					(Times
					 (Optional (Pattern d (Blank)))
					 (Pattern x (Blank))))))))
				   -1))
		     (Pattern x (Blank Symbol)))
		(Condition (Int (Times (Plus
					(Times
					 A
					 (Cos (Plus c (Times d x))))
					(Times
					 B
					 (Power
					  (Cos (Plus c (Times d x)))
					  2)))
				       (Power
					(Times
					 (Sqrt (Cos (Plus c (Times d x))))
					 (Sqrt
					  (Plus
					   a
					   (Times
					    b
					    (Cos (Plus c (Times d x)))))))
					-1))
				x)
			   (And (FreeQ (List a b c d A B) x)
				(NonzeroQ
				 (Plus (Power a 2) (Times -1 (Power b 2))))
				(NonzeroQ
				 (Plus (Times b A)
				       (Times -1 (Times a B))))))) 
     
     
     
    (SetDelayed (Int (Times (Plus (Pattern A (Blank))
				  (Times (Optional (Pattern B (Blank)))
					 (Cos
					  (Plus
					   (Optional (Pattern c (Blank)))
					   (Times
					    (Optional (Pattern d (Blank)))
					    (Pattern x (Blank)))))))
			    (Power (Times
				    (Sqrt
				     (Cos
				      (Plus
				       (Optional (Pattern c (Blank)))
				       (Times
					(Optional (Pattern d (Blank)))
					(Pattern x (Blank))))))
				    (Plus
				     (Pattern a (Blank))
				     (Times
				      (Optional (Pattern b (Blank)))
				      (Cos
				       (Plus
					(Optional (Pattern c (Blank)))
					(Times
					 (Optional (Pattern d (Blank)))
					 (Pattern x (Blank))))))))
				   -1))
		     (Pattern x (Blank Symbol)))
		(Condition (Plus (Times B (Power b -1)
					(Int
					 (Times
					  1
					  (Power
					   (Sqrt
					    (Cos (Plus c (Times d x))))
					   -1))
					 x))
				 (Times (Plus
					 (Times b A)
					 (Times -1 (Times a B)))
					(Power b -1)
					(Int
					 (Times
					  1
					  (Power
					   (Times
					    (Sqrt
					     (Cos (Plus c (Times d x))))
					    (Plus
					     a
					     (Times
					      b
					      (Cos (Plus c (Times d x))))))
					   -1))
					 x)))
			   (And (FreeQ (List a b c d A B) x)
				(NonzeroQ
				 (Plus (Power a 2) (Times -1 (Power b 2))))
				(NonzeroQ
				 (Plus (Times b A)
				       (Times -1 (Times a B))))))) 
     
     
     
    (SetDelayed (Int (Times (Plus (Pattern A (Blank))
				  (Times (Optional (Pattern B (Blank)))
					 (Cos
					  (Plus
					   (Optional (Pattern c (Blank)))
					   (Times
					    (Optional (Pattern d (Blank)))
					    (Pattern x (Blank)))))))
			    (Power (Times
				    (Sqrt
				     (Cos
				      (Plus
				       (Optional (Pattern c (Blank)))
				       (Times
					(Optional (Pattern d (Blank)))
					(Pattern x (Blank))))))
				    (Sqrt
				     (Plus
				      (Pattern a (Blank))
				      (Times
				       (Optional (Pattern b (Blank)))
				       (Cos
					(Plus
					 (Optional (Pattern c (Blank)))
					 (Times
					  (Optional (Pattern d (Blank)))
					  (Pattern x (Blank)))))))))
				   -1))
		     (Pattern x (Blank Symbol)))
		(Condition (Plus (Times B
					(Int
					 (Times
					  (Plus
					   1
					   (Cos (Plus c (Times d x))))
					  (Power
					   (Times
					    (Sqrt
					     (Cos (Plus c (Times d x))))
					    (Sqrt
					     (Plus
					      a
					      (Times
					       b
					       (Cos
						(Plus c (Times d x)))))))
					   -1))
					 x))
				 (Times (Plus A (Times -1 B))
					(Int
					 (Times
					  1
					  (Power
					   (Times
					    (Sqrt
					     (Cos (Plus c (Times d x))))
					    (Sqrt
					     (Plus
					      a
					      (Times
					       b
					       (Cos
						(Plus c (Times d x)))))))
					   -1))
					 x)))
			   (And (FreeQ (List a b c d A B) x)
				(NonzeroQ
				 (Plus (Power a 2) (Times -1 (Power b 2))))
				(NonzeroQ (Plus A (Times -1 B)))))) 
     
     
     
    (SetDelayed (Int (Times (Power (Cos
				    (Plus
				     (Optional (Pattern c (Blank)))
				     (Times
				      (Optional (Pattern d (Blank)))
				      (Pattern x (Blank)))))
				   (Pattern m (Blank)))
			    (Plus (Pattern A (Blank))
				  (Times (Optional (Pattern B (Blank)))
					 (Cos
					  (Plus
					   (Optional (Pattern c (Blank)))
					   (Times
					    (Optional (Pattern d (Blank)))
					    (Pattern x (Blank))))))))
		     (Pattern x (Blank Symbol)))
		(Condition (Plus (Times (Times -1 A)
					(Sin (Plus c (Times d x)))
					(Power
					 (Cos (Plus c (Times d x)))
					 (Plus m 1))
					(Power (Times d (Plus m 1)) -1))
				 (Dist (Times 1 (Power (Plus m 1) -1))
				       (Int (Times
					     (Power
					      (Cos (Plus c (Times d x)))
					      (Plus m 1))
					     (Plus
					      (Times B (Plus m 1))
					      (Times
					       A
					       (Plus m 2)
					       (Cos (Plus c (Times d x))))))
					    x)))
			   (And (FreeQ (List c d A B) x) (RationalQ m)
				(Comparison m Less -1)))) 
     
     
     
    (SetDelayed (Int (Times (Power (Cos
				    (Plus
				     (Optional (Pattern c (Blank)))
				     (Times
				      (Optional (Pattern d (Blank)))
				      (Pattern x (Blank)))))
				   (Pattern m (Blank)))
			    (Plus (Pattern A (Blank))
				  (Times (Optional (Pattern B (Blank)))
					 (Cos
					  (Plus
					   (Optional (Pattern c (Blank)))
					   (Times
					    (Optional (Pattern d (Blank)))
					    (Pattern x (Blank))))))))
		     (Pattern x (Blank Symbol)))
		(Condition (Plus (Times B (Sin (Plus c (Times d x)))
					(Power
					 (Cos (Plus c (Times d x)))
					 m)
					(Power (Times d (Plus m 1)) -1))
				 (Dist (Times 1 (Power (Plus m 1) -1))
				       (Int (Times
					     (Power
					      (Cos (Plus c (Times d x)))
					      (Plus m -1))
					     (Plus
					      (Times B m)
					      (Times
					       A
					       (Plus m 1)
					       (Cos (Plus c (Times d x))))))
					    x)))
			   (And (FreeQ (List c d A B) x) (RationalQ m)
				(Comparison m Greater 1)))) 
     
     
     
    (SetDelayed (Int (Times (Plus (Pattern A (Blank))
				  (Times (Optional (Pattern B (Blank)))
					 (Cos
					  (Plus
					   (Optional (Pattern c (Blank)))
					   (Times
					    (Optional (Pattern d (Blank)))
					    (Pattern x (Blank)))))))
			    (Power (Plus
				    (Pattern a (Blank))
				    (Times
				     (Optional (Pattern b (Blank)))
				     (Cos
				      (Plus
				       (Optional (Pattern c (Blank)))
				       (Times
					(Optional (Pattern d (Blank)))
					(Pattern x (Blank)))))))
				   (Pattern n (Blank))))
		     (Pattern x (Blank Symbol)))
		(Condition (Dist (Times B (Power b -1))
				 (Int (Power (Plus
					      a
					      (Times
					       b
					       (Cos (Plus c (Times d x)))))
					     (Plus n 1))
				      x))
			   (And (FreeQ (List a b c d A B n) x)
				(ZeroQ (Plus
					(Times b A)
					(Times -1 (Times a B))))))) 
     
     
     
    (SetDelayed (Int (Times (Plus (Optional (Pattern A (Blank)))
				  (Times (Optional (Pattern B (Blank)))
					 (Cos
					  (Plus
					   (Optional (Pattern c (Blank)))
					   (Times
					    (Optional (Pattern d (Blank)))
					    (Pattern x (Blank)))))))
			    (Power (Plus
				    (Pattern a (Blank))
				    (Times
				     (Optional (Pattern b (Blank)))
				     (Cos
				      (Plus
				       (Optional (Pattern c (Blank)))
				       (Times
					(Optional (Pattern d (Blank)))
					(Pattern x (Blank)))))))
				   (Pattern n (Blank))))
		     (Pattern x (Blank Symbol)))
		(Condition (Plus (Times (Plus
					 (Times b A)
					 (Times -1 (Times a B)))
					(Sin (Plus c (Times d x)))
					(Power
					 (Plus
					  a
					  (Times
					   b
					   (Cos (Plus c (Times d x)))))
					 (Plus n 1))
					(Power
					 (Times
					  d
					  (Plus n 1)
					  (Plus
					   (Power a 2)
					   (Times -1 (Power b 2))))
					 -1))
				 (Dist
				  (Times 1
					 (Power
					  (Times
					   (Plus n 1)
					   (Plus
					    (Power a 2)
					    (Times -1 (Power b 2))))
					  -1))
				  (Int (Times
					(Sim
					 (Plus
					  (Times
					   (Plus n 1)
					   (Plus
					    (Times a A)
					    (Times -1 (Times b B))))
					  (Times
					   -1
					   (Times
					    (Plus n 2)
					    (Plus
					     (Times b A)
					     (Times -1 (Times a B)))
					    (Cos (Plus c (Times d x))))))
					 x)
					(Power
					 (Plus
					  a
					  (Times
					   b
					   (Cos (Plus c (Times d x)))))
					 (Plus n 1)))
				       x)))
			   (And (FreeQ (List a b c d A B) x)
				(NonzeroQ
				 (Plus (Times b A) (Times -1 (Times a B))))
				(NonzeroQ
				 (Plus (Power a 2) (Times -1 (Power b 2))))
				(RationalQ n) (Comparison n Less -1/2)
				(Comparison n Unequal -1)))) 
     
     
     
    (SetDelayed (Int (Times (Plus (Optional (Pattern A (Blank)))
				  (Times (Optional (Pattern B (Blank)))
					 (Cos
					  (Plus
					   (Optional (Pattern c (Blank)))
					   (Times
					    (Optional (Pattern d (Blank)))
					    (Pattern x (Blank)))))))
			    (Plus (Pattern a (Blank))
				  (Times (Optional (Pattern b (Blank)))
					 (Cos
					  (Plus
					   (Optional (Pattern c (Blank)))
					   (Times
					    (Optional (Pattern d (Blank)))
					    (Pattern x (Blank))))))))
		     (Pattern x (Blank Symbol)))
		nil) 
    (Condition (Plus (Times (Plus (Times 2 a A) (Times b B)) x 1/2)
		     (Times (Plus (Times A b) (Times a B))
			    (Sin (Plus c (Times d x))) (Power d -1))
		     (Times b B (Cos (Plus c (Times d x)))
			    (Sin (Plus c (Times d x)))
			    (Power (Times 2 d) -1)))
	       (FreeQ (List a b c d A B) x)) 
     
     
     
    (SetDelayed (Int (Times (Plus (Optional (Pattern A (Blank)))
				  (Times (Optional (Pattern B (Blank)))
					 (Cos
					  (Plus
					   (Optional (Pattern c (Blank)))
					   (Times
					    (Optional (Pattern d (Blank)))
					    (Pattern x (Blank)))))))
			    (Power (Plus
				    (Pattern a (Blank))
				    (Times
				     (Optional (Pattern b (Blank)))
				     (Cos
				      (Plus
				       (Optional (Pattern c (Blank)))
				       (Times
					(Optional (Pattern d (Blank)))
					(Pattern x (Blank)))))))
				   (Pattern n (Blank))))
		     (Pattern x (Blank Symbol)))
		(Condition (Plus (Times B (Sin (Plus c (Times d x)))
					(Power
					 (Plus
					  a
					  (Times
					   b
					   (Cos (Plus c (Times d x)))))
					 n)
					(Power (Times d (Plus n 1)) -1))
				 (Dist (Times 1 (Power (Plus n 1) -1))
				       (Int (Times
					     (Sim
					      (Plus
					       (Times b B n)
					       (Times a A (Plus n 1))
					       (Times
						(Plus
						 (Times a B n)
						 (Times b A (Plus n 1)))
						(Cos (Plus c (Times d x)))))
					      x)
					     (Power
					      (Plus
					       a
					       (Times
						b
						(Cos (Plus c (Times d x)))))
					      (Plus n -1)))
					    x)))
			   (And (FreeQ (List a b c d A B) x)
				(NonzeroQ
				 (Plus (Times b A) (Times -1 (Times a B))))
				(NonzeroQ
				 (Plus (Power a 2) (Times -1 (Power b 2))))
				(RationalQ n)
				(Comparison n GreaterEqual 1/2)))) 
     
     
     
    (SetDelayed (Int (Times (Power (Cos
				    (Plus
				     (Optional (Pattern c (Blank)))
				     (Times
				      (Optional (Pattern d (Blank)))
				      (Pattern x (Blank)))))
				   (Pattern m (Blank)))
			    (Plus (Pattern A (Blank))
				  (Times (Optional (Pattern B (Blank)))
					 (Cos
					  (Plus
					   (Optional (Pattern c (Blank)))
					   (Times
					    (Optional (Pattern d (Blank)))
					    (Pattern x (Blank)))))))
			    (Plus (Pattern a (Blank))
				  (Times (Optional (Pattern b (Blank)))
					 (Cos
					  (Plus
					   (Optional (Pattern c (Blank)))
					   (Times
					    (Optional (Pattern d (Blank)))
					    (Pattern x (Blank))))))))
		     (Pattern x (Blank Symbol)))
		(Condition (Plus (Times (Times -1 a) A
					(Sin (Plus c (Times d x)))
					(Power
					 (Cos (Plus c (Times d x)))
					 (Plus m 1))
					(Power (Times d (Plus m 1)) -1))
				 (Dist (Times 1 (Power (Plus m 1) -1))
				       (Int (Times
					     (Power
					      (Cos (Plus c (Times d x)))
					      (Plus m 1))
					     (Plus
					      (Times
					       (Plus (Times b A) (Times a B))
					       (Plus m 1))
					      (Times
					       (Plus
						(Times a A (Plus m 2))
						(Times b B (Plus m 1)))
					       (Cos (Plus c (Times d x))))))
					    x)))
			   (And (FreeQ (List a b c d A B) x)
				(NonzeroQ
				 (Plus (Power a 2) (Times -1 (Power b 2))))
				(NonzeroQ
				 (Plus (Times b A) (Times -1 (Times a B))))
				(RationalQ m) (Comparison m Less -1)))) 
     
     
     
    (SetDelayed (Int (Times (Power (Cos
				    (Plus
				     (Optional (Pattern c (Blank)))
				     (Times
				      (Optional (Pattern d (Blank)))
				      (Pattern x (Blank)))))
				   (Optional (Pattern m (Blank))))
			    (Plus (Pattern A (Blank))
				  (Times (Optional (Pattern B (Blank)))
					 (Cos
					  (Plus
					   (Optional (Pattern c (Blank)))
					   (Times
					    (Optional (Pattern d (Blank)))
					    (Pattern x (Blank)))))))
			    (Plus (Pattern a (Blank))
				  (Times (Optional (Pattern b (Blank)))
					 (Cos
					  (Plus
					   (Optional (Pattern c (Blank)))
					   (Times
					    (Optional (Pattern d (Blank)))
					    (Pattern x (Blank))))))))
		     (Pattern x (Blank Symbol)))
		(Condition (Plus (Times b B (Sin (Plus c (Times d x)))
					(Power
					 (Cos (Plus c (Times d x)))
					 (Plus m 1))
					(Power (Times d (Plus m 2)) -1))
				 (Dist (Times 1 (Power (Plus m 2) -1))
				       (Int (Times
					     (Power
					      (Cos (Plus c (Times d x)))
					      m)
					     (Sim
					      (Plus
					       (Times a A (Plus m 2))
					       (Times b B (Plus m 1))
					       (Times
						(Plus (Times b A) (Times a B))
						(Plus m 2)
						(Cos (Plus c (Times d x)))))
					      x))
					    x)))
			   (And (FreeQ (List a b c d A B) x)
				(NonzeroQ
				 (Plus (Power a 2) (Times -1 (Power b 2))))
				(NonzeroQ
				 (Plus (Times b A) (Times -1 (Times a B))))
				(RationalQ m)
				(Comparison m GreaterEqual -1)))) 
     
     
    (SetDelayed (Int (Times (Cos (Plus (Optional (Pattern c (Blank)))
				       (Times
					(Optional (Pattern d (Blank)))
					(Pattern x (Blank)))))
			    (Plus (Pattern A (Blank))
				  (Times (Optional (Pattern B (Blank)))
					 (Cos
					  (Plus
					   (Optional (Pattern c (Blank)))
					   (Times
					    (Optional (Pattern d (Blank)))
					    (Pattern x (Blank)))))))
			    (Power (Plus
				    (Pattern a (Blank))
				    (Times
				     (Optional (Pattern b (Blank)))
				     (Cos
				      (Plus
				       (Optional (Pattern c (Blank)))
				       (Times
					(Optional (Pattern d (Blank)))
					(Pattern x (Blank)))))))
				   (Pattern n (Blank))))
		     (Pattern x (Blank Symbol)))
		(Condition (Plus (Times (Times -1 a)
					(Plus
					 (Times b A)
					 (Times -1 (Times a B)))
					(Sin (Plus c (Times d x)))
					(Power
					 (Plus
					  a
					  (Times
					   b
					   (Cos (Plus c (Times d x)))))
					 (Plus n 1))
					(Power
					 (Times
					  b
					  d
					  (Plus n 1)
					  (Plus
					   (Power a 2)
					   (Times -1 (Power b 2))))
					 -1))
				 (Times -1
					(Dist
					 (Times
					  1
					  (Power
					   (Times
					    b
					    (Plus n 1)
					    (Plus
					     (Power a 2)
					     (Times -1 (Power b 2))))
					   -1))
					 (Int
					  (Times
					   (Sim
					    (Plus
					     (Times
					      b
					      (Plus n 1)
					      (Plus
					       (Times b A)
					       (Times -1 (Times a B))))
					     (Times
					      (Plus
					       (Times (Power a 2) B)
					       (Times
						-1
						(Times a b A (Plus n 2)))
					       (Times
						(Power b 2)
						B
						(Plus n 1)))
					      (Cos (Plus c (Times d x)))))
					    x)
					   (Power
					    (Plus
					     a
					     (Times
					      b
					      (Cos (Plus c (Times d x)))))
					    (Plus n 1)))
					  x))))
			   (And (FreeQ (List a b c d A B) x)
				(NonzeroQ
				 (Plus (Power a 2) (Times -1 (Power b 2))))
				(NonzeroQ
				 (Plus (Times b A) (Times -1 (Times a B))))
				(RationalQ n) (Comparison n Less -1)))) 
     
     
     
    (SetDelayed (Int (Times (Cos (Plus (Optional (Pattern c (Blank)))
				       (Times
					(Optional (Pattern d (Blank)))
					(Pattern x (Blank)))))
			    (Plus (Pattern A (Blank))
				  (Times (Optional (Pattern B (Blank)))
					 (Cos
					  (Plus
					   (Optional (Pattern c (Blank)))
					   (Times
					    (Optional (Pattern d (Blank)))
					    (Pattern x (Blank)))))))
			    (Power (Plus
				    (Pattern a (Blank))
				    (Times
				     (Optional (Pattern b (Blank)))
				     (Cos
				      (Plus
				       (Optional (Pattern c (Blank)))
				       (Times
					(Optional (Pattern d (Blank)))
					(Pattern x (Blank)))))))
				   (Pattern n (Blank))))
		     (Pattern x (Blank Symbol)))
		(Condition (Plus (Times B (Sin (Plus c (Times d x)))
					(Power
					 (Plus
					  a
					  (Times
					   b
					   (Cos (Plus c (Times d x)))))
					 (Plus n 1))
					(Power (Times b d (Plus n 2)) -1))
				 (Dist
				  (Times 1 (Power (Times b (Plus n 2)) -1))
				  (Int (Times
					(Sim
					 (Plus
					  (Times b B (Plus n 1))
					  (Times
					   -1
					   (Times
					    (Plus
					     (Times a B)
					     (Times
					      -1
					      (Times b A (Plus n 2))))
					    (Cos (Plus c (Times d x))))))
					 x)
					(Power
					 (Plus
					  a
					  (Times
					   b
					   (Cos (Plus c (Times d x)))))
					 n))
				       x)))
			   (And (FreeQ (List a b c d A B) x)
				(NonzeroQ
				 (Plus (Power a 2) (Times -1 (Power b 2))))
				(NonzeroQ
				 (Plus (Times b A) (Times -1 (Times a B))))
				(RationalQ n)
				(Comparison n GreaterEqual -1)))) 
     
     
     
    (SetDelayed (Int (Times (Power (Cos
				    (Plus
				     (Optional (Pattern c (Blank)))
				     (Times
				      (Optional (Pattern d (Blank)))
				      (Pattern x (Blank)))))
				   (Optional (Pattern m (Blank))))
			    (Plus (Pattern A (Blank))
				  (Times (Optional (Pattern B (Blank)))
					 (Cos
					  (Plus
					   (Optional (Pattern c (Blank)))
					   (Times
					    (Optional (Pattern d (Blank)))
					    (Pattern x (Blank)))))))
			    (Power (Plus
				    (Pattern a (Blank))
				    (Times
				     (Optional (Pattern b (Blank)))
				     (Cos
				      (Plus
				       (Optional (Pattern c (Blank)))
				       (Times
					(Optional (Pattern d (Blank)))
					(Pattern x (Blank)))))))
				   (Pattern n (Blank))))
		     (Pattern x (Blank Symbol)))
		(Condition (Dist (Times B (Power b -1))
				 (Int (Times (Power
					      (Cos (Plus c (Times d x)))
					      m)
					     (Power
					      (Plus
					       a
					       (Times
						b
						(Cos (Plus c (Times d x)))))
					      (Plus n 1)))
				      x))
			   (And (FreeQ (List a b c d A B m n) x)
				(ZeroQ (Plus
					(Times b A)
					(Times -1 (Times a B))))))) 
     
     
     
    (SetDelayed (Int (Times (Power (Cos
				    (Plus
				     (Optional (Pattern c (Blank)))
				     (Times
				      (Optional (Pattern d (Blank)))
				      (Pattern x (Blank)))))
				   (Pattern m (Blank)))
			    (Plus (Pattern A (Blank))
				  (Times (Optional (Pattern B (Blank)))
					 (Cos
					  (Plus
					   (Optional (Pattern c (Blank)))
					   (Times
					    (Optional (Pattern d (Blank)))
					    (Pattern x (Blank)))))))
			    (Power (Plus
				    (Pattern a (Blank))
				    (Times
				     (Optional (Pattern b (Blank)))
				     (Cos
				      (Plus
				       (Optional (Pattern c (Blank)))
				       (Times
					(Optional (Pattern d (Blank)))
					(Pattern x (Blank)))))))
				   (Pattern n (Blank))))
		     (Pattern x (Blank Symbol)))
		(Condition (Plus (Times (Times -1 a)
					(Plus
					 (Times b A)
					 (Times -1 (Times a B)))
					(Sin (Plus c (Times d x)))
					(Power
					 (Cos (Plus c (Times d x)))
					 (Plus m -1))
					(Power
					 (Plus
					  a
					  (Times
					   b
					   (Cos (Plus c (Times d x)))))
					 (Plus n 1))
					(Power
					 (Times
					  b
					  d
					  (Plus n 1)
					  (Plus
					   (Power a 2)
					   (Times -1 (Power b 2))))
					 -1))
				 (Times -1
					(Dist
					 (Times
					  1
					  (Power
					   (Times
					    b
					    (Plus n 1)
					    (Plus
					     (Power a 2)
					     (Times -1 (Power b 2))))
					   -1))
					 (Int
					  (Times
					   (Power
					    (Cos (Plus c (Times d x)))
					    (Plus m -2))
					   (Sim
					    (Plus
					     (Times
					      a
					      (Plus m -1)
					      (Plus
					       (Times b A)
					       (Times -1 (Times a B))))
					     (Times
					      b
					      (Plus n 1)
					      (Plus
					       (Times b A)
					       (Times -1 (Times a B)))
					      (Cos (Plus c (Times d x))))
					     (Times
					      (Plus
					       (Times (Power a 2) B m)
					       (Times
						-1
						(Times a b A (Plus m n 1)))
					       (Times
						(Power b 2)
						B
						(Plus n 1)))
					      (Power
					       (Cos (Plus c (Times d x)))
					       2)))
					    x)
					   (Power
					    (Plus
					     a
					     (Times
					      b
					      (Cos (Plus c (Times d x)))))
					    (Plus n 1)))
					  x))))
			   (And (FreeQ (List a b c d A B) x)
				(NonzeroQ
				 (Plus (Power a 2) (Times -1 (Power b 2))))
				(NonzeroQ
				 (Plus (Times b A) (Times -1 (Times a B))))
				(RationalQ (List m n))
				(Comparison m Greater 1)
				(Comparison n Less -1/2)
				(Comparison n Unequal -1)))) 
     
     
    (SetDelayed (Int (Times (Power (Cos
				    (Plus
				     (Optional (Pattern c (Blank)))
				     (Times
				      (Optional (Pattern d (Blank)))
				      (Pattern x (Blank)))))
				   (Pattern m (Blank)))
			    (Plus (Pattern A (Blank))
				  (Times (Optional (Pattern B (Blank)))
					 (Cos
					  (Plus
					   (Optional (Pattern c (Blank)))
					   (Times
					    (Optional (Pattern d (Blank)))
					    (Pattern x (Blank)))))))
			    (Power (Plus
				    (Pattern a (Blank))
				    (Times
				     (Optional (Pattern b (Blank)))
				     (Cos
				      (Plus
				       (Optional (Pattern c (Blank)))
				       (Times
					(Optional (Pattern d (Blank)))
					(Pattern x (Blank)))))))
				   (Pattern n (Blank))))
		     (Pattern x (Blank Symbol)))
		(Condition (Plus (Times (Plus
					 (Times b A)
					 (Times -1 (Times a B)))
					(Sin (Plus c (Times d x)))
					(Power
					 (Cos (Plus c (Times d x)))
					 m)
					(Power
					 (Plus
					  a
					  (Times
					   b
					   (Cos (Plus c (Times d x)))))
					 (Plus n 1))
					(Power
					 (Times
					  d
					  (Plus n 1)
					  (Plus
					   (Power a 2)
					   (Times -1 (Power b 2))))
					 -1))
				 (Dist
				  (Times 1
					 (Power
					  (Times
					   (Plus n 1)
					   (Plus
					    (Power a 2)
					    (Times -1 (Power b 2))))
					  -1))
				  (Int (Times
					(Power
					 (Cos (Plus c (Times d x)))
					 (Plus m -1))
					(Sim
					 (Plus
					  (Times
					   m
					   (Plus
					    (Times b A)
					    (Times -1 (Times a B))))
					  (Times
					   (Plus n 1)
					   (Plus
					    (Times a A)
					    (Times -1 (Times b B)))
					   (Cos (Plus c (Times d x))))
					  (Times
					   -1
					   (Times
					    (Plus
					     (Times b A)
					     (Times -1 (Times a B)))
					    (Plus m n 2)
					    (Power
					     (Cos (Plus c (Times d x)))
					     2))))
					 x)
					(Power
					 (Plus
					  a
					  (Times
					   b
					   (Cos (Plus c (Times d x)))))
					 (Plus n 1)))
				       x)))
			   (And (FreeQ (List a b c d A B) x)
				(NonzeroQ
				 (Plus (Power a 2) (Times -1 (Power b 2))))
				(NonzeroQ
				 (Plus (Times b A) (Times -1 (Times a B))))
				(RationalQ (List m n))
				(Comparison 0 Less m Less 1)
				(Comparison n Less -1/2)
				(Comparison n Unequal -1)))) 
     
     
    (SetDelayed (Int (Times (Power (Cos
				    (Plus
				     (Optional (Pattern c (Blank)))
				     (Times
				      (Optional (Pattern d (Blank)))
				      (Pattern x (Blank)))))
				   (Pattern m (Blank)))
			    (Plus (Pattern A (Blank))
				  (Times (Optional (Pattern B (Blank)))
					 (Cos
					  (Plus
					   (Optional (Pattern c (Blank)))
					   (Times
					    (Optional (Pattern d (Blank)))
					    (Pattern x (Blank)))))))
			    (Power (Plus
				    (Pattern a (Blank))
				    (Times
				     (Optional (Pattern b (Blank)))
				     (Cos
				      (Plus
				       (Optional (Pattern c (Blank)))
				       (Times
					(Optional (Pattern d (Blank)))
					(Pattern x (Blank)))))))
				   (Pattern n (Blank))))
		     (Pattern x (Blank Symbol)))
		(Condition (Plus (Times B (Sin (Plus c (Times d x)))
					(Power
					 (Cos (Plus c (Times d x)))
					 (Plus m -1))
					(Power
					 (Plus
					  a
					  (Times
					   b
					   (Cos (Plus c (Times d x)))))
					 (Plus n 1))
					(Power
					 (Times b d (Plus m n 1))
					 -1))
				 (Dist
				  (Times 1
					 (Power (Times b (Plus m n 1)) -1))
				  (Int (Times
					(Power
					 (Cos (Plus c (Times d x)))
					 (Plus m -2))
					(Sim
					 (Plus
					  (Times a B (Plus m -1))
					  (Times
					   b
					   B
					   (Plus m n)
					   (Cos (Plus c (Times d x))))
					  (Times
					   (Plus
					    (Times b A (Plus m n 1))
					    (Times -1 (Times a B m)))
					   (Power
					    (Cos (Plus c (Times d x)))
					    2)))
					 x)
					(Power
					 (Plus
					  a
					  (Times
					   b
					   (Cos (Plus c (Times d x)))))
					 n))
				       x)))
			   (And (FreeQ (List a b c d A B) x)
				(NonzeroQ
				 (Plus (Power a 2) (Times -1 (Power b 2))))
				(NonzeroQ
				 (Plus (Times b A) (Times -1 (Times a B))))
				(RationalQ (List m n))
				(Comparison m Greater 1)
				(Or (Comparison -1/2 LessEqual n Less 1/2)
				    (Comparison n Equal -1))))) 
     
     
     
    (SetDelayed (Int (Times (Power (Cos
				    (Plus
				     (Optional (Pattern c (Blank)))
				     (Times
				      (Optional (Pattern d (Blank)))
				      (Pattern x (Blank)))))
				   (Pattern m (Blank)))
			    (Plus (Pattern A (Blank))
				  (Times (Optional (Pattern B (Blank)))
					 (Cos
					  (Plus
					   (Optional (Pattern c (Blank)))
					   (Times
					    (Optional (Pattern d (Blank)))
					    (Pattern x (Blank)))))))
			    (Power (Plus
				    (Pattern a (Blank))
				    (Times
				     (Optional (Pattern b (Blank)))
				     (Cos
				      (Plus
				       (Optional (Pattern c (Blank)))
				       (Times
					(Optional (Pattern d (Blank)))
					(Pattern x (Blank)))))))
				   (Pattern n (Blank))))
		     (Pattern x (Blank Symbol)))
		(Condition (Plus (Times b B (Sin (Plus c (Times d x)))
					(Power
					 (Cos (Plus c (Times d x)))
					 (Plus m 1))
					(Power
					 (Plus
					  a
					  (Times
					   b
					   (Cos (Plus c (Times d x)))))
					 (Plus n -1))
					(Power (Times d (Plus m n 1)) -1))
				 (Dist (Times 1 (Power (Plus m n 1) -1))
				       (Int (Times
					     (Power
					      (Cos (Plus c (Times d x)))
					      m)
					     (Sim
					      (Plus
					       (Times
						a
						(Plus
						 (Times b B (Plus m 1))
						 (Times a A (Plus m n 1))))
					       (Times
						(Plus
						 (Times
						  (Power b 2)
						  B
						  (Plus m n))
						 (Times
						  a
						  (Plus
						   (Times 2 A b)
						   (Times a B))
						  (Plus m n 1)))
						(Cos (Plus c (Times d x))))
					       (Times
						b
						(Plus
						 (Times A b (Plus m n 1))
						 (Times
						  a
						  B
						  (Plus m (Times 2 n))))
						(Power
						 (Cos (Plus c (Times d x)))
						 2)))
					      x)
					     (Power
					      (Plus
					       a
					       (Times
						b
						(Cos (Plus c (Times d x)))))
					      (Plus n -2)))
					    x)))
			   (And (FreeQ (List a b c d A B) x)
				(NonzeroQ
				 (Plus (Power a 2) (Times -1 (Power b 2))))
				(NonzeroQ
				 (Plus (Times b A) (Times -1 (Times a B))))
				(RationalQ (List m n))
				(Comparison m GreaterEqual -1)
				(Comparison n GreaterEqual 3/2)))) 
     
     
     
    (SetDelayed (Int (Times (Power (Cos
				    (Plus
				     (Optional (Pattern c (Blank)))
				     (Times
				      (Optional (Pattern d (Blank)))
				      (Pattern x (Blank)))))
				   (Pattern m (Blank)))
			    (Plus (Pattern A (Blank))
				  (Times (Optional (Pattern B (Blank)))
					 (Cos
					  (Plus
					   (Optional (Pattern c (Blank)))
					   (Times
					    (Optional (Pattern d (Blank)))
					    (Pattern x (Blank)))))))
			    (Power (Plus
				    (Pattern a (Blank))
				    (Times
				     (Optional (Pattern b (Blank)))
				     (Cos
				      (Plus
				       (Optional (Pattern c (Blank)))
				       (Times
					(Optional (Pattern d (Blank)))
					(Pattern x (Blank)))))))
				   (Pattern n (Blank))))
		     (Pattern x (Blank Symbol)))
		(Condition (Plus (Times B (Sin (Plus c (Times d x)))
					(Power
					 (Cos (Plus c (Times d x)))
					 m)
					(Power
					 (Plus
					  a
					  (Times
					   b
					   (Cos (Plus c (Times d x)))))
					 n)
					(Power (Times d (Plus m n 1)) -1))
				 (Dist (Times 1 (Power (Plus m n 1) -1))
				       (Int (Times
					     (Power
					      (Cos (Plus c (Times d x)))
					      (Plus m -1))
					     (Sim
					      (Plus
					       (Times a B m)
					       (Times
						(Plus
						 (Times a A (Plus m n 1))
						 (Times b B (Plus m n)))
						(Cos (Plus c (Times d x))))
					       (Times
						(Plus
						 (Times b A (Plus m n 1))
						 (Times a B n))
						(Power
						 (Cos (Plus c (Times d x)))
						 2)))
					      x)
					     (Power
					      (Plus
					       a
					       (Times
						b
						(Cos (Plus c (Times d x)))))
					      (Plus n -1)))
					    x)))
			   (And (FreeQ (List a b c d A B) x)
				(NonzeroQ
				 (Plus (Power a 2) (Times -1 (Power b 2))))
				(NonzeroQ
				 (Plus (Times b A) (Times -1 (Times a B))))
				(RationalQ (List m n))
				(Comparison m Greater 0)
				(Comparison 1/2 LessEqual n Less 3/2)))) 
     
     
     
    (SetDelayed (Int (Times (Power (Cos
				    (Plus
				     (Optional (Pattern c (Blank)))
				     (Times
				      (Optional (Pattern d (Blank)))
				      (Pattern x (Blank)))))
				   (Pattern m (Blank)))
			    (Plus (Pattern A (Blank))
				  (Times (Optional (Pattern B (Blank)))
					 (Cos
					  (Plus
					   (Optional (Pattern c (Blank)))
					   (Times
					    (Optional (Pattern d (Blank)))
					    (Pattern x (Blank)))))))
			    (Power (Plus
				    (Pattern a (Blank))
				    (Times
				     (Optional (Pattern b (Blank)))
				     (Cos
				      (Plus
				       (Optional (Pattern c (Blank)))
				       (Times
					(Optional (Pattern d (Blank)))
					(Pattern x (Blank)))))))
				   (Pattern n (Blank))))
		     (Pattern x (Blank Symbol)))
		(Condition (Plus (Times (Times -1 a) A
					(Sin (Plus c (Times d x)))
					(Power
					 (Cos (Plus c (Times d x)))
					 (Plus m 1))
					(Power
					 (Plus
					  a
					  (Times
					   b
					   (Cos (Plus c (Times d x)))))
					 (Plus n -1))
					(Power (Times d (Plus m 1)) -1))
				 (Dist (Times 1 (Power (Plus m 1) -1))
				       (Int (Times
					     (Power
					      (Cos (Plus c (Times d x)))
					      (Plus m 1))
					     (Sim
					      (Plus
					       (Times
						a
						(Plus
						 (Times
						  (Plus (Times b A) (Times a B))
						  (Plus m 1))
						 (Times
						  -1
						  (Times b A (Plus n -1)))))
					       (Times
						(Plus
						 (Times
						  b
						  (Plus
						   (Times b A)
						   (Times 2 a B))
						  (Plus m 1))
						 (Times
						  (Power a 2)
						  A
						  (Plus m 2)))
						(Cos (Plus c (Times d x))))
					       (Times
						b
						(Plus
						 (Times b B (Plus m 1))
						 (Times a A (Plus m n 1)))
						(Power
						 (Cos (Plus c (Times d x)))
						 2)))
					      x)
					     (Power
					      (Plus
					       a
					       (Times
						b
						(Cos (Plus c (Times d x)))))
					      (Plus n -2)))
					    x)))
			   (And (FreeQ (List a b c d A B) x)
				(NonzeroQ
				 (Plus (Power a 2) (Times -1 (Power b 2))))
				(NonzeroQ
				 (Plus (Times b A) (Times -1 (Times a B))))
				(RationalQ (List m n))
				(Comparison m Less -1)
				(Comparison n GreaterEqual 3/2)))) 
     
     
     
    (SetDelayed (Int (Times (Power (Cos
				    (Plus
				     (Optional (Pattern c (Blank)))
				     (Times
				      (Optional (Pattern d (Blank)))
				      (Pattern x (Blank)))))
				   (Pattern m (Blank)))
			    (Plus (Pattern A (Blank))
				  (Times (Optional (Pattern B (Blank)))
					 (Cos
					  (Plus
					   (Optional (Pattern c (Blank)))
					   (Times
					    (Optional (Pattern d (Blank)))
					    (Pattern x (Blank)))))))
			    (Power (Plus
				    (Pattern a (Blank))
				    (Times
				     (Optional (Pattern b (Blank)))
				     (Cos
				      (Plus
				       (Optional (Pattern c (Blank)))
				       (Times
					(Optional (Pattern d (Blank)))
					(Pattern x (Blank)))))))
				   (Optional (Pattern n (Blank)))))
		     (Pattern x (Blank Symbol)))
		(Condition (Plus (Times (Times -1 A)
					(Sin (Plus c (Times d x)))
					(Power
					 (Cos (Plus c (Times d x)))
					 (Plus m 1))
					(Power
					 (Plus
					  a
					  (Times
					   b
					   (Cos (Plus c (Times d x)))))
					 n)
					(Power (Times d (Plus m 1)) -1))
				 (Dist (Times 1 (Power (Plus m 1) -1))
				       (Int (Times
					     (Power
					      (Cos (Plus c (Times d x)))
					      (Plus m 1))
					     (Sim
					      (Plus
					       (Times a B (Plus m 1))
					       (Times -1 (Times b A n))
					       (Times
						(Plus
						 (Times b B (Plus m 1))
						 (Times a A (Plus m 2)))
						(Cos (Plus c (Times d x))))
					       (Times
						b
						A
						(Plus m n 2)
						(Power
						 (Cos (Plus c (Times d x)))
						 2)))
					      x)
					     (Power
					      (Plus
					       a
					       (Times
						b
						(Cos (Plus c (Times d x)))))
					      (Plus n -1)))
					    x)))
			   (And (FreeQ (List a b c d A B) x)
				(NonzeroQ
				 (Plus (Power a 2) (Times -1 (Power b 2))))
				(NonzeroQ
				 (Plus (Times b A) (Times -1 (Times a B))))
				(RationalQ (List m n))
				(Comparison m Less 0)
				(Comparison m Unequal -1)
				(Comparison 1/2 LessEqual n Less 3/2)))) 
     
     
    (SetDelayed (Int (Times (Power (Cos
				    (Plus
				     (Optional (Pattern c (Blank)))
				     (Times
				      (Optional (Pattern d (Blank)))
				      (Pattern x (Blank)))))
				   (Pattern m (Blank)))
			    (Plus (Pattern A (Blank))
				  (Times (Optional (Pattern B (Blank)))
					 (Cos
					  (Plus
					   (Optional (Pattern c (Blank)))
					   (Times
					    (Optional (Pattern d (Blank)))
					    (Pattern x (Blank)))))))
			    (Power (Plus
				    (Pattern a (Blank))
				    (Times
				     (Optional (Pattern b (Blank)))
				     (Cos
				      (Plus
				       (Optional (Pattern c (Blank)))
				       (Times
					(Optional (Pattern d (Blank)))
					(Pattern x (Blank)))))))
				   (Pattern n (Blank))))
		     (Pattern x (Blank Symbol)))
		(Condition (Plus (Times (Times -1 A)
					(Sin (Plus c (Times d x)))
					(Power
					 (Cos (Plus c (Times d x)))
					 (Plus m 1))
					(Power
					 (Plus
					  a
					  (Times
					   b
					   (Cos (Plus c (Times d x)))))
					 (Plus n 1))
					(Power (Times a d (Plus m 1)) -1))
				 (Dist
				  (Times 1 (Power (Times a (Plus m 1)) -1))
				  (Int (Times
					(Power
					 (Cos (Plus c (Times d x)))
					 (Plus m 1))
					(Sim
					 (Plus
					  (Times a B (Plus m 1))
					  (Times
					   -1
					   (Times b A (Plus m n 2)))
					  (Times
					   a
					   A
					   (Plus m 2)
					   (Cos (Plus c (Times d x))))
					  (Times
					   b
					   A
					   (Plus m n 3)
					   (Power
					    (Cos (Plus c (Times d x)))
					    2)))
					 x)
					(Power
					 (Plus
					  a
					  (Times
					   b
					   (Cos (Plus c (Times d x)))))
					 n))
				       x)))
			   (And (FreeQ (List a b c d A B) x)
				(NonzeroQ
				 (Plus (Power a 2) (Times -1 (Power b 2))))
				(NonzeroQ
				 (Plus (Times b A) (Times -1 (Times a B))))
				(RationalQ (List m n))
				(Comparison m Less -1)
				(Or (Comparison -1/2 LessEqual n Less 1/2)
				    (Comparison n Equal -1))))) 
     
     
     
    (SetDelayed (Int (Times (Power (Cos
				    (Plus
				     (Optional (Pattern c (Blank)))
				     (Times
				      (Optional (Pattern d (Blank)))
				      (Pattern x (Blank)))))
				   (Pattern m (Blank)))
			    (Plus (Pattern A (Blank))
				  (Times (Optional (Pattern B (Blank)))
					 (Cos
					  (Plus
					   (Optional (Pattern c (Blank)))
					   (Times
					    (Optional (Pattern d (Blank)))
					    (Pattern x (Blank)))))))
			    (Power (Plus
				    (Pattern a (Blank))
				    (Times
				     (Optional (Pattern b (Blank)))
				     (Cos
				      (Plus
				       (Optional (Pattern c (Blank)))
				       (Times
					(Optional (Pattern d (Blank)))
					(Pattern x (Blank)))))))
				   (Pattern n (Blank))))
		     (Pattern x (Blank Symbol)))
		(Condition (Plus (Times (Times -1 b)
					(Plus
					 (Times b A)
					 (Times -1 (Times a B)))
					(Sin (Plus c (Times d x)))
					(Power
					 (Cos (Plus c (Times d x)))
					 (Plus m 1))
					(Power
					 (Plus
					  a
					  (Times
					   b
					   (Cos (Plus c (Times d x)))))
					 (Plus n 1))
					(Power
					 (Times
					  a
					  d
					  (Plus n 1)
					  (Plus
					   (Power a 2)
					   (Times -1 (Power b 2))))
					 -1))
				 (Dist
				  (Times 1
					 (Power
					  (Times
					   a
					   (Plus n 1)
					   (Plus
					    (Power a 2)
					    (Times -1 (Power b 2))))
					  -1))
				  (Int (Times
					(Power
					 (Cos (Plus c (Times d x)))
					 m)
					(Sim
					 (Plus
					  (Times
					   A
					   (Plus
					    (Times (Power a 2) (Plus n 1))
					    (Times
					     -1
					     (Times
					      (Power b 2)
					      (Plus m n 2)))))
					  (Times a b B (Plus m 1))
					  (Times
					   -1
					   (Times
					    a
					    (Plus n 1)
					    (Plus
					     (Times b A)
					     (Times -1 (Times a B)))
					    (Cos (Plus c (Times d x)))))
					  (Times
					   b
					   (Plus
					    (Times b A)
					    (Times -1 (Times a B)))
					   (Plus m n 3)
					   (Power
					    (Cos (Plus c (Times d x)))
					    2)))
					 x)
					(Power
					 (Plus
					  a
					  (Times
					   b
					   (Cos (Plus c (Times d x)))))
					 (Plus n 1)))
				       x)))
			   (And (FreeQ (List a b c d A B) x)
				(NonzeroQ
				 (Plus (Power a 2) (Times -1 (Power b 2))))
				(NonzeroQ
				 (Plus (Times b A) (Times -1 (Times a B))))
				(RationalQ (List m n))
				(Comparison m Less 0)
				(Comparison n Less -1/2)
				(Comparison n Unequal -1)))) 
     
     
    (SetDelayed (Int (Times (Plus (Pattern A (Blank))
				  (Times (Optional (Pattern C (Blank)))
					 (Power
					  (Cos
					   (Plus
					    (Optional (Pattern c (Blank)))
					    (Times
					     (Optional (Pattern d (Blank)))
					     (Pattern x (Blank)))))
					  2)))
			    (Power (Times
				    (Sqrt
				     (Cos
				      (Plus
				       (Optional (Pattern c (Blank)))
				       (Times
					(Optional (Pattern d (Blank)))
					(Pattern x (Blank))))))
				    (Plus
				     (Pattern a (Blank))
				     (Times
				      (Optional (Pattern b (Blank)))
				      (Cos
				       (Plus
					(Optional (Pattern c (Blank)))
					(Times
					 (Optional (Pattern d (Blank)))
					 (Pattern x (Blank))))))))
				   -1))
		     (Pattern x (Blank Symbol)))
		(Condition (Plus (Dist (Times C (Power b -1))
				       (Int (Sqrt (Cos (Plus c (Times d x))))
					    x))
				 (Dist (Times 1 (Power b -1))
				       (Int (Times
					     (Plus
					      (Times A b)
					      (Times
					       -1
					       (Times
						a
						C
						(Cos (Plus c (Times d x))))))
					     (Power
					      (Times
					       (Sqrt (Cos (Plus c (Times d x))))
					       (Plus
						a
						(Times
						 b
						 (Cos (Plus c (Times d x))))))
					      -1))
					    x)))
			   (And (FreeQ (List a b c d A C) x)
				(NonzeroQ
				 (Plus (Power a 2)
				       (Times -1 (Power b 2))))))) 
     
    (SetDelayed (Int (Times (Plus (Pattern A (Blank))
				  (Times (Optional (Pattern C (Blank)))
					 (Power
					  (Cos
					   (Plus
					    (Optional (Pattern c (Blank)))
					    (Times
					     (Optional (Pattern d (Blank)))
					     (Pattern x (Blank)))))
					  2)))
			    (Power (Times
				    (Sqrt
				     (Cos
				      (Plus
				       (Optional (Pattern c (Blank)))
				       (Times
					(Optional (Pattern d (Blank)))
					(Pattern x (Blank))))))
				    (Sqrt
				     (Plus
				      (Pattern a (Blank))
				      (Times
				       (Optional (Pattern b (Blank)))
				       (Cos
					(Plus
					 (Optional (Pattern c (Blank)))
					 (Times
					  (Optional (Pattern d (Blank)))
					  (Pattern x (Blank)))))))))
				   -1))
		     (Pattern x (Blank Symbol)))
		(Condition (Plus (Times C (Sqrt (Cos (Plus c (Times d x))))
					(Sqrt
					 (Plus
					  a
					  (Times
					   b
					   (Cos (Plus c (Times d x))))))
					(Tan
					 (Times (Plus c (Times d x)) 1/2))
					(Power (Times b d) -1))
				 (Dist (Times C (Power b -1))
				       (Int (Times
					     (Sqrt
					      (Plus
					       a
					       (Times
						b
						(Cos (Plus c (Times d x))))))
					     (Power
					      (Times
					       (Sqrt (Cos (Plus c (Times d x))))
					       (Plus
						1
						(Cos (Plus c (Times d x)))))
					      -1))
					    x))
				 (Dist (Times 1 (Power (Times 2 b) -1))
				       (Int (Times
					     (Plus
					      (Times 2 A b)
					      (Times -1 (Times a C))
					      (Times
					       -1
					       (Times
						a
						C
						(Cos (Plus c (Times d x))))))
					     (Power
					      (Times
					       (Sqrt (Cos (Plus c (Times d x))))
					       (Sqrt
						(Plus
						 a
						 (Times
						  b
						  (Cos (Plus c (Times d x)))))))
					      -1))
					    x)))
			   (And (FreeQ (List a b c d A C) x)
				(NonzeroQ
				 (Plus (Power a 2)
				       (Times -1 (Power b 2))))))) 
     
     
    (SetDelayed (Int (Times (Power (Cos
				    (Plus
				     (Optional (Pattern c (Blank)))
				     (Times
				      (Optional (Pattern d (Blank)))
				      (Pattern x (Blank)))))
				   (Optional (Pattern m (Blank))))
			    (Plus (Pattern A (Blank))
				  (Times (Optional (Pattern C (Blank)))
					 (Power
					  (Cos
					   (Plus
					    (Optional (Pattern c (Blank)))
					    (Times
					     (Optional (Pattern d (Blank)))
					     (Pattern x (Blank)))))
					  2))))
		     (Pattern x (Blank Symbol)))
		(Condition (Times C (Sin (Plus c (Times d x)))
				  (Power (Cos (Plus c (Times d x)))
					 (Plus m 1))
				  (Power (Times d (Plus m 2)) -1))
			   (And (FreeQ (List c d A C m) x)
				(ZeroQ (Plus
					(Times A (Plus m 2))
					(Times C (Plus m 1))))))) 
     
     
    (SetDelayed (Int (Times (Power (Cos
				    (Plus
				     (Optional (Pattern c (Blank)))
				     (Times
				      (Optional (Pattern d (Blank)))
				      (Pattern x (Blank)))))
				   (Pattern m (Blank)))
			    (Plus (Pattern A (Blank))
				  (Times (Optional (Pattern C (Blank)))
					 (Power
					  (Cos
					   (Plus
					    (Optional (Pattern c (Blank)))
					    (Times
					     (Optional (Pattern d (Blank)))
					     (Pattern x (Blank)))))
					  2))))
		     (Pattern x (Blank Symbol)))
		(Condition (Plus (Times (Times -1 A)
					(Sin (Plus c (Times d x)))
					(Power
					 (Cos (Plus c (Times d x)))
					 (Plus m 1))
					(Power (Times d (Plus m 1)) -1))
				 (Dist
				  (Times (Plus
					  (Times A (Plus m 2))
					  (Times C (Plus m 1)))
					 (Power (Plus m 1) -1))
				  (Int (Power
					(Cos (Plus c (Times d x)))
					(Plus m 2))
				       x)))
			   (And (FreeQ (List c d A C) x)
				(NonzeroQ
				 (Plus (Times A (Plus m 2))
				       (Times C (Plus m 1))))
				(RationalQ m) (Comparison m Less -1)))) 
     
     
    (SetDelayed (Int (Times (Power (Cos
				    (Plus
				     (Optional (Pattern c (Blank)))
				     (Times
				      (Optional (Pattern d (Blank)))
				      (Pattern x (Blank)))))
				   (Optional (Pattern m (Blank))))
			    (Plus (Pattern A (Blank))
				  (Times (Optional (Pattern C (Blank)))
					 (Power
					  (Cos
					   (Plus
					    (Optional (Pattern c (Blank)))
					    (Times
					     (Optional (Pattern d (Blank)))
					     (Pattern x (Blank)))))
					  2))))
		     (Pattern x (Blank Symbol)))
		(Condition (Plus (Times C (Sin (Plus c (Times d x)))
					(Power
					 (Cos (Plus c (Times d x)))
					 (Plus m 1))
					(Power (Times d (Plus m 2)) -1))
				 (Dist
				  (Times (Plus
					  (Times A (Plus m 2))
					  (Times C (Plus m 1)))
					 (Power (Plus m 2) -1))
				  (Int (Power (Cos (Plus c (Times d x))) m)
				       x)))
			   (And (FreeQ (List c d A C) x)
				(NonzeroQ
				 (Plus (Times A (Plus m 2))
				       (Times C (Plus m 1))))
				(RationalQ m)
				(Comparison m GreaterEqual -1)))) 
     
     
    (SetDelayed (Int (Times (Plus (Pattern A (Blank))
				  (Times (Optional (Pattern C (Blank)))
					 (Power
					  (Cos
					   (Plus
					    (Optional (Pattern c (Blank)))
					    (Times
					     (Optional (Pattern d (Blank)))
					     (Pattern x (Blank)))))
					  2)))
			    (Power (Plus
				    (Pattern a (Blank))
				    (Times
				     (Optional (Pattern b (Blank)))
				     (Cos
				      (Plus
				       (Optional (Pattern c (Blank)))
				       (Times
					(Optional (Pattern d (Blank)))
					(Pattern x (Blank)))))))
				   (Optional (Pattern n (Blank)))))
		     (Pattern x (Blank Symbol)))
		(Condition (Times -1
				  (Dist (Times C (Power (Power b 2) -1))
					(Int
					 (Times
					  (Plus
					   a
					   (Times
					    -1
					    (Times
					     b
					     (Cos (Plus c (Times d x))))))
					  (Power
					   (Plus
					    a
					    (Times
					     b
					     (Cos (Plus c (Times d x)))))
					   (Plus n 1)))
					 x)))
			   (And (FreeQ (List a b c d A C) x)
				(ZeroQ (Plus
					(Times (Power a 2) C)
					(Times (Power b 2) A)))
				(RationalQ n) (Comparison n Less -1)))) 
     
     
     
    (SetDelayed (Int (Times (Plus (Optional (Pattern A (Blank)))
				  (Times (Optional (Pattern C (Blank)))
					 (Power
					  (Cos
					   (Plus
					    (Optional (Pattern c (Blank)))
					    (Times
					     (Optional (Pattern d (Blank)))
					     (Pattern x (Blank)))))
					  2)))
			    (Power (Plus
				    (Pattern a (Blank))
				    (Times
				     (Optional (Pattern b (Blank)))
				     (Cos
				      (Plus
				       (Optional (Pattern c (Blank)))
				       (Times
					(Optional (Pattern d (Blank)))
					(Pattern x (Blank)))))))
				   (Optional (Pattern n (Blank)))))
		     (Pattern x (Blank Symbol)))
		(Condition (Plus (Times (Plus
					 (Times (Power a 2) C)
					 (Times (Power b 2) A))
					(Sin (Plus c (Times d x)))
					(Power
					 (Plus
					  a
					  (Times
					   b
					   (Cos (Plus c (Times d x)))))
					 (Plus n 1))
					(Power
					 (Times
					  b
					  d
					  (Plus n 1)
					  (Plus
					   (Power a 2)
					   (Times -1 (Power b 2))))
					 -1))
				 (Dist
				  (Times 1
					 (Power
					  (Times
					   b
					   (Plus n 1)
					   (Plus
					    (Power a 2)
					    (Times -1 (Power b 2))))
					  -1))
				  (Int (Times
					(Sim
					 (Plus
					  (Times a b (Plus A C) (Plus n 1))
					  (Times
					   -1
					   (Times
					    (Plus
					     (Times
					      (Power b 2)
					      A
					      (Plus n 2))
					     (Times
					      C
					      (Plus
					       (Power a 2)
					       (Times
						(Power b 2)
						(Plus n 1)))))
					    (Cos (Plus c (Times d x))))))
					 x)
					(Power
					 (Plus
					  a
					  (Times
					   b
					   (Cos (Plus c (Times d x)))))
					 (Plus n 1)))
				       x)))
			   (And (FreeQ (List a b c d A C) x)
				(NonzeroQ
				 (Plus (Power a 2) (Times -1 (Power b 2))))
				(NonzeroQ
				 (Plus (Times (Power a 2) C)
				       (Times (Power b 2) A)))
				(RationalQ n) (Comparison n Less -1)))) 
     
     
    (SetDelayed (Int (Times (Plus (Optional (Pattern A (Blank)))
				  (Times (Optional (Pattern C (Blank)))
					 (Power
					  (Cos
					   (Plus
					    (Optional (Pattern c (Blank)))
					    (Times
					     (Optional (Pattern d (Blank)))
					     (Pattern x (Blank)))))
					  2)))
			    (Power (Plus
				    (Pattern a (Blank))
				    (Times
				     (Optional (Pattern b (Blank)))
				     (Cos
				      (Plus
				       (Optional (Pattern c (Blank)))
				       (Times
					(Optional (Pattern d (Blank)))
					(Pattern x (Blank)))))))
				   (Optional (Pattern n (Blank)))))
		     (Pattern x (Blank Symbol)))
		(Condition (Plus (Times C (Sin (Plus c (Times d x)))
					(Power
					 (Plus
					  a
					  (Times
					   b
					   (Cos (Plus c (Times d x)))))
					 (Plus n 1))
					(Power (Times b d (Plus n 2)) -1))
				 (Dist
				  (Times 1 (Power (Times b (Plus n 2)) -1))
				  (Int (Times
					(Sim
					 (Plus
					  (Times
					   b
					   (Plus
					    (Times A (Plus n 2))
					    (Times C (Plus n 1))))
					  (Times
					   -1
					   (Times
					    a
					    C
					    (Cos (Plus c (Times d x))))))
					 x)
					(Power
					 (Plus
					  a
					  (Times
					   b
					   (Cos (Plus c (Times d x)))))
					 n))
				       x)))
			   (And (FreeQ (List a b c d A C) x) (RationalQ n)
				(Comparison n GreaterEqual -1)))) 
     
     
    (SetDelayed (Int (Times (Power (Cos
				    (Plus
				     (Optional (Pattern c (Blank)))
				     (Times
				      (Optional (Pattern d (Blank)))
				      (Pattern x (Blank)))))
				   (Optional (Pattern m (Blank))))
			    (Plus (Pattern A (Blank))
				  (Times (Optional (Pattern C (Blank)))
					 (Power
					  (Cos
					   (Plus
					    (Optional (Pattern c (Blank)))
					    (Times
					     (Optional (Pattern d (Blank)))
					     (Pattern x (Blank)))))
					  2)))
			    (Power (Plus
				    (Pattern a (Blank))
				    (Times
				     (Optional (Pattern b (Blank)))
				     (Cos
				      (Plus
				       (Optional (Pattern c (Blank)))
				       (Times
					(Optional (Pattern d (Blank)))
					(Pattern x (Blank)))))))
				   (Optional (Pattern n (Blank)))))
		     (Pattern x (Blank Symbol)))
		(Condition (Dist (Times C (Power (Power b 2) -1))
				 (Int (Times (Power
					      (Cos (Plus c (Times d x)))
					      m)
					     (Plus
					      (Times -1 a)
					      (Times
					       b
					       (Cos (Plus c (Times d x)))))
					     (Power
					      (Plus
					       a
					       (Times
						b
						(Cos (Plus c (Times d x)))))
					      (Plus n 1)))
				      x))
			   (And (FreeQ (List a b c d A C m) x)
				(ZeroQ (Plus
					(Times (Power a 2) C)
					(Times (Power b 2) A)))
				(RationalQ n) (Comparison n Less -1/2)))) 
     
     
    (SetDelayed (Int (Times (Power (Cos
				    (Plus
				     (Optional (Pattern c (Blank)))
				     (Times
				      (Optional (Pattern d (Blank)))
				      (Pattern x (Blank)))))
				   (Optional (Pattern m (Blank))))
			    (Plus (Pattern A (Blank))
				  (Times (Optional (Pattern C (Blank)))
					 (Power
					  (Cos
					   (Plus
					    (Optional (Pattern c (Blank)))
					    (Times
					     (Optional (Pattern d (Blank)))
					     (Pattern x (Blank)))))
					  2)))
			    (Power (Plus
				    (Pattern a (Blank))
				    (Times
				     (Optional (Pattern b (Blank)))
				     (Cos
				      (Plus
				       (Optional (Pattern c (Blank)))
				       (Times
					(Optional (Pattern d (Blank)))
					(Pattern x (Blank)))))))
				   (Pattern n (Blank))))
		     (Pattern x (Blank Symbol)))
		(Condition (Plus (Times (Plus
					 (Times (Power a 2) C)
					 (Times (Power b 2) A))
					(Sin (Plus c (Times d x)))
					(Power
					 (Cos (Plus c (Times d x)))
					 m)
					(Power
					 (Plus
					  a
					  (Times
					   b
					   (Cos (Plus c (Times d x)))))
					 (Plus n 1))
					(Power
					 (Times
					  b
					  d
					  (Plus n 1)
					  (Plus
					   (Power a 2)
					   (Times -1 (Power b 2))))
					 -1))
				 (Dist
				  (Times 1
					 (Power
					  (Times
					   b
					   (Plus n 1)
					   (Plus
					    (Power a 2)
					    (Times -1 (Power b 2))))
					  -1))
				  (Int (Times
					(Power
					 (Cos (Plus c (Times d x)))
					 (Plus m -1))
					(Sim
					 (Plus
					  (Times
					   m
					   (Plus
					    (Times (Power a 2) C)
					    (Times (Power b 2) A)))
					  (Times
					   a
					   b
					   (Plus n 1)
					   (Plus C A)
					   (Cos (Plus c (Times d x))))
					  (Times
					   -1
					   (Times
					    (Plus
					     (Times
					      (Power a 2)
					      C
					      (Plus m 1))
					     (Times
					      (Power b 2)
					      (Plus
					       (Times A (Plus m n 2))
					       (Times C (Plus n 1)))))
					    (Power
					     (Cos (Plus c (Times d x)))
					     2))))
					 x)
					(Power
					 (Plus
					  a
					  (Times
					   b
					   (Cos (Plus c (Times d x)))))
					 (Plus n 1)))
				       x)))
			   (And (FreeQ (List a b c d A C) x)
				(NonzeroQ
				 (Plus (Power a 2) (Times -1 (Power b 2))))
				(NonzeroQ
				 (Plus (Times (Power a 2) C)
				       (Times (Power b 2) A)))
				(RationalQ (List m n))
				(Comparison m Greater 0)
				(Comparison n Less -1/2)
				(Comparison n Unequal -1)))) 
     
     
     
    (SetDelayed (Int (Times (Power (Cos
				    (Plus
				     (Optional (Pattern c (Blank)))
				     (Times
				      (Optional (Pattern d (Blank)))
				      (Pattern x (Blank)))))
				   (Optional (Pattern m (Blank))))
			    (Plus (Pattern A (Blank))
				  (Times (Optional (Pattern C (Blank)))
					 (Power
					  (Cos
					   (Plus
					    (Optional (Pattern c (Blank)))
					    (Times
					     (Optional (Pattern d (Blank)))
					     (Pattern x (Blank)))))
					  2)))
			    (Power (Plus
				    (Pattern a (Blank))
				    (Times
				     (Optional (Pattern b (Blank)))
				     (Cos
				      (Plus
				       (Optional (Pattern c (Blank)))
				       (Times
					(Optional (Pattern d (Blank)))
					(Pattern x (Blank)))))))
				   (Pattern n (Blank))))
		     (Pattern x (Blank Symbol)))
		(Condition (Plus (Times C (Sin (Plus c (Times d x)))
					(Power
					 (Cos (Plus c (Times d x)))
					 m)
					(Power
					 (Plus
					  a
					  (Times
					   b
					   (Cos (Plus c (Times d x)))))
					 (Plus n 1))
					(Power
					 (Times b d (Plus m n 2))
					 -1))
				 (Dist
				  (Times 1
					 (Power (Times b (Plus m n 2)) -1))
				  (Int (Times
					(Power
					 (Cos (Plus c (Times d x)))
					 (Plus m -1))
					(Sim
					 (Plus
					  (Times a C m)
					  (Times
					   (Plus
					    (Times b A (Plus m n 2))
					    (Times b C (Plus m n 1)))
					   (Cos (Plus c (Times d x))))
					  (Times
					   -1
					   (Times
					    a
					    C
					    (Plus m 1)
					    (Power
					     (Cos (Plus c (Times d x)))
					     2))))
					 x)
					(Power
					 (Plus
					  a
					  (Times
					   b
					   (Cos (Plus c (Times d x)))))
					 n))
				       x)))
			   (And (FreeQ (List a b c d A C) x)
				(NonzeroQ
				 (Plus (Power a 2) (Times -1 (Power b 2))))
				(RationalQ (List m n))
				(Comparison m Greater 0)
				(Or (Comparison -1/2 LessEqual n Less 1/2)
				    (Comparison n Equal -1))))) 
     
     
     
    (SetDelayed (Int (Times (Power (Cos
				    (Plus
				     (Optional (Pattern c (Blank)))
				     (Times
				      (Optional (Pattern d (Blank)))
				      (Pattern x (Blank)))))
				   (Optional (Pattern m (Blank))))
			    (Plus (Pattern A (Blank))
				  (Times (Optional (Pattern C (Blank)))
					 (Power
					  (Cos
					   (Plus
					    (Optional (Pattern c (Blank)))
					    (Times
					     (Optional (Pattern d (Blank)))
					     (Pattern x (Blank)))))
					  2)))
			    (Power (Plus
				    (Pattern a (Blank))
				    (Times
				     (Optional (Pattern b (Blank)))
				     (Cos
				      (Plus
				       (Optional (Pattern c (Blank)))
				       (Times
					(Optional (Pattern d (Blank)))
					(Pattern x (Blank)))))))
				   (Optional (Pattern n (Blank)))))
		     (Pattern x (Blank Symbol)))
		(Condition (Plus (Times C (Sin (Plus c (Times d x)))
					(Power
					 (Cos (Plus c (Times d x)))
					 (Plus m 1))
					(Power
					 (Plus
					  a
					  (Times
					   b
					   (Cos (Plus c (Times d x)))))
					 n)
					(Power (Times d (Plus m n 2)) -1))
				 (Dist (Times 1 (Power (Plus m n 2) -1))
				       (Int (Times
					     (Power
					      (Cos (Plus c (Times d x)))
					      m)
					     (Sim
					      (Plus
					       (Times
						a
						(Plus
						 (Times A (Plus m n 2))
						 (Times C (Plus m 1))))
					       (Times
						(Plus
						 (Times b A (Plus m n 2))
						 (Times b C (Plus m n 1)))
						(Cos (Plus c (Times d x))))
					       (Times
						a
						C
						n
						(Power
						 (Cos (Plus c (Times d x)))
						 2)))
					      x)
					     (Power
					      (Plus
					       a
					       (Times
						b
						(Cos (Plus c (Times d x)))))
					      (Plus n -1)))
					    x)))
			   (And (FreeQ (List a b c d A C) x)
				(NonzeroQ
				 (Plus (Power a 2) (Times -1 (Power b 2))))
				(RationalQ (List m n))
				(Comparison m GreaterEqual -1)
				(Comparison n GreaterEqual 1/2)))) 
     
     
     
    (SetDelayed (Int (Times (Power (Cos
				    (Plus
				     (Optional (Pattern c (Blank)))
				     (Times
				      (Optional (Pattern d (Blank)))
				      (Pattern x (Blank)))))
				   (Pattern m (Blank)))
			    (Plus (Pattern A (Blank))
				  (Times (Optional (Pattern C (Blank)))
					 (Power
					  (Cos
					   (Plus
					    (Optional (Pattern c (Blank)))
					    (Times
					     (Optional (Pattern d (Blank)))
					     (Pattern x (Blank)))))
					  2)))
			    (Power (Plus
				    (Pattern a (Blank))
				    (Times
				     (Optional (Pattern b (Blank)))
				     (Cos
				      (Plus
				       (Optional (Pattern c (Blank)))
				       (Times
					(Optional (Pattern d (Blank)))
					(Pattern x (Blank)))))))
				   (Optional (Pattern n (Blank)))))
		     (Pattern x (Blank Symbol)))
		(Condition (Plus (Times (Times -1 A)
					(Sin (Plus c (Times d x)))
					(Power
					 (Cos (Plus c (Times d x)))
					 (Plus m 1))
					(Power
					 (Plus
					  a
					  (Times
					   b
					   (Cos (Plus c (Times d x)))))
					 n)
					(Power (Times d (Plus m 1)) -1))
				 (Dist (Times 1 (Power (Plus m 1) -1))
				       (Int (Times
					     (Power
					      (Cos (Plus c (Times d x)))
					      (Plus m 1))
					     (Sim
					      (Plus
					       (Times (Times -1 b) A n)
					       (Times
						(Plus
						 (Times a C (Plus m 1))
						 (Times a A (Plus m 2)))
						(Cos (Plus c (Times d x))))
					       (Times
						b
						(Plus
						 (Times C (Plus m 1))
						 (Times A (Plus m n 2)))
						(Power
						 (Cos (Plus c (Times d x)))
						 2)))
					      x)
					     (Power
					      (Plus
					       a
					       (Times
						b
						(Cos (Plus c (Times d x)))))
					      (Plus n -1)))
					    x)))
			   (And (FreeQ (List a b c d A C) x)
				(NonzeroQ
				 (Plus (Power a 2) (Times -1 (Power b 2))))
				(RationalQ (List m n))
				(Comparison m Less -1)
				(Comparison n GreaterEqual 1/2)))) 
     
     
     
    (SetDelayed (Int (Times (Power (Cos
				    (Plus
				     (Optional (Pattern c (Blank)))
				     (Times
				      (Optional (Pattern d (Blank)))
				      (Pattern x (Blank)))))
				   (Pattern m (Blank)))
			    (Plus (Pattern A (Blank))
				  (Times (Optional (Pattern C (Blank)))
					 (Power
					  (Cos
					   (Plus
					    (Optional (Pattern c (Blank)))
					    (Times
					     (Optional (Pattern d (Blank)))
					     (Pattern x (Blank)))))
					  2)))
			    (Power (Plus
				    (Pattern a (Blank))
				    (Times
				     (Optional (Pattern b (Blank)))
				     (Cos
				      (Plus
				       (Optional (Pattern c (Blank)))
				       (Times
					(Optional (Pattern d (Blank)))
					(Pattern x (Blank)))))))
				   (Pattern n (Blank))))
		     (Pattern x (Blank Symbol)))
		(Condition (Plus (Times (Times -1 A)
					(Sin (Plus c (Times d x)))
					(Power
					 (Cos (Plus c (Times d x)))
					 (Plus m 1))
					(Power
					 (Plus
					  a
					  (Times
					   b
					   (Cos (Plus c (Times d x)))))
					 (Plus n 1))
					(Power (Times a d (Plus m 1)) -1))
				 (Dist
				  (Times 1 (Power (Times a (Plus m 1)) -1))
				  (Int (Times
					(Power
					 (Cos (Plus c (Times d x)))
					 (Plus m 1))
					(Sim
					 (Plus
					  (Times
					   (Times -1 b)
					   A
					   (Plus m n 2))
					  (Times
					   a
					   (Plus
					    (Times A (Plus m 2))
					    (Times C (Plus m 1)))
					   (Cos (Plus c (Times d x))))
					  (Times
					   b
					   A
					   (Plus m n 3)
					   (Power
					    (Cos (Plus c (Times d x)))
					    2)))
					 x)
					(Power
					 (Plus
					  a
					  (Times
					   b
					   (Cos (Plus c (Times d x)))))
					 n))
				       x)))
			   (And (FreeQ (List a b c d A C) x)
				(NonzeroQ
				 (Plus (Power a 2) (Times -1 (Power b 2))))
				(RationalQ (List m n))
				(Comparison m Less -1)
				(Or (Comparison -1/2 LessEqual n Less 1/2)
				    (Comparison n Equal -1))))) 
     
     
     
    (SetDelayed (Int (Times (Power (Cos
				    (Plus
				     (Optional (Pattern c (Blank)))
				     (Times
				      (Optional (Pattern d (Blank)))
				      (Pattern x (Blank)))))
				   (Pattern m (Blank)))
			    (Plus (Pattern A (Blank))
				  (Times (Optional (Pattern C (Blank)))
					 (Power
					  (Cos
					   (Plus
					    (Optional (Pattern c (Blank)))
					    (Times
					     (Optional (Pattern d (Blank)))
					     (Pattern x (Blank)))))
					  2)))
			    (Power (Plus
				    (Pattern a (Blank))
				    (Times
				     (Optional (Pattern b (Blank)))
				     (Cos
				      (Plus
				       (Optional (Pattern c (Blank)))
				       (Times
					(Optional (Pattern d (Blank)))
					(Pattern x (Blank)))))))
				   (Pattern n (Blank))))
		     (Pattern x (Blank Symbol)))
		(Condition (Plus (Times (Times
					 -1
					 (Plus
					  (Times (Power a 2) C)
					  (Times (Power b 2) A)))
					(Sin (Plus c (Times d x)))
					(Power
					 (Cos (Plus c (Times d x)))
					 (Plus m 1))
					(Power
					 (Plus
					  a
					  (Times
					   b
					   (Cos (Plus c (Times d x)))))
					 (Plus n 1))
					(Power
					 (Times
					  a
					  d
					  (Plus n 1)
					  (Plus
					   (Power a 2)
					   (Times -1 (Power b 2))))
					 -1))
				 (Dist
				  (Times 1
					 (Power
					  (Times
					   a
					   (Plus n 1)
					   (Plus
					    (Power a 2)
					    (Times -1 (Power b 2))))
					  -1))
				  (Int (Times
					(Power
					 (Cos (Plus c (Times d x)))
					 m)
					(Sim
					 (Plus
					  (Times
					   A
					   (Plus
					    (Times (Power a 2) (Plus n 1))
					    (Times
					     -1
					     (Times
					      (Power b 2)
					      (Plus m n 2)))))
					  (Times
					   -1
					   (Times
					    (Power a 2)
					    C
					    (Plus m 1)))
					  (Times
					   -1
					   (Times
					    a
					    (Plus n 1)
					    (Plus (Times b A) (Times b C))
					    (Cos (Plus c (Times d x)))))
					  (Times
					   (Plus m n 3)
					   (Plus
					    (Times (Power a 2) C)
					    (Times (Power b 2) A))
					   (Power
					    (Cos (Plus c (Times d x)))
					    2)))
					 x)
					(Power
					 (Plus
					  a
					  (Times
					   b
					   (Cos (Plus c (Times d x)))))
					 (Plus n 1)))
				       x)))
			   (And (FreeQ (List a b c d A C) x)
				(NonzeroQ
				 (Plus (Power a 2) (Times -1 (Power b 2))))
				(NonzeroQ
				 (Plus (Times (Power a 2) C)
				       (Times (Power b 2) A)))
				(RationalQ (List m n))
				(Comparison m Less 0)
				(Comparison n Less -1/2)
				(Comparison n Unequal -1)))) 
     
     
     
    (SetDelayed (Int (Times (Plus (Pattern A (Blank))
				  (Times (Optional (Pattern B (Blank)))
					 (Cos
					  (Plus
					   (Optional (Pattern c (Blank)))
					   (Times
					    (Optional (Pattern d (Blank)))
					    (Pattern x (Blank))))))
				  (Times (Optional (Pattern C (Blank)))
					 (Power
					  (Cos
					   (Plus
					    (Optional (Pattern c (Blank)))
					    (Times
					     (Optional (Pattern d (Blank)))
					     (Pattern x (Blank)))))
					  2)))
			    (Power (Times
				    (Sqrt
				     (Cos
				      (Plus
				       (Optional (Pattern c (Blank)))
				       (Times
					(Optional (Pattern d (Blank)))
					(Pattern x (Blank))))))
				    (Plus
				     (Pattern a (Blank))
				     (Times
				      (Optional (Pattern b (Blank)))
				      (Cos
				       (Plus
					(Optional (Pattern c (Blank)))
					(Times
					 (Optional (Pattern d (Blank)))
					 (Pattern x (Blank))))))))
				   -1))
		     (Pattern x (Blank Symbol)))
		(Condition (Plus (Dist (Times C (Power b -1))
				       (Int (Sqrt (Cos (Plus c (Times d x))))
					    x))
				 (Dist (Times 1 (Power b -1))
				       (Int (Times
					     (Plus
					      (Times A b)
					      (Times
					       (Plus
						(Times b B)
						(Times -1 (Times a C)))
					       (Cos (Plus c (Times d x)))))
					     (Power
					      (Times
					       (Sqrt (Cos (Plus c (Times d x))))
					       (Plus
						a
						(Times
						 b
						 (Cos (Plus c (Times d x))))))
					      -1))
					    x)))
			   (And (FreeQ (List a b c d A B C) x)
				(NonzeroQ
				 (Plus (Power a 2)
				       (Times -1 (Power b 2))))))) 
     
     
    (SetDelayed (Int (Times (Plus (Optional (Pattern A (Blank)))
				  (Times (Optional (Pattern B (Blank)))
					 (Cos
					  (Plus
					   (Optional (Pattern c (Blank)))
					   (Times
					    (Optional (Pattern d (Blank)))
					    (Pattern x (Blank))))))
				  (Times (Optional (Pattern C (Blank)))
					 (Power
					  (Cos
					   (Plus
					    (Optional (Pattern c (Blank)))
					    (Times
					     (Optional (Pattern d (Blank)))
					     (Pattern x (Blank)))))
					  2)))
			    (Power (Times
				    (Sqrt
				     (Cos
				      (Plus
				       (Optional (Pattern c (Blank)))
				       (Times
					(Optional (Pattern d (Blank)))
					(Pattern x (Blank))))))
				    (Sqrt
				     (Plus
				      (Pattern a (Blank))
				      (Times
				       (Optional (Pattern b (Blank)))
				       (Cos
					(Plus
					 (Optional (Pattern c (Blank)))
					 (Times
					  (Optional (Pattern d (Blank)))
					  (Pattern x (Blank)))))))))
				   -1))
		     (Pattern x (Blank Symbol)))
		(Condition (Plus (Times C (Sqrt (Cos (Plus c (Times d x))))
					(Sqrt
					 (Plus
					  a
					  (Times
					   b
					   (Cos (Plus c (Times d x))))))
					(Tan
					 (Times (Plus c (Times d x)) 1/2))
					(Power (Times b d) -1))
				 (Dist (Times C (Power b -1))
				       (Int (Times
					     (Sqrt
					      (Plus
					       a
					       (Times
						b
						(Cos (Plus c (Times d x))))))
					     (Power
					      (Times
					       (Sqrt (Cos (Plus c (Times d x))))
					       (Plus
						1
						(Cos (Plus c (Times d x)))))
					      -1))
					    x))
				 (Dist (Times 1 (Power (Times 2 b) -1))
				       (Int (Times
					     (Plus
					      (Times 2 A b)
					      (Times -1 (Times a C))
					      (Times
					       (Plus
						(Times 2 b B)
						(Times -1 (Times a C)))
					       (Cos (Plus c (Times d x)))))
					     (Power
					      (Times
					       (Sqrt (Cos (Plus c (Times d x))))
					       (Sqrt
						(Plus
						 a
						 (Times
						  b
						  (Cos (Plus c (Times d x)))))))
					      -1))
					    x)))
			   (And (FreeQ (List a b c d A B C) x)
				(NonzeroQ
				 (Plus (Power a 2)
				       (Times -1 (Power b 2))))))) 
     
     
     
    (SetDelayed (Int (Times (Power (Cos
				    (Plus
				     (Optional (Pattern c (Blank)))
				     (Times
				      (Optional (Pattern d (Blank)))
				      (Pattern x (Blank)))))
				   (Optional (Pattern m (Blank))))
			    (Plus (Times (Optional (Pattern B (Blank)))
					 (Cos
					  (Plus
					   (Optional (Pattern c (Blank)))
					   (Times
					    (Optional (Pattern d (Blank)))
					    (Pattern x (Blank))))))
				  (Times (Optional (Pattern C (Blank)))
					 (Power
					  (Cos
					   (Plus
					    (Optional (Pattern c (Blank)))
					    (Times
					     (Optional (Pattern d (Blank)))
					     (Pattern x (Blank)))))
					  2))))
		     (Pattern x (Blank Symbol)))
		(Condition (Int (Times (Power
					(Cos (Plus c (Times d x)))
					(Plus m 1))
				       (Plus
					B
					(Times
					 C
					 (Cos (Plus c (Times d x))))))
				x)
			   (And (FreeQ (List c d B C) x) (RationalQ m)
				(Comparison m LessEqual -1)))) 
     
     
    (SetDelayed (Int (Times (Power (Cos
				    (Plus
				     (Optional (Pattern c (Blank)))
				     (Times
				      (Optional (Pattern d (Blank)))
				      (Pattern x (Blank)))))
				   (Pattern m (Blank)))
			    (Plus (Pattern A (Blank))
				  (Times (Optional (Pattern B (Blank)))
					 (Cos
					  (Plus
					   (Optional (Pattern c (Blank)))
					   (Times
					    (Optional (Pattern d (Blank)))
					    (Pattern x (Blank))))))
				  (Times (Optional (Pattern C (Blank)))
					 (Power
					  (Cos
					   (Plus
					    (Optional (Pattern c (Blank)))
					    (Times
					     (Optional (Pattern d (Blank)))
					     (Pattern x (Blank)))))
					  2))))
		     (Pattern x (Blank Symbol)))
		(Condition (Plus (Times (Times -1 A)
					(Sin (Plus c (Times d x)))
					(Power
					 (Cos (Plus c (Times d x)))
					 (Plus m 1))
					(Power (Times d (Plus m 1)) -1))
				 (Dist (Times 1 (Power (Plus m 1) -1))
				       (Int (Times
					     (Power
					      (Cos (Plus c (Times d x)))
					      (Plus m 1))
					     (Sim
					      (Plus
					       (Times B (Plus m 1))
					       (Times
						(Plus
						 (Times A (Plus m 2))
						 (Times C (Plus m 1)))
						(Cos (Plus c (Times d x)))))
					      x))
					    x)))
			   (And (FreeQ (List c d A B C) x) (RationalQ m)
				(Comparison m Less -1)))) 
     
     
     
    (SetDelayed (Int (Times (Power (Cos
				    (Plus
				     (Optional (Pattern c (Blank)))
				     (Times
				      (Optional (Pattern d (Blank)))
				      (Pattern x (Blank)))))
				   (Optional (Pattern m (Blank))))
			    (Plus (Optional (Pattern A (Blank)))
				  (Times (Optional (Pattern B (Blank)))
					 (Cos
					  (Plus
					   (Optional (Pattern c (Blank)))
					   (Times
					    (Optional (Pattern d (Blank)))
					    (Pattern x (Blank))))))
				  (Times (Optional (Pattern C (Blank)))
					 (Power
					  (Cos
					   (Plus
					    (Optional (Pattern c (Blank)))
					    (Times
					     (Optional (Pattern d (Blank)))
					     (Pattern x (Blank)))))
					  2))))
		     (Pattern x (Blank Symbol)))
		(Condition (Plus (Times C (Sin (Plus c (Times d x)))
					(Power
					 (Cos (Plus c (Times d x)))
					 (Plus m 1))
					(Power (Times d (Plus m 2)) -1))
				 (Dist (Times 1 (Power (Plus m 2) -1))
				       (Int (Times
					     (Power
					      (Cos (Plus c (Times d x)))
					      m)
					     (Sim
					      (Plus
					       (Times A (Plus m 2))
					       (Times C (Plus m 1))
					       (Times
						B
						(Plus m 2)
						(Cos (Plus c (Times d x)))))
					      x))
					    x)))
			   (And (FreeQ (List c d A B C) x) (RationalQ m)
				(Comparison m GreaterEqual -1)))) 
     
     
     
    (SetDelayed (Int (Times (Plus (Optional (Pattern A (Blank)))
				  (Times (Optional (Pattern B (Blank)))
					 (Cos
					  (Plus
					   (Optional (Pattern c (Blank)))
					   (Times
					    (Optional (Pattern d (Blank)))
					    (Pattern x (Blank))))))
				  (Times (Optional (Pattern C (Blank)))
					 (Power
					  (Cos
					   (Plus
					    (Optional (Pattern c (Blank)))
					    (Times
					     (Optional (Pattern d (Blank)))
					     (Pattern x (Blank)))))
					  2)))
			    (Power (Plus
				    (Pattern a (Blank))
				    (Times
				     (Optional (Pattern b (Blank)))
				     (Cos
				      (Plus
				       (Optional (Pattern c (Blank)))
				       (Times
					(Optional (Pattern d (Blank)))
					(Pattern x (Blank)))))))
				   (Optional (Pattern n (Blank)))))
		     (Pattern x (Blank Symbol)))
		(Condition (Dist (Times 1 (Power (Power b 2) -1))
				 (Int (Times (Sim
					      (Plus
					       (Times b B)
					       (Times -1 (Times a C))
					       (Times
						b
						C
						(Cos (Plus c (Times d x)))))
					      x)
					     (Power
					      (Plus
					       a
					       (Times
						b
						(Cos (Plus c (Times d x)))))
					      (Plus n 1)))
				      x))
			   (And (FreeQ (List a b c d A B C) x)
				(ZeroQ (Plus
					(Times (Power a 2) C)
					(Times -1 (Times a b B))
					(Times (Power b 2) A)))
				(RationalQ n) (Comparison n Less -1)))) 
     
     
    (SetDelayed (Int (Times (Plus (Optional (Pattern A (Blank)))
				  (Times (Optional (Pattern B (Blank)))
					 (Cos
					  (Plus
					   (Optional (Pattern c (Blank)))
					   (Times
					    (Optional (Pattern d (Blank)))
					    (Pattern x (Blank))))))
				  (Times (Optional (Pattern C (Blank)))
					 (Power
					  (Cos
					   (Plus
					    (Optional (Pattern c (Blank)))
					    (Times
					     (Optional (Pattern d (Blank)))
					     (Pattern x (Blank)))))
					  2)))
			    (Power (Plus
				    (Pattern a (Blank))
				    (Times
				     (Optional (Pattern b (Blank)))
				     (Cos
				      (Plus
				       (Optional (Pattern c (Blank)))
				       (Times
					(Optional (Pattern d (Blank)))
					(Pattern x (Blank)))))))
				   (Optional (Pattern n (Blank)))))
		     (Pattern x (Blank Symbol)))
		(Condition (Plus (Times (Plus
					 (Times (Power a 2) C)
					 (Times -1 (Times a b B))
					 (Times (Power b 2) A))
					(Sin (Plus c (Times d x)))
					(Power
					 (Plus
					  a
					  (Times
					   b
					   (Cos (Plus c (Times d x)))))
					 (Plus n 1))
					(Power
					 (Times
					  b
					  d
					  (Plus n 1)
					  (Plus
					   (Power a 2)
					   (Times -1 (Power b 2))))
					 -1))
				 (Dist
				  (Times 1
					 (Power
					  (Times
					   b
					   (Plus n 1)
					   (Plus
					    (Power a 2)
					    (Times -1 (Power b 2))))
					  -1))
				  (Int (Times
					(Sim
					 (Plus
					  (Times
					   b
					   (Plus
					    (Times a (Plus A C))
					    (Times -1 (Times b B)))
					   (Plus n 1))
					  (Times
					   -1
					   (Times
					    (Plus
					     (Times
					      b
					      (Plus
					       (Times b A)
					       (Times -1 (Times a B)))
					      (Plus n 2))
					     (Times
					      C
					      (Plus
					       (Power a 2)
					       (Times
						(Power b 2)
						(Plus n 1)))))
					    (Cos (Plus c (Times d x))))))
					 x)
					(Power
					 (Plus
					  a
					  (Times
					   b
					   (Cos (Plus c (Times d x)))))
					 (Plus n 1)))
				       x)))
			   (And (FreeQ (List a b c d A B C) x)
				(NonzeroQ
				 (Plus (Power a 2) (Times -1 (Power b 2))))
				(NonzeroQ
				 (Plus (Times (Power a 2) C)
				       (Times -1 (Times a b B))
				       (Times (Power b 2) A)))
				(RationalQ n) (Comparison n Less -1)))) 
     
     
    (SetDelayed (Int (Times (Plus (Optional (Pattern A (Blank)))
				  (Times (Optional (Pattern B (Blank)))
					 (Cos
					  (Plus
					   (Optional (Pattern c (Blank)))
					   (Times
					    (Optional (Pattern d (Blank)))
					    (Pattern x (Blank))))))
				  (Times (Optional (Pattern C (Blank)))
					 (Power
					  (Cos
					   (Plus
					    (Optional (Pattern c (Blank)))
					    (Times
					     (Optional (Pattern d (Blank)))
					     (Pattern x (Blank)))))
					  2)))
			    (Power (Plus
				    (Pattern a (Blank))
				    (Times
				     (Optional (Pattern b (Blank)))
				     (Cos
				      (Plus
				       (Optional (Pattern c (Blank)))
				       (Times
					(Optional (Pattern d (Blank)))
					(Pattern x (Blank)))))))
				   (Optional (Pattern n (Blank)))))
		     (Pattern x (Blank Symbol)))
		(Condition (Plus (Times C (Sin (Plus c (Times d x)))
					(Power
					 (Plus
					  a
					  (Times
					   b
					   (Cos (Plus c (Times d x)))))
					 (Plus n 1))
					(Power (Times b d (Plus n 2)) -1))
				 (Dist
				  (Times 1 (Power (Times b (Plus n 2)) -1))
				  (Int (Times
					(Sim
					 (Plus
					  (Times
					   b
					   (Plus
					    (Times A (Plus n 2))
					    (Times C (Plus n 1))))
					  (Times
					   (Plus
					    (Times b B (Plus n 2))
					    (Times -1 (Times a C)))
					   (Cos (Plus c (Times d x)))))
					 x)
					(Power
					 (Plus
					  a
					  (Times
					   b
					   (Cos (Plus c (Times d x)))))
					 n))
				       x)))
			   (And (FreeQ (List a b c d A B C) x)
				(RationalQ n)
				(Comparison n GreaterEqual -1)))) 
     
     
    (SetDelayed (Int (Times (Power (Cos
				    (Plus
				     (Optional (Pattern c (Blank)))
				     (Times
				      (Optional (Pattern d (Blank)))
				      (Pattern x (Blank)))))
				   (Optional (Pattern m (Blank))))
			    (Plus (Optional (Pattern A (Blank)))
				  (Times (Optional (Pattern B (Blank)))
					 (Cos
					  (Plus
					   (Optional (Pattern c (Blank)))
					   (Times
					    (Optional (Pattern d (Blank)))
					    (Pattern x (Blank))))))
				  (Times (Optional (Pattern C (Blank)))
					 (Power
					  (Cos
					   (Plus
					    (Optional (Pattern c (Blank)))
					    (Times
					     (Optional (Pattern d (Blank)))
					     (Pattern x (Blank)))))
					  2)))
			    (Power (Plus
				    (Pattern a (Blank))
				    (Times
				     (Optional (Pattern b (Blank)))
				     (Cos
				      (Plus
				       (Optional (Pattern c (Blank)))
				       (Times
					(Optional (Pattern d (Blank)))
					(Pattern x (Blank)))))))
				   (Optional (Pattern n (Blank)))))
		     (Pattern x (Blank Symbol)))
		(Condition (Dist (Times 1 (Power (Power b 2) -1))
				 (Int (Times (Power
					      (Cos (Plus c (Times d x)))
					      m)
					     (Sim
					      (Plus
					       (Times b B)
					       (Times -1 (Times a C))
					       (Times
						b
						C
						(Cos (Plus c (Times d x)))))
					      x)
					     (Power
					      (Plus
					       a
					       (Times
						b
						(Cos (Plus c (Times d x)))))
					      (Plus n 1)))
				      x))
			   (And (FreeQ (List a b c d A B C m) x)
				(ZeroQ (Plus
					(Times (Power a 2) C)
					(Times -1 (Times a b B))
					(Times (Power b 2) A)))
				(RationalQ n) (Comparison n Less -1/2)))) 
     
     
    (SetDelayed (Int (Times (Power (Cos
				    (Plus
				     (Optional (Pattern c (Blank)))
				     (Times
				      (Optional (Pattern d (Blank)))
				      (Pattern x (Blank)))))
				   (Optional (Pattern m (Blank))))
			    (Plus (Optional (Pattern A (Blank)))
				  (Times (Optional (Pattern B (Blank)))
					 (Cos
					  (Plus
					   (Optional (Pattern c (Blank)))
					   (Times
					    (Optional (Pattern d (Blank)))
					    (Pattern x (Blank))))))
				  (Times (Optional (Pattern C (Blank)))
					 (Power
					  (Cos
					   (Plus
					    (Optional (Pattern c (Blank)))
					    (Times
					     (Optional (Pattern d (Blank)))
					     (Pattern x (Blank)))))
					  2)))
			    (Power (Plus
				    (Pattern a (Blank))
				    (Times
				     (Optional (Pattern b (Blank)))
				     (Cos
				      (Plus
				       (Optional (Pattern c (Blank)))
				       (Times
					(Optional (Pattern d (Blank)))
					(Pattern x (Blank)))))))
				   (Pattern n (Blank))))
		     (Pattern x (Blank Symbol)))
		(Condition (Plus (Times (Plus
					 (Times (Power a 2) C)
					 (Times -1 (Times a b B))
					 (Times (Power b 2) A))
					(Sin (Plus c (Times d x)))
					(Power
					 (Cos (Plus c (Times d x)))
					 m)
					(Power
					 (Plus
					  a
					  (Times
					   b
					   (Cos (Plus c (Times d x)))))
					 (Plus n 1))
					(Power
					 (Times
					  b
					  d
					  (Plus n 1)
					  (Plus
					   (Power a 2)
					   (Times -1 (Power b 2))))
					 -1))
				 (Dist
				  (Times 1
					 (Power
					  (Times
					   b
					   (Plus n 1)
					   (Plus
					    (Power a 2)
					    (Times -1 (Power b 2))))
					  -1))
				  (Int (Times
					(Power
					 (Cos (Plus c (Times d x)))
					 (Plus m -1))
					(Sim
					 (Plus
					  (Times
					   m
					   (Plus
					    (Times (Power a 2) C)
					    (Times -1 (Times a b B))
					    (Times (Power b 2) A)))
					  (Times
					   b
					   (Plus n 1)
					   (Plus
					    (Times a (Plus C A))
					    (Times -1 (Times b B)))
					   (Cos (Plus c (Times d x))))
					  (Times
					   -1
					   (Times
					    (Plus
					     (Times
					      (Power a 2)
					      C
					      (Plus m 1))
					     (Times
					      -1
					      (Times a b B (Plus m n 2)))
					     (Times
					      (Power b 2)
					      (Plus
					       (Times A (Plus m n 2))
					       (Times C (Plus n 1)))))
					    (Power
					     (Cos (Plus c (Times d x)))
					     2))))
					 x)
					(Power
					 (Plus
					  a
					  (Times
					   b
					   (Cos (Plus c (Times d x)))))
					 (Plus n 1)))
				       x)))
			   (And (FreeQ (List a b c d A B C) x)
				(NonzeroQ
				 (Plus (Power a 2) (Times -1 (Power b 2))))
				(NonzeroQ
				 (Plus (Times (Power a 2) C)
				       (Times -1 (Times a b B))
				       (Times (Power b 2) A)))
				(RationalQ (List m n))
				(Comparison m Greater 0)
				(Comparison n Less -1/2)
				(Comparison n Unequal -1)))) 
     
     
    (SetDelayed (Int (Times (Power (Cos
				    (Plus
				     (Optional (Pattern c (Blank)))
				     (Times
				      (Optional (Pattern d (Blank)))
				      (Pattern x (Blank)))))
				   (Optional (Pattern m (Blank))))
			    (Plus (Optional (Pattern A (Blank)))
				  (Times (Optional (Pattern B (Blank)))
					 (Cos
					  (Plus
					   (Optional (Pattern c (Blank)))
					   (Times
					    (Optional (Pattern d (Blank)))
					    (Pattern x (Blank))))))
				  (Times (Optional (Pattern C (Blank)))
					 (Power
					  (Cos
					   (Plus
					    (Optional (Pattern c (Blank)))
					    (Times
					     (Optional (Pattern d (Blank)))
					     (Pattern x (Blank)))))
					  2)))
			    (Power (Plus
				    (Pattern a (Blank))
				    (Times
				     (Optional (Pattern b (Blank)))
				     (Cos
				      (Plus
				       (Optional (Pattern c (Blank)))
				       (Times
					(Optional (Pattern d (Blank)))
					(Pattern x (Blank)))))))
				   (Pattern n (Blank))))
		     (Pattern x (Blank Symbol)))
		(Condition (Plus (Times C (Sin (Plus c (Times d x)))
					(Power
					 (Cos (Plus c (Times d x)))
					 m)
					(Power
					 (Plus
					  a
					  (Times
					   b
					   (Cos (Plus c (Times d x)))))
					 (Plus n 1))
					(Power
					 (Times b d (Plus m n 2))
					 -1))
				 (Dist
				  (Times 1
					 (Power (Times b (Plus m n 2)) -1))
				  (Int (Times
					(Power
					 (Cos (Plus c (Times d x)))
					 (Plus m -1))
					(Sim
					 (Plus
					  (Times a C m)
					  (Times
					   b
					   (Plus
					    (Times A (Plus m n 2))
					    (Times C (Plus m n 1)))
					   (Cos (Plus c (Times d x))))
					  (Times
					   (Plus
					    (Times b B (Plus m n 2))
					    (Times
					     -1
					     (Times a C (Plus m 1))))
					   (Power
					    (Cos (Plus c (Times d x)))
					    2)))
					 x)
					(Power
					 (Plus
					  a
					  (Times
					   b
					   (Cos (Plus c (Times d x)))))
					 n))
				       x)))
			   (And (FreeQ (List a b c d A B C) x)
				(NonzeroQ
				 (Plus (Power a 2) (Times -1 (Power b 2))))
				(RationalQ (List m n))
				(Comparison m Greater 0)
				(Or (Comparison -1/2 LessEqual n Less 1/2)
				    (Comparison n Equal -1))))) 
     
     
    (SetDelayed (Int (Times (Power (Cos
				    (Plus
				     (Optional (Pattern c (Blank)))
				     (Times
				      (Optional (Pattern d (Blank)))
				      (Pattern x (Blank)))))
				   (Optional (Pattern m (Blank))))
			    (Plus (Optional (Pattern A (Blank)))
				  (Times (Optional (Pattern B (Blank)))
					 (Cos
					  (Plus
					   (Optional (Pattern c (Blank)))
					   (Times
					    (Optional (Pattern d (Blank)))
					    (Pattern x (Blank))))))
				  (Times (Optional (Pattern C (Blank)))
					 (Power
					  (Cos
					   (Plus
					    (Optional (Pattern c (Blank)))
					    (Times
					     (Optional (Pattern d (Blank)))
					     (Pattern x (Blank)))))
					  2)))
			    (Power (Plus
				    (Pattern a (Blank))
				    (Times
				     (Optional (Pattern b (Blank)))
				     (Cos
				      (Plus
				       (Optional (Pattern c (Blank)))
				       (Times
					(Optional (Pattern d (Blank)))
					(Pattern x (Blank)))))))
				   (Optional (Pattern n (Blank)))))
		     (Pattern x (Blank Symbol)))
		(Condition (Plus (Times C (Sin (Plus c (Times d x)))
					(Power
					 (Cos (Plus c (Times d x)))
					 (Plus m 1))
					(Power
					 (Plus
					  a
					  (Times
					   b
					   (Cos (Plus c (Times d x)))))
					 n)
					(Power (Times d (Plus m n 2)) -1))
				 (Dist (Times 1 (Power (Plus m n 2) -1))
				       (Int (Times
					     (Power
					      (Cos (Plus c (Times d x)))
					      m)
					     (Sim
					      (Plus
					       (Times
						a
						(Plus
						 (Times A (Plus m n 2))
						 (Times C (Plus m 1))))
					       (Times
						(Plus
						 (Times
						  (Plus (Times b A) (Times a B))
						  (Plus m n 2))
						 (Times b C (Plus m n 1)))
						(Cos (Plus c (Times d x))))
					       (Times
						(Plus
						 (Times b B (Plus m n 2))
						 (Times a C n))
						(Power
						 (Cos (Plus c (Times d x)))
						 2)))
					      x)
					     (Power
					      (Plus
					       a
					       (Times
						b
						(Cos (Plus c (Times d x)))))
					      (Plus n -1)))
					    x)))
			   (And (FreeQ (List a b c d A B C) x)
				(NonzeroQ
				 (Plus (Power a 2) (Times -1 (Power b 2))))
				(RationalQ (List m n))
				(Comparison m GreaterEqual -1)
				(Comparison n GreaterEqual 1/2)))) 
     
     
     
    (SetDelayed (Int (Times (Power (Cos
				    (Plus
				     (Optional (Pattern c (Blank)))
				     (Times
				      (Optional (Pattern d (Blank)))
				      (Pattern x (Blank)))))
				   (Pattern m (Blank)))
			    (Plus (Pattern A (Blank))
				  (Times (Optional (Pattern B (Blank)))
					 (Cos
					  (Plus
					   (Optional (Pattern c (Blank)))
					   (Times
					    (Optional (Pattern d (Blank)))
					    (Pattern x (Blank))))))
				  (Times (Optional (Pattern C (Blank)))
					 (Power
					  (Cos
					   (Plus
					    (Optional (Pattern c (Blank)))
					    (Times
					     (Optional (Pattern d (Blank)))
					     (Pattern x (Blank)))))
					  2)))
			    (Power (Plus
				    (Pattern a (Blank))
				    (Times
				     (Optional (Pattern b (Blank)))
				     (Cos
				      (Plus
				       (Optional (Pattern c (Blank)))
				       (Times
					(Optional (Pattern d (Blank)))
					(Pattern x (Blank)))))))
				   (Optional (Pattern n (Blank)))))
		     (Pattern x (Blank Symbol)))
		(Condition (Plus (Times (Times -1 A)
					(Sin (Plus c (Times d x)))
					(Power
					 (Cos (Plus c (Times d x)))
					 (Plus m 1))
					(Power
					 (Plus
					  a
					  (Times
					   b
					   (Cos (Plus c (Times d x)))))
					 n)
					(Power (Times d (Plus m 1)) -1))
				 (Dist (Times 1 (Power (Plus m 1) -1))
				       (Int (Times
					     (Power
					      (Cos (Plus c (Times d x)))
					      (Plus m 1))
					     (Sim
					      (Plus
					       (Times a B (Plus m 1))
					       (Times -1 (Times b A n))
					       (Times
						(Plus
						 (Times
						  (Plus (Times a C) (Times b B))
						  (Plus m 1))
						 (Times a A (Plus m 2)))
						(Cos (Plus c (Times d x))))
					       (Times
						b
						(Plus
						 (Times C (Plus m 1))
						 (Times A (Plus m n 2)))
						(Power
						 (Cos (Plus c (Times d x)))
						 2)))
					      x)
					     (Power
					      (Plus
					       a
					       (Times
						b
						(Cos (Plus c (Times d x)))))
					      (Plus n -1)))
					    x)))
			   (And (FreeQ (List a b c d A B C) x)
				(NonzeroQ
				 (Plus (Power a 2) (Times -1 (Power b 2))))
				(RationalQ (List m n))
				(Comparison m Less -1)
				(Comparison n GreaterEqual 1/2)))) 
     
     
     
    (SetDelayed (Int (Times (Power (Cos
				    (Plus
				     (Optional (Pattern c (Blank)))
				     (Times
				      (Optional (Pattern d (Blank)))
				      (Pattern x (Blank)))))
				   (Pattern m (Blank)))
			    (Plus (Pattern A (Blank))
				  (Times (Optional (Pattern B (Blank)))
					 (Cos
					  (Plus
					   (Optional (Pattern c (Blank)))
					   (Times
					    (Optional (Pattern d (Blank)))
					    (Pattern x (Blank))))))
				  (Times (Optional (Pattern C (Blank)))
					 (Power
					  (Cos
					   (Plus
					    (Optional (Pattern c (Blank)))
					    (Times
					     (Optional (Pattern d (Blank)))
					     (Pattern x (Blank)))))
					  2)))
			    (Power (Plus
				    (Pattern a (Blank))
				    (Times
				     (Optional (Pattern b (Blank)))
				     (Cos
				      (Plus
				       (Optional (Pattern c (Blank)))
				       (Times
					(Optional (Pattern d (Blank)))
					(Pattern x (Blank)))))))
				   (Pattern n (Blank))))
		     (Pattern x (Blank Symbol)))
		(Condition (Plus (Times (Times -1 A)
					(Sin (Plus c (Times d x)))
					(Power
					 (Cos (Plus c (Times d x)))
					 (Plus m 1))
					(Power
					 (Plus
					  a
					  (Times
					   b
					   (Cos (Plus c (Times d x)))))
					 (Plus n 1))
					(Power (Times a d (Plus m 1)) -1))
				 (Dist
				  (Times 1 (Power (Times a (Plus m 1)) -1))
				  (Int (Times
					(Power
					 (Cos (Plus c (Times d x)))
					 (Plus m 1))
					(Sim
					 (Plus
					  (Times a B (Plus m 1))
					  (Times
					   -1
					   (Times b A (Plus m n 2)))
					  (Times
					   a
					   (Plus
					    (Times A (Plus m 2))
					    (Times C (Plus m 1)))
					   (Cos (Plus c (Times d x))))
					  (Times
					   b
					   A
					   (Plus m n 3)
					   (Power
					    (Cos (Plus c (Times d x)))
					    2)))
					 x)
					(Power
					 (Plus
					  a
					  (Times
					   b
					   (Cos (Plus c (Times d x)))))
					 n))
				       x)))
			   (And (FreeQ (List a b c d A B C) x)
				(NonzeroQ
				 (Plus (Power a 2) (Times -1 (Power b 2))))
				(RationalQ (List m n))
				(Comparison m Less -1)
				(Or (Comparison -1/2 LessEqual n Less 1/2)
				    (Comparison n Equal -1))))) 
     
     
     
    (SetDelayed (Int (Times (Power (Cos
				    (Plus
				     (Optional (Pattern c (Blank)))
				     (Times
				      (Optional (Pattern d (Blank)))
				      (Pattern x (Blank)))))
				   (Pattern m (Blank)))
			    (Plus (Optional (Pattern A (Blank)))
				  (Times (Optional (Pattern B (Blank)))
					 (Cos
					  (Plus
					   (Optional (Pattern c (Blank)))
					   (Times
					    (Optional (Pattern d (Blank)))
					    (Pattern x (Blank))))))
				  (Times (Optional (Pattern C (Blank)))
					 (Power
					  (Cos
					   (Plus
					    (Optional (Pattern c (Blank)))
					    (Times
					     (Optional (Pattern d (Blank)))
					     (Pattern x (Blank)))))
					  2)))
			    (Power (Plus
				    (Pattern a (Blank))
				    (Times
				     (Optional (Pattern b (Blank)))
				     (Cos
				      (Plus
				       (Optional (Pattern c (Blank)))
				       (Times
					(Optional (Pattern d (Blank)))
					(Pattern x (Blank)))))))
				   (Pattern n (Blank))))
		     (Pattern x (Blank Symbol)))
		(Condition (Plus (Times (Times
					 -1
					 (Plus
					  (Times (Power a 2) C)
					  (Times -1 (Times a b B))
					  (Times (Power b 2) A)))
					(Sin (Plus c (Times d x)))
					(Power
					 (Cos (Plus c (Times d x)))
					 (Plus m 1))
					(Power
					 (Plus
					  a
					  (Times
					   b
					   (Cos (Plus c (Times d x)))))
					 (Plus n 1))
					(Power
					 (Times
					  a
					  d
					  (Plus n 1)
					  (Plus
					   (Power a 2)
					   (Times -1 (Power b 2))))
					 -1))
				 (Dist
				  (Times 1
					 (Power
					  (Times
					   a
					   (Plus n 1)
					   (Plus
					    (Power a 2)
					    (Times -1 (Power b 2))))
					  -1))
				  (Int (Times
					(Power
					 (Cos (Plus c (Times d x)))
					 m)
					(Sim
					 (Plus
					  (Times
					   A
					   (Plus
					    (Times (Power a 2) (Plus n 1))
					    (Times
					     -1
					     (Times
					      (Power b 2)
					      (Plus m n 2)))))
					  (Times
					   -1
					   (Times
					    a
					    (Plus
					     (Times a C)
					     (Times -1 (Times b B)))
					    (Plus m 1)))
					  (Times
					   -1
					   (Times
					    a
					    (Plus n 1)
					    (Plus
					     (Times b A)
					     (Times -1 (Times a B))
					     (Times b C))
					    (Cos (Plus c (Times d x)))))
					  (Times
					   (Plus m n 3)
					   (Plus
					    (Times (Power a 2) C)
					    (Times -1 (Times a b B))
					    (Times (Power b 2) A))
					   (Power
					    (Cos (Plus c (Times d x)))
					    2)))
					 x)
					(Power
					 (Plus
					  a
					  (Times
					   b
					   (Cos (Plus c (Times d x)))))
					 (Plus n 1)))
				       x)))
			   (And (FreeQ (List a b c d A B C) x)
				(NonzeroQ
				 (Plus (Power a 2) (Times -1 (Power b 2))))
				(NonzeroQ
				 (Plus (Times (Power a 2) C)
				       (Times -1 (Times a b B))
				       (Times (Power b 2) A)))
				(RationalQ (List m n))
				(Comparison m Less 0)
				(Comparison n Less -1/2)
				(Comparison n Unequal -1)))) 
					;parsing from text file .m time
					; cpu time (non-gc) 919 msec user, 15 msec system
					; cpu time (gc)     112 msec user, 0 msec system
					; cpu time (total)  1,031 msec user, 15 msec system
					; real time  1,641 msec
					; space allocation:
					;  101,592 cons cells, 66,115,960 other bytes, 0 static bytes
					;done
					;MockMMA(22)
    ))
