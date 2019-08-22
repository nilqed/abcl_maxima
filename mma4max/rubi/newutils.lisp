(in-package :mma)
 
(defparameter utils
  '((SetDelayed (Int (Pattern a (Blank)) (Pattern x (Blank Symbol)))
		(Condition (Times a x) (FreeQ a x))) 
 
    (SetDelayed (Int (Pattern u (Blank)) (Pattern x (Blank Symbol)))
		(Condition (Map (Function (Int (Slot 1) x)) u) (Comparison (Head u) SameQ Plus))) 
 
    (SetDelayed (Int (Times (Pattern a (Blank)) (Pattern u (Blank))) (Pattern x (Blank Symbol)))
		(Condition (Dist a (Int u x))
			   (And (FreeQ a x)
				(Not
				 (MatchQ u
					 (Condition (Times (Pattern b (Blank))
							   (Optional (Pattern v (Blank))))
						    (FreeQ b x))))))) 
 
    (SetDelayed (Int (Power (Pattern x (Blank)) (Optional (Pattern n (Blank))))
		     (Pattern x (Blank Symbol)))
		(Condition (Times (Power x (Plus n 1)) (Power (Plus n 1) -1))
			   (And (RationalQ n) (Comparison n Unequal -1)))) 
 
    (SetDelayed (SumQ (Pattern u (Blank))) (Comparison (Head u) SameQ Plus)) 
 
    (SetDelayed (ProductQ (Pattern u (Blank))) (Comparison (Head u) SameQ Times)) 
 
    (SetDelayed (RationalQ (Pattern u (Blank)))
		(If (ListQ u)
		    (Or (Comparison u SameQ (List))
			(And (RationalQ (First u)) (RationalQ (Rest u))))
		    (Or (IntegerQ u) (Comparison (Head u) SameQ Rational)))) 
 
    (SetDelayed (RealNumericQ (Pattern u (Blank))) (And (NumericQ u) (PossibleZeroQ (Im (N u))))) 
 
    (SetDelayed (PositiveQ (Pattern u (Blank)))
		(Module (List (Set v (Together u)))
			(And (RealNumericQ v) (Comparison (Re (N v)) Greater 0)))) 
 
    (SetDelayed (NegativeQ (Pattern u (Blank)))
		(Module (List (Set v (Together u)))
			(And (RealNumericQ v) (Comparison (Re (N v)) Less 0)))) 
 
    (SetDelayed (ZeroQ (Pattern u (Blank))) (PossibleZeroQ u)) 
 
    (SetDelayed (NonzeroQ (Pattern u (Blank))) (Not (PossibleZeroQ u))) 
 
    (SetDelayed (Dist (Pattern u (Blank)) (Pattern v (Blank)))
		(If (SumQ v) (Map (Function (Dist u (Slot 1))) v) (Times u v))) 
 
    (SetDelayed (Rt (Pattern u (Blank)) (Pattern n (Blank Integer)))
		(Power u (Times 1 (Power n -1)))) 
 
    (SetDelayed (PosQ (Pattern u (Blank)))
		(If (RationalQ u) (Comparison u Greater 0)
		    (If (NumberQ u)
			(If (PossibleZeroQ (Re u)) (Comparison (Im u) Greater 0)
			    (Comparison (Re u) Greater 0))
			(If (NumericQ u)
			    (Module (List (Set v (N u)))
				    (If (PossibleZeroQ (Re v)) (Comparison (Im v) Greater 0)
					(Comparison (Re v) Greater 0)))
			    (If (ProductQ u) (If (PosQ (First u)) (PosQ (Rest u)) (NegQ (Rest u)))
				(If (SumQ u)
				    (Module (List (Set v (Together u)))
					    (If (SumQ v) (PosQ (First v)) (PosQ v)))
				    True)))))) 
 
    (SetDelayed (NegQ (Pattern u (Blank))) (If (PossibleZeroQ u) False (Not (PosQ u)))) 
 
    (CompoundExpression (SetDelayed (Sim (Pattern u (Blank))) (Together u)) Null) 
 
    (SetDelayed (Sim (Pattern u (Blank)) (Pattern x (Blank)))
		(If (SumQ u)
		    (Module (List (Set tmp1 0) (Set tmp2 0) lst)
			    (CompoundExpression (Scan (Function
						       (CompoundExpression (Set
									    lst
									    (SplitFreeFactors (Slot 1) x))
									   (If
									    (Comparison
									     (Part lst 2)
									     SameQ
									     1)
									    (AddTo tmp1 (Slot 1))
									    (Set
									     tmp2
									     (Plus
									      tmp2
									      (Times
									       (Together (Part lst 1))
									       (Part lst 2)))))))
						      u)
						(Plus (Together tmp1) tmp2)))
		    (Module (List (Set lst (SplitFreeFactors u x)))
			    (Times (Together (Part lst 1)) (Part lst 2))))) 
    ))