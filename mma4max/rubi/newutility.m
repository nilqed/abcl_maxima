
Int[a_,x_Symbol] :=   a*x /;FreeQ[a,x];

Int[a_*u_,x_Symbol]:=a*Int[u,x]/; FreeQ[a,x];

Int[u_Plus,x_Symbol] :=  Map[Int[#,x]&,u];

Int[x_^n_.,x_Symbol] :=  x^(n+1)/(n+1) /;RationalQ[n] && n != -1;

SumQ[_]:=False;

SumQ[_Plus] := True;

ProductQ[_]:=False;

ProductQ[_Times] := True;

RationalQ[u_] :=   If[ListQ[u], u==={} || (RationalQ[First[u]] && RationalQ[Rest[u]]), IntegerQ[u] || Head[u]===Rational];

(* RealNumericQ[u] returns True if u is a real numeric quantity, else returns False. *)
RealNumericQ[u_] := NumericQ[u] && PossibleZeroQ[Im[N[u]]]

(* PositiveQ[u] returns True if u is a positive numeric quantity, else returns False. *)
PositiveQ[u_] :=  Module[{v=Together[u]},  RealNumericQ[v] && Re[N[v]]>0]

(* NegativeQ[u] returns True if u is a negative numeric quantity, else returns False. *)
NegativeQ[u_] :=  Module[{v=Together[u]},  RealNumericQ[v] && Re[N[v]]<0]

(* ZeroQ[u] returns True if u is any 0; else returns False *)
ZeroQ[u_] := PossibleZeroQ[u]

(* NonzeroQ[u] returns True if u is not any 0, else it returns False. *)
NonzeroQ[u_] := Not[PossibleZeroQ[u]]

Dist[u_,v_]:= u*v  (*default, not a sum *)
Dist[u_,v_Plus] :=   Map[Function[Dist[u,#]],v]

Rt[u_,n_Integer] := u^(1/n)

PosQ[u_]:=True

PosQ[u_?RationalQ]:= u>0

PosQn[u_]:=Re[u]>0

PosQ[u_Times]:=(PosQ[First[u]] && PosQ[Rest[u]]) || NegQ[Rest[u]]

PosQ[u_Plus]:=PosQ[First[u]]

PosQ[u_?NumericQ]:=PosQn[N[u]]

PosQ[u_?NumberQ]:=PosQn[u]

NegQ[u_] :=  If[PossibleZeroQ[u],    False,  Not[PosQ[u]]]

Sim[u_] := Together[u];

Sim[u_,x_]:=Sim[u]

(* SplitFreeFactors[u,x] returns the list {v,w} where v is the product of the factors of u free of x	and w is the product of the other factors. *)

SplitFreeFactors[u_,x_Symbol] :=  If[ProductQ[u],    Map[Function[If[FreeQ[#,x],{#,1},{1,#}]],u],  If[FreeQ[u,x],    {u,1},  {1,u}]]

Together[u_]:=UnRat[RatExpand[u]];  
