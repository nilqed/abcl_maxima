(* ::Package:: *)

(* ::Title:: *)
(*Integration Problems for Products of Cosines and Cosines*)


(* ::Section::Closed:: *)
(*Cos[c+d x]^m (a+b Cos[c+d x])^n*)


(* ::Subsection::Closed:: *)
(*Integrands of the form Cos[c+d x]^m*)
test[{grand_,var_,count_,ans_}]:=print[Simp[Int[grand,var]-ans]==0]

  p1={{a x,x,1,1/2*a*x^2},  (*simple trial *)
      {Cos[a + b*x], x, 1, Sin[a + b*x]/b},
      {Cos[a + b*x]^2, x, 1, x/2 + (Cos[a + b*x]*Sin[a + b*x])/(2*b)},
      {Cos[a + b*x]^3, x, 2, Sin[a + b*x]/b - Sin[a + b*x]^3/(3*b)},
      {Cos[a + b*x]^4, x, 2, (3*x)/8 + (3*Cos[a + b*x]*Sin[a + b*x])/(8*b) + (Cos[a + b*x]^3*Sin[a + b*x])/(4*b)},
      {Cos[a + b*x]^5, x, 2, Sin[a + b*x]/b - (2*Sin[a + b*x]^3)/(3*b) + Sin[a + b*x]^5/(5*b)}};

(* ::Subsection::Closed:: *)
(*Integrands of the form Cos[c+d x]^(m/2)*)
(**)

  p2={
    {Cos[a + b*x]^(5/2), x, 2, (6*EllipticE[(1/2)*(a + b*x), 2])/(5*b) + (2*Cos[a + b*x]^(3/2)*Sin[a + b*x])/(5*b)},
    {Cos[a + b*x]^(3/2), x, 2, (2*EllipticF[(1/2)*(a + b*x), 2])/(3*b) + (2*Sqrt[Cos[a + b*x]]*Sin[a + b*x])/(3*b)},
    {Cos[a + b*x]^(1/2), x, 1, (2*EllipticE[(1/2)*(a + b*x), 2])/b},
    {1/Cos[a + b*x]^(1/2), x, 1, (2*EllipticF[(1/2)*(a + b*x), 2])/b},
    {1/Cos[a + b*x]^(3/2), x, 2, -((2*EllipticE[(1/2)*(a + b*x), 2])/b) + (2*Sin[a + b*x])/(b*Sqrt[Cos[a + b*x]])},
{1/Cos[a + b*x]^(5/2), x, 2, (2*EllipticF[(1/2)*(a + b*x), 2])/(3*b) + (2*Sin[a + b*x])/(3*b*Cos[a + b*x]^(3/2))}
  }


(* ::Subsection::Closed:: *)
(*Integrands of the form (a+b Cos[c+d x])^n*)

p3={
{(a + b*Cos[c + d*x])^5, x, 4, (1/8)*a*(8*a^4 + 40*a^2*b^2 + 15*b^4)*x + (b*(107*a^4 + 192*a^2*b^2 + 16*b^4)*Sin[c + d*x])/(30*d) + (7*a*b^2*(22*a^2 + 23*b^2)*Cos[c + d*x]*Sin[c + d*x])/(120*d) + (b*(47*a^2 + 16*b^2)*(a + b*Cos[c + d*x])^2*Sin[c + d*x])/(60*d) + (9*a*b*(a + b*Cos[c + d*x])^3*Sin[c + d*x])/(20*d) + (b*(a + b*Cos[c + d*x])^4*Sin[c + d*x])/(5*d)},
{(a + b*Cos[c + d*x])^4, x, 3, (1/8)*(8*a^4 + 24*a^2*b^2 + 3*b^4)*x + (a*b*(19*a^2 + 16*b^2)*Sin[c + d*x])/(6*d) + (b^2*(26*a^2 + 9*b^2)*Cos[c + d*x]*Sin[c + d*x])/(24*d) + (7*a*b*(a + b*Cos[c + d*x])^2*Sin[c + d*x])/(12*d) + (b*(a + b*Cos[c + d*x])^3*Sin[c + d*x])/(4*d)},
{(a + b*Cos[c + d*x])^3, x, 2, (1/2)*a*(2*a^2 + 3*b^2)*x + (2*b*(4*a^2 + b^2)*Sin[c + d*x])/(3*d) + (5*a*b^2*Cos[c + d*x]*Sin[c + d*x])/(6*d) + (b*(a + b*Cos[c + d*x])^2*Sin[c + d*x])/(3*d)},
{(a + b*Cos[c + d*x])^2, x, 1, (1/2)*(2*a^2 + b^2)*x + (2*a*b*Sin[c + d*x])/d + (b^2*Cos[c + d*x]*Sin[c + d*x])/(2*d)},
{(a + b*Cos[c + d*x]), x, 2, a*x + (b*Sin[c + d*x])/d},
{1/(a + b*Cos[c + d*x]), x, 1, (2*ArcTan[((a - b)*Tan[(1/2)*(c + d*x)])/Sqrt[a^2 - b^2]])/(Sqrt[a^2 - b^2]*d)},
{1/(a + b*Cos[c + d*x])^2, x, 3, (2*a*ArcTan[((a - b)*Tan[(1/2)*(c + d*x)])/Sqrt[a^2 - b^2]])/((a^2 - b^2)^(3/2)*d) - (b*Sin[c + d*x])/((a^2 - b^2)*d*(a + b*Cos[c + d*x]))},
{1/(a + b*Cos[c + d*x])^3, x, 4, ((2*a^2 + b^2)*ArcTan[((a - b)*Tan[(1/2)*(c + d*x)])/Sqrt[a^2 - b^2]])/((a^2 - b^2)^(5/2)*d) - (b*Sin[c + d*x])/(2*(a^2 - b^2)*d*(a + b*Cos[c + d*x])^2) - (3*a*b*Sin[c + d*x])/(2*(a^2 - b^2)^2*d*(a + b*Cos[c + d*x]))},
{1/(a + b*Cos[c + d*x])^4, x, 5, (a*(2*a^2 + 3*b^2)*ArcTan[((a - b)*Tan[(1/2)*(c + d*x)])/Sqrt[a^2 - b^2]])/((a^2 - b^2)^(7/2)*d) - (b*Sin[c + d*x])/(3*(a^2 - b^2)*d*(a + b*Cos[c + d*x])^3) - (5*a*b*Sin[c + d*x])/(6*(a^2 - b^2)^2*d*(a + b*Cos[c + d*x])^2) - (b*(11*a^2 + 4*b^2)*Sin[c + d*x])/(6*(a^2 - b^2)^3*d*(a + b*Cos[c + d*x]))},
{1/(3 + 5*Cos[c + d*x]), x, 1, ArcTanh[(1/2)*Tan[(1/2)*(c + d*x)]]/(2*d)},
{1/(3 + 5*Cos[c + d*x])^2, x, 3, -((3*ArcTanh[(1/2)*Tan[(1/2)*(c + d*x)]])/(32*d)) + (5*Sin[c + d*x])/(16*d*(3 + 5*Cos[c + d*x]))},
{1/(3 + 5*Cos[c + d*x])^3, x, 4, (43*ArcTanh[(1/2)*Tan[(1/2)*(c + d*x)]])/(1024*d) + (5*Sin[c + d*x])/(32*d*(3 + 5*Cos[c + d*x])^2) - (45*Sin[c + d*x])/(512*d*(3 + 5*Cos[c + d*x]))},
{1/(3 + 5*Cos[c + d*x])^4, x, 5, -((279*ArcTanh[(1/2)*Tan[(1/2)*(c + d*x)]])/(16384*d)) + (5*Sin[c + d*x])/(48*d*(3 + 5*Cos[c + d*x])^3) - (25*Sin[c + d*x])/(512*d*(3 + 5*Cos[c + d*x])^2) + (995*Sin[c + d*x])/(24576*d*(3 + 5*Cos[c + d*x]))},
{1/(5 + 3*Cos[c + d*x]), x, 1, x/4 - ArcTan[Sin[c + d*x]/(3 + Cos[c + d*x])]/(2*d)},
{1/(5 + 3*Cos[c + d*x])^2, x, 3, (5*x)/64 - (5*ArcTan[Sin[c + d*x]/(3 + Cos[c + d*x])])/(32*d) - (3*Sin[c + d*x])/(16*d*(5 + 3*Cos[c + d*x]))},
{1/(5 + 3*Cos[c + d*x])^3, x, 4, (59*x)/2048 - (59*ArcTan[Sin[c + d*x]/(3 + Cos[c + d*x])])/(1024*d) - (3*Sin[c + d*x])/(32*d*(5 + 3*Cos[c + d*x])^2) - (45*Sin[c + d*x])/(512*d*(5 + 3*Cos[c + d*x]))},
{1/(5 + 3*Cos[c + d*x])^4, x, 5, (385*x)/32768 - (385*ArcTan[Sin[c + d*x]/(3 + Cos[c + d*x])])/(16384*d) - Sin[c + d*x]/(16*d*(5 + 3*Cos[c + d*x])^3) - (25*Sin[c + d*x])/(512*d*(5 + 3*Cos[c + d*x])^2) - (311*Sin[c + d*x])/(8192*d*(5 + 3*Cos[c + d*x]))}}

(* ::Subsection::Closed:: *)
(*Integrands of the form (a+b Cos[c+d x])^(n/2)*)

p4={
  {(a + b*Cos[c + d*x])^(7/2), x, 8, (32*a*(11*a^2 + 13*b^2)*Sqrt[a + b*Cos[c + d*x]]*EllipticE[(1/2)*(c + d*x), (2*b)/(a + b)])/(105*d*Sqrt[(a + b*Cos[c + d*x])/(a + b)]) - (2*(71*a^4 - 46*a^2*b^2 - 25*b^4)*Sqrt[(a + b*Cos[c + d*x])/(a + b)]*EllipticF[(1/2)*(c + d*x), (2*b)/(a + b)])/(105*d*Sqrt[a + b*Cos[c + d*x]]) + (2*b*(71*a^2 + 25*b^2)*Sqrt[a + b*Cos[c + d*x]]*Sin[c + d*x])/(105*d) + (24*a*b*(a + b*Cos[c + d*x])^(3/2)*Sin[c + d*x])/(35*d) + (2*b*(a + b*Cos[c + d*x])^(5/2)*Sin[c + d*x])/(7*d)},
  {(a + b*Cos[c + d*x])^(5/2), x, 7, (2*(23*a^2 + 9*b^2)*Sqrt[a + b*Cos[c + d*x]]*EllipticE[(1/2)*(c + d*x), (2*b)/(a + b)])/(15*d*Sqrt[(a + b*Cos[c + d*x])/(a + b)]) - (16*a*(a^2 - b^2)*Sqrt[(a + b*Cos[c + d*x])/(a + b)]*EllipticF[(1/2)*(c + d*x), (2*b)/(a + b)])/(15*d*Sqrt[a + b*Cos[c + d*x]]) + (16*a*b*Sqrt[a + b*Cos[c + d*x]]*Sin[c + d*x])/(15*d) + (2*b*(a + b*Cos[c + d*x])^(3/2)*Sin[c + d*x])/(5*d)},
  {(a + b*Cos[c + d*x])^(3/2), x, 6, (8*a*Sqrt[a + b*Cos[c + d*x]]*EllipticE[(1/2)*(c + d*x), (2*b)/(a + b)])/(3*d*Sqrt[(a + b*Cos[c + d*x])/(a + b)]) - (2*(a^2 - b^2)*Sqrt[(a + b*Cos[c + d*x])/(a + b)]*EllipticF[(1/2)*(c + d*x), (2*b)/(a + b)])/(3*d*Sqrt[a + b*Cos[c + d*x]]) + (2*b*Sqrt[a + b*Cos[c + d*x]]*Sin[c + d*x])/(3*d)},
  {(a + b*Cos[c + d*x])^(1/2), x, 2, (2*Sqrt[a + b*Cos[c + d*x]]*EllipticE[(1/2)*(c + d*x), (2*b)/(a + b)])/(d*Sqrt[(a + b*Cos[c + d*x])/(a + b)])},
  {1/(a + b*Cos[c + d*x])^(1/2), x, 2, (2*Sqrt[(a + b*Cos[c + d*x])/(a + b)]*EllipticF[(1/2)*(c + d*x), (2*b)/(a + b)])/(d*Sqrt[a + b*Cos[c + d*x]])},
  {1/(a + b*Cos[c + d*x])^(3/2), x, 4, (2*Sqrt[a + b*Cos[c + d*x]]*EllipticE[(1/2)*(c + d*x), (2*b)/(a + b)])/((a^2 - b^2)*d*Sqrt[(a + b*Cos[c + d*x])/(a + b)]) - (2*b*Sin[c + d*x])/((a^2 - b^2)*d*Sqrt[a + b*Cos[c + d*x]])},
  {1/(a + b*Cos[c + d*x])^(5/2), x, 7, (8*a*Sqrt[a + b*Cos[c + d*x]]*EllipticE[(1/2)*(c + d*x), (2*b)/(a + b)])/(3*(a^2 - b^2)^2*d*Sqrt[(a + b*Cos[c + d*x])/(a + b)]) - (2*Sqrt[(a + b*Cos[c + d*x])/(a + b)]*EllipticF[(1/2)*(c + d*x), (2*b)/(a + b)])/(3*(a^2 - b^2)*d*Sqrt[a + b*Cos[c + d*x]]) - (2*b*Sin[c + d*x])/(3*(a^2 - b^2)*d*(a + b*Cos[c + d*x])^(3/2)) - (8*a*b*Sin[c + d*x])/(3*(a^2 - b^2)^2*d*Sqrt[a + b*Cos[c + d*x]])},
  {1/(a + b*Cos[c + d*x])^(7/2), x, 8, (2*(23*a^2 + 9*b^2)*Sqrt[a + b*Cos[c + d*x]]*EllipticE[(1/2)*(c + d*x), (2*b)/(a + b)])/(15*(a^2 - b^2)^3*d*Sqrt[(a + b*Cos[c + d*x])/(a + b)]) - (16*a*Sqrt[(a + b*Cos[c + d*x])/(a + b)]*EllipticF[(1/2)*(c + d*x), (2*b)/(a + b)])/(15*(a^2 - b^2)^2*d*Sqrt[a + b*Cos[c + d*x]]) - (2*b*Sin[c + d*x])/(5*(a^2 - b^2)*d*(a + b*Cos[c + d*x])^(5/2)) - (16*a*b*Sin[c + d*x])/(15*(a^2 - b^2)^2*d*(a + b*Cos[c + d*x])^(3/2)) - (2*b*(23*a^2 + 9*b^2)*Sin[c + d*x])/(15*(a^2 - b^2)^3*d*Sqrt[a + b*Cos[c + d*x]])}}

(* ::Subsection::Closed:: *)
(*Integrands of the form (a+b Cos[c+d x])^(n/3)*)
(**)


p5={
  {(a + b*Cos[c + d*x])^(5/3), x, 1, (3/5)*Int[((1/3)*(5*a^2 + 2*b^2) + (1/3)*(7*a*b)*Cos[c + d*x])/(a + b*Cos[c + d*x])^(1/3), x] + (3*b*(a + b*Cos[c + d*x])^(2/3)*Sin[c + d*x])/(5*d)},
  {(a + b*Cos[c + d*x])^(4/3), x, 0, Int[(a + b*Cos[c + d*x])^(4/3), x]},
  {(a + b*Cos[c + d*x])^(2/3), x, 0, Int[(a + b*Cos[c + d*x])^(2/3), x]},
  {(a + b*Cos[c + d*x])^(1/3), x, 0, Int[(a + b*Cos[c + d*x])^(1/3), x]},
  {1/(a + b*Cos[c + d*x])^(1/3), x, 0, Int[1/(a + b*Cos[c + d*x])^(1/3), x]},
  {1/(a + b*Cos[c + d*x])^(2/3), x, 0, Int[1/(a + b*Cos[c + d*x])^(2/3), x]},
  {1/(a + b*Cos[c + d*x])^(4/3), x, 1, -((3*Int[(-(a/3) - (1/3)*(2*b)*Cos[c + d*x])/(a + b*Cos[c + d*x])^(1/3), x])/(a^2 - b^2)) - (3*b*Sin[c + d*x])/((a^2 - b^2)*d*(a + b*Cos[c + d*x])^(1/3))},
  {1/(a + b*Cos[c + d*x])^(5/3), x, 2, -((9*Int[((1/9)*(-2*a^2 + b^2) + (1/9)*(4*a*b)*Cos[c + d*x])*(a + b*Cos[c + d*x])^(1/3), x])/(2*(a^2 - b^2)^2)) - (3*b*Sin[c + d*x])/(2*(a^2 - b^2)*d*(a + b*Cos[c + d*x])^(2/3)) + (3*a*b*(a + b*Cos[c + d*x])^(1/3)*Sin[c + d*x])/(2*(a^2 - b^2)^2*d)}}


(* ::Subsection::Closed:: *)
(*Integrands of the form Cos[c+d x]^m (a+b Cos[c+d x])^n*)


(* ::Subsubsection::Closed:: *)
(*n>0*)

p6={
  {Cos[c + d*x]*(a + b*Cos[c + d*x]), x, 1, (b*x)/2 + (a*Sin[c + d*x])/d + (b*Cos[c + d*x]*Sin[c + d*x])/(2*d)},
  {Cos[c + d*x]^2*(a + b*Cos[c + d*x]), x, 2, (a*x)/2 + (2*b*Sin[c + d*x])/(3*d) + (a*Cos[c + d*x]*Sin[c + d*x])/(2*d) + (b*Cos[c + d*x]^2*Sin[c + d*x])/(3*d)},
  {Cos[c + d*x]^3*(a + b*Cos[c + d*x]), x, 3, (3*b*x)/8 + (2*a*Sin[c + d*x])/(3*d) + (3*b*Cos[c + d*x]*Sin[c + d*x])/(8*d) + (a*Cos[c + d*x]^2*Sin[c + d*x])/(3*d) + (b*Cos[c + d*x]^3*Sin[c + d*x])/(4*d)},
  {Cos[c + d*x]^4*(a + b*Cos[c + d*x]), x, 4, (3*a*x)/8 + (8*b*Sin[c + d*x])/(15*d) + (3*a*Cos[c + d*x]*Sin[c + d*x])/(8*d) + (4*b*Cos[c + d*x]^2*Sin[c + d*x])/(15*d) + (a*Cos[c + d*x]^3*Sin[c + d*x])/(4*d) + (b*Cos[c + d*x]^4*Sin[c + d*x])/(5*d)},
  {Cos[c + d*x]^5*(a + b*Cos[c + d*x]), x, 5, (5*b*x)/16 + (8*a*Sin[c + d*x])/(15*d) + (5*b*Cos[c + d*x]*Sin[c + d*x])/(16*d) + (4*a*Cos[c + d*x]^2*Sin[c + d*x])/(15*d) + (5*b*Cos[c + d*x]^3*Sin[c + d*x])/(24*d) + (a*Cos[c + d*x]^4*Sin[c + d*x])/(5*d) + (b*Cos[c + d*x]^5*Sin[c + d*x])/(6*d)},
  {Cos[c + d*x]^6*(a + b*Cos[c + d*x]), x, 6, (5*a*x)/16 + (16*b*Sin[c + d*x])/(35*d) + (5*a*Cos[c + d*x]*Sin[c + d*x])/(16*d) + (8*b*Cos[c + d*x]^2*Sin[c + d*x])/(35*d) + (5*a*Cos[c + d*x]^3*Sin[c + d*x])/(24*d) + (6*b*Cos[c + d*x]^4*Sin[c + d*x])/(35*d) + (a*Cos[c + d*x]^5*Sin[c + d*x])/(6*d) + (b*Cos[c + d*x]^6*Sin[c + d*x])/(7*d)},
  {Cos[c + d*x]*(a + b*Cos[c + d*x])^2, x, 2, a*b*x + ((3*a^2 + 2*b^2)*Sin[c + d*x])/(3*d) + (a*b*Cos[c + d*x]*Sin[c + d*x])/d + (b^2*Cos[c + d*x]^2*Sin[c + d*x])/(3*d)},
  {Cos[c + d*x]^2*(a + b*Cos[c + d*x])^2, x, 3, (1/8)*(4*a^2 + 3*b^2)*x + (4*a*b*Sin[c + d*x])/(3*d) + ((4*a^2 + 3*b^2)*Cos[c + d*x]*Sin[c + d*x])/(8*d) + (2*a*b*Cos[c + d*x]^2*Sin[c + d*x])/(3*d) + (b^2*Cos[c + d*x]^3*Sin[c + d*x])/(4*d)},  {Cos[c + d*x]^3*(a + b*Cos[c + d*x])^2, x, 4, (3*a*b*x)/4 + (2*(5*a^2 + 4*b^2)*Sin[c + d*x])/(15*d) + (3*a*b*Cos[c + d*x]*Sin[c + d*x])/(4*d) + ((5*a^2 + 4*b^2)*Cos[c + d*x]^2*Sin[c + d*x])/(15*d) + (a*b*Cos[c + d*x]^3*Sin[c + d*x])/(2*d) + (b^2*Cos[c + d*x]^4*Sin[c + d*x])/(5*d)},
  {Cos[c + d*x]^4*(a + b*Cos[c + d*x])^2, x, 5, (1/16)*(6*a^2 + 5*b^2)*x + (16*a*b*Sin[c + d*x])/(15*d) + ((6*a^2 + 5*b^2)*Cos[c + d*x]*Sin[c + d*x])/(16*d) + (8*a*b*Cos[c + d*x]^2*Sin[c + d*x])/(15*d) + ((6*a^2 + 5*b^2)*Cos[c + d*x]^3*Sin[c + d*x])/(24*d) + (2*a*b*Cos[c + d*x]^4*Sin[c + d*x])/(5*d) + (b^2*Cos[c + d*x]^5*Sin[c + d*x])/(6*d)},
  {Cos[c + d*x]^5*(a + b*Cos[c + d*x])^2, x, 6, (5*a*b*x)/8 + (8*(7*a^2 + 6*b^2)*Sin[c + d*x])/(105*d) + (5*a*b*Cos[c + d*x]*Sin[c + d*x])/(8*d) + (4*(7*a^2 + 6*b^2)*Cos[c + d*x]^2*Sin[c + d*x])/(105*d) + (5*a*b*Cos[c + d*x]^3*Sin[c + d*x])/(12*d) + ((7*a^2 + 6*b^2)*Cos[c + d*x]^4*Sin[c + d*x])/(35*d) + (a*b*Cos[c + d*x]^5*Sin[c + d*x])/(3*d) + (b^2*Cos[c + d*x]^6*Sin[c + d*x])/(7*d)},
  {Cos[c + d*x]^6*(a + b*Cos[c + d*x])^2, x, 7, (5/128)*(8*a^2 + 7*b^2)*x + (32*a*b*Sin[c + d*x])/(35*d) + (5*(8*a^2 + 7*b^2)*Cos[c + d*x]*Sin[c + d*x])/(128*d) + (16*a*b*Cos[c + d*x]^2*Sin[c + d*x])/(35*d) + (5*(8*a^2 + 7*b^2)*Cos[c + d*x]^3*Sin[c + d*x])/(192*d) + (12*a*b*Cos[c + d*x]^4*Sin[c + d*x])/(35*d) + ((8*a^2 + 7*b^2)*Cos[c + d*x]^5*Sin[c + d*x])/(48*d) + (2*a*b*Cos[c + d*x]^6*Sin[c + d*x])/(7*d) + (b^2*Cos[c + d*x]^7*Sin[c + d*x])/(8*d)},
  {Cos[c + d*x]*(a + b*Cos[c + d*x])^3, x, 3, (3/8)*b*(4*a^2 + b^2)*x + (a*(a^2 + 4*b^2)*Sin[c + d*x])/(2*d) + (b*(2*a^2 + 3*b^2)*Cos[c + d*x]*Sin[c + d*x])/(8*d) + (a*(a + b*Cos[c + d*x])^2*Sin[c + d*x])/(4*d) + ((a + b*Cos[c + d*x])^3*Sin[c + d*x])/(4*d)},
  {Cos[c + d*x]^2*(a + b*Cos[c + d*x])^3, x, 4, (1/8)*a*(4*a^2 + 9*b^2)*x - ((3*a^4 - 52*a^2*b^2 - 16*b^4)*Sin[c + d*x])/(30*b*d) - (a*(6*a^2 - 71*b^2)*Cos[c + d*x]*Sin[c + d*x])/(120*d) - ((3*a^2 - 16*b^2)*(a + b*Cos[c + d*x])^2*Sin[c + d*x])/(60*b*d) - (a*(a + b*Cos[c + d*x])^3*Sin[c + d*x])/(20*b*d) + ((a + b*Cos[c + d*x])^4*Sin[c + d*x])/(5*b*d)},
  {Cos[c + d*x]^3*(a + b*Cos[c + d*x])^3, x, 5, (1/16)*b*(18*a^2 + 5*b^2)*x + (2*a*(5*a^2 + 12*b^2)*Sin[c + d*x])/(15*d) + (b*(18*a^2 + 5*b^2)*Cos[c + d*x]*Sin[c + d*x])/(16*d) + (a*(5*a^2 + 12*b^2)*Cos[c + d*x]^2*Sin[c + d*x])/(15*d) + (b*(18*a^2 + 5*b^2)*Cos[c + d*x]^3*Sin[c + d*x])/(24*d) + (13*a*b^2*Cos[c + d*x]^4*Sin[c + d*x])/(30*d) + (b^2*Cos[c + d*x]^4*(a + b*Cos[c + d*x])*Sin[c + d*x])/(6*d)},
  {Cos[c + d*x]^4*(a + b*Cos[c + d*x])^3, x, 6, (3/16)*a*(2*a^2 + 5*b^2)*x + (8*b*(7*a^2 + 2*b^2)*Sin[c + d*x])/(35*d) + (3*a*(2*a^2 + 5*b^2)*Cos[c + d*x]*Sin[c + d*x])/(16*d) + (4*b*(7*a^2 + 2*b^2)*Cos[c + d*x]^2*Sin[c + d*x])/(35*d) + (a*(2*a^2 + 5*b^2)*Cos[c + d*x]^3*Sin[c + d*x])/(8*d) + (3*b*(7*a^2 + 2*b^2)*Cos[c + d*x]^4*Sin[c + d*x])/(35*d) + (5*a*b^2*Cos[c + d*x]^5*Sin[c + d*x])/(14*d) + (b^2*Cos[c + d*x]^5*(a + b*Cos[c + d*x])*Sin[c + d*x])/(7*d)},
  {Cos[c + d*x]^5*(a + b*Cos[c + d*x])^3, x, 7, (5/128)*b*(24*a^2 + 7*b^2)*x + (8*a*(7*a^2 + 18*b^2)*Sin[c + d*x])/(105*d) + (5*b*(24*a^2 + 7*b^2)*Cos[c + d*x]*Sin[c + d*x])/(128*d) + (4*a*(7*a^2 + 18*b^2)*Cos[c + d*x]^2*Sin[c + d*x])/(105*d) + (5*b*(24*a^2 + 7*b^2)*Cos[c + d*x]^3*Sin[c + d*x])/(192*d) + (a*(7*a^2 + 18*b^2)*Cos[c + d*x]^4*Sin[c + d*x])/(35*d) + (b*(24*a^2 + 7*b^2)*Cos[c + d*x]^5*Sin[c + d*x])/(48*d) + (17*a*b^2*Cos[c + d*x]^6*Sin[c + d*x])/(56*d) + (b^2*Cos[c + d*x]^6*(a + b*Cos[c + d*x])*Sin[c + d*x])/(8*d)},
  {Cos[c + d*x]^6*(a + b*Cos[c + d*x])^3, x, 8, (5/128)*a*(8*a^2 + 21*b^2)*x + (16*b*(27*a^2 + 8*b^2)*Sin[c + d*x])/(315*d) + (5*a*(8*a^2 + 21*b^2)*Cos[c + d*x]*Sin[c + d*x])/(128*d) + (8*b*(27*a^2 + 8*b^2)*Cos[c + d*x]^2*Sin[c + d*x])/(315*d) + (5*a*(8*a^2 + 21*b^2)*Cos[c + d*x]^3*Sin[c + d*x])/(192*d) + (2*b*(27*a^2 + 8*b^2)*Cos[c + d*x]^4*Sin[c + d*x])/(105*d) + (a*(8*a^2 + 21*b^2)*Cos[c + d*x]^5*Sin[c + d*x])/(48*d) + (b*(27*a^2 + 8*b^2)*Cos[c + d*x]^6*Sin[c + d*x])/(63*d) + (19*a*b^2*Cos[c + d*x]^7*Sin[c + d*x])/(72*d) + (b^2*Cos[c + d*x]^7*(a + b*Cos[c + d*x])*Sin[c + d*x])/(9*d)},
  {Cos[c + d*x]*(a + b*Cos[c + d*x])^4, x, 4, (1/2)*a*b*(4*a^2 + 3*b^2)*x + (2*(3*a^4 + 28*a^2*b^2 + 4*b^4)*Sin[c + d*x])/(15*d) + (a*b*(6*a^2 + 29*b^2)*Cos[c + d*x]*Sin[c + d*x])/(30*d) + ((3*a^2 + 4*b^2)*(a + b*Cos[c + d*x])^2*Sin[c + d*x])/(15*d) + (a*(a + b*Cos[c + d*x])^3*Sin[c + d*x])/(5*d) + ((a + b*Cos[c + d*x])^4*Sin[c + d*x])/(5*d)},
  {Cos[c + d*x]^2*(a + b*Cos[c + d*x])^4, x, 5, (1/16)*(8*a^4 + 36*a^2*b^2 + 5*b^4)*x - (a*(4*a^4 - 121*a^2*b^2 - 128*b^4)*Sin[c + d*x])/(60*b*d) - ((8*a^4 - 178*a^2*b^2 - 75*b^4)*Cos[c + d*x]*Sin[c + d*x])/(240*d) - (a*(4*a^2 - 53*b^2)*(a + b*Cos[c + d*x])^2*Sin[c + d*x])/(120*b*d) - ((4*a^2 - 25*b^2)*(a + b*Cos[c + d*x])^3*Sin[c + d*x])/(120*b*d) - (a*(a + b*Cos[c + d*x])^4*Sin[c + d*x])/(30*b*d) + ((a + b*Cos[c + d*x])^5*Sin[c + d*x])/(6*b*d)},
  {Cos[c + d*x]^3*(a + b*Cos[c + d*x])^4, x, 6, (1/4)*a*b*(6*a^2 + 5*b^2)*x + (2*(35*a^4 + 168*a^2*b^2 + 24*b^4)*Sin[c + d*x])/(105*d) + (a*b*(6*a^2 + 5*b^2)*Cos[c + d*x]*Sin[c + d*x])/(4*d) + ((35*a^4 + 168*a^2*b^2 + 24*b^4)*Cos[c + d*x]^2*Sin[c + d*x])/(105*d) + (a*b*(6*a^2 + 5*b^2)*Cos[c + d*x]^3*Sin[c + d*x])/(6*d) + (b^2*(71*a^2 + 18*b^2)*Cos[c + d*x]^4*Sin[c + d*x])/(105*d) + (8*a*b^2*Cos[c + d*x]^4*(a + b*Cos[c + d*x])*Sin[c + d*x])/(21*d) + (b^2*Cos[c + d*x]^4*(a + b*Cos[c + d*x])^2*Sin[c + d*x])/(7*d)},
  {Cos[c + d*x]^4*(a + b*Cos[c + d*x])^4, x, 7, (1/128)*(48*a^4 + 240*a^2*b^2 + 35*b^4)*x + (32*a*b*(7*a^2 + 6*b^2)*Sin[c + d*x])/(105*d) + ((48*a^4 + 240*a^2*b^2 + 35*b^4)*Cos[c + d*x]*Sin[c + d*x])/(128*d) + (16*a*b*(7*a^2 + 6*b^2)*Cos[c + d*x]^2*Sin[c + d*x])/(105*d) + ((48*a^4 + 240*a^2*b^2 + 35*b^4)*Cos[c + d*x]^3*Sin[c + d*x])/(192*d) + (4*a*b*(7*a^2 + 6*b^2)*Cos[c + d*x]^4*Sin[c + d*x])/(35*d) + (b^2*(186*a^2 + 49*b^2)*Cos[c + d*x]^5*Sin[c + d*x])/(336*d) + (9*a*b^2*Cos[c + d*x]^5*(a + b*Cos[c + d*x])*Sin[c + d*x])/(28*d) + (b^2*Cos[c + d*x]^5*(a + b*Cos[c + d*x])^2*Sin[c + d*x])/(8*d)},
  {Cos[c + d*x]^5*(a + b*Cos[c + d*x])^4, x, 8, (5/32)*a*b*(8*a^2 + 7*b^2)*x + (8*(21*a^4 + 108*a^2*b^2 + 16*b^4)*Sin[c + d*x])/(315*d) + (5*a*b*(8*a^2 + 7*b^2)*Cos[c + d*x]*Sin[c + d*x])/(32*d) + (4*(21*a^4 + 108*a^2*b^2 + 16*b^4)*Cos[c + d*x]^2*Sin[c + d*x])/(315*d) + (5*a*b*(8*a^2 + 7*b^2)*Cos[c + d*x]^3*Sin[c + d*x])/(48*d) + ((21*a^4 + 108*a^2*b^2 + 16*b^4)*Cos[c + d*x]^4*Sin[c + d*x])/(105*d) + (a*b*(8*a^2 + 7*b^2)*Cos[c + d*x]^5*Sin[c + d*x])/(12*d) + (b^2*(59*a^2 + 16*b^2)*Cos[c + d*x]^6*Sin[c + d*x])/(126*d) + (5*a*b^2*Cos[c + d*x]^6*(a + b*Cos[c + d*x])*Sin[c + d*x])/(18*d) + (b^2*Cos[c + d*x]^6*(a + b*Cos[c + d*x])^2*Sin[c + d*x])/(9*d)},
  {Cos[c + d*x]^6*(a + b*Cos[c + d*x])^4, x, 9, (1/256)*(80*a^4 + 420*a^2*b^2 + 63*b^4)*x + (64*a*b*(9*a^2 + 8*b^2)*Sin[c + d*x])/(315*d) + ((80*a^4 + 420*a^2*b^2 + 63*b^4)*Cos[c + d*x]*Sin[c + d*x])/(256*d) + (32*a*b*(9*a^2 + 8*b^2)*Cos[c + d*x]^2*Sin[c + d*x])/(315*d) + ((80*a^4 + 420*a^2*b^2 + 63*b^4)*Cos[c + d*x]^3*Sin[c + d*x])/(384*d) + (8*a*b*(9*a^2 + 8*b^2)*Cos[c + d*x]^4*Sin[c + d*x])/(105*d) + ((80*a^4 + 420*a^2*b^2 + 63*b^4)*Cos[c + d*x]^5*Sin[c + d*x])/(480*d) + (4*a*b*(9*a^2 + 8*b^2)*Cos[c + d*x]^6*Sin[c + d*x])/(63*d) + (b^2*(292*a^2 + 81*b^2)*Cos[c + d*x]^7*Sin[c + d*x])/(720*d) + (11*a*b^2*Cos[c + d*x]^7*(a + b*Cos[c + d*x])*Sin[c + d*x])/(45*d) + (b^2*Cos[c + d*x]^7*(a + b*Cos[c + d*x])^2*Sin[c + d*x])/(10*d)}}



