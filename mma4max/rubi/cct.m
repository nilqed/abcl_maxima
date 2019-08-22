test[{grand_,var_,count_,ans_}]:=print[Simp[Int[grand,var]-ans]==0]
test1[{grand_,var_,count_,ans_}]:=Simp[Int[grand,var]]


  p1={{a x,x,1,1/2*a*x^2},  (*simple trial *)
      {Cos[a + b*x], x, 1, Sin[a + b*x]/b},
      {Cos[a + b*x]^2, x, 1, x/2 + (Cos[a + b*x]*Sin[a + b*x])/(2*b)},
      {Cos[a + b*x]^3, x, 2, Sin[a + b*x]/b - Sin[a + b*x]^3/(3*b)},
      {Cos[a + b*x]^4, x, 2, (3*x)/8 + (3*Cos[a + b*x]*Sin[a + b*x])/(8*b) + (Cos[a + b*x]^3*Sin[a + b*x])/(4*b)},
      {Cos[a + b*x]^5, x, 2, Sin[a + b*x]/b - (2*Sin[a + b*x]^3)/(3*b) + Sin[a + b*x]^5/(5*b)}};

  p2={
    {Cos[a + b*x]^(5/2), x, 2, (6*EllipticE[(1/2)*(a + b*x), 2])/(5*b) + (2*Cos[a + b*x]^(3/2)*Sin[a + b*x])/(5*b)},
    {Cos[a + b*x]^(3/2), x, 2, (2*EllipticF[(1/2)*(a + b*x), 2])/(3*b) + (2*Sqrt[Cos[a + b*x]]*Sin[a + b*x])/(3*b)},
    {Cos[a + b*x]^(1/2), x, 1, (2*EllipticE[(1/2)*(a + b*x), 2])/b},
    {1/Cos[a + b*x]^(1/2), x, 1, (2*EllipticF[(1/2)*(a + b*x), 2])/b},
    {1/Cos[a + b*x]^(3/2), x, 2, -((2*EllipticE[(1/2)*(a + b*x), 2])/b) + (2*Sin[a + b*x])/(b*Sqrt[Cos[a + b*x]])},
{1/Cos[a + b*x]^(5/2), x, 2, (2*EllipticF[(1/2)*(a + b*x), 2])/(3*b) + (2*Sin[a + b*x])/(3*b*Cos[a + b*x]^(3/2))}
  }
