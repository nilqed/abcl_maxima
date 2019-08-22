Clear[gg,c,d,x,z]

 gg[c_.+d_.*x_,x_]:=big[c,d,x] /; c>d

 gg[c_.+d_.*x_,x_]:=littlec[c,d,x] /; c<d

 gg[3+4*z,z]  ok
 gg[4+3*z,z]  ok
 gg[2+2*z,z]  loops.  It should stop with gg[2+2*z,z], as does Mathematica.

If I add the rule

 gg[c_.+d_.*x_,x_]:=equalc[c,d,x] /; c==d

then it wins.


