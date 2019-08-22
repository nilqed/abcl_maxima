#

rm emardpatch.diff
diff -c diffrat.lisp.orig     diffrat.lisp > emardpatch.diff
diff -c disp1.lisp.orig       disp1.lisp >> emardpatch.diff
diff -c eval.lisp.orig        eval.lisp >> emardpatch.diff  
diff -c eval2.lisp.orig       eval2.lisp >> emardpatch.diff
diff -c hcons.lisp.orig       hcons.lisp >> emardpatch.diff
diff -c init.lisp.orig        init.lisp >> emardpatch.diff
diff -c init2.lisp.orig       init2.lisp >> emardpatch.diff
diff -c match.lisp.orig       match.lisp >> emardpatch.diff
diff -c match16.lisp.orig     match16.lisp >> emardpatch.diff
diff -c mma.lisp.orig         mma.lisp >> emardpatch.diff 
diff -c parser.lisp.orig      parser.lisp >> emardpatch.diff
diff -c pf.lisp.orig          pf.lisp >> emardpatch.diff
diff -c poly.lisp.orig        poly.lisp >> emardpatch.diff
diff -c primeq.lisp.orig      primeq.lisp >> emardpatch.diff
diff -c rat1.lisp.orig        rat1.lisp >> emardpatch.diff
diff -c simp1.lisp.orig       simp1.lisp >> emardpatch.diff
diff -c stack1.lisp.orig      stack1.lisp >> emardpatch.diff
diff -c ucons1.lisp.orig      ucons1.lisp >> emardpatch.diff
diff -c uconsalt.lisp.orig    uconsalt.lisp >> emardpatch.diff
