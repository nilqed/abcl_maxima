n;;; -*- Mode:Common-Lisp;  Base:10 -*-
;; Written by: Richard J. Fateman
;; File: integrate.lisp
;; (c) 199 Richard J. Fateman

"Design:

Figure that all the cleverness of the algorithmic programs has
been included in the preprocessing before this stage.  IN particular,
the problems left for this program have not succumbed to
the simple derivative divides program
other parts of Moses' SIN scheme
the Risch algorithm and extensions as available has failed.

There are really 2 parts of this problem, quite separable.

The indefinite "antiderivative" problem, and the definite
integration problem.

We can also try to segment this into integration of a function
of strictly a single variable (no parameters), and other
functions.  

A function of a single variable is a lot easier, especially
since we don't really have a very good handle of functions of
several complex variables.

Since we rely on pattern matching, we must make sure that the time to
find a match is only a weak function of the number of patterns in the
data base. 

If the data base doubles, the match time should not go from t to 2t, but
perhaps from t to t+1.

Indefinite integrals: 

Heuristic substitutions for change of variable in expression E1

Find the n deepest nested subexpression.  call them u1, u2, ...un.
with u1 being the deepest non-atomic expression containing the
variable of integration x.
(should we exclude linear ax+b ?)
It could be as simple as u1=x^2.  in general u=f(x), invertible to
x=g(u) / with side conditions..

Let E2 be the result of replacing in E1, (and limits), f(x)--> u, and
everywhere there occurs some h(x), replace with h(g(u)); replace dx by
d(g(u)).

See if E2 is simpler -- has lower maximum depth of nested than
E1 -- and if so, (or even if not) try to integrate it instead.


Problems.  

1.The form of the derivative d(g(u)) can be critical.  See the
problems with (say) Bessel functions, where the result has many
variations.  Even with trig functions, the simplicity of E2 may depend
critically on subtle simplifications like Sin(ArcCos...)..

2. Maximum Depth isn't a great measure.  arcsinh(x) may be worse than
1/(x^2+1).  Some level of "transcendence" may be needed.

3. Non terminating heuristic, generally.

4. Solve for inverse may provide multiple values and/or restrictions
"

;; this is a odd function... we are trying to get a plausible
;; collection of subexpressions to use for a change of variables
;; from *var* to some newvar.




(defun depthrank(tree *var* &aux *ans*)
 (declare (special *var* *ans*))

 "return a list of sub-expressions {s[i]} of tree, each depending on *var*,
  indexed and sorted by how deep s[i] is in the tree."

 (dr tree 0)  ;; top level (tree itself) is 0
 (sort *ans* #'(lambda(r s)(> (car r)(car s))))
;;debug
;; (mapc #'(lambda(r) (format t "~s~%" r)) *ans*)
 *ans* )

(defun dr(tree n)
 (declare (special *var* *ans*p))
  (cond ((atom tree) (return-from dr nil))
	((depends-on *var* tree)
	 (push(list n tree) *ans*)))
 (mapcar #'(lambda(z) (dr z (1+ n))) tree))

(defun depends-on(v tr)
 "true if v occurs in the tree tr anywhere"
 (cond ((eq v tr)t)
       ((atom tr) nil)
       (t (or (depends-on v (car tr))(depends-on v (cdr tr))))))

(setq m '(+ (* A B E) E (* C (^ D E))))
(setq n '(+ y z (* 2 x)(+ (sin x) 1) (sin(+ (cos x) (* h theta)))))

		       
  
(defun changevar(e new fold)
"in the integral e replace occurrences of oldvar by new=fold"
;; example changevar( integrate(sin(x),x,0,a) , u, sin(x))
;; yields integrate(u/sqrt(1-u^2),u,0,sin(a))
;; macsyma does this with changevar('integrate(sin(x),x),sin(x)=u,u,x);

