;; an example function with everything wired in...

(defun sinplot()
 (with-open-file (s "/tmp/sinplot" :direction :output :if-exists :supersede)
		 (format s "TitleText: Sin(x)~%")
		 (format s "0.Color: white~%")
			 (do ((i 0.0 (+ i 0.1)))
			     ((> i 6.28)
			      (run-shell-command "xgraph /tmp/sinplot" :wait nil)
			      'XGraphOutput)
			     (format s "~12,4G ~12,4G~%" i (sin i)))))



 Here's some stuff to do:
(a) figure out how to numerical_real_meval as much as possible
(b) figure out how to make xgraph options settable from the command line
(c)adaptive plotting using divided differences
(d) interval stuff: how to mark by changing color or style
(e) multiple plots
...

(n) compile mma to lisp (and compile)

e.g. in a plot command like
plot(Exp[-x]Sin[x^2], ...)  change the first argument, namely

(Times (Exp (Times -1 x))(Sin (Power x 2)))

to lisp, in the process
checking that the only free variable remaining is the plot variable..


(defun tempfun(x) (declare(float x))
  (setq x (coerce x 'float))
  (the float 
   (* (exp (- x))(sin (expt x 2)))))

and then compile that into machine language.

Problems:

expressions that involve other functions:

e.g.  plot(f[x]*f[1-x], ..

need to have a (* numericalmeval `(f ,x) `(f (Plus ,x (Times -1 1))

or some such.

