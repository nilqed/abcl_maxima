
;;;#+Allegro (excl::set-case-mode :case-sensitive-lower)
#+sparc (tpl::do-command "cd" "mma.sparc.bin")
#+hpux (tpl::do-command "cd" "mma.hp.bin")
;;
(load "mma.lisp")
(in-package :mma)
;;;(load "hcons")
(load "uconsalt.lisp")
;;(load "ucons1.lisp") ;; original was good for allegro 3.1 not 4.2; now fixed
;(load  "parser")
(load  "capsonlyparser.lisp")
(load "stack1.lisp")
(load "disp1.lisp")
(load "eval.lisp")
(load "poly.lisp")
(load "rat1.lisp")
(load "simp1.lisp")
(load "pf.lisp")
(load "newmatch.lisp")
(load "diffrat.lisp")
(load "mma2maxfun.lisp")


#| ; to make all this in order..
(mapc #'compile-file
      '("mma.lisp" "capsonlyparser.lisp" "stack1.lisp" "disp1.lisp" 
					"eval.lisp" 
	"poly.lisp" "rat1.lisp" "simp1.lisp" "pf.lisp" "match.lisp" "diffrat.lisp"
	

"uconsalt.lisp"))
|#

(eval-when (load compile)
  
;;  (shadow '(and or not plusp minusp) :mma) ;make these unbound in :mma
  
  (setf *package* (find-package :mma)))


