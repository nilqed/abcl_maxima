

;;  do this   file_search_lisp:cons("C:/lisp/mma-1.7/###.{o,lisp,lisp}",file_search_lisp);
(load "c:/lisp/mma-1.7/mma.lisp")
(in-package :mma)
(load "c:/lisp/mma-1.7/maxucons.lisp") 
(load  "c:/lisp/mma-1.7/maxcapsonlyparser.lisp")
#|(load "stack1")
(load "disp1")
(load "eval")
(load "poly")
(load "rat1")
(load "simp1")
(load "pf")
(load "match")
(load "diffrat")
|#

(defun maxima::$mma(S) (mma::pst (make-string-input-stream 
				  (concatenate 'string S 
				    "

"))))

(eval-when (load eval)
  (setf *package* (find-package :mma)))


