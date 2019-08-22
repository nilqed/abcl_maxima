;;; convert from mma char string to maxima internal form.
;;; Uses mma2max, probably in need of additional diddling around
;;; to convert more "stuff" from mma language to maxima,
;;; and of course some mma stuff, esp. patterns, does not
;;; make much sense in maxima, anyway.  Still to do, add a few
;;; hundred special functions etc. if you want them.

(defun maxima::$from_mma (x)
    (mma::mma2max(mma::pstring x)))

;;; THIS IS THE IMPORTANT INTERFACE PROGRAM, I THINK

;;; Read the char string to mma internal form, then
;;; evaluate it in mma, and then convert the result to maxima.

(defun maxima::$eval_string_mma (x)
  (mma::mma2max(mma::meval(mma::pstring x))))

;; One can write simple maxima functions for simple
;; interfaces. Here int_by_mma  takes two
;; ordinary maxima arguments f,x and integrates: Int(f,x) in MMA!!
;; It then returns the answer in maxima form.
;; It is a special case of fun_by_mma, for function "Int".

;; Let us say you want to execute the MockMMA function FooBar on arguments
;; x+y, w+z.   In Maxima, do this:   fun_by_mma("FooBar",x+y,w+z);

;; Thus:  int_by_mma(f,x):=fun_by_mma("Int",f,x);

(defun maxima::$fun_by_mma (fun &rest args)
  (mma::mma2max  (mma::meval 
		  (cons (mma::pstring fun) 
			(mapcar #'mma::max2mma args)))))

;;   eval_string_mma("foo[x_]:=x+42");
;;   eval_string_mma("foo[4]");    
;;  or ...
;;   fun_by_mma ("foo", sin(q));
;;  eval_string_mma("Clear[a,b,c,x, z,quad]");
;;  eval_string_mma("quad[a_.*z_^2+b_.*z_+c_. ,z_] := ans[a,b,c,z]");
;;  eval_string_mma("quad[z^2+4 z, z]");
;;     returns ans(1,4,0,z)



