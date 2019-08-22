;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: mma -*-
(defun show4tilu (x)
  (disp
   (BuildFormat
    (sublis 
     '(( plus . Plus)
       (times . Times)
       (expt . Power)
       )
     x))))