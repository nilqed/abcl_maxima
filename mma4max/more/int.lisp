;; -*- Mode:Common-Lisp;Package:mma; Base:10 -*-
(in-package :mma)
;; interval package ;; rational endpoints, usually

(defstruct (interval
	    (:constructor make-int
			  (low hi 
				    &key (leftopen nil)(rightopen nil) (exterior nil)))
	    (:print-function intervalprintfunction))
  (left  0                  :type number :read-only t)
  (right 0                  :type number :read-only t)
  (exterior nil :readonly nil))

;; print regular interval as [< 0 , 2>]; exterior as [< 2 ,><, 0]

(defun intervalprintfunction (x s pl)  ;pl, print-level, is not used.
  (if (interval-exterior x)

     (format s "[<~s ,><, ~s>]"  ;; exterior  e.g. (make-int 2 1 :exterior t)
		 (interval-left  x)
		 (interval-right x))
    	 (format s "[<~s , ~s>]"
		 (interval-left  x)
		 (interval-right x))))
    
		 
