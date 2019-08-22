(defvar size 100)

(defstruct (stack (:print-function stackprinter))
  (size 100 :type fixnum) ;if no size is specified, how about 100?
  ;; ptr points to top of stack.  0<=ptr<size : the index in which to
  ;; store at the next "push".
  (ptr 0 :type fixnum) 
  ;;frameptr points to the bottom of current call-frame 
  ;; a pair that looks like  <name of function> <next lower frameptr>
  ;; -1 <= frameptr < ptr
  (frameptr -1 :type fixnum) 
  (vars (make-array size))
  (vals (make-array size))
  (time (make-array size)))  ;; alternative, with timestamp

(defun spush(s var val)
  (setf (aref (stack-vars s) (stack-ptr s)) var)
  (setf (aref (stack-vals s) (stack-ptr s)) val)
  (setf (aref (stack-time s) (stack-ptr s)) (timestamp)
  ;;could check for overflow here
  (incf (stack-ptr s))
  s)


(defun sfind(s var)
  (let ((loc (position var (stack-vars s)
		       :start (1+(stack-frameptr s)) :end (stack-ptr s))))
    (if loc (values (aref (stack-vals s) loc) 
		    loc			; found: 2nd val is index
		    (aref (stack-time s))); 3rd val is timestamp
		    )	
      (values var loc)  ;;2nd value will be nil, first will be var itself
      )))




