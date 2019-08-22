(in-package :mma)
(declaim (special env *expand* *numer*)) ;; environment
#|
possible forms


Function[{x1,x2}, Foo[x1,x2]]
      which is
             (Function (List x1 x2) (Foo x1 x2))     
Function[{x}, Foo[x,x2]]  
       which is
                 (Function (List x) (Foo x x2))
Function[ Foo[#,x2]]  or Foo[#,x2]& 
       which is 
                 (Function (Foo (Slot 1) x2))

Function[Foo[Slot[1], Slot[2]]]
       which is 
                 (Function (Foo (Slot 1) (Slot 2))


 f&  or (Function  f)  is possible.  A function that when applied returns f.
			   
To fully do Function we would need to have mapply  do the following:

  look for the highest numbered (Slot i). There are i arguments.
  transform function without args by replacing (Slot i) by gensym equivalent
  transformed function can be stored in hash table.
  applied when needed in usual way, e.g. push frame, push vars meval.

  Check for SlotSequences, which require picking out Sequences of args.
  There are SlotSequences e.g. ##2 means all args from 2 to end
  There is also #0 which is "the whole function"  and there is a "3rd argument"
  to set Attributes. For example,
  Function[{v1,v2}, stuff; morestuff;evenmorestuff  , Listable]
  or
  Function[{v1,v2}, stuff; morestuff;evenmorestuff  , {HoldAll,Listable}]


  fwhew.
  |#


;; hash table for functions with parameters made explicit
(defparameter functionht (make-hash-table :test 'eq)  )

(defun funfixcompute (fargs) ;; fargs = ((List ...)  stuff)   or  ( stuff)
  ;; if explicit args, easy case
  ;; we will store in functionht stuff like ((List x1 x2) (foofoo...))
  ;; why not 
  (let (		  
	(body nil)
	(varlist nil)
	(newfun nil))
			
    (cond ((and (consp (car fargs))
		(eql '|List| (caar fargs)))
	   (setf body (cadr fargs))
	   (setf varlist (car fargs))
	   (setf newfun fargs))
	  (t (setf body fargs)
	     (let ((slotcount 0)	 )
	   
	       (labels((funfixer1
			(body)
			(map nil 
			     #'(lambda(r)
				 ;;(format t "~% test binding for ~s" r)
				 (cond ((atom r) nil)
				       ((consp r)
					(cond((eq (car r)'|Slot|)
					      (setf slotcount (max slotcount (cadr r))))
					     (t (funfixer1 r))))))

			     body)))
		 (funfixer1 fargs)
		 ;; make a list of count  gensyms 
		 (setf varlist (loop for i from 0 to (1- slotcount) collect (gensym)))
		 ;;   (format t "~% varlist =~s" varlist)
		 (setf newfun(ucons (cons '|List| varlist )
			      (sublis (loop for i in varlist for j from 1 to slotcount
					    collect (cons  (list '|Slot| j) i))

				       body :test 'equal)))))))
    ;; here we could add processing for SlotSequence
    ;;(format t "~% new function is ~s" newfun)
    (setf (gethash fargs functionht) newfun)
    newfun))
  
  
;; functional object in hash table...
;; um, Mathematica doesn't allow for a separate environment for a functional object.
;; Internal references to variables are resolved either at the time the Function is
;; first encountered, or when it is executed and the names are resolved dynamically.

(defun funfix(funargsbody)  ;; cdr of (Function ...)
  (let((r (gethash funargsbody functionht))) ;; did we already fix this function
    (cond ((null r)
	   (setf (gethash funargsbody functionht)  ;; make a fixed copy now.
	     (setf r (funfixcompute funargsbody)))))
    r))



(defun mappfun (h args expr env);; note that args= (cdr expr).  expr is not explicitly used
  (declare(ignore expr))
  
  
  ;; h = (Function (List x y) Expression )
  ;; h = (Function (List x y) (CompoundExpression ..) )
  ;; h = (Function (List x y) (CompoundExpression ..)  HoldAll )
  ;; h = (Function (List x y) (CompoundExpression ..) (List HoldAll Numeric))
  ;; args = ( List a b )  etc
  
  (let* ((thefun (or (gethash (cdr h) functionht) (funfixcompute (cdr h))))
	 (theparams (cdr (first thefun))) ;; (List ...), actually ( ...)
	 (thebody (second thefun)) ;; expr or compound expr
	 (attribs (third args)))
    
 ;;  (format t "~% fun=~s param=~s body=~s attrib=~s" thefun theparams thebody attribs)
    
    (cond((null attribs))		;do nothing, no attributes
	 ((eql (car attribs) '|List|) (setf attribs (cdr attribs)))
	 (t (setf attribs (list attribs)))) ; only one attribute.

    (setf args (mevalargs attribs args)) ;evaluate args as per recipe of attributes

   (spushframe env 'function-block)
    (loop for i in theparams for j in args do
	  (spush env i j))
    (prog1 (meval thebody)
           (spopframe env)
      )))
    
    ;;remove all the (Slot 1) etc stuff.
    
  
  ;; 
(defun dumpht(r)(maphash #'(lambda(r s)(format t "~%key=~s val=~s" r s)) r))
  
