;;Combinatorica functions in Common Lisp
;; author: Richard Fateman with C. Ruan
;; Since there is a choice of data structures that can be used for
;; Mathematica's List  {a,b,c}  ... this could be either
;; a lisp list  (a b c)  or a lisp simple vector  #(a b c).
;; some functions are set to accept either.

;; These are not the simplest functions that can be written. Almost the same
;; deliberately naive techniques could be used as is used in Combinatorica.
;; However, one of the goals in rewriting in Lisp is to provide massively
;; faster programs. By compiling these lisp programs, orders of magnitude
;; speedup should be demonstrable.

(defun range (n &aux ans)
  "Return a list of elements from 1 to n"
  (declare (fixnum n))
  (do ((i n (1- i)))
      ((= i 0) ans)
      (declare (fixnum i))
      (setq ans (cons i ans))))


(defun vectorrange (g)
  "Return a vector of elements from 1 to n"
(declare (fixnum g))
  (let ((ans (make-array g :element-type 'fixnum)))
    (do ((i 1 (1+ i)))
	((> i g) ans)
	(declare (fixnum i))
	(setf (aref ans (1- i)) i))))


(defun permutations(bag)
  "Return a list of all the permutations of the input: vector or list"
  (if (listp bag) (permutationslist bag) 
    (mapcar #'(lambda(r)(coerce r 'vector)) 
	    (permutationslist (coerce bag 'list)))))

(defun permutationslist(bag)
  "Return a list of all the permutations of the input list."
  (if (null bag) '(())
    ;; otherwise take an element, e, out of the bag.
    ;; generate all permutations of the remaining elements.
    ;; and add e to the front of each of these.
    ;; do this for all possible e to generate all permutations.
    (mapcan #'(lambda(e)(mapcar #'(lambda(p)(cons e p))
				(permutationslist
				 (remove e bag :count 1 :test #'eq))))
	    bag)))


(defun permutationQ (x)		       ;linear time

  "Return T if x is a list or vector of the integers from 1 to 
  (length x), permuted.  Gives an error if x is not a sequence"
  
;;  If there are L numbers e[1], e[2],... e[L],  and each
;;  number can be put in one of the boxes labelled with its value
;;  1, 2, ... L, with no duplicates, then {e[i]} must be a permutation
;;  of the integers from 1 to L. (Pigeon-Hole principle)

   (let* ((len (length x)) 
	(ar (make-array (1+ len) :element-type 'bit :initial-element 0)))
     (every
      #'(lambda(e)(and(integerp e)
		      (<= 1 e len)
		      (zerop (aref ar e))
		      (incf (aref ar e))))
      x)))

;;Here's another one
;; (from Peter Norvig --)
;; This has the interesting property of not needing any intermediate
;; storage for lists of length shorter than (log most-positive-fixnum 2).
;; It is also more interesting to prove it correct.  Sketch: if x has n
;; 1 bits in its binary representation, then x + 2^i will have n+1 1 bits
;; if x had a 0 at position i, and will have at most n 1 bits otherwise.
;; So if the list were not a permutation, we couldn't get all the bits set.

#+ignore (defun permutationQ (list)
  "Is list a permutation of the integers from 1 to (length list)?"
  (= (loop for e in list sum (expt 2 (- e 1)))
     (- (expt 2 (length list)) 1)))

#+ignore (defun permutationQ(list &aux (len (length list))) ;yet another
  (= (loop 
	 for e in list 
	 do (unless (and (integerp e) (<= 1 e len))
	      (return-from perm5 nil))
	 sum (ash 1 (- e 1)))
     (- (ash 1 len) 1)))

(defun permute (l p)
  "Permute l according to the permutation p, assumes l and p have the
   same length."
;; could be done without coerce p to a list if it is a vector,
;; by using elt. But this works..
;; returns a list or a vector depending on what l is.
  (cond ((permutationQ p)
	 (if (listp l)
	     (mapcar #'(lambda (x)
			 (nth (1- x) l))
		     (coerce p 'list))
	     (coerce (mapcar #'(lambda (x)
				 (aref l (1- x)))
			     (coerce p 'list)) 'vector)))	     
	(t (error "~s is not a permutation.~%" p))))


