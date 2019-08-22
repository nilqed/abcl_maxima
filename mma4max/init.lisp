;;; mod 22-AUG-2019/kfp

(defparameter *mma-files*
  '(;;"mma"  
    "uconsalt" 
    "parser"
    "stack1"
    "disp1"
    "poly"
    "rat1"
    "simp1"
    "pf"
    "eval"
    "newmatch"
    "diffrat"
    "morefuns"
    "function"
    "mma2maxfun"
    "batch"
    ))
(defparameter *mma-dir* "mma4max/")

(defparameter *mma-source-files*
    (mapcar #'(lambda(r)(concatenate 'string *mma-dir* r ".lisp")) *mma-files*))


 
(defparameter *mma-object-files*
    (mapcar #'(lambda(r)(concatenate 'string *mma-dir* r ".abcl")) *mma-files*))


(defun compile-mma ()
  (load "mma4max/mma.lisp") ;; establish mma package
 (load "mma4max/uconsalt.lisp")
    (load "mma4max/poly.lisp")	; establish some macros for compiler
   (load "mma4max/rat1.lisp")
   (map nil #'compile-file *mma-source-files*))


;; make sure all dependencies taken care of
(defun recompile-mma()(dotimes (i 2)(compile-mma)(load-mma)))


(defun load-mma()
  (load "mma4max/mma.lisp") ;; establish mma package   ?? 
  (map nil #'load *mma-object-files*)
       (setf *package* (find-package :mma)))

(defun load-mma-lisp()
  (load "mma4max/mma.lisp") ;; establish mma package   ?? 
  (map nil #'load *mma-source-files*)
       (setf *package* (find-package :mma)))



