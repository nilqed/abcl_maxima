;;;; -*- Mode: lisp -*-
;;;; Copyright (c) 2011 Richard Fateman
;;;; disclaimers at end of file
;;;; Modified KFP


(in-package :common-lisp-user)

(asdf:defsystem :mma
  :serial t
  :description "MMA4MAX"
  :version "1.0.0"
  :author "Richard Fateman"
  :license "MIT, see file LICENSE"    
  :components
  ((:file "mma")
   #+acl  
   (:file "ucons"  :depends-on ("mma"))
   #-acl 
   (:file "uconsalt" :depends-on ("mma"))
   (:file "maxcapsonlyparser"  :depends-on ("mma"))
   (:file "stack1.lisp" :depends-on ("mma"))
   (:file "disp1.lisp" :depends-on ("mma"))
   (:file "eval.lisp" :depends-on ("mma"))
   (:file "poly.lisp" :depends-on ("mma"))
   (:file "rat1.lisp" :depends-on ("mma"))
   (:file "simp1.lisp" :depends-on ("mma"))
   (:file "pf.lisp" :depends-on ("mma"))
   (:file "newmatch.lisp" :depends-on ("mma"))
   (:file "diffrat.lisp" :depends-on ("mma"))
   (:file "mma2maxfun.lisp" :depends-on ("mma"))))




#|
Permission is hereby granted, free of charge, to any person
 obtaining a copy of this software and associated documentation
 files (the "Software"), to deal in the Software without
 restriction, including without limitation the rights to use,
 copy, modify, merge, publish, distribute, sublicense, and/or sell
 copies of the Software, and to permit persons to whom the
 Software is furnished to do so, subject to the following
 conditions:

 The above copyright notice and this permission notice shall be
 included in all copies or substantial portions of the Software.

 THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
 OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
 HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
 WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
 OTHER DEALINGS IN THE SOFTWARE.
 |#
