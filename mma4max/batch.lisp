;; -*- Mode:Common-Lisp;Package:mma; Base:10 -*-
;;(eval-when (:compile-toplevel) (load "mma"))
(in-package :mma)
(defvar *linebuf* nil)
(defvar *linestream* nil)
(defvar *echobatch* nil)
(defvar filestream t) ;; just std input, if nothing else
(declaim (special stream *echobatch* *linebuf*))

;;(defun pc()(peek-char nil stream nil 'e-o-l nil))  ;in parser.lisp
;;(defun pc()(peek-char nil stream nil  #\newline))
;;(defun rc()(read-char stream nil 'e-o-l)) ;in parser.lisp

;; fake out the file-name reading so that  Batch[foo/bar.m] works as
;; well as Batch["foo/bar.m"]

(defun fakefilename(r) 
  (cond ((stringp r) r)  ;; "filename. m"
	((symbolp r) (symbol-name r)) ;; 'foo  -> "foo"
	((eql (|Head| r) '|Dot|)
	 (format nil "~a.~a"(cadr r)(caddr r))) ; (Dot foo m)->"foo.m"
	((and (eql (|Head| r) '|Power|) (eql (caddr r) -1)) ;
	 (format nil "/~a"(fakefilename(cadr r))))
	((eql (|Head| r) '|Times|) ;; maybe foo/bar.m (Times foo (Power (Dot bar m) -1)) 
	 (apply #'concatenate (cons 'string (mapcar #'(lambda(z)(fakefilename z)) (cdr r)))))))
			     

;;Batch["file.m"]   or Batch[dir/file.m] no quotes
;;Batch[fn,False]  does not display the file as it is read in.

(defun |Batch| (filename-in &optional (*echobatch* t)) ;; top level
  (declare (special stream env *package* *echobatch* COUNT))
  ;; to not echo, do Batch[file,False]
  ;; (print 101)
  (let*
      ((*package* (find-package :mma))
       h hs hin 
       (filename(fakefilename filename-in))
       (stream (open filename))
       (filestream stream)
       (timesofar 0)
       (timeunit (/ 1.0 internal-time-units-per-second))
       (env (make-stack :size 50))   ;environment for matching binding
       ;;   (COUNT 1)
       (*linebuf* nil))
    
    (if *echobatch* (format t "~%-- Batch processing file ~a -- ~%" filename))
    (loop
     (setq timesofar (get-internal-run-time))
      ;; check for eof
     (cond ((eql 'eof (peek-char t filestream nil 'eof nil))
	    ;;  need to peek to see if end-of-file really.
	    (if *echobatch*  (format t "~%Batch Done"))
	    (return '|Null|)))
     (unless (or (eql hin'|Null|)(null *echobatch*))      
       (format t "~%~%In[~s] := " COUNT)) ;; actually In and Out are variables too.
      
      ;; get the input
      
     (setq hin  (handler-case (mma::p stream)
			      (error(x)(format t "~%syntax error ~s" x)
				;; comment out line below after debugging
				    (return-from |Batch| (format nil "error in Batch line ~s" 'unknown))
		     
				    (clear-input t) '|Null|)))
      
      ;;   evaluate it
      ;;  (format t "~% Input =~s" hin)
      ;;  (format t "~% evaluating input=~s" hin)

     (unless (eql hin '|Null|) ;; parser returned NULL ... a comment probably
       (setq h
	     (handler-case (meval hin)(error(x) (format t "~%evaluation error ~s" x)`(|Hold| , hin))))
       (|SetQQ| (ulist '|In| COUNT) hin)
      
       (setq timesofar (- (get-internal-run-time) timesofar))
       ;; this is not the same as mathematica but I find it more convenient
       ;; this way. We've also implementing "Timing", if you prefer.
					;  (print 'xx2)
       (unless (null *echobatch*)   (if (eq '|True| (meval '|$Showtime|))
					(format t "~%time = ~3,$ secs." (* timesofar timeunit))))
   
       (cond ((or (eql h '|Exit|)
		  (eql h 'EXIT) ;; so I can get out of tl in GCL without readtable-case set properly
		  (and (listp h)(or (eq (car h) 'QUIT)(eq (car h) '|Quit|)))) ;;Quit[]
	      (format t"~%Exited to Lisp~%")
	      (return t))
	     (t	

	      (cond((eq h '|Null|) nil)
		   ;; don't print nil-valued answers
		   (t
		    (setq hs (list '|Set| (ulist '|Out| COUNT) h))
		    (unless (null *echobatch*)   (disp (BuildFormat hs)))))))
					; (print 'xx4)
       (|SetQQ| (ulist '|Out| COUNT) h)      
       (|Set|  '|$Line| (incf COUNT))
       ;;(format t "~%peek=~s"  (peek-char t filestream nil 'eof nil)) ;scan past whitespace
       ) ;; continue loop
     )))

;; this program replaces mread1 in parser file and in eval file.


(defun mread1() ;; called by parser (p)
  (declare (special *echobatch* stream))
  (cond ((null *linebuf*) ;; if there is nothing in the line buffer
	 ;; the stream in the next line is filestream if we are in batch.
	 (setf *linebuf* (read-line filestream nil 'e-o-l nil)) ;fill it up
	 (if (eql *linebuf* 'e-o-l)(return-from mread1 'e-o-l));if nothing remains, return e-o-l
	 ;;(format t "~%echobatch=~s" *echobatch*)
	 (if *echobatch* (write-line *linebuf* t)) ; if batch echoing, do so with the linebuf
	 (setf *linestream* (make-string-input-stream *linebuf*)))) ;; form a stream to read from
  (let ((stream *linestream*)) ;; pc and rc etc will now be reading from the string/stream
    ;; line  buffer has something in it
    (cond((eql (pc) 'e-o-l)		; end of line
	  (setf *linebuf* nil)		;set up to read new line if mread1 is called again
	  (return-from mread1 'e-o-l)))
     (let* ((rr (read-preserving-whitespace stream nil 'e-o-l))
	    (c(chash #+:allegro rr
		     #-:allegro (recase rr))))
       (if (and (numberp c)(eq (pc) #\.)(rc)(typep (pc) 'character) (digit-char-p (pc)))
	   (list '|Real|  c (parse-frac nil)) ;;
	  
	 c))))

;; old version, crufty, messes up on 4.0. Make it (Dot 4 0) 
;; version in parser.lisp works though, producing (Real 4 0)
#+ignore 
(defun mread1() ;; called by parser (p)
  (declare (special *echobatch* stream))
  (cond ((null *linebuf*) ;; if there is nothing in the line buffer
	 ;; the stream in the next line is filestream if we are in batch.
	 (setf *linebuf* (read-line filestream nil 'e-o-l nil)) ;fill it up
	 (if (eql *linebuf* 'e-o-l)(return-from mread1 'e-o-l));if nothing remains, return e-o-l
	 ;;(format t "~%echobatch=~s" *echobatch*)
	 (if *echobatch* (write-line *linebuf* t)) ; if batch echoing, do so with the linebuf
	 (setf *linestream* (make-string-input-stream *linebuf*)))) ;; form a stream to read from
  (let ((stream *linestream*)) ;; pc and rc etc will now be reading from the string/stream
    ;; line  buffer has something in it
    (cond((eql (pc) 'e-o-l)		; end of line
	  (setf *linebuf* nil)		;set up to read new line if mread1 is called again
	  (return-from mread1 'e-o-l)))

	;; we have temporarily rebound stream so pc, rc rt work
	;;(format t "~%stream=~s" stream)
   				; skip whitespace

    (cond
     ((digit-char-p (pc));; next character is a digit 0-9
	(collect-integer 
	 (char-to-int(read-char stream)) 10)) ;radix 10 default
       
       ;; for every alphanumeric symbol, set up a hash table
       (t 
	  (let* ((rr (or(read-preserving-whitespace stream nil 'e-o-l) nil))
		 (c(chash #+:allegro rr
			  #-:allegro (recase rr))))
	  
	    c)
	   ;; nil reads as False
	  ))))

;; replaces program in parser file

(defun commentskip (stream &aux x ) ;; we have enccountered (* on input. look for *)
  (declare (special stream *linestream* filestream))
  (loop
    (setf x (rc))  ;; maybe read a #\*  ?
  ;; (format t "~% in commentskip, x=~s" x)
    (cond ((eql x 'e-o-l) 
	   ;; hm. read another line from file or std input
	   (setf *linebuf* (read-line filestream nil 'e-o-f nil)) ;fill it up
;	   (format t "~%linebuf =~a" *linebuf*)
	   (cond ((not(stringp *linebuf*)) ; maybe e-o-f
		  (format t "~% end of file within comment")
		  (error 'syntax))
		 (t ;; it is a string
		  (if *echobatch* (write-line *linebuf* t))
		  (setf *linestream* (make-string-input-stream *linebuf*))
		  (setf stream *linestream*)
		  (setf x (rc))		; get next char from new line
		     ))))			; if batch echoing, do so with the linebuf
    ;;	   (format t "~%linebuf echoed~%" )
    ;;	   (format t "~%linestream=~s" *linestream* )
  
 ;     (format t "~% in commentskip, x=~s" x)
    (cond
       ((eql x #\( ) (sawlpar stream x))    ;; recursive comments allowed.
       ((and (eql x #\* )
	     (eql (pc) #\) ))
	;(format t "~% flush ~s" x)
	;(format t "~% flush ~s" (rc))
	(rc)				; flush the last leftpar
	
	(return-from commentskip (mread1)))
       ;; just continue in loop
       )
    )
    )	;; lexing comment returns Null? or next 



(defun opp(body vals rules) ;; binds lisp stuff, evals in lisp
  (eval  (list 'let (mapcar #'list (rest vals) (rest (|ReplaceAll|  vals rules))) body)))


;;(opp '(print (list x y))   '(List x y) '(List (Rule x 3)(Rule y 4)))

;; probably what is needed is binding MMA stuff, MEVAL in MMA.
;; choose how to bind, e.g. via module or block..


(defun testr()(let ((*readtable* mathrt)) (read-preserving-whitespace t)))