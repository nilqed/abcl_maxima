(defun sinplot()
 (with-open-file (s "/tmp/sinplot" :direction :output :if-exists :supersede)
		 (format s "TitleText: Sin(x)~%")
		 (format s "0.Color: white~%")
			 (do ((i 0.0 (+ i 0.1)))
			     ((> i 6.28)
			      (run-shell-command "xgraph /tmp/sinplot" :wait nil)
			      'XGraphOutput)
			     (format s "~12,4G ~12,4G~%" i (sin i)))))

(defmacro h(x) `(block() (print ',x) (print ,x) 'done))
(h (cos 1))

(defmacro plot (f (var min max) 
		  &key
		  (points 100) 
		  (label " ") ;;maybe put something like line number
		  boundbox
		  markers
		  ticks
		  ymax   
		  ymin 
		  )
  `(block ()
       (with-open-file (xtmp "/tmp/xtmp" 
			:direction :output :if-exists :supersede)

	 (let ((interval (/ (- ,max ,min), points)))
	   	 (format xtmp "0.Color: black~%")
	   	 (format xtmp "TitleText: ~a~%" ,label)
	   	 (if ,boundbox (format xtmp "BoundBox: 1~%"))
	   	 (if ,markers (format xtmp "LargePixels: 1~%"))
	   	 (if ,ticks (format xtmp "Ticks: 1~%"))
	   	 (if ,ymin (format xtmp "YLowLimit: ~s~%" ,ymin))
		 (if ,ymax (format xtmp "YHighLimit: ~s~%" ,ymax))

		 (format t " f is ~s, cdr f is ~s~%" ',f  (cdr ',f))
		 (cond((and (consp ',f)(eq (car ',f) 'list))
		       (format t "hello~%")
		       (mapc #'(lambda(z)
				 (plot1 z ',var ',min ',max interval xtmp)
				 (format xtmp "~%"))
			     (cdr ',f)))
		      ;; just one function
		      (t (plot1 f var min max interval xtmp)))))
	 (run-shell-command "xgraph /tmp/xtmp" :wait nil)
	 'Xgraphout))

(defun plot1(f var min max interval xtmp)
 "plot one function f on the open file xtmp"
 (let* ((fun  `(lambda (%var%), (subst '%var% var f))))
   (setq interval (coerce interval 'single-float))
 (format xtmp "\"~a\"~%" f)
   (do ((var (coerce min 'single-float)
	     (+ var interval)))

	      ((> var max) nil)
	      (format xtmp "~12,4G ~12,4G~%" 
		      var
		      (coerce (funcall fun var)'single-float)))))




;;;;;;;;;;;february 1998, rjf...  some things don't work now

#|  (plot (sin x) (x 0 pi) 0.1)  doesn;t anymore
but
(setf foo (open "/tmp/foo" :direction :output))
(PLOT1 '(SIN X) 'X 0 10 0.1 FOO)

seems to produce a file. Changes in lisp since 1992...

|#
