; input:
; 1) to plot one function--
;     (plot fx (x xmin xmax) [:keys...])
; 2) to plot several functions together--
;     (plot fx (x xmin xmax)) [:keys...])
(defmacro plot (f (var min max) 
		  &key
		  (points 100) 
		  (title " ")
		  (xlable "X")
		  (ylable "Y")
		  boundbox
		  markers
		  ticks
		  ymax   
		  ymin 
		  logx
		  logy
		  )
  `(block ()
     (with-open-file (xtmp "/tmp/xtmp" 
			   :direction :output :if-exists :supersede)
		     
        (let ((interval (/ (- ,max ,min), points)))
	  (format xtmp "0.Color: black~%")
	  (format xtmp "TitleText: ~a~%" ,title)
	  (format xtmp "XUnitText: ~a~%" ,xlable)
	  (format xtmp "YUnitText: ~a~%" ,ylable)
	  (if ,boundbox (format xtmp "BoundBox: 1~%"))
	  (if ,markers (format xtmp "LargePixels: 1~%"))
	  (if ,ticks (format xtmp "Ticks: 1~%"))
	  (if ,ymin (format xtmp "YLowLimit: ~s~%" ,ymin))
	  (if ,ymax (format xtmp "YHighLimit: ~s~%" ,ymax))
	  (if ,logx (format xtmp "LogX: 1~%"))
	  (if ,logy (format xtmp "LogY: 1~%"))
	  
	  (cond((and (consp ',f)(eq (car ',f) 'list))
		(mapc #'(lambda(z)
			  (plot1 z ',var ',min ',max interval xtmp)
			  (format xtmp "~%"))
		      (cdr ',f)))
	       ;; just one function
	       (t (plot1 ',f ',var ,min ,max interval xtmp)))
	  (run-shell-command "/usr/local/bin/xgraph /tmp/xtmp" :wait nil)
	  'XgraphOutput))))

(defun plot1(f var min max interval xtmp)
 "plot one function f on the open file xtmp"
 (let* ((fun  `(lambda (,var) ,f)))
   (setq interval (coerce interval 'single-float))
   (format xtmp "\"~a\"~%" f)
   (do* ((vtmp (coerce min 'single-float)
	       (+ vtmp interval))
	 (var vtmp
	      (jigglept vtmp interval)))
       ((>= var max)
	(format xtmp "~12,4G ~12,4G~%"
		(coerce max 'single-float)
		(coerce (funcall fun max) 'single-float)))
       (format xtmp "~12,4G ~12,4G~%" 
	       var
	       (coerce (funcall fun var)'single-float)))))


; input:
; 1) to make a parametric plot--
;    (parametric_plot (list fx fy) (t tmin tmax) [:keys...])   
; 2) to plot several parametric curves together--
;    (parametric_plot (list (list fx fy) (list gx gy) ...) 
;                     (t tmin tmax) [:keys...])
(defmacro parametric_plot ((l &rest f) (var min max)
			   &key
			   (points 100) 
			   (title " ")
			   (xlable "X")
			   (ylable "Y")
			   boundbox
			   markers
			   ticks
			   ymax   
			   ymin 
			   logx
			   logy
			   )
  `(block ()
     (with-open-file (xtmp "/tmp/xtmp" 
			   :direction :output :if-exists :supersede)
		     
        (let ((interval (/ (- ,max ,min), points)))
	  (format xtmp "0.Color: black~%")
	  (format xtmp "TitleText: ~a~%" ,title)
	  (format xtmp "XUnitText: ~a~%" ,xlable)
	  (format xtmp "YUnitText: ~a~%" ,ylable)
	  (if ,boundbox (format xtmp "BoundBox: 1~%"))
	  (if ,markers (format xtmp "LargePixels: 1~%"))
	  (if ,ticks (format xtmp "Ticks: 1~%"))
	  (if ,ymin (format xtmp "YLowLimit: ~s~%" ,ymin))
	  (if ,ymax (format xtmp "YHighLimit: ~s~%" ,ymax))
	  (if ,logx (format xtmp "LogX: 1~%"))
	  (if ,logy (format xtmp "LogY: 1~%"))

	  (cond ((and (consp (car ',f)) (eq (caar ',f) 'list))
		 (mapc #'(lambda (z)
			 (let ((fx (nth 1 z))
			       (fy (nth 2 z)))
			   (para_plot1 fx fy ',var ,min ,max interval xtmp)
			   (format xtmp "~%")))
		       ',f))
	       ;; just one function
		(t (let ((fx (nth 0 ',f))
			 (fy (nth 1 ',f)))
		     (para_plot1 fx fy ',var ,min ,max interval xtmp))))
	  (run-shell-command "xgraph /tmp/xtmp" :wait nil)
	  'XgraphOutput))))

(defun para_plot1 (fx fy var min max interval xtmp)
  (let* ((xfun `(lambda (,var) ,fx))
	 (yfun `(lambda (,var) ,fy)))
   (setq interval (coerce interval 'single-float))
   (format xtmp "\"~a, ~a\"~%" fx fy)
   (do ((var (coerce min 'single-float)
	     (+ var interval)))       
       ((>= var max)
	(format xtmp "~12,4G 12,4G~%"
		(coerce (funcall xfun max) 'single-float)
		(coerce (funcall yfun max) 'single-float)))
       (format xtmp "~12,4G ~12,4G~%" 
	       (coerce (funcall xfun var) 'single-float)
	       (coerce (funcall yfun var) 'single-float)))))

(defun jigglept (x width)
  ;; jiggle the point x in a random direction by a random amount <= 1/100.0
  ;; of the width
  (+ x (* 
	;; compute random +1 or -1
	(- (random 2) 1)
	;; compute a random number from [0, width/100.0]
	(random (/ width 100.0)))))

