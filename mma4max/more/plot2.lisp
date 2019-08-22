;; an example function with everything wired in...

(defun sinplot()
 (with-open-file (s "/tmp/sinplot" :direction :output :if-exists :supersede)
		 (format s "TitleText: Sin(x)~%")
		 (format s "0.Color: blue~%")
			 (do ((i 0.0 (+ i 0.1)))
			     ((> i 6.28)
			      (run-shell-command "xgraph /tmp/sinplot" :wait nil)
			      'XGraphOutput)
			     (format s "~12,4G ~12,4G~%" i (sin i)))))


;; an example displaying boxes, again with everything wired in...

(defun sinboxplot()
 (with-open-file (s "/tmp/sinplot" :direction :output :if-exists :supersede)
		 (format s "TitleText: Sin(x)~%")
		 (format s "0.Color: blue~%") 
			 (do ((i 0.0 (+ i 0.1)) ;; lower left x coord
			      (ip1 0.1 i) ;; lower right x coord
			      (si 0.0 (sin i)) ;; upper left y coord
			      (sip1 (sin 0.1) si)) ;; upper right y coord
			     
			     ((> i 6.283)
			      (run-shell-command "xgraph /tmp/sinplot" :wait nil)
			      'XGraphOutput)
			     (format s "move ~12,4G ~12,4G
 ~12,4G ~12,4G
 ~12,4G ~12,4G
 ~12,4G ~12,4G
 ~12,4G ~12,4G~%"
 i si i sip1 ip1 sip1 ip1 si i si) ;;draw a box
)))




