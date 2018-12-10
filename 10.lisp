(eval-when (:load-toplevel :compile-toplevel :execute)
  (ql:quickload :cl-ppcre)
  (ql:quickload :alexandria)
  (use-package (list :cl-ppcre
		     :alexandria)))

(defun get-point (string)
  (register-groups-bind (x y vx vy)
      ("position=<(.*), (.*)> velocity=<(.*), (.*)>" string)
    (mapcar #'read-from-string (list  x y vx vy))))

(defun get-points (file)
  (with-open-file (s file)
    (loop
       :for line = (read-line s nil nil)
       :while line
       :collect (get-point line))))

(defun point-boundaries (points)
  (reduce (lambda (b p)
	    (destructuring-bind (x y vx vy)
		p
	      (declare (ignore vx vy))
	      (destructuring-bind (min_x max_x min_y max_y)
		  b
		(cond ((< x min_x) (setq min_x x))
		      ((> x max_x) (setq max_x x))
		      ((< y min_y) (setq min_y y))
		      ((> y max_y) (setq max_y y)))
		(list min_x max_x min_y max_y))))
	  points
	  :initial-value (list 0 0 0 0)))

(defun n-consecutive-p (n numbers &optional (delta 1))
  (loop
     :with snum = (sort numbers #'<)
     :with first = (car snum)
     :with i = 0
     :for num :in snum
     :while (= num (+ first (* i delta)))
     :do (incf i)
     :finally (return (> i (1+ n)))))


(defun after-sec (points)
  (mapcar (lambda (p)
	    (destructuring-bind (x y vx vy)
		p
	      (list (+ x vx) (+ y vy) vx vy)))
	  points))
(defun after-n-sec (n points)
  (loop
     :with p = points
     :repeat n :do (setf p (after-sec p))
     :finally (return p)))

(defun plot (points)
  (destructuring-bind (xmin xmax ymin ymax)
      (point-boundaries points)
    (let ((a (make-array (mapcar #'1+ (list (- ymax ymin)
					    (- xmax xmin)))
			 :initial-element 0)))
      (loop :for p :in points
	 :do (setf (aref a
			 (- (cadr p) ymin)
			 (- (car p) xmin))
		   1))
      a)))

(defun area (points)
  (destructuring-bind (minx maxx miny maxy)
      (point-boundaries points)
    (* (1+ (- maxy miny)) (1+ (- maxx minx)))))

(defun plot-image (points output-file)
  (with-open-file (s output-file
		     :direction :output
		     :if-exists :supersede)
    (format s "P1~%")
    (let ((matrix (plot points)))
      (destructuring-bind (x y)
	  (array-dimensions matrix)
	(format s "~A ~A~%" y x)
	(dotimes (i x)
	  (dotimes (j y)
	    (format s "~A " (aref matrix i j)))
	  (format s "~%"))))))

(defun solution (input-file)
  (loop
     :with initial-points = (get-points input-file)
     :with min-area = (area initial-points)
     :with min-sec = 0
     :with min-points = initial-points
     :for n  :from 0 :below 50000
     :for points = initial-points :then (after-sec points)
     :when (< (area points) min-area)
     :do (setq min-area (area points)
	       min-sec n
	       min-points points)
     :finally (plot-image min-points (format nil "day10-in-secs-~A.pbm" min-sec))))
