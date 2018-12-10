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
     :finally (> i n)))

(defun message-p (points)
  (and (loop
	  :with h = (make-hash-table)
	  :for p :in points
	  :do (setf (gethash (car p) h)
		    (cons (cadr p) (gethash (car p) h nil)))
	  :finally (progn
		     (return (loop
				:for v :being :each :hash-value :in h
				:thereis (n-consecutive-p 5 v)))))
       points))

(defun after-sec (points)
  (mapcar (lambda (p)
	    (destructuring-bind (x y vx vy)
		p
	      (list (+ x vx) (+ y vy) vx vy)))
	  points))

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

(defun part1 (file)
  (loop
     :with n = 0
     :for points = (get-points file) :then (after-sec points)
     :do (incf n)
     :thereis (message-p points)))
