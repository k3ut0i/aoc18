(eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload '(:alexandria))
  (use-package '(:alexandria)))

(defparameter *unit-chars*
  '((#\# . :lumber)
    (#\. . :empty)
    (#\| . :tree)))

(defun unit2char (u)
  (car (find u *unit-chars* :key #'cdr)))
(defun char2unit (c)
  (cdr (find c *unit-chars* :key #'car)))
(defun read-area (file)
  (with-open-file (s file)
    (let* ((lines (loop :for line = (read-line s nil nil)
		     :while line :collect line))
	   (area (make-array (list (length (car lines))
				   (length lines))
			     :element-type 'keyword
			     :initial-element :empty)))
      (loop
	 :for j :below (length lines)
	 :for line :in lines
	 :do (loop :for c :across line
		:for i :below (length line)
		:do (setf (aref area i j) (char2unit c))))
      area)))

(defun print-area (a &optional (stream *standard-output*))
  (destructuring-bind (w h)
      (array-dimensions a)
    (dotimes (j h)
      (dotimes (i w)
	(princ (unit2char (aref a i j)) stream))
      (princ #\Newline stream))))

(defun next-unit (unit neighbors)
  "The next unit of an UNIT acre with NEIGHBORS."
  (loop
     :with h = (make-hash-table)
     :for n :in neighbors
     :do (incf (gethash n h 0))
     :finally (return
		(cond ((eq unit :empty) (if (>= (gethash :tree h 0) 3)
					    :tree
					    unit))
		      ((eq unit :tree) (if (>= (gethash :lumber h 0) 3)
					   :lumber
					   unit))
		      ((eq unit :lumber) (if (and (>= (gethash :lumber h 0) 1)
						  (>= (gethash :tree h 0) 1))
					     unit
					     :empty))
		      (t unit)))))

(defun get-neighbors (a i j)
  (let (neighbors)
    (dotimes (jd 3 neighbors)
      (dotimes (id 3)
	(when (and (array-in-bounds-p a (+ i (1- id)) (+ j (1- jd)))
		   (not (and (= id 1) (= jd 1))))
	  (push (aref a (+ i (1- id)) (+ j (1- jd))) neighbors))))))

(defun step-minute (area)
  (destructuring-bind (w h)
      (array-dimensions area)
    (let ((new-area (make-array (array-dimensions area))))
      (dotimes (j h new-area)
	(dotimes (i w)
	  (setf (aref new-area i j)
		(next-unit (aref area i j)
			   (get-neighbors area i j))))))))

(defun resource-value (area)
  (loop :with trees = 0 :and lumber = 0
     :for i :below (array-total-size area)
     :do (case (row-major-aref area i)
	   ((:tree) (incf trees))
	   ((:lumber) (incf lumber)))
     :finally (return (* trees lumber))))

(defun part1 (n file &optional  (analyze-p nil) (analysis-file "day18_data.txt"))
  (with-open-file (s analysis-file
		     :direction :output
		     :if-exists :supersede)
    (loop
       :with area = (read-area file)
       :for i :below n
       :do (setf area (step-minute area))
       :do (when analyze-p
	     (format s "~A ~A~%" i (resource-value area)))
       :finally (return (resource-value area)))))
