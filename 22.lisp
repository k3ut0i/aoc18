(eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload '(:cl-ppcre))
  (use-package '(:cl-ppcre)))

(defparameter *region-types*
  '((#\. . :rocky)
    (#\| . :narrow)
    (#\= . :wet)))

(defun region2char (r)
  (car (find r *region-types* :key #'cdr)))
(defun char2region (c)
  (cdr (find c *region-types* :key #'car)))

(defun create-empty-maze (file &optional (size-multiplier 1))
  (with-open-file (s file)
    (register-groups-bind (depth-str)
	("depth: (.*)" (read-line s nil nil))
      (register-groups-bind (tx-str ty-str)
	  ("target: (.*),(.*)" (read-line s nil nil))
	(destructuring-bind (depth tx ty)
	    (mapcar #'read-from-string (list depth-str tx-str ty-str))
	  (let ((array-size (floor (* (1+ (max tx ty))
				      size-multiplier))))
	    (list (make-array (list array-size array-size)
			      :element-type 'keyword
			      :initial-element :empty)
		  depth
		  (cons tx ty))))))))

(defun set-region-types (maze depth)
  (destructuring-bind (w h)
      (array-dimensions maze)
    (let ((gi-array (make-array (array-dimensions maze)
				:element-type 'fixnum)))
      (setf (aref gi-array 0 0) 0)
      
      (loop :for i :from 1 :below w
	 :do (setf (aref gi-array i 0) (mod (* i 16807) (* 3 20183))))

      (loop :for j :from 1 :below h
         :do (setf (aref gi-array 0 j) (mod (* j 48271) (* 3 20183))))
      
      (loop :for j :from 1 :below h
         :do (loop :for i :from 1 :below w
      		:do (setf (aref gi-array i j)
      			  (mod (* (aref gi-array (1- i) j)
      				  (aref gi-array i (1- j)))
      			       (* 3 20183)))))
      (dotimes (j h)
	(dotimes (i w)
	  (setf (aref maze i j)
		(case (mod (mod (+ (aref gi-array i j)
				   depth)
				20183) 3)
		  ((0) :rocky)
		  ((1) :wet)
		  ((2) :narrow)
		  (t (error "mod erosion is > 3")))))))))



(defun print-maze (maze target &optional (stream *standard-output*))
  (destructuring-bind (w h)
      (array-dimensions maze)
    (dotimes (j h)
      (dotimes (i w)
	(princ
	 (if (equal (cons i j) target)
	     #\T
	     (region2char (aref maze i j)))
	 stream))
      (princ #\Newline stream))))

(defun print-problem-instance (file)
  (destructuring-bind (maze depth target)
      (create-empty-maze file 1)
    (set-region-types maze depth)
    (print-maze maze target)))
