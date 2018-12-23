(eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload '(:cl-ppcre))
  (use-package '(:cl-ppcre)))

(defstruct region
  type ; Type
  gi   ; Geologic Index
  el   ; Erosion Level
  pl)  ; Path length from Mouth for each tool

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
	  (let ((array-size (list (floor (* (1+ tx) size-multiplier))
				  (floor (* (1+ ty) size-multiplier)))))
	    (list (make-array array-size
			      :element-type 'region)
		  depth
		  (cons tx ty))))))))

(defun set-region-types (maze depth target)
  (destructuring-bind (w h)
      (array-dimensions maze)

    (loop :for idx :below (array-total-size maze)
       :do (setf (row-major-aref maze idx) (make-region :type :empty
							:gi -1
							:el -1
							:pl nil)))
    
    (setf (region-gi (aref maze 0 0)) 0)
    (setf (region-el (aref maze 0 0)) (mod depth 20183))
    (setf (region-gi (aref maze (car target) (cdr target))) 0)
    (setf (region-el (aref maze (car target) (cdr target)))
	  (mod depth 20183))
    (loop :for i :from 1 :below w
       :when (eq (region-gi (aref maze i 0)) -1)
       :do (setf (region-gi (aref maze i 0)) (mod (* i 16807) 20183)) :and
       :do (setf (region-el (aref maze i 0))
		 (mod (+ (region-gi (aref maze i 0)) depth) 20183)))

    (loop :for j :from 1 :below h
       :when (eq (region-gi (aref maze 0 j)) -1)
       :do (setf (region-gi (aref maze 0 j)) (mod (* j 48271) 20183)) :and
       :do (setf (region-el (aref maze 0 j))
		 (mod (+ (region-gi (aref maze 0 j)) depth) 20183)))
    
    (loop :for j :from 1 :below h
       :do (loop :for i :from 1 :below w
	      :when (eq (region-gi (aref maze i j)) -1)
      	      :do (setf (region-gi (aref maze i j))
      			(mod (* (region-el (aref maze (1- i) j))
      				(region-el (aref maze i (1- j))))
      			     20183))
	      :and
	      :do (setf (region-el (aref maze i j))
			(mod (+ (region-gi (aref maze i j)) depth) 20183))))
    (dotimes (j h)
      (dotimes (i w)
	(setf (region-type (aref maze i j))
	      (case (mod (region-el (aref maze i j)) 3)
		((0) :rocky)
		((1) :wet)
		((2) :narrow)
		(t (error "mod erosion is > 3"))))))))



(defun print-maze (maze target &optional (stream *standard-output*))
  (destructuring-bind (w h)
      (array-dimensions maze)
    (dotimes (j h)
      (dotimes (i w)
	(princ
	 (if (equal (cons i j) target)
	     #\T
	     (region2char (region-type (aref maze i j))))
	 stream))
      (princ #\Newline stream))))

(defun print-problem-instance (file)
  (destructuring-bind (maze depth target)
      (create-empty-maze file 1)
    (set-region-types maze depth target)
    (print-maze maze target)))

(defun risk-level (maze target)
  (let ((r 0))
    (dotimes (j (1+ (cdr target)) r)
      (dotimes (i (1+ (car target)))
	(incf r (case (region-type (aref maze i j))
		  ((:rocky) 0)
		  ((:wet) 1)
		  ((:narrow) 2)
		  (t (error "unknown region type"))))))))

(defun part1 (file)
  (destructuring-bind (maze depth target)
      (create-empty-maze file 1)
    (set-region-types maze depth target)
    (risk-level maze target)))

(defun move-cost (maze from to src-tool dest-tool)
  "Cost to move FROM with SRC-TOOL to TO with DEST-TOOL in MAZE.
Assuming the FROM and TO are straight neighbors."
  (let ((dest-type (region-type (aref maze (car to) (cdr to)))))
    (cond ((or (and (eq dest-type :rocky)
		    (eq dest-tool :neither))
	       (and (eq dest-type :wet)
		    (eq dest-tool :torch))
	       (an (eq dest-type :narrow)
		   (eq dest-tool :gear)))
	   nil)
	  ((eq dest-tool src-tool) 1)
	  (t 8))))


(defun set-min-paths-from-neighbors (maze i j)
  "Set the path costs at co-ordinates (I,J).
Look at the neighboring regions and set the path costs for all tools."
  (labels ((get-neighbors (i j)
	     (remove-if-not (lambda (p)
			      (array-in-bounds-p maze (car p) (cdr p)))
			    (list (cons (1- i) j)
				  (cons (1+ i) j)
				  (cons i (1- j))
				  (cons i (1+ j))))))
    (destructuring-bind (w h)
	(array-dimensions maze)
      (dotimes (j h)
	(dotimes (i w)
	  )))))
