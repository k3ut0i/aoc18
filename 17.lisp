(eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload :cl-ppcre)
  (ql:quickload :alexandria)
  (use-package '(:cl-ppcre :alexandria)))

(defun get-range (str)
  (let ((range-scan (nth-value 1 (scan-to-strings "(\\d+)\\.\\.(\\d+)|(\\d+)" str))))
    (cond ((and (null (aref range-scan 0))
		(null (aref range-scan 1))
		(aref range-scan 2))
	   (mapcar #'read-from-string
		   (make-list 2 :initial-element (aref range-scan 2))))
	  ((and (aref range-scan 0)
		 (aref range-scan 1)
		 (null (aref range-scan 2)))
	   (mapcar #'read-from-string
		   (list (aref range-scan 0) (aref range-scan 1))))
	  (t (error "range-scan-error ~A" range-scan)))))


(defun get-scan-entry (str)
  (multiple-value-bind (xy-order xy-order-ranges)
      (scan-to-strings "x=(.*), y=(.*)" str)
    (if xy-order
	(list (get-range (aref xy-order-ranges 0))
	      (get-range (aref xy-order-ranges 1)))
	(let ((yx-order-ranges
	       (nth-value 1 (scan-to-strings "y=(.*), x=(.*)" str))))
	  (list (get-range (aref yx-order-ranges 1))
		(get-range (aref yx-order-ranges 0)))))))

(defun get-all-entries (file)
  (with-open-file (s file)
    (loop
       :for line = (read-line s nil nil)
       :while line
       :collect (get-scan-entry line))))

(defun find-boundries (entries)
  (loop
     :with max-x = 500 :and min-x = 500
     :and max-y = 0 :and min-y = 0
     :for e :in entries
     :do (when (< (car (car e)) min-x)
	   (setq min-x (car (car e))))
     :do (when (> (cadr (car e)) max-x)
	   (setq max-x (cadr (car e))))
     :do (when (> (cadr (cadr e)) max-y)
	   (setq max-y (cadr (cadr e))))
     :finally (return (list (1- min-x) (1+ max-x)
			    min-y (1+ max-y)))))

(defun make-ground (file)
  (let ((entries (get-all-entries file)))
    (destructuring-bind (min-x max-x min-y max-y)
	(find-boundries entries)
      (let ((garray (make-array (list (- max-x min-x)
				      (- max-y min-y))
				:initial-element :sand)))
	(loop
	   :for e :in entries
	   :do (loop :for j :from (caadr e) :to (cadadr e)
		  :do (loop :for i :from (caar e) :to (cadar e)
			 :do (setf (aref garray (- i min-x) (- j min-y)) :clay))))
	(setf (aref garray (- 500 min-x) (- 0 min-y)) :water-source)
	garray))))

(defun print-ground (g &optional (stream *standard-output*) (type nil))
  (destructuring-bind (w h)
      (array-dimensions g)
    (when type
      (format stream "P2~%")
      (format stream "~A ~A~%" w h)
      (format stream "~A~%" 255))
    (dotimes (j h)
      (dotimes (i w)
	(princ (case (aref g i j)
		 ((:sand) (if type 0 #\.))
		 ((:clay) (if type 255 #\#))
		 ((:water-source) (if type 50 #\+))
		 ((:still-water) (if type 200 #\~))
		 ((:flowing-water) (if type 100 #\|))
		 (t (error "Unknown case in ground ~A" (aref g i j))))
	       stream)
	(when type (princ #\Space stream)))
      (princ #\Newline stream))))

(defun print-to-pgm (g file)
  (with-open-file (s file
		     :direction :output
		     :if-exists :supersede)
    (print-ground g s t)))

(defun push-water (g i j)
  (when (and (array-in-bounds-p g i j)
	     (eq (aref g i j) :sand))
    (setf (aref g i j) :flowing-water)))


(defun num-water-squares (g)
  (loop :for i :below (array-total-size g)
     :count (or (eq (row-major-aref g i) :still-water)
		(eq (row-major-aref g i) :flowing-water))))

(defun get-lvl-boundry (g i j)
  (let (left-b 
	right-b)
    (loop
       :for left :downfrom i :downto 0
       :while (and (or (eq (aref g left j) :sand)
		       (eq (aref g left j) :flowing-water))
		   (or (eq (aref g left (1+ j)) :clay)
		       (eq (aref g left (1+ j)) :still-water)))
       :finally (setq left-b left))
    (loop
       :for right :from i :to (array-dimension g 0)
       :while (and (or (eq (aref g right j) :sand)
		       (eq (aref g right j) :flowing-water))
		   (or (eq (aref g right (1+ j)) :clay)
		       (eq (aref g right (1+ j)) :still-water)))
       :finally (setq right-b right))
    (list left-b right-b)))

(defun flow-water-very-inefficient (g i j)
  (if (array-in-bounds-p g i (1+ j))
      (case (aref g i (1+ j))
	((:flowing-water)
	 (destructuring-case (flow-water g i (1+ j))
	   ((:out-of-bounds) (list :out-of-bounds))
	   ((:filled-up) (flow-water g i j))))
	((:still-water :clay)
	 (destructuring-bind (l r)
	     (get-lvl-boundry g i j)
	   (cond ((and (eq (aref g l j) :clay)
		       (eq (aref g r j) :clay))
		  (loop :for idx :from (1+ l) :below r
		     :do (setf (aref g idx j) :still-water))
		  (list :filled-up))
		 ((eq (aref g l j) :clay)
		  (loop :for idx :from (1+ l) :to r
		     :do (setf (aref g idx j) :flowing-water))
		  (flow-water g r j))
		 ((eq (aref g r j) :clay)
		  (loop :for idx :from l :below r
		     :do (setf (aref g idx j) :flowing-water))
		  (flow-water g l j))
		 (t (loop :for idx :from l :to r
		       :do (setf (aref g idx j) :flowing-water))
		    (let ((flow-left (flow-water g l j))
			  (flow-right (flow-water g r j)))
		      (if (and (eq (car flow-left) :filled-up)
			       (eq (car flow-right) :filled-up))
			  (flow-water g i j)
			  (list :out-of-bounds)))))))
	((:sand)
	 (setf (aref g i (1+ j)) :flowing-water)
	 (destructuring-case (flow-water g i (1+ j))
	   ((:out-of-bounds) (list :out-of-bounds))
	   ((:filled-up) (flow-water g i j)))))
      (list :out-of-bounds)))

(defun part1 (file)
  (let* ((g (make-ground file))
	 (water-source-idx 
	  (loop :for idx :from 0 :below (array-dimension g 0)
	     :when (eq (aref g idx 0) :water-source)
	     :do (return idx))))
    (setf (aref g water-source-idx 1) :flowing-water)
    (flow-water g water-source-idx 1)
    (num-water-squares g)))
