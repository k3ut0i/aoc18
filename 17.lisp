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
	garray))))

(defun print-ground (g stream &optional (type nil))
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
