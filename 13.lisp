(eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload :cl-ppcre)
  (ql:quickload :alexandria)
  (use-package (list :cl-ppcre :alexandria)))

(defparameter +map-chars+
  '((:nesw . #\/)  ; north east or south west
    (:nwse . #\\)  ; north west or south east
    (:north-south . #\|)
    (:east-west . #\-)
    (:intersection . #\+)
    (:empty . #\Space)
    (:car-north . #\^)
    (:car-south . #\v)
    (:car-east . #\>)
    (:car-west  . #\<)))

(defun get-map-char-type (c)
  (car (find c +map-chars+
	     :key #'cdr
	     :test #'eq)))

;; Just used for printing.
(defun get-map-type-char (char-type)
  (cdr (find char-type +map-chars+
	     :key #'car
	     :test #'eq)))

;;TODO: If a car is on the road change the map to (list :car-* :road-type)
(defun read-map (file)
  (with-open-file (s file)
    (let* ((lines (loop
		     :for line = (read-line s nil nil)
		     :while line :collect line))
	   (height (length lines))
	   (width (loop :for line :in lines :maximizing (length line)))
	   (map (make-array (list width height) :initial-element :empty))
	   (cars (make-hash-table :test #'equal)))
      (loop
	 :for line :in lines
	 :for j :below height
	 :do(loop :for c :across line
	       :for i :below width
	       :do (let ((char-type (get-map-char-type c)))
		     (case char-type
		       ((:car-north :car-east :car-south :car-west)
			(setf (gethash (cons i j) cars) (cons char-type 0))
			(setf (aref map i j) (case char-type
					       ((:car-north :car-south)
						:north-south)
					       (t :east-west))))
		       (t (setf (aref map i j) char-type))))))
      (values map cars))))

(defun print-road-map (map cars)
  (destructuring-bind (w h)
      (array-dimensions map)
    (dotimes (j h)
      (dotimes (i w)
	(format t "~A" (get-map-type-char (if (gethash (cons i j) cars)
					      (car (gethash (cons i j) cars))
					      (aref map i j)))))
      (princ #\Newline))))

;; To handle crashes we need to incremental updates for cars
;; so they need to be 
(defun step-map (map cars)
  (loop
     :with new-cars = (make-hash-table :test #'equal)
     :for k :in (sort (hash-table-keys cars)
		      (lambda (p1 p2)
			(or (< (car p1) (car p2))
			    (and (= (car p1) (car p2))
				 (< (cdr p1) (cdr p2))))))
     :do (destructuring-bind (i . j)
	     k
	   (destructuring-bind (direction . intersection)
	       (gethash k cars)
	     (case direction
	       ((:car-north)
		(if (gethash (cons i (1- j)) new-cars)
		    (remhash (cons i (1- j)) new-cars)
		    (setf (gethash (cons i (1- j)) new-cars)
			  (case (aref map i (1- j))
			    ((:nwse) (cons :car-west intersection))
			    ((:nesw) (cons :car-east intersection))
			    ((:intersection)
			     (cons (case intersection
				     ((0) :car-west)
				     ((1) :car-north)
				     ((2) :car-east))
				   (mod (1+ intersection) 3)))
			    (t (cons :car-north intersection))))))
	       ((:car-east)
		(if (gethash (cons (1+ i) j) new-cars)
		    (remhash (cons (1+ i) j) new-cars)
		    (setf (gethash (cons (1+ i) j) new-cars)
			  (case (aref map (1+ i) j)
			    ((:nwse) (cons :car-south intersection))
			    ((:nesw) (cons :car-north intersection))
			    ((:intersection)
			     (cons (case intersection
				     ((0) :car-north)
				     ((1) :car-east)
				     ((2) :car-south))
				   (mod (1+ intersection) 3)))
			    (t (cons :car-east intersection))))))
	       ((:car-south)
		(if (gethash (cons i (1+ j)) new-cars)
		    (remhash (cons i (1+ j)) new-cars)
		    (setf (gethash (cons i (1+ j)) new-cars)
			  (case (aref map i (1+ j))
			    ((:nwse) (cons :car-east intersection))
			    ((:nesw) (cons :car-west intersection))
			    ((:intersection)
			     (cons (case intersection
				     ((0) :car-east)
				     ((1) :car-south)
				     ((2) :car-west))
				   (mod (1+ intersection) 3)))
			    (t (cons :car-south intersection))))))
	       ((:car-west)
		(if (gethash (cons (1- i) j) new-cars)
		    (remhash (cons (1- i) j) new-cars)
		    (setf (gethash (cons (1- i) j) new-cars)
			  (case (aref map (1- i) j)
			    ((:nwse) (cons :car-north intersection))
			    ((:nesw) (cons :car-south intersection))
			    ((:intersection)
			     (cons (case intersection
				     ((0) :car-south)
				     ((1) :car-west)
				     ((2) :car-north))
				   (mod (1+ intersection) 3)))
			    (t (cons :car-west intersection)))))))))
     :finally (return new-cars)))

(defun step-map-times (n file)
  (multiple-value-bind (map cars)
      (read-map file)
    (loop
       :with start-cars = cars
       :repeat n
       :do (setf start-cars (step-map map start-cars))
       :finally (return start-cars))))

(defun repeat-until-last (file)
  (multiple-value-bind (map cars)
      (read-map file)
    (loop
       :with start-cars = cars
       :and ticks = 0
       :while (> (hash-table-count start-cars) 1)
       :do (progn (setf start-cars (step-map map start-cars))
		  (incf ticks))
       :finally (return (list (hash-table-alist start-cars)
			      ticks)))))
