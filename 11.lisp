(eval-when (:compile-toplevel)
  (ql:quickload :cl-ppcre)
  (use-package :cl-ppcre))

(defun calculate-power-level (x y input)
  (let* ((rack-id (+ 1 x 10))
	 (power-begin (* rack-id (1+ y)))
	 (power-level (* rack-id (+ power-begin
				    input)))
	 (hundredth (rem power-level 1000)))
    ;;(format t "~A ~A ~A ~A~%" rack-id power-begin power-level hundredth)
    (- (floor (/ hundredth 100)) 5)))

(defun make-grid (grid-size input)
  (let ((grid (make-array (list grid-size
				grid-size)
			  :initial-element 0)))
    (dotimes (x grid-size grid)
      (dotimes (y grid-size)
	(setf (aref grid x y)
	      (calculate-power-level x y input))))))

(defun accumulate-grid (grid)
  (destructuring-bind (xdim ydim)
      (array-dimensions grid)
    (dotimes (x xdim grid)
      (dotimes (y ydim)
	(incf (aref grid x y)
	      (if (= x 0) 0 (aref grid (1- x) y)))))))

(defun total-power (x y size grid)
  (let ((acc 0))
    (dotimes (j size acc)
      (incf acc (- (aref grid (+ (1- x)  size) (+ y j))
		   (if (= x 0) 0 (aref grid (1- x) (+ y j))))))))

(defun max-power (square-size grid-size input)
  (let ((grid (accumulate-grid (make-grid grid-size input)))
	(max 0)
	(max-tl (cons 0 0)))
    (dotimes (i (- grid-size (1- square-size)))
      (dotimes (j (- grid-size (1- square-size)))
	(let ((current-power (total-power i j square-size grid)))
	  (when (> current-power max)
	    (setq max current-power
		  max-tl (list (1+ i) (1+ j)))))))
    (values max max-tl)))

(defun print-grid (grid)
  (destructuring-bind (xdim ydim)
      (array-dimensions grid)
    (dotimes (j ydim)
      (dotimes (i xdim)
	(format t "~2<~A~> " (aref grid i j)))
      (princ #\Newline))))

(defun part1 (input)
  (max-power 3 300 input))

(defun part2 (input)
  (loop
     :with mpower = 0
     :with mpoint = nil
     :with msize = 0
     :for s :from 1 :to 300
     :do (multiple-value-bind (power point)
	     (max-power s 300 input)
	   (when (> power mpower)
	     (setq mpower power
		   mpoint point
		   msize s)))
     :finally (return (values mpower mpoint msize))))
