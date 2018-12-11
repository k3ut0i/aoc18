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

(defun make-grid (input)
  (let ((grid (make-array (list 300
				300)
			  :initial-element 0)))
    (dotimes (x 300 grid)
      (dotimes (y 300)
	(setf (aref grid x y)
	      (calculate-power-level x y input))))))

(defun total-power (x y size grid)
  (let ((acc 0))
    (dotimes (i size acc)
      (dotimes (j size)
	(incf acc (aref grid
			(+ x i)
			(+ y j)))))))

(defun part1 (input)
  (let* ((grid (make-grid input))
	 (max (total-power 0 0 3 grid))
	 (max-tl (aref grid 0 0)))
    (dotimes (i 298 max-tl)
      (dotimes (j 298)
	(let ((current (total-power i j 3 grid)))
	  (when (> current max)
	    (setq max current
		  max-tl (cons (1+ i)
			       (1+ j)))))))))

(defun max-power (size input)
  (let ((grid (make-grid input))
	(max 0)
	(max-tl (cons 0 0)))
    (dotimes (i (- 300 (1- size)))
      (dotimes (j (- 300 (1- size)))
	(let ((current-power (total-power i j size grid)))
	  (when (> current-power max)
	    (setq max current-power
		  max-tl (list (1+ i) (1+ j)))))))
    (values max max-tl)))

(defun part2 (input)
  (let ((max-tl nil)
	(max 0)
	(max-size 0))
    (loop
       :for size :from 1 :to 300
       :do (multiple-value-bind (power tl)
	       (max-power size input)
	     (when (> power max)
	       (setq max power
		     max-tl tl
		     max-size size)))
       :finally (return (list max-tl size )))))
