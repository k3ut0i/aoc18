(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :alexandria)
  (ql:quickload :split-sequence)
  (use-package '(:alexandria :split-sequence)))

(defun read-points (line)
  (apply #'cons (mapcar #'read-from-string (split-sequence #\, line))))

(defun get-points (file)
  (with-open-file (s file)
    (loop :for line = (read-line s nil nil)
       :while (and line (not (string-equal "" line)))
       :collect (read-points line))))

;; Get top-left and bottom-right corners
(defun get-geography (points)
  (reduce (lambda (c p)
	    (destructuring-bind (left top right bottom)
		c
	      (when (< (car p) left) (setq left (car p)))
	      (when (< (cdr p) top) (setq top (cdr p)))
	      (when (> (car p) right) (setq right (car p)))
	      (when (> (cdr p) bottom) (setq bottom (cdr p)))
	      (list left top right bottom)))
	  points
	  :initial-value (list (car (car points)) ;left
			       (cdr (car points)) ;top
			       (car (car points));right
			       (cdr (car points)))));bottom

(defun distance (p1 p2)
  (+ (abs (- (car p1) (car p2)))
     (abs (- (cdr p1) (cdr p2)))))

(defun find-closest (p points)
  (loop
     :with idx = 0 :and min = most-positive-fixnum
     :and min_idx = 1 :and contested = nil
     :for point :in points
     :for dist = (distance p point)
     :do (incf idx)
     :when (= dist min)
     :do (setq contested t)
     :when (< dist min)
     :do (setq min dist
	       min_idx idx
	       contested nil)
     :finally (return (if contested 0 min_idx))))

(defun set-grid-with (points f)
  (destructuring-bind (left top right bottom)
      (get-geography points)
    (let* ((width (+ (- right left) 1))
	   (height (+ (- bottom top) 1))
	   (grid (make-array (list width height) :initial-element 0)))
      (loop
	 :for p :in points
	 :for i :from 1 :to (length points)
	 :do (setf (aref grid (- (car p) left) (- (cdr p) top)) i))
      (dotimes (w width)
	(dotimes (h height)
	  (setf (aref grid w h)
		(funcall f (cons (+ w left) (+ h top)) points))))
      grid)))

;; Part1
(defun find-max-area (grid)
  (let ((h (make-hash-table)))
    (loop :for i :below (array-total-size grid)
       :do (setf (gethash (row-major-aref grid i) h)
		 (1+ (gethash (row-major-aref grid i) h 0))))
    (destructuring-bind (width height)
	(array-dimensions grid)
      ;; Set bordering areas to infinite
      (dotimes (i width)
	(setf (gethash (aref grid i (1- height)) h) 0
	      (gethash (aref grid i 0) h) 0))
      (dotimes (j height)
	(setf (gethash (aref grid (1- width) j) h) 0
	      (gethash (aref grid 0 j) h) 0)))
    (loop :for v :being :each :hash-value :in h
	 :maximizing v)))

(defun part1 (file)
  (find-max-area (set-grid-with (get-points file)
				#'find-closest)))
;;Part2
(defparameter +min-dist+ 10000)

(defun in-region-p (p points)
  (loop
     :for point :in points
     :summing (distance point p) :into total_distance
     :finally (return (< total_distance +min-dist+))))

(defun find-region-area (grid)
  (loop :for i :below (array-total-size grid)
     :count (row-major-aref grid i)))

(defun part2 (file)
  (find-region-area (set-grid-with (get-points file)
				   #'in-region-p)))
