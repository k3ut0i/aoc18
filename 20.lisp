(eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload '(:alexandria :split-sequence))
  (use-package '(:alexandria :split-sequence)))

(defun read-tree (regex-stream &optional (end-char #\$))
  (loop
     :with current-tree = nil :and trees = nil
     :for c = (read-char regex-stream nil nil)
     :until (char= c end-char)
     :do (case c
	   ((#\E #\W #\N #\S) (push c current-tree))
	   ((#\|)
	    (push (nreverse current-tree) trees)
	    (setf current-tree nil))
	   ((#\() (push (read-tree regex-stream #\)) current-tree)))
     :finally (progn
		(push (nreverse current-tree) trees)
		(return (nreverse trees)))))

(defun print-tree (tree &optional (indent-level 0) (stream *standard-output*))
  (loop
     :initially (loop :repeat indent-level :do (princ #\Space stream))
     :for node :in tree
     :if (atom node)
     :do (princ node stream)
     :else
     :do (princ #\Newline stream) :and
     :do (print-tree node (1+ indent-level) stream)
     :end))

(defun room-points (direction-tree &optional (initial-point (cons 0 0)))
  (loop
     :with all-points = (list initial-point)
     :and current-point = initial-point
     :for node :in direction-tree
     :if (atom node)
     :do (case node
	   ((#\N) (incf (cdr current-point) 2))
	   ((#\E) (incf (car current-point) 2))
	   ((#\S) (decf (cdr current-point) 2))
	   ((#\W) (decf (car current-point) 2)))
     :and :do (push (copy-list current-point) all-points)
     :else
     :do (setf all-points (append (room-points node
					       (copy-list current-point))
				  all-points))
     :end
     :finally (return all-points)))

(defun get-points (direction-tree &optional (initial-point (cons 0 0)))
  (loop
     :with room-points = (list initial-point)
     :and door-points = nil
     :and current-point = initial-point
     :for node :in direction-tree
     :if (atom node)
     :do (case node
	   ((#\N)
	    (push (cons (car current-point)
			(1+ (cdr current-point)))
		  door-points)
	    (incf (cdr current-point) 2))
	   ((#\E)
	    (push (cons (1+ (car current-point))
			(cdr current-point))
		  door-points)
	    (incf (car current-point) 2))
	   ((#\S)
	    (push (cons (car current-point)
			(1- (cdr current-point)))
		  door-points)
	    (decf (cdr current-point) 2))
	   ((#\W)
	    (push (cons (1- (car current-point))
			(cdr current-point))
		  door-points)
	    (decf (car current-point) 2)))
     :and :do (push (copy-list current-point) room-points)
     :else
     :do (destructuring-bind (sub-room-points sub-door-points)
	     (get-points node (copy-list current-point))
	   (setf room-points (append room-points sub-room-points)
		 door-points (append door-points sub-door-points)))
     :finally (return (list room-points door-points))))

(defun get-boundaries (room-pts)
  (loop
     :with min-x = 0 :and min-y = 0 :and max-x = 0 :and max-y = 0
     :for p :in room-pts
     :when (< (car p) min-x) :do (setq min-x (car p))
     :when (< (cdr p) min-y) :do (setq min-y (cdr p))
     :when (> (car p) max-x) :do (setq max-x (car p))
     :when (> (cdr p) max-y) :do (setq max-y (cdr p))
     :finally (return
		(list (1- min-x) (1- min-y) (1+ max-x) (1+ max-y)))))

(defun create-maze (file)
  (let ((direction-tree (with-open-file (s file)
			  (read-tree s))))
    (destructuring-bind (room-pts door-pts)
	(get-points direction-tree)
      (destructuring-bind (xmin ymin xmax ymax)
	  (get-boundaries room-pts)
	(let ((maze (make-array (list (1+ (- xmax xmin))
				      (1+ (- ymax ymin)))
				:element-type 'character
				:initial-element #\#)))
	  (loop :for p :in room-pts
	     :do (setf (aref maze
			     (- (car p) xmin)
			     (- (cdr p) ymin))
		       #\.))
	  (loop :for p :in door-pts
	     :do (setf (aref maze
			     (- (car p) xmin)
			     (- (cdr p) ymin))
		       #\|))
	  (setf (aref maze (- xmin) (- ymin)) #\X)
	  (values maze (cons (- xmin) (- ymin))))))))

(defun print-maze (maze)
  (destructuring-bind (w h)
      (array-dimensions maze)
    (dotimes (j h)
      (dotimes (i w)
	(princ (aref maze i (- h j 1))))
      (princ #\Newline))))

(defun mark-maze (maze center)
  (destructuring-bind (w h)
      (array-dimensions maze)
    (let ((mark-array (make-array (array-dimensions maze)
				  :element-type 'fixnum
				  :initial-element (array-total-size maze))))
      (setf (aref mark-array (car center) (cdr center)) 0)
      (labels ((mark-start (i j)
		 (when (eq (aref maze i (1+ j)) #\|)
		   (when (> (aref mark-array i (+ j 2))
			    (+ (aref mark-array i j) 1))
		     (setf (aref mark-array i (+ j 2))
			   (+ (aref mark-array i j) 1))
		     (mark-start i (+ j 2))))
		 ;;FIXME: handle all direction not just north
		 ;; i (1- j)
		 ;; (1+ i) j
		 ;; (1- i) j
		 ))
	(mark-start (car center) (cdr center)))
      (loop :for i :below (array-total-size mark-array)
	 :minimizing (row-major-aref mark-array i)))))
