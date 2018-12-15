(eval-when (:load-toplevel :compile-toplevel)
  (ql:quickload :alexandria)
  (use-package (list :alexandria)))

(defparameter *unit-glyphs*
  '((#\# . :wall)
    (#\. . :empty)
    (#\G . :goblin)
    (#\E . :elf)))

(defun unit2char (u)
  (car (find u *unit-glyphs*
	     :key #'cdr)))

(defun char2unit (c)
  (cdr (find c *unit-glyphs*
	     :key #'car
	     :test #'char=)))

(defun parse-map-line (line)
  (map 'list #'char2unit line))


(defun read-map (file)
  (with-open-file (s file)
    (let ((map-lines (loop
			:for line = (read-line s nil nil)
			:while line
			:collect (parse-map-line line))))
      (create-game map-lines))))

(defun create-game (lines)
  (let* ((width (length (car lines)))
	 (height (length lines))
	 (game-map (make-array (list width height)
			       :element-type 'keyword
			       :initial-element :uninit))
	 (units (make-hash-table :test #'equal)))
    (loop
       :for line :in lines
       :for j :below height
       :do (loop
	      :for u :in line
	      :for i :below width
	      :do (setf (aref game-map i j) u)
	      :when (or (eq u :goblin) (eq u :elf))
	      :do (setf (gethash (cons i j) units) (make-unit u))))
    (make-instance 'game :map game-map :units units)))

(defun print-map (game-map stream)
  (destructuring-bind (w h)
      (array-dimensions game-map)
    (dotimes (j h)
      (dotimes (i w)
	(princ (unit2char (aref game-map i j)) stream))
      (princ #\Newline stream))))

(defclass game ()
  ((g-map :accessor game-map :initarg :map)
   (units :type 'hash-table :accessor game-units :initarg :units)
   (num-elves :initform 0 :type 'fixnum :accessor game-num-elves)
   (num-goblins :initform 0 :type 'fixnum :accessor game-num-goblins)))

(defmethod initialize-instance :after ((g game) &rest initargs)
  (declare (ignore initargs))
  (loop :for i :below (array-total-size (game-map g))
     :do (case (row-major-aref (game-map g) i)
	   ((:elf) (incf (game-num-elves g)))
	   ((:goblin) (incf (game-num-goblins g))))))

(defmethod print-object ((g game) stream)
  (print-map (game-map g) stream)
  (loop :for pos :being :each :hash-key :in (game-units g)
     :do (format stream "~A ~A~%" pos (gethash pos (game-units g))))
  (format stream "Elves: ~A Goblins: ~A~%" (game-num-elves g) (game-num-goblins g)))

(defclass unit ()
  ((hp :type 'fixnum :accessor unit-hp :initarg :hp :initform 200)))

(defmethod print-object ((u unit) stream)
  (princ (unit-hp u) stream))

(defclass elf (unit)
  ())

(defmethod print-object ((e elf) stream)
  (princ "E[" stream)
  (call-next-method)
  (princ "]" stream))

(defclass goblin (unit)
  ())
(defmethod print-object ((g goblin) stream)
  (princ "G[" stream)
  (call-next-method)
  (princ "]" stream))

(defun make-unit (unit-keyword)
  (case unit-keyword
    ((:goblin) (make-instance 'goblin))
    ((:elf) (make-instance 'elf))
    (t (error 'game-unit-unknown))))

(defun get-neighbors (point)
  (list (cons (cons (car point) (1- (cdr point))) :u)
	(cons (cons (1- (car point)) (cdr point)) :l)
	(cons (cons (1+ (car point)) (cdr point)) :r)
	(cons (cons (car point) (1+ (cdr point))) :d)))

(defun empty-p (point game)
  (eq (aref (game-map game) (car point) (cdr point)) :empty))

(defun get-empty-neighbors (point game)
  (remove-if-not (lambda (p)
		   (and (array-in-bounds-p (game-map game) (car p) (cdr p))
			(empty-p p game)))
		 (get-neighbors point)
		 :key #'car))

(defun betterp (p1 p2)
  (cond ((< (length p1) (length p2)) p1)
	((> (length p1) (length p2)) p2)
	(t 
	 (let ((step-preference '((:u . 0) (:l . 1) (:r . 2) (:d . 3))))
	   (if (and (null p1) (null p2))
	       nil
	       (if (eq (car p1) (car p2))
		   (betterp (cdr p1) (cdr p2))
		   (< (cdr (assoc (car p1) step-preference))
		      (cdr (assoc (car p2) step-preference)))))))))

(defun best-path (paths)
  (reduce (lambda (p1 p2)
	    (if (betterp p1 p2)
		p1
		p2))
	  (cdr paths)
	  :initial-value (car paths)))

(defun get-optimal-path-in (from to game max-steps)
  (when (> max-steps 0)
    (let* ((valid-neighbors (get-empty-neighbors from game))
	   (is-neighbor (find to valid-neighbors :key #'car :test #'equal)))
      (if is-neighbor
	  (list (cdr is-neighbor))
	  (best-path (remove-if #'null (mapcar (lambda (p)
						 (let ((optimal (get-optimal-path-in
								 (car p)
								 to
								 game
								 (1- max-steps))))
						   (when optimal
						     (cons (cdr p) optimal))))
					       valid-neighbors)))))))
(defun get-optimal-path (from to game)
  (loop
     :for n :from (+ (abs (- (car from) (car to)))
		     (abs (- (cdr from) (cdr to))))
     :to (* 2 (array-dimension (game-map game) 0))
     :thereis (get-optimal-path-in from to game n)
     :do (incf n)))

(defun all-in-range (from game type)
  (loop :for k :being :each :hash-key :in (game-units game)
     :when (and (eq type (aref (game-map game) (car k) (cdr k)))
		(not (equal from k)))
     :append (get-empty-neighbors k game)))

(defun enemy (type)
  (if (eq type :elf) :goblin :elf))

(defun enemy-in-range (point game  type)
  (loop
     :with min-hp = 200
     :with min-pos = nil
     :for n :in (get-neighbors point)
     :when (eq (aref (game-map game) (car (car n)) (cdr (car n))) (enemy type))
     :do (progn
	   (if (null min-pos) (setf min-pos n))
	   (when (< (unit-hp (gethash (car n) (game-units game))) min-hp)
	     (setf min-hp (unit-hp (gethash (car n) (game-units game))))
	     (setf min-pos n)))
     :finally (return min-pos)))

(defun new-pos (pos direction)
  (case direction
    ((:u) (cons (car pos) (1- (cdr pos))))
    ((:l) (cons (1- (car pos)) (cdr pos)))
    ((:r) (cons (1+ (car pos)) (cdr pos)))
    ((:d) (cons (car pos) (1+ (cdr pos))))
    (t (error 'cannot-match-direction pos direction))))

(defun move (from game)
  "Move an unit at FROM in GAME"
  (let* ((type (aref (game-map game) (car from) (cdr from)))
	 (enemy-at (enemy-in-range from game type)))
    (assert (or (eq type :elf) (eq type :goblin)))
    (if enemy-at
	(attack (car enemy-at) game)
	(let* ((direction
		(car (best-path
		      (remove-if #'null
				 (mapcar (lambda (to)
					   (get-optimal-path from (car to) game))
					 (all-in-range from game (enemy type)))))))
	       (unit (gethash from (game-units game))))
	  (when direction
	    (let ((newp (new-pos from direction)))
	      (setf (aref (game-map game) (car from) (cdr from)) :empty)
	      (setf (aref (game-map game) (car newp) (cdr newp)) type)
	      (setf (gethash newp (game-units game)) unit)
	      (remhash from (game-units game))
	      (if (enemy-in-range newp game type)
		  (move newp game))))))))

(defun attack (at game)
  (decf (unit-hp (gethash at (game-units game))) 3)
  (when (< (unit-hp (gethash at (game-units game))) 0)
    (remhash at (game-units game))
    (case (aref (game-map game) (car at) (cdr at))
      ((:elf) (decf (game-num-elves game)))
      ((:goblin) (decf (game-num-goblins game))))
    (setf (aref (game-map game) (car at) (cdr at)) :empty)))

(defun step-round (game)
  (loop :for pos :in (sort (hash-table-keys (game-units game))
			   (lambda (p1 p2)
			     (or (< (car p1) (car p2))
				 (and (= (car p1) (car p2))
				      (< (cdr p1) (cdr p2))))))
     :do (move pos game))
  game)

(defun one-survives (game)
  (loop
     :with num-rounds = 0
     :until (or (zerop (game-num-elves game))
		   (zerop (game-num-goblins game)))
     :do (step-round game)
     :do (incf num-rounds)
     :finally (return (values game num-rounds))))


(defun part1 (file)
  (one-survives (read-map file)))
