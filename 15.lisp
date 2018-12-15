(eval-when (:load-toplevel :compile-toplevel)
  (ql:quickload :cl-ppcre)
  (use-package (list :cl-ppcre)))

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
  ((game-map :type 'array :accessor game-map :initarg :map)
   (units :type 'hash-table :accessor game-units :initarg :units)))

(defmethod print-object ((g game) stream)
  (print-map (game-map g) stream)
  (loop :for pos :being :each :hash-key :in (game-units g)
     :do (format stream "~A ~A~%" pos (gethash pos (game-units g)))))

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
	   (if (eq (car p1) (car p2))
	       (betterp (cdr p1) (cdr p2))
	       (< (cdr (assoc (car p1) step-preference))
		  (cdr (assoc (car p2) step-preference))))))))

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
     :with n = (+ (abs (- (car from) (car to)))
		     (abs (- (cdr from) (cdr to))))
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
  (loop :for n :in (get-neighbors point)
     :when (eq (aref (game-map game) (car (car n)) (cdr (car n))) (enemy type))
     :do (return n)))


(defun move (from game)
  "Move an unit at FROM in GAME"
  (let* ((type (aref (game-map game) (car from) (cdr from)))
	 (enemy-at (enemy-in-range from game type)))
    (if enemy-at
	(list :attack enemy-at)
	(car (best-path (mapcar (lambda (to)
				  (get-optimal-path from (car to) game))
				(all-in-range from game (enemy type))))))))
