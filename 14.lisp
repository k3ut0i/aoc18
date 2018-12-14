(defparameter *mlen* 10000000)

(defstruct scoreboard
  scores
  first
  second
  len)

(defmethod print-object ((obj scoreboard) stream)
  (funcall #'print-scoreboard obj stream))

(defun new-scoreboard ()
  (let ((sb (make-scoreboard :scores (make-array (list *mlen*))
			     :first 0
			     :second 1
			     :len 2)))
    (setf (aref (scoreboard-scores sb) 0) 3)
    (setf (aref (scoreboard-scores sb) 1) 7)
    sb))

(defun print-scoreboard (sb stream)
  (let ((len (scoreboard-len sb))
	(scores (scoreboard-scores sb))
	(first-idx (scoreboard-first sb))
	(second-idx (scoreboard-second sb)))
    (loop :for i :below len
       :do (format stream (cond ((= first-idx i) "(~A)")
				((= second-idx i) "[~A]")
				(t " ~A "))
		   (aref scores i)))))

(defun get-score-seq (sb)
  (loop
     :with s = (make-string-output-stream)
     :for i :below (scoreboard-len sb)
     :do (princ (aref (scoreboard-scores sb) i) s)
     :finally (return (get-output-stream-string s))))

(defun add-new-scores (sb)  
  (let* ((scores (scoreboard-scores sb))
	 (first (scoreboard-first sb))
	 (second (scoreboard-second sb))
	 (len (scoreboard-len sb))
	 (sum (+ (aref scores first)
		 (aref scores second)))
	 (digits (cons (floor sum 10)
		       (mod sum 10))))
    (if (= 0 (car digits))
	(progn (setf (aref scores len) (cdr digits))
	       (incf (scoreboard-len sb)))
	(progn (setf (aref scores len) (car digits))
	       (setf (aref scores (1+ len)) (cdr digits))
	       (incf (scoreboard-len sb) 2)))
    sb))

(defun step-forward (sb)
  (let* ((scores (scoreboard-scores sb))
	 (first (scoreboard-first sb))
	 (second (scoreboard-second sb))
	 (len (scoreboard-len sb)))
    (setf (scoreboard-first sb)
	  (mod (+ first (aref scores first) 1) len))
    (setf (scoreboard-second sb)
	  (mod (+ second (aref scores second) 1) len))
    sb))

(defun next-step (sb)
  (add-new-scores sb)
  (step-forward sb))

(defun nsteps (n)
  (loop
     :with sb = (new-scoreboard)
     :repeat n
     :do (next-step sb)
     :finally (return sb)))

(defun scores-ten-after (n)
  (loop
     :with sb = (new-scoreboard)
     :while (< (scoreboard-len sb) (+ n 10))
     :do (next-step sb)
     :finally (return sb)))

(defun part1 (n)
  (loop
     :with scores = (scoreboard-scores (scores-ten-after n))
     :for i :from n :below (+ n 10)
     :do (princ (aref scores i))))

(defun part2 (seq max)
  (search seq (get-score-seq (nsteps max))))
