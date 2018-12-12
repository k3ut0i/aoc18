(eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload :cl-ppcre)
  (use-package (list :cl-ppcre)))

(defun get-input (file)
  (with-open-file (s file)
    (let ((init-string (read-line s))
	  (rules (cdr (loop
			 :for line = (read-line s nil nil)
			 :while line
			 :collect line))))
      (register-groups-bind (state-string)
	  ("initial state: (.*)" init-string)
	(make-plant-state
	 :state (make-state state-string)
	 :rules (loop :for rule-string :in rules
		   :collect (make-rule rule-string)))))))

(defun make-state (state-string)
  (let* ((string-length (length state-string))
	 (s (make-string-input-stream state-string))
	 (state (make-array (list string-length))))
    (loop
       :for i :below string-length
       :do (setf (aref state i)
		 (if (char-equal (read-char s) #\#) 1 0)))
    state))

(defun make-rule (rule-string)
  (register-groups-bind (condition result)
      ("(.....) => (.)" rule-string)
    (let ((rule (make-array '(5)))
	  (s (make-string-input-stream condition)))
      (loop :for i :below 5
	 :do (setf (aref rule i)
		   (if (char-equal (read-char s) #\#) 1 0)))
      (cons rule (if (string-equal result "#") 1 0)))))

(defstruct plant-state
  state
  rules
  (left 0)
  (right 0))

(defun get-neighbors (n state)
  "Get neighbors of N th plant from STATE."
  (let ((max (array-dimension state 0)))
    (loop :for i :from (- n 2) :to (+ n 2)
       :collect (cond ((< i 0) 0)
		      ((>= i max) 0)
		      (t (aref state i))))))

(defun match-rule-for-nth (n rule state)
  "Match N th plant in STATE with RULE."
  (loop
     :with neighbors = (get-neighbors n state)
     :for i :below 5
     :always (= (nth i neighbors)
		(aref (car rule) i))))

(defun get-match-for-nth (n plants)
  (loop :for rule :in (plant-state-rules plants)
     :when (match-rule-for-nth n rule (plant-state-state plants))
     :do (return rule)))

(defun step-state (plants)
  (let* ((num-plants (array-dimension (plant-state-state plants) 0))
	 (2nd-left (get-match-for-nth -2 plants))
	 (1st-left (get-match-for-nth -1 plants))
	 (1st-right (get-match-for-nth num-plants plants))
	 (2nd-right (get-match-for-nth (1+ num-plants) plants))
	 (left-padding  (cond ((and 2nd-left (= (cdr 2nd-left) 1))
			       2)
			      ((and 1st-left (= (cdr 1st-left) 1))
			       1)
			      (t 0)))
	 (right-padding (cond ((and 2nd-right (= (cdr 2nd-right) 1))
			       2)
			      ((and 1st-right (= (cdr 1st-right) 1))
			       1)
			      (t 0)))
	 (new-state (make-array (list (+ num-plants left-padding right-padding)))))
    (incf (plant-state-left plants) left-padding)
    (incf (plant-state-right plants) right-padding)
    (case left-padding
      (2 (setf (aref new-state 0) (cdr 2nd-left))
	 (setf (aref new-state 1) (cdr 1st-left)))
      (1 (setf (aref new-state 0) (cdr 1st-left))))
    (case right-padding
      (2 (setf (aref new-state (+ num-plants left-padding)) (cdr 1st-right))
	 (setf (aref new-state (+ num-plants left-padding 1 )) (cdr 2nd-right)))
      (1 (setf (aref new-state (+ num-plants left-padding)) (cdr 1st-right))))
    (loop :for i :below num-plants
       :do (let ((rule (get-match-for-nth i plants)))
	     (when rule
	       (setf (aref new-state (+ i left-padding)) (cdr rule)))))
    (setf (plant-state-state plants) new-state)
    plants))

(defun print-plants (plants)
  (let ((state (plant-state-state plants)))
    (loop :for i :below (array-dimension state 0)
       :do (princ (if (zerop (aref state i)) #\. #\#))
       :finally (princ #\Newline))))

(defun print-generations (n initial)
  (loop
     :for i :from 1 :to n
     :initially (progn (princ "00") (print-plants initial))
     :do (progn (step-state initial)
		(format t "~A " i)
		(print-plants initial))))

(defun part1 (n file)
  (let ((plants (get-input file)))
    (loop :repeat n :do (step-state plants))
    (loop :for i :below (array-dimension (plant-state-state plants) 0)
       :unless (zerop (aref (plant-state-state plants) i))
       :sum (- i (plant-state-left plants)))))
