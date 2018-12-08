(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :cl-ppcre)
  (use-package (list :cl-ppcre)))

(defun parse-line (string)
  (scan-to-strings "Step (.*) must be finished before step (.*) can begin." string))

(defun read-deps (file)
  (with-open-file (s file)
    (loop
       :for line = (read-line s nil nil)
       :while line
       :collect (nth-value 1 (parse-line line)))))

(defun pretty-deps (deps)
  (loop :for dep :in deps
     :collect (cons (aref dep 0) (aref dep 1))))

(defun all-steps (deps)
  (loop
     :with steps = nil
     :for dep :in deps
     :do (progn (pushnew (car dep) steps :test #'equal)
		(pushnew (cdr dep) steps :test #'equal))
     :finally (return steps)))

(defun get-free (deps)
  (let ((all (all-steps deps)))
    (sort (remove-if (lambda (step)
		       (find step deps :test (lambda (s d)
					       (equal s (cdr d)))))
		     all)
	  #'string-lessp)))

(defun part1 (file)
  (let* ((deps (pretty-deps (read-deps file)))
	 (all (all-steps deps)))
    (loop
       :with rest = deps
       :and done = nil
       :and step-no = nil
       :for step :from 0
       :while rest
       :do (let* ((all-possible (sort (get-free rest) #'string-lessp))
		  (possible (car all-possible)))
	     (setq rest (remove-if (lambda (dep)
				     (equal (car dep) possible))
				   rest))
	     (loop :for pos :in all-possible
		:do (pushnew (cons pos step) step-no
			     :test (lambda (o1 o2)
				     (equal (car o1) (car o2)))))
	     (pushnew possible done))
       :finally (progn
		  (princ step-no)
		  (return (append (reverse done)
				  (sort (set-difference all done :test #'equal)
					#'string-lessp)))))))

(defun get-costs (steps offset)
  (loop
     :with costs = (make-hash-table :test #'equal)
     :for s :in (sort steps #'string-lessp)
     :for i :from (+ offset 1) :to (+ offset 26)
     :do (setf (gethash s costs) i)
     :finally (return costs)))

(defclass sstate ()
  ((dep-graph :accessor dep-graph :initarg :deps)
   (workers :accessor workers)
   (nworkers :reader nworkers :initarg :nworkers)
   (done :accessor done :initform nil)
   (to-do :accessor to-do)
   (costs :accessor costs)
   (cost-offset :reader cost-offset :initarg :cost-offset)))

(defmethod initialize-instance :after ((state sstate) &rest initargs)
  (declare (ignore initargs))
  (setf (workers state) (make-array (list (nworkers state))
				    :initial-element nil))
  (setf (to-do state) (sort (all-steps (dep-graph state))
			    #'string-lessp))
  (setf (costs state) (get-costs (all-steps (dep-graph state))
				 (cost-offset state))))

(defmethod print-object ((state sstate) stream)
  (format stream "Done: ~A~%" (done state))
  (format stream "Todo: ~A~%" (to-do state))
  (princ "Workers:" stream)
  (loop :for i :below (array-total-size (workers state))
     :do (format stream "[~A] " (aref (workers state) i)))
  (princ #\Space stream))

(defun on-bench (sch)
  (let (steps)
    (dotimes (i (nworkers sch))
      (when (aref (workers sch) i)
	(pushnew (car (aref (workers sch) i)) steps :test #'equal)))
    steps))

(defun next-step (sch)
  (let ((free-steps (get-free (dep-graph sch))))
    (let ((available-steps (sort (set-difference (or free-steps (to-do sch)) (on-bench sch))
				 #'string-lessp)))
      (when (consp available-steps)
	(car available-steps)))))

(defun distribute (sch)
  (dotimes (i (nworkers sch))
    (unless (aref (workers sch) i)
      (when (next-step sch)
	(let ((step (next-step sch)))
	  (setf (aref (workers sch) i) (cons step (gethash step (costs sch)))))))))

(defun step-through (sch)
  (dotimes (i (nworkers sch))
    (cond ((null (aref (workers sch) i)) (distribute sch))
	  ((= (cdr (aref (workers sch) i)) 1)
	   (let ((done-step (car (aref (workers sch) i))))
	     (push done-step (done sch))
	     (setf (to-do sch)
		   (remove done-step (to-do sch) :test #'equal))
	     (setf (dep-graph sch)
		   (remove-if (lambda (s)
				(equal (car s) done-step))
			      (dep-graph sch)))
	     (setf (aref (workers sch) i) nil)
	     (distribute sch)))
	  (t (decf (cdr (aref (workers sch) i)))))))

;; If the work is completed in nth second, the time taken is (n+1) seconds.
(defun part2-nice (file nworkers offset)
  (let ((schedule-obj (make-instance 'sstate
				     :nworkers nworkers
				     :deps (pretty-deps (get-deps file))
				     :cost-offset offset)))
    (loop
       :with nsteps = 0
       :while (to-do schedule-obj)
       :do (progn
	     (step-through schedule-obj)
	     (incf nsteps)
	     ;;(format t "~A~%" (on-bench schedule-obj))
	     )
       :finally (return nsteps))))
