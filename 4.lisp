(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :cl-ppcre)
  (use-package '(:cl-ppcre)))

(defstruct event
  time guard-id type)

(defun get-max (vec)
  (let ((max-idx 0)
	(max-value (aref vec 0)))
    (loop :for idx :below (array-dimension vec 0)
       :when (> (aref vec idx) max-value)
       :do (setq max-idx idx
		 max-value (aref vec max-idx)))
    max-idx))

(defun get-time (string)
  (register-groups-bind (year month day hour min)
      ("(.*)-(.*)-(.*) (.*):(.*)" string)
    (mapcar #'read-from-string (list year month day hour min))))

(defun time-diff (time1 time2)
  (destructuring-bind (year1 month1 day1 hour1 min1)
      (get-time time1)
    (destructuring-bind (year2 month2 day2 hour2 min2)
	(get-time time2)
      (assert (and (= year1 year2)
		   (= month1 month2)
		   (= day1 day2)
		   (= hour1 hour2)))
      (values min1 min2))))

(defun get-shift-start (string)
  (scan-to-strings "\\\[(.*)\\\] Guard #(\\d+)" string))

(defun get-wakes (string)
  (scan-to-strings "\\\[(.*)\\\] wakes up" string))

(defun get-sleeps (string)
  (scan-to-strings "\\\[(.*)\\\] falls asleep" string))

(defun scan-event (string current-id)
  (let ((start (get-shift-start string))
	(wakes (get-wakes string))
	(sleep (get-sleeps string)))
    (cond (start (let ((match (nth-value 1 (get-shift-start string))))
		   (make-event :guard-id (read-from-string (svref match 1))
			       :time (svref match 0)
			       :type :start)))
	  (wakes (let ((match (nth-value 1 (get-wakes string))))
		   (make-event :time (svref match 0)
			       :guard-id current-id
			       :type :wakes)))
	  (sleep (let ((match (nth-value 1 (get-sleeps string))))
		   (make-event :time (svref match 0)
			       :guard-id current-id
			       :type :sleeps)))
	  (t (error 'scan-error)))))

(defun get-all-entries (file)
  (with-open-file (s file)
    (let ((entries
	   (sort (loop :for e = (read-line s nil nil)
		    :while e :collect e)
		 #'string-lessp)))
      (loop
	 :with current-id = -1
	 :for event-string :in entries
	 :for e = (scan-event event-string current-id)
	 :when (eql (event-type e)
		    :start)
	 :do (setq current-id (event-guard-id e))
	 :collect e))))

(defun get-event-hashes (file)
  (let ((sleeps (make-hash-table)))
    (loop
       :with id = -1
       :with sleep-time = ""
       :for e :in (get-all-entries file)
       :when (eql (event-type e) :start) :do (setq id (event-guard-id e))
       :when (eql (event-type e) :sleeps) :do (setq sleep-time (event-time e))
       :when (eql (event-type e) :wakes)
       :do (setf (gethash id sleeps)
		 (let ((sleep-vector
			(gethash id sleeps (make-array '(60) :initial-element 0))))
		   (multiple-value-bind (w s)
		       (time-diff (event-time e)
				  sleep-time)
		     (loop :for minute :from s :below w
			  :do (incf (svref sleep-vector minute))))
		   sleep-vector)))
    sleeps))

(defun part1 (file)
  (let ((sleeps (get-event-hashes file)))
    (loop
       :with max_id = -1 :and max_value = 0
       :for id :being :each :hash-key :in sleeps
       :do (let ((minutes (reduce #'+ (gethash id sleeps))))
	     (when (> minutes max_value)
	       (setq max_id id
		     max_value minutes)))
       :finally (return (* max_id (get-max (gethash max_id sleeps)))))))

(defun part2 (file)
  (let ((sleeps (get-event-hashes file)))
    (loop :with max_id = -1 :and max_minute = 0 :and max_freq = 0
       :for id :being :each :hash-key :in sleeps
       :do (let* ((sleep-vector (gethash id sleeps))
		  (minute (get-max sleep-vector))
		  (freq (aref sleep-vector minute)))
	     (when (> freq max_freq)
	       (setq max_minute minute
		     max_freq freq
		     max_id id)))
       :finally (return (* max_id max_minute)))))

