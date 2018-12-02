(eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload :alexandria))

(use-package '(:alexandria))

(defun string-char-diff (str1 str2)
  (loop
     :with diff = 0
     :and out = (make-string-output-stream)
     :and s1 = (make-string-input-stream str1)
     :and s2 = (make-string-input-stream str2)
     :for c1 = (read-char s1 nil) :and c2 = (read-char s2 nil)
     :while (and c1 c2) :do (if (eql c1 c2)
				(princ c1 out)
				(incf diff))
     :finally (return (values diff (get-output-stream-string out)))))

(defun key-diffs (key1 key2)
  (cond ((= (car key1) (car key2)) (= 1 (abs (- (cdr key1) (cdr key2)))))
	((= (cdr key1) (cdr key2)) (= 1 (abs (- (car key1) (car key2)))))))

(defun two-three (str)
  (let ((h (make-hash-table))
	(s (make-string-input-stream str))
	(twos 0)
	(threes 0))
    (loop :for c = (read-char s nil)
       :while c :do (setf (gethash c h)
			  (1+ (gethash c h 0))))
    (with-hash-table-iterator (fn h)
      (loop
	 (multiple-value-bind (more? key value) (fn)
	   (declare (ignore key))
	   (unless more? (return))
	   (cond ((= 2 value) (incf twos))
		 ((= 3 value) (incf threes))))))

    (values twos threes)))

(defun part1 (file)
  (with-open-file (s file)
    (loop :with two = 0 :and three = 0
       :for str = (read-line s nil)
       :when (null str) :return (* two three)
       :do (multiple-value-bind (twos threes)
	       (two-three str)
	     (format t "~S ~S~%" twos threes)
	     (incf two (if (> twos 0) 1 0))
	     (incf three (if (> threes 0) 1 0))))))

;; Brute search 
(defun part2-brute (file)
  (with-open-file (s file)
    (let ((strings (loop :for str = (read-line s nil)
		      :while str :collect str)))
      (loop :for str :in strings
	 :do (find str strings
		   :test
		   (lambda (s1 s2)
		     (multiple-value-bind (count str)
			 (string-char-diff s1 s2)
		       (when (= 1 count)
			 (return (values s1 s2 str))))))))))
