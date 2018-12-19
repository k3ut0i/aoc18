(eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload '(:split-sequence :alexandria))
  (use-package '(:split-sequence :alexandria)))

;;; Part of 16th problem
(defvar *sys* (make-array (list 6)
			  :element-type 'fixnum
			  :initial-element 0))

(defun set-all-reg (lst)
  (loop
     :for e :in lst
     :for i :below 6
     :do (setf (aref *sys* i) e)))
(defun is-sys (lst)
  (loop :for e :in lst
     :for i :below 6
     :always (= e (aref *sys* i))))

(defun get-reg (a)
  (aref *sys* a))
(defun set-reg (a val)
  (setf (aref *sys* a) val))

(defun op-addi (a b c)
  (set-reg c (+ (get-reg a) b)))
(defun op-addr (a b c)
  (set-reg c (+ (get-reg a) (get-reg b))))

(defun op-muli (a b c)
  (set-reg c (* (get-reg a) b)))
(defun op-mulr (a b c)
  (set-reg c (* (get-reg a) (get-reg b))))

(defun op-bani (a b c)
  (set-reg c (logand (get-reg a) b)))
(defun op-banr (a b c)
  (set-reg c (logand (get-reg a) (get-reg b))))

(defun op-bori (a b c)
  (set-reg c (logior (get-reg a) b)))
(defun op-borr (a b c)
  (set-reg c (logior (get-reg a) (get-reg b))))

(defun op-setr (a b c)
  (declare (ignore b))
  (set-reg c (get-reg a)))
(defun op-seti (a b c)
  (declare (ignore b))
  (set-reg c a))

(defun op-gtir (a b c)
  (set-reg c (if (> a (get-reg b)) 1 0)))
(defun op-gtri (a b c)
  (set-reg c (if (> (get-reg a) b) 1 0)))
(defun op-gtrr (a b c)
  (set-reg c (if (> (get-reg a) (get-reg b)) 1 0)))

(defun op-eqir (a b c)
  (set-reg c (if (= a (get-reg b)) 1 0)))
(defun op-eqri (a b c)
  (set-reg c (if (= (get-reg a) b) 1 0)))
(defun op-eqrr (a b c)
  (set-reg c (if (= (get-reg a) (get-reg b)) 1 0)))
;;; End of code written for 16th problem

(defun parse-line (line)
  (let ((atoms (split-sequence #\Space line)))
    (cons (get-op-fun (car atoms)) (mapcar #'read-from-string (cdr atoms)))))

(defun read-program (file)
  (with-open-file (s file)
    (cons (read-line s nil nil)
	  (loop :for line = (read-line s nil nil)
	     :while line :collect (parse-line line)))))

(defun get-op-fun (instr-name)
  (ensure-symbol (concatenate 'string "OP-" (string-upcase instr-name))))

(defun get-ip-reg (line)
  (let ((instr (split-sequence #\Space line)))
    (assert (string-equal (car instr) "#ip"))
    (read-from-string (cadr instr))))

(defun run-prog (instr-lst &optional
			     (part 0)
			     (dump-p nil)
			     (dump-n-entries 1000)
			     (dump-file "day19_dump.txt"))
  (with-open-file (s dump-file
		     :direction :output
		     :if-exists :supersede)
    (if (zerop part)
	(set-all-reg '(0 0 0 0 0 0))
	(set-all-reg '(1 0 0 0 0 0)))
    (loop
       :with ip-reg = (get-ip-reg (car instr-lst))
       :and prog-file = (cdr instr-lst)
       :and num-instr = 0
       :for ip = (get-reg ip-reg)
       :while (< ip (length prog-file))
       :do (incf num-instr)
       :do (when dump-p
	     (format s "~{~A~^ ~}~%" (loop :for i :below (array-total-size *sys*)
				 :collect (row-major-aref *sys* i)))
	     (when (> num-instr dump-n-entries)
	       (return :dump-completed)))
       :do (let ((current-instr (nth ip prog-file)))
	     (apply (car current-instr) (cdr current-instr)))
       :do (set-reg ip-reg (1+ (get-reg ip-reg)))
       :finally (return *sys*))))

