(eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload :alexandria)
  (ql:quickload :cl-ppcre)
  (ql:quickload :split-sequence))

(use-package '(:alexandria :cl-ppcre :split-sequence))
(defvar *sys* (make-array (list 4)
			  :element-type 'fixnum
			  :initial-element 0))

(defun set-all-reg (lst)
  (loop
     :for e :in lst
     :for i :below 4
     :do (setf (aref *sys* i) e)))
(defun is-sys (lst)
  (loop :for e :in lst
     :for i :below 4
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

(defvar *opcodes*
  (loop
     :for s :being :each :symbol :in (find-package 'cl-user)
     :when (and (> (length (symbol-name s)) 3)
		(string= "OP-" (symbol-name s) :end2 3))
     :collect s))

(defun search-opcode (before opcode after)
  (loop
     :for o :in *opcodes*
     :do (set-all-reg before)
     :do (apply o (cdr opcode))
     :when (is-sys after)
     :collect o))

(defun read-register (prefix str)
  (register-groups-bind (r0 r1 r2 r3)
      ((concatenate 'string prefix "\\[(\\d+), (\\d+), (\\d+), (\\d+)\\]") str)
    (mapcar #'read-from-string (list r0 r1 r2 r3))))

(defun read-entry (s1 s2 s3)
  (list (read-register "Before: " s1)
	(mapcar #'read-from-string (split-sequence #\Space s2))
	(read-register "After:  " s3)))

(defun read-all-entries (file)
  (with-open-file (s file)
    (loop
       :for line = (read-line s nil nil)
       :while (and (> (length line) 7) (string= "Before:" line :end2 7))
       :collect (prog1 (read-entry line (read-line s nil nil) (read-line s nil nil))
		  (read-line s nil nil)))))

(defun part1 (file)
  (let ((entries (read-all-entries file)))
    (loop :for e :in entries
       :count (>= (length (apply #'search-opcode e)) 3))))

(defun solved-hash-unique-p (h)
  (loop :for v :being :each :hash-value :in h
     :always (= 1 (length v))))

(defun solve-hash-unique (h)
  (loop :for k :being :each :hash-key :in h
     :when (= (length (gethash k h)) 1)
     :do (loop
	    :for ok :being :each :hash-key :in h
	    :unless (= k ok)
	    :do (setf (gethash ok h)
		      (remove (car (gethash k h)) (gethash ok h)))))
  (if (solved-hash-unique-p h)
      (hash-table-alist h)
      (solve-hash-unique h)))

(defun analyze-opcodes (entries)
  (let ((h (make-hash-table)))
    (loop :for e :in entries
       :do (let ((opcode (car (second e))))
	     (if (null (gethash opcode h nil))
		 (setf (gethash opcode h)
		       (apply #'search-opcode e))
		 (setf (gethash opcode h)
		       (intersection (gethash opcode h) (apply #'search-opcode e))))))
    (solve-hash-unique h)
    h))

(defun read-program (file)
  (with-open-file (s file)
    (loop :for line = (read-line s nil nil)
       :while line
       :collect (mapcar #'read-from-string (split-sequence #\Space line)))))

(defun execute-program (program opcode-h)
  (set-all-reg '(0 0 0 0))
  (loop :for op :in program
     :do (apply (car (gethash (car op) opcode-h)) (cdr op))
     :finally (return *sys*)))


(defun part2 (entries-file program-file)
  (execute-program (read-program program-file)
		   (analyze-opcodes (read-all-entries entries-file))))
