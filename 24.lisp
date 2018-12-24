(eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload '(:alexandria :cl-ppcre :split-sequence))
  (use-package '(:alexandria :cl-ppcre :split-sequence)))

(defparameter *damage-types*
  '(:slashing
    :bludgeoning
    :cold
    :fire
    :radiation))

(defun keyword-from-string (s)
  (multiple-value-bind (keyname keyin)
      (find-symbol (string-trim " " (string-upcase s)) "KEYWORD")
    (unless (eq keyin :external)
      (error "Cannot find keyword ~A" s))
    keyname))

(defun read-weakness-immunity (str)
  (let ((strs (mapcar (lambda (s)
			(string-trim " " s))
		      (split-sequence #\; str)))
	(result (make-hash-table :test #'equal)))
    (loop :for str :in strs
       :do (cond ((string-equal str "immune to " :end1 10)
		  (push (mapcar #'keyword-from-string
				(split-sequence #\, (subseq str 10)))
			(gethash :immune result)))
		 ((string-equal str "weak to " :end1 8)
		  (push (mapcar #'keyword-from-string
				(split-sequence #\, (subseq str 8)))
			(gethash :weak result)))))
    (hash-table-alist result)))

(defun read-unit (str)
  (register-groups-bind ((#'read-from-string nunits hp)
			 (#'(lambda (a) (not (string-equal "" a)))
			    weakness-immunity-str-p)
			 weakness-immunity-str
			 (#'read-from-string damage)
			 (#'keyword-from-string damage-type)
			 (#'read-from-string initiative))
      ("(.*) units each with (.*) hit points( \\((.*)\\)|) with an attack that does (.*) (.*) damage at initiative (.*)"
       str)
    (list nunits hp
	  (when weakness-immunity-str-p
	    (read-weakness-immunity weakness-immunity-str))
	  damage damage-type
	  initiative)))


(defun read-data (file)
  (with-open-file (s file)
    (loop
       :with immune-system = nil :and infection = nil
       :for line = (read-line s nil nil)
       :while line
       :when (string-equal line "Immune System:")
       :do (setf immune-system (loop :for line = (read-line s nil nil)
				  :until (string-equal line "")
				  :collect (read-unit line)))
       :when (string-equal line "Infection:")
       :do (setf infection (loop :for line = (read-line s nil nil)
			      :while line
			      :collect (read-unit line)))
       :finally (return (list :immune-system immune-system
			      :infection infection)))))
