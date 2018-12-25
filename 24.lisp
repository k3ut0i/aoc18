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
    (hash-table-plist result)))

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
       :with immune-system = nil :and immune-system-idx = 0
       :and infection = nil :and infection-idx = 0
       :for line = (read-line s nil nil)
       :while line
       :when (string-equal line "Immune System:")
       :do (setf immune-system (loop :for line = (read-line s nil nil)
				  :until (string-equal line "")
				  :do (incf immune-system-idx)
				  :collect (cons immune-system-idx
						 (read-unit line))))
       :when (string-equal line "Infection:")
       :do (setf infection (loop :for line = (read-line s nil nil)
			      :while line
			      :do (incf infection-idx)
			      :collect (cons infection-idx
					     (read-unit line))))
       :finally (return
		  (list
		   :immune-system immune-system 
		   :infection infection)))))

(defun group-id (g)
  (first g))
(defun num-units (g)
  (second g))
(defun hit-points (g)
  (third g))
(defun defense-types (g)
  (fourth g))
(defun damage (g)
  (fifth g))
(defun damage-type (g)
  (sixth g))
(defun initiative (g)
  (seventh g))

(defun effective-power (g)
  (* (num-units g) (damage g)))

(defun sel-pref-p (g1 g2)
  (or (> (effective-power g1) (effective-power g2))
      (and (= (effective-power g1) (effective-power g2))
	   (> (initiative g1) (initiative g2)))))

(defun effective-damage (a d)
  "Effective damage that can be done by attacker A on defender D"
  (let ((attack-type (damage-type a))
	(defense-types (defense-types d)))
    (cond ((member attack-type (getf defense-types :weak))
	   (* 2 (effective-power a)))
	  ((member attack-type (getf defense-types :immune))
	   0)
	  (t (effective-power a)))))

(defun target-pref-p (a d1 d2)
  "Would an attacker A prefer D1 over D2?"
  (let ((damage-d1 (effective-damage a d1))
	(damage-d2 (effective-damage a d2)))
    (or (> damage-d1 damage-d2)
	(and (= damage-d1 damage-d2)
	     (or (> (effective-power d1) (effective-power d2))
		 (and (= (effective-power d1) (effective-power d2))
		      (> (initiative d1) (initiative d2))))))))

(defun get-target (a def-list)
  (let ((target (reduce (lambda (acc d)
			  (if (target-pref-p a acc d)	acc d))
			def-list)))
    (and (> (effective-damage a target) 0) target)))

(defun target-selection (armies)
  (list
   (loop
      :with infection = (getf armies :infection)
      :and immune-system = (getf armies :immune-system)
      :and immune-system-targets = nil
      :for g :in (sort immune-system #'sel-pref-p)
      :do (let ((target (get-target g infection)))
	    (when target
	      (setq infection (delete target infection :test #'equal))
	      (push (cons (group-id g) (group-id target))
		    immune-system-targets)))
      :finally (return immune-system-targets))
   (loop
      :with infection = (getf armies :infection)
      :and immune-system = (getf armies :immune-system)
      :and infection-targets = nil
      :for i :in (sort infection #'sel-pref-p)
      :do (let ((target (get-target i immune-system)))
	    (when target
	      (setq immune-system (delete target immune-system :test #'equal))
	      (push (cons (group-id i) (group-id target))
		    infection-targets)))
      :finally (return infection-targets))))
