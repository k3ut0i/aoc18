(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :cl-ppcre))
(use-package (list :cl-ppcre))

(defun trigger-poly (poly new-unit)
  (when (consp poly)
    (let ((top-char (car poly)))
      (cond ((eq top-char new-unit) nil)
	    ((eq top-char (char-upcase new-unit)) t)
	    ((eq top-char (char-downcase new-unit)) t)
	    (t nil)))))

(defun insert-new-unit (poly new-unit)
  (if (trigger-poly poly new-unit)
      (cdr poly)
      (cons new-unit poly)))

(defun get-input (file)
  (with-open-file (s file)
    (read-line s)))


(defun reduction (string)
    (length (reduce (lambda (poly c)
		    (insert-new-unit poly c))
		    string
		  :initial-value nil)))
(defun part1 (file)
  (reduction (get-input file)))

(defun part2 (file)
  (let ((poly-string (get-input file)))
    (reduce (lambda (min char)
	      (let ((current-length
		     (reduction (remove-if (lambda (c)
					     (or (eql c char)
						 (eql c (char-upcase char))))
					   poly-string))))
		(if (< current-length (cdr min))
		    (cons char current-length)
		    min)))
	    "abcdefghijklmnopqrstuvxyz"
	    :initial-value (cons #\a (reduction poly-string)))))
