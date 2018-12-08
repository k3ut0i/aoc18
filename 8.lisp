(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :split-sequence)
  (use-package (list :split-sequence)))

(defstruct node
  metadata
  children)

(defun read-node-data (num-list)
  (when num-list
    (let ((num-child-nodes (pop num-list))
	  (num-metadata-entries (pop num-list))
	  (children nil)
	  (metadata nil))
      (loop
	 :for n :below num-child-nodes
	 :do (multiple-value-bind (child data)
		 (read-node-data num-list)
	       (push child children)
	       (setf num-list data)))
      (dotimes (n num-metadata-entries)
	(push (pop num-list) metadata))
      (values (make-node :metadata metadata
			 :children (reverse children))
	      num-list))))

(defun read-all-nodes (file)
  (with-open-file (s file)
    (let ((nodes (mapcar #'read-from-string
			 (split-sequence #\Space (read-line s)))))
      nodes)))


(defun metadata-sum (tree)
  (if (node-metadata tree)
      (+ (reduce #'+ (mapcar #'metadata-sum (node-children tree))
		 :initial-value 0)
	 (reduce #'+ (node-metadata tree)))
      0))

(defun part1 (file)
  (let ((tree (read-node-data (read-all-nodes file))))
    (metadata-sum tree)))

(defun node-value (node)
  (if node
      (if (node-children node)
	  (loop
	     :for idx :in (node-metadata node)
	     :unless (= idx 0) 
	     :summing (node-value (nth (1- idx) (node-children node)))
	     :into value
	     :finally (return value))
	  (reduce #'+ (node-metadata node)))
      0))

(defun part2 (file)
  (let ((tree (read-node-data (read-all-nodes file))))
    (node-value tree)))
