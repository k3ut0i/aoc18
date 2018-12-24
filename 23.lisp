(eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload '(:cl-ppcre))
  (use-package '(:cl-ppcre)))

(defun read-bot (str)
  (register-groups-bind (x y z r)
      ("pos=<(.*),(.*),(.*)>, r=(.*)" str)
    (mapcar #'read-from-string (list x y z r))))

(defun read-all-bots (file)
  (with-open-file (s file)
    (loop :for line = (read-line s nil nil)
       :while line
       :collect (read-bot line))))

(defun strongest-bot (bot-list)
  (loop
     :with sb = (car bot-list)
     :for b :in (cdr bot-list)
     :do (when (> (car (last b)) (car (last sb)))
	   (setq sb b))
     :finally (return sb)))

(defun distance (b1 b2)
  (loop
     :repeat 3 
     :for v1 :in b1
     :for v2 :in b2
     :summing (abs (- v1 v2))))

(defun num-in-range (sb bot-list)
  (loop :for b :in bot-list
     :count (<= (distance b sb)
		(car (last sb)))))

(defun in-range (sb bot-list)
  (loop :for b :in bot-list
     :when (<= (distance b sb)
	       (car (last sb)))
     :collect b))

(defun boundries (bot-list)
  (loop :with
     xmin = 0 :and xmax = 0 :and
     ymin = 0 :and ymax = 0 :and
     zmin = 0 :and zmax = 0
     :for b :in bot-list
     :do (progn (when (< (car b) xmin) (setq xmin (car b)))
		(when (> (car b) xmax) (setq xmax (car b)))
		(when (< (cadr b) ymin) (setq ymin (cadr b)))
		(when (> (cadr b) ymax) (setq ymax (cadr b)))
		(when (< (caddr b) zmin) (setq zmin (caddr b)))
		(when (> (caddr b) zmax) (setq zmax (caddr b))))
     :finally (return (list xmin xmax ymin ymax zmin zmax))))

(defun point-in-range-count (point bot-list)
  (loop :for b :in bot-list
     :count (<= (distance point b) (car (last b)))))

(defun part2-crude (bot-list)
  (destructuring-bind (xmin xmax ymin ymax zmin zmax)
      (boundries bot-list)
    (loop
       :with max-val = 0 :and max-point = '(0 0 0)
       :for x :from xmin :to xmax
       :do (loop :for y :from ymin :to ymax
	      :do (loop :for z :from zmin :to zmax
		     :do (let ((d (point-in-range-count (list x y z)
							bot-list)))
			   (when (> d max-val)
			     (setq max-val d)
			     (setq max-point (list x y z))))))
       :finally (return (list max-point max-val)))))

