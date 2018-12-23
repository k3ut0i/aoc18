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
