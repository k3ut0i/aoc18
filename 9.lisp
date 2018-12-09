(eval-when (:compile-toplevel)
  (ql:quickload :cl-ppcre)
  (use-package (list :cl-ppcre)))
(declaim (optimize (speed 3) (safety 0) (debug 0)))
(defstruct circle
  (marbles (list 0 1) :type list)
  (idx 1 :type fixnum)
  (len 2 :type fixnum))

(defun insert-after (lst idx elt)
  (push elt (cdr (nthcdr idx lst)))
  lst)


(defun push-marble-after (marble n c)
  (declare (type fixnum marble n))
  (insert-after (circle-marbles c)
	     (mod (+ n (circle-idx c))
		  (circle-len c))
	     marble)
  (setf (circle-idx c) (1+ (mod (+ n (circle-idx c))
				(circle-len c))))
  (incf (circle-len c))
  c)

(defun remove-nth (n lst)
  (declare (type fixnum n))
  (if (= n 0)
      (values (car lst) (cdr lst))
      (values (nth n lst) (remove-if (constantly t) lst :start n :end (1+ n)))))

(defun pop-marble-at (n c)
  (multiple-value-bind (marble rest-marbles)
      (remove-nth (mod (+ n (circle-idx c))
		       (circle-len c))
		  (circle-marbles c))
    (setf (circle-marbles c) rest-marbles)
    (setf (circle-idx c) (mod (+ n (circle-idx c))
			      (circle-len c)))
    (decf (circle-len c))
    marble))

(defun read-input (file)
  (with-open-file (s file)
    (loop
       :for line = (read-line s nil nil)
       :while line
       :collect (parse-line line))))

(defun parse-line (line)
  (register-groups-bind (num-players last-points)
      ("(\\d+) players; last marble is worth (\\d+) points*" line)
    (mapcar #'read-from-string (list num-players
				     last-points))))

(defstruct game
  (board (make-circle))
  (score)
  (next-marble 2 :type fixnum)
  (next-player 1 :type fixnum)
  (num-players 0 :type fixnum))

(defun new-game (nplayers)
  (make-game :score (make-array (list nplayers) :initial-element 0 :element-type 'fixnum)
	     :num-players nplayers))

(defun step-game (g)
  (if (zerop (mod (game-next-marble g) 23))
      (incf  (aref (game-score g) (game-next-player g)) 
	     (+ (pop-marble-at -7 (game-board g))
		(game-next-marble g)))
      (push-marble-after (game-next-marble g) 1 (game-board g)))
  (incf (game-next-marble g))
  (setf (game-next-player g)
	(mod (1+ (game-next-player g))
	     (game-num-players g)))
  g)


(defun high-score-after-nth-marble (nplayers last-marble)
  (loop
     :with game = (new-game nplayers)
     :while (<= (game-next-marble game) last-marble)
     :do (step-game game)
     :finally (return (loop
			 :for i :below nplayers
			 :maximizing (aref (game-score game) i)))))
