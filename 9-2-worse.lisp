(defpackage :day09-part2
  (:use :cl))
(in-package :day09-part2)

;; current-marble is always first in the list
(defstruct board
  (marbles (list 1 0) :type list)
  (num 1 :type fixnum))

(defun rotate-list-left (lst)
  (let ((top (car lst)))
    (setf (car lst) (cadr lst))
    (setf (cdr lst) (cddr lst))
    (rplacd (last lst) (list top))))

;; I've some how made it even worse
(defun add-new-marble (b)
  (let ((new-num (incf (board-num b))))
    (if (zerop (mod new-num 23))
	(let* ((last-7 (last (board-marbles b) 7)))
	  (setf (cdr (last (board-marbles b) 8)) nil)
	  (rplacd (last last-7) (board-marbles b))
	  (setf (board-marbles b) (cdr last-7))
	  (+  (car last-7) new-num))
	(progn (rotate-list-left (board-marbles b))
	       (rotate-list-left (board-marbles b))
	       (push new-num (board-marbles b))
	       0))))
(defstruct game
  board players current-player total-players)

(defun new-game (nplayers)
  (make-game :board (make-board)
	     :current-player 2
	     :total-players nplayers
	     :players (make-array (list nplayers) :initial-element 0)))

(defun step-game (g)
  (let ((score (add-new-marble (game-board g))))
    (incf (aref (game-players g) (game-current-player g))
	  score)
    (setf (game-current-player g)
	  (mod (1+ (game-current-player g))
	       (game-total-players g)))
    g))

(defun high-score (nplayers steps)
  (loop
     :with game = (new-game nplayers)
     :while (<= (board-num (game-board game)) steps)
     :do (step-game game)
     :finally (return (loop
			 :for i :below nplayers
			 :maximizing (aref (game-players game) i))) ))
