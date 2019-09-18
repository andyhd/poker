; (ql:quickload "poker")

(in-package #:poker)

(defparameter *straights*
  (mapcar #'reverse (mapcar #'range (range 0 8) (range 4 12))))

(defparameter *num-hole-cards* 2)

(let* ((players (list (make-instance 'player :name "alice" :stack 100)
                     (make-instance 'player :name "bob" :stack 100)
                     (make-instance 'player :name "carol" :stack 100)))
       (table (make-instance 'table :dealer 0))
       (hand (make-instance 'hand :table table :stake 10)))
  (dolist (player players)
    (take-seat player table))
  (play hand))

