(in-package #:poker)

(defclass game ()
  ((players :accessor players)
   (hand :accessor hand)
   (table :accessor table)
   (dealer :initform 0)))

