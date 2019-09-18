(in-package #:poker)

(defclass table ()
  ((dealer :initarg :dealer :reader dealer)
   (num-players :initform 0)
   (num-seats :initform 10)
   (seats :reader seats)))

(defmethod initialize-instance :after ((table table) &rest initargs)
  (declare (ignore initargs))
  (with-slots (num-seats seats) table
    (setf seats (make-list num-seats))))

(defmethod seat-number ((table table) n)
  (rem n (slot-value table 'num-seats)))

(defmethod nth-player ((table table) n &aux (count -1))
  (with-slots (num-players seats) table
    (let ((offset (rem n num-players)))
      (dolist (player seats)
        (when (and player
                   (= (incf count) offset))
          (return player))))))

(defmethod orbit ((table table) &key start)
  ; get list of players around the table starting at `start`
  (with-slots (seats) table
    (let ((start-pos (position start seats)))
      (remove nil (append (nthcdr start-pos seats)
                          (subseq seats start-pos))))))

