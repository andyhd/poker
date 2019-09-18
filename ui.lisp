(in-package #:poker)

(defun display-value (value)
  (cond
    ((< value 9) (+ 2 value))
    ((= value 9) 'J)
    ((= value 10) 'Q)
    ((= value 11) 'K)
    ((= value 12) 'A)))

(defun display-suit (suit)
  (cond
    ((= suit 0) '♣︎)
    ((= suit 1) '♦︎)
    ((= suit 2) '♥︎)
    ((= suit 3) '♠︎)))

(defun display-card (card)
  (let ((value (car card))
        (suit (cdr card)))
    (format nil "~A~A" (display-value value) (display-suit suit))))

(defun display-hand (hand)
  (format nil "~{~A~^ ~}" (mapcar #'display-card hand)))

(defun display-value-long (value)
  (cond
    ((= 12 value) "Ace")
    ((= 11 value) "King")
    ((= 10 value) "Queen")
    ((= 9 value) "Jack")
    (t (format nil "~r" (+ 2 value)))))

(defun display-rank (rank)
  (destructuring-bind (rank &rest value) rank
    (setf value (mapcar #'display-value-long value))
    (cond
      ((and (= 8 rank) (eq 'A (first value))) "Royal flush")
      ((= 8 rank) (format nil "~A-high straight flush" (first value)))
      ((= 7 rank) (format nil "Four ~As" (first value)))
      ((= 6 rank) (format nil "~As full of ~As" (first value) (second value)))
      ((= 5 rank) (format nil "~A-high flush" (first value)))
      ((= 4 rank) (format nil "~A-high straight" (first value)))
      ((= 3 rank) (format nil "Three ~As" (first value)))
      ((= 2 rank) (format nil "Two pairs, ~As over ~As" (first value) (second value)))
      ((= 1 rank) (format nil "Pair of ~As" (first value)))
      ((= 0 rank) (format nil "High card")))))

(defun display-hand-rank (hand)
  (format nil "~A ~A" (display-hand hand) (display-rank (rank hand))))

(defun display-player (player)
  (if player
    (with-slots (cards stack stake) player
        (format nil "~a: stack ~a, stake ~a, cards ~a~%"
                (name player) stack stake (display-hand cards)))
    ""))

(defun display-table (table)
  (format nil "players:~%~{  ~a~^,~%~}~%"
          (mapcar #'display-player (seats table))))

