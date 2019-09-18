(in-package #:poker)

(defclass hand ()
  ((community-cards :initform nil :accessor community-cards)
   (pot :initform 0 :accessor pot)
   (pre-flop :initform t)
   (stake :initarg :stake :reader stake)
   (table :initarg :table :reader table)))

(defmethod players ((hand hand))
  (with-slots (table) hand
    (remove nil (slot-value table 'seats))))

(defmethod update-stake ((hand hand) amount)
  (with-slots (stake) hand
    (when (> amount stake)
      (setf stake amount))))

(defmethod orbit ((hand hand) &key start)
  (declare (ignore start)
    (orbit (table hand) :start (first-to-act hand))))

(defmethod small-blind ((hand hand))
  (with-slots (table) hand
    (nth-player table (+ (dealer table) 1))))

(defmethod big-blind ((hand hand))
  (with-slots (table) hand
    (nth-player table (+ (dealer table) 2))))

(defmethod first-to-act ((hand hand))
  (with-slots (pre-flop table) hand
    (nth-player table (+ (dealer table)
                         (if pre-flop 3 1)))))

(defmethod betting-round ((hand hand))
  (let ((players (orbit hand)))
    (mapc #'start-betting-round players)
    ; orbit table continuously
    (dolist (player (nconc players players))
      (when (or (= 1 (count-if-not #'folded players))
                (every #'has-acted players))
        (return))
      (act player hand))))

(defun winners (players &aux (best-rank '(-1)))
  (reduce (lambda (player winners &aux (rank (hand-rank player)))
            (cond
              ((list> rank best-rank) (setf best-rank rank)
                                      (list player))
              ((list> best-rank rank) winners)
              (t (cons player winners))))
          players
          :initial-value nil))

(defun share (amount players &aux shares)
  (multiple-value-bind (share remainder) (truncate amount (length players))
    (setf shares (make-list (length players) :initial-element share))
    ; TODO award remainder to first to act
    (unless (zerop remainder)
      (incf (first shares) remainder)))
  shares)

(defmethod distribute-pot ((hand hand) &optional (players (players hand)))
  (let* ((lowest-stake (min (mapcar #'stake players)))
         (amount (* lowest-stake (length players))))
    (decf (pot hand) (apply #'+ (mapcar #'receive winners (share amount winners))))
    (unless (zerop (pot hand))
      (format t "distributing side pot~%")
      (distribute-pot hand (remove lowest-stake players :key #'stake)))))

(defmethod start-hand ((hand hand))
  (with-slots (community-cards pot pre-flop table) hand
    (setf community-cards nil)
    (setf pot 0)
    (setf pre-flop t)
    (dolist (player (slot-value table 'seats))
      (when player
        (start-hand player)))))

(defmethod deal-hole-cards ((hand hand) deck)
  (dotimes (i *num-hole-cards*)
    (dolist (player (orbit hand))
      (push (pop deck) (cards player)))))

(defmethod post-blinds ((hand hand))
  (with-slots (pot stake table) hand
    (let ((small-blind (nth-player table (+ (dealer table) 1)))
          (big-blind (nth-player table (+ (dealer table) 2))))
      (format t "~a pays small blind of ~a~%" (name small-blind) (/ stake 2))
      (incf pot (spend small-blind (/ stake 2)))
      (format t "~a pays big blind of ~a~%" (name big-blind) stake)
      (incf pot (spend big-blind stake)))))

(defmethod deal-community-card ((hand hand) deck)
  (with-slots (community-cards pre-flop table) hand
    (setf pre-flop nil)
    (push (pop deck) community-cards)))

(defmethod showdown ((hand hand))
  (with-slots (community-cards table) hand
    (format t "showdown~%")
    (dolist (player (orbit hand))
      (calculate-best-hand player community-cards))))

(defmethod play ((hand hand) &key (deck (shuffle (deck))))
  (start-hand hand)
  ; deal the hole cards
  (deal-hole-cards hand deck)
  ; post blinds
  (post-blinds hand)
  ; pre-flop, flop, turn and river
  (dolist (num-cards '(0 3 1 1))
    ; deal
    (dotimes (i num-cards)
      (deal-community-card hand deck))
    (print (display-table (table hand)))
    ; bet
    (betting-round hand)
    ; if all but one players fold, we have a winner
    (when (= 1 (count-if-not #'folded (orbit hand)))
      (return)))
  ; showdown
  (showdown hand)
  (distribute-pot hand))

