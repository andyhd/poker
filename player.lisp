(in-package #:poker)

(defclass player ()
  ((all-in :initform nil :accessor all-in)
   (best-hand :reader best-hand)
   (cards :initform '() :accessor cards)
   (folded :initform nil :accessor folded)
   (hand-rank :reader hand-rank)
   (has-acted :initform nil :reader has-acted)
   (name :initarg :name :accessor name)
   (stack :initarg :stack :initform 0 :accessor stack)
   (stake :initform 0 :accessor stake)))

(defmethod act ((player player) hand)
  ; can't act if already all-in or folded
  (unless (or (all-in player) (folded player))
    ; prompt player to bet or fold
    (destructuring-bind (action amount) (prompt player (stake hand))
      (cond
        ((eq action 'check)  ())
        ((eq action 'fold)   (setf (folded player) t))
        ((eq action 'call)   (incf (pot hand) (spend player (stake hand))))
        ((eq action 'raise)  (incf (pot hand) (spend player amount))
                             (update-stake hand amount))
        ((eq action 'all-in) (let ((amount (stack player)))
                               (incf (pot hand) (spend player amount))
                               (update-stake hand amount)))))
    (setf (has-acted player) t)))

(defmethod calculate-best-hand ((player player) community-cards)
  (first (ranked-hands (possible-hands (cons (cards player) community-cards)))))

(defmethod called ((player player) stake)
  (= (stake player) stake))

(defmethod spend ((player player) amount)
  (setf amount
    (cond
        ((< amount (stack player)) amount)
        (t (setf (all-in player) t)
           (stack player))))
  (decf (stack player) amount)
  (incf (stake player) amount)
  (format t "~a pays ~a~%" (name player) amount)
  amount)

(defmethod receive ((player player) amount)
  (incf (stack player) amount)
  (format t "~a receives ~a~%" (name player) amount)
  amount)

(defmethod start-betting-round ((player player))
  (with-slots (has-acted) player
    (setf has-acted nil)))

(defmethod start-hand ((player player))
  (with-slots (all-in best-hand cards folded hand-rank has-acted stake) player
    (setf all-in nil)
    (setf best-hand nil)
    (setf folded nil)
    (setf hand-rank nil)
    (setf has-acted nil)
    (setf cards '())
    (setf stake 0)))

(defmethod prompt ((player player) stake)
  (let ((amount-to-call (- stake (stake player)))
        ; a player can always fold
        (options '(fold all-in))
        action
        amount)
    ; if the amount to call is greater than or equal to the player's stack, then
    ; they can only go all-in or fold
    (unless (>= amount-to-call (stack player))
      ; otherwise, if the amount to call is zero, they can check
      (if (zerop amount-to-call)
        (push 'check options)
        ; otherwise, they can call
        (push 'call options))
      (when (> (stack player) (- (* 2 stake) (stake player)))
        ; and they can raise
        (push 'raise options)))
    (do () (nil)
      (when (member 'call options)
        (format t "~a to call~%" amount-to-call))
      (format t "~%~a to act: ~{~#[~;~a~;~a or ~a~:;~@{~a~#[~;, or ~:;, ~]~}~]~}~%"
              (name player) options)
      (setf action (read))
      (when (member action options)
        (return))
      (format t "~%You can't ~a~%" action))
    (when (eq action 'raise)
      (do () (nil)
        (format t "~%Raise amount (min: ~a)?~%" (* 2 stake))
        (setf amount (read))
        (when (and (integerp amount)
                   (>= amount (- (* 2 stake) (stake player))))
          (return))))
    (list action amount)))

(defmethod take-seat ((player player) (table table) &key (start 0))
  (with-slots (num-players num-seats seats) table
    (unless (>= num-players num-seats)
      (do* ((start (seat-number table start))
            (i start (seat-number table (1+ i)))
            (tries 0))
           ((and (> tries 0) (= i start)))
        (unless (elt seats i)
          (setf (elt seats i) player)
          (incf num-players)
          (return))
        (incf tries)))))

