(defun range (min max &optional (step 1))
  (when (<= min max)
    (cons min (range (+ min step) max step))))


(defun deck ()
  (mapcar (lambda (card)
            (multiple-value-bind (suit value) (truncate card 13)
              (cons value suit)))
          (range 0 51)))


(defun shuffle (cards)
  (let ((index -1)
        (num-cards (length cards)))
    (mapcar
         (lambda (element)
           (declare (ignore element))
           (incf index)
           (let* ((rand (+ index (random (- num-cards index))))
                  (card (elt cards rand)))
             (rotatef (elt cards index) (elt cards rand))
             card))
         cards)))


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


(defparameter *straights*
  (mapcar #'reverse (mapcar #'range (range 0 8) (range 4 12))))


(defun card-values (hand)
  (sort (mapcar #'first hand) #'>))


(defun straight (hand)
  (let ((sorted-values (card-values hand)))
    (cond
      ((equal sorted-values '(12 3 2 1 0)) '(3))
      ((member sorted-values *straights* :test #'equal) (list (first sorted-values)))
      (t nil))))


(defun card-suits (hand)
  (mapcar #'rest hand))


(defun flush (hand)
  (when (apply #'= (card-suits hand))
    (card-values hand)))


(defun count-values (cards)
  (let (result)
    (mapc (lambda (item)
            (let ((pair (assoc item result)))
              (if pair
                  (incf (cdr pair))
                  (push (cons item 1) result))))
          (card-values cards))
    (sort result (lambda (a b)
                   (if (> (cdr a) (cdr b))
                     t
                     (if (= (cdr a) (cdr b))
                       (> (car a) (car b))
                       nil))))))


(defun four-of-a-kind (value-count)
  (= 4 (cdar value-count)))


(defun full-house (value-count)
  (when (and (= 3 (cdar value-count)))
       (= 2 (cdadr value-count))
    t))


(defun three-of-a-kind (value-count)
  (= 3 (cdar value-count)))


(defun two-pair (value-count)
  (and (= 2 (cdar value-count))
       (= 2 (cdadr value-count))))


(defun pair (value-count)
  (= 2 (cdar value-count)))


(defun rank (hand)
  (let* ((straight (straight hand))
         (flush (flush hand))
         (value-count (count-values hand))
         (vals (mapcar #'car value-count)))
    (cond
      ((and straight flush) (cons 8 straight))
      ((four-of-a-kind value-count) (cons 7 vals))
      ((full-house value-count) (cons 6 vals))
      (flush (cons 5 flush))
      (straight (cons 4 straight))
      ((three-of-a-kind value-count) (cons 3 vals))
      ((two-pair value-count) (cons 2 vals))
      ((pair value-count) (cons 1 vals))
      (t (cons 0 vals)))))


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


(defun count-set-bits (int &aux (count 0))
  ; count the number of set bits in an integer
  ; (count-set-bits 4) => 1 => #b0100
  ; (count-set-bits 5) => 2 => #b0101
  (dotimes (i (integer-length int) count)
    (when (logbitp i int)
      (incf count))))


(defun min-integer-bits (num-bits)
  ; return the lowest integer with `num-bits` set bits
  ; (min-integer-bits 2) => 3 => #b0011
  ; (min-integer-bits 3) => 5 => #b0111
  (- (expt 2 num-bits) 1))


(defun max-integer-bits (num-bits limit)
  ; return the highest integer with `num-bits` set bits of `limit` total bits
  ; (max-integer-bits 1 4) =>  8 ; #b1000
  ; (max-integer-bits 3 4) => 14 ; #b1110
  (- (- (expt 2 limit) 1) (- (expt 2 (- limit num-bits)) 1)))


(defun combinations (num-items &key (length 5))
  ; a combination of items is represented by a bit set, where 1 means the item
  ; is in the combination and 0 means it is not.
  ; start is lowest number with `length` set bits (eg: 0011111)
  ; end is highest number with `length` set bits (eg: 1111100)
  ; discard any integer between start and end where number of set bits is not
  ; equal to `length`
  ; this gives a list of numbers representing all possible combinations of
  ; `length` items
  (remove-if-not (lambda (i) (= (count-set-bits i) length))
                 (range (min-integer-bits length)
                        (max-integer-bits length num-items))))


(defun select-by-bitset (items bitset)
  ; select a subset of `items` based on a corresponding bitset
  ; (select-by-bitset '(A B C) #b001) => '(C)
  ; (select-by-bitset '(A B C) #b101) => '(A C)
  (let ((index (length items))
        (selection '()))
    (dolist (item items (reverse selection))
      (decf index)
      (when (logbitp index bitset)
        (push item selection)))))


(defun possible-hands (cards &key (hand-size 5))
  ; return list of all possible hands from the list of cards
  (let ((hands '()))
    (when (>= (length cards) hand-size)
      (dolist (bitset (combinations (length cards)) hands)
        (push (select-by-bitset cards bitset) hands)))))


(defun list> (a b)
  (cond
    ((null b) (not (null a)))
    ((null a) nil)
    ((= (first a) (first b)) (list> (rest a) (rest b)))
    (t (> (first a) (first b)))))


(defun ranked-hands (possible-hands)
  (let* ((ranks (mapcar #'rank possible-hands)))
    (sort (mapcar #'list possible-hands ranks)
          #'list>
          :key #'second)))


(defun display-hand-rank (hand)
  (format nil "~A ~A" (display-hand hand) (display-rank (rank hand))))


(defclass game ()
  ((players :accessor players)
   (hand :accessor hand)
   (table :accessor table)
   (dealer :initform 0)))


(defclass player ()
  ((all-in :accessor all-in)
   (best-hand :reader best-hand)
   (cards :initform '() :accessor cards)
   (folded :accessor folded)
   (hand-rank :reader hand-rank)
   (name :initarg :name :accessor name)
   (stack :initarg :stack :initform 0 :accessor stack)
   (stake :initform 0 :accessor stake)))


(defclass hand ()
  ((community-cards :accessor community-cards)
   (dealer :initarg :dealer :reader dealer)
   (players :initarg :players :accessor players)
   (pot :initform 0 :accessor pot)
   (stake :initarg :stake :reader stake)))


(defun player-at (pos players)
  (nth (rem pos (length players)) players))


(defmethod calculate-best-hand ((player player) community-cards)
  (first (ranked-hands (possible-hands (cons (cards player) community-cards)))))


(defmethod fold ((player player))
  (setf (folded player) t))


(defmethod spend ((player player) amount)
  (setf amount
    (cond
        ((< amount (stack player)) amount)
        (t (setf (all-in player) t)
           (stack player))))
  (decf (stack player) amount)
  (incf (stake player) amount)
  amount)


(defmethod receive ((player player) amount)
  (incf (stack player) amount)
  amount)


(defmethod start-hand ((player player))
  (with-slots (all-in best-hand cards folded hand-rank stake) player
    (setf all-in nil)
    (setf best-hand nil)
    (setf folded nil)
    (setf hand-rank nil)
    (setf cards '())
    (setf stake 0)))


(defmethod prompt ((player player) stake)
  (let ((amount-to-call (- stake (stake player)))
        ; a player can always fold
        (options '(fold))
        action
        amount)
    ; if the amount to call is greater than or equal to the player's stack, then
    ; they can only go all-in
    (if (>= amount-to-call (stack player))
      (push 'all-in options)
      ; otherwise, if the amount to call is zero, they can check
      (progn
        (if (zerop amount-to-call)
          (push 'check options)
          ; otherwise, they can call
          (push 'call options))
        ; and they can raise
        (push 'raise options)))
    (do () (nil)
      (format t "~%~a to act: ~{~#[~;~a~;~a or ~a~:;~@{~a~#[~;, or ~:;, ~]~}~]~}"
              (name player) options)
      (setf action (read))
      (when (member action options)
        (return))
      (format t "~%You can't ~a~%" action))
    (when (eq action 'raise)
      (do () (nil)
        (format t "~%Raise amount (min: ~a)?~%" stake)
        (setf amount (read))
        (when (and (integerp amount)
                   (> amount amount-to-call))
          (return))))
    (list action amount)))


(defmethod update-stake ((hand hand) amount)
  (when (> amount (stake hand))
    (setf (stake hand) amount)))


(defun act (player hand)
  ; can't act if already all-in or folded
  (unless (or (all-in player) (folded player))
    ; prompt player to bet or fold
    (destructuring-bind (action amount) (prompt player (stake hand))
      (cond
        ((eq action 'fold) (fold player))
        ((eq action 'bet)  (incf (pot hand) (spend player amount))
                           (update-stake hand amount))))))


(defclass table ()
  ((dealer :initarg :dealer)
   (num-players :initform 0)
   (num-seats :initform 10)
   (seats)))


(defmethod initialize-instance :after ((table table) &rest initargs)
  (declare (ignore initargs))
  (with-slots (num-seats seats) table
    (setf seats (make-list num-seats))))


(defmethod seat-number ((table table) n)
  (rem n (slot-value table 'num-seats)))

(defmethod take-seat ((player player) (table table) &key (start 0))
  (with-slots (dealer num-players num-seats seats) table
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


(defmethod orbit ((table table) &key (start 0))
  (reverse
    (do* ((start (seat-number table start))
          (i start (seat-number table (1+ i)))
          (started nil)
          (players))
         ((and started (= i start)) players)
      (setf started t)
      (push (elt (slot-value table 'seats) i) players))))


(defmethod betting-round ((hand hand) &key (pre-flop nil))
  (with-slots (dealer pot stake) hand
    (let ((big-blind-is-live t)
          (players (copy-list (players hand))))
      ; make circular list
      (setf (cdr (last players)) players)
      ; foreach player from start
      (setf players (nthcdr (+ (if pre-flop 3 1) dealer) players))
      (dolist (player players)
      ; (do* ((pos (rem (+ (if pre-flop 3 1) dealer) (length players))
      ;            (rem (incf pos) (length players))))
           ; if all but one folded - last player wins
           ; ((= 1 (- (length (remove-if #'folded players)))))
        ; (setf player (player-at pos players))
        ; prompt player to bet or fold
        (act player hand)
        ; if all but one folded, last player wins
        (when (length))
        ; until all players either folded, all-in or called
        (when (every (lambda (player)
                        (or (folded player)
                            (all-in player)
                            (called player stake)))
                     players)
          ; if the big blind has yet to act, continue the round
          (unless big-blind-is-live
            (return))
          (setf big-blind-is-live nil)
          (format t "big blind has acted")))
      ; remove folded players from hand
      (setf players (set-difference players folded))))
  'betting-ends)


(let* ((players (list (make-instance 'player :name "alice" :stack 100)
                     (make-instance 'player :name "bob" :stack 100)
                     (make-instance 'player :name "carol" :stack 100)))
       (hand (make-instance 'hand :dealer 0 :players players :stake 10)))
  (betting-round hand))


(defun distribute (pot players)
  ; calculate pot total = num players * lowest stake
  (let* ((lowest-stake (min (mapcar #'stake players)))
         (amount (* lowest-stake (length players)))
         ; get the player(s) with the highest card rank
         (best-rank '(-1))
         (winners (reduce
                    (lambda (player winners &aux (rank (hand-rank player)))
                      (cond
                        ((list> rank best-rank) (setf best-rank rank)
                                                (list player))
                        ((list> best-rank rank) winners)
                        (t (cons player winners))))
                    players
                    :initial-value nil)))
    ; split the pot between the winning players
    (decf pot amount)
    (multiple-value-bind (share remainder) (truncate amount (length winners))
      (dolist (winner winners)
        (receive winner share))
      ; TODO: award remainder to first to act
      (unless (zerop remainder)
        (receive (first winners) remainder)))
    ; if money is left in a side pot
    (unless (zerop pot)
      ; remove players with lowest stake and distribute side pot
      (distribute pot (remove-if (lambda (player)
                                   (= lowest-stake (stake player)))
                                 players)))))


(defmethod play ((hand hand) &key (deck (shuffle (deck))))
  (block play-hand
    ; play through a hand of poker
    (with-slots (community-cards dealer players pot stake) hand
      (mapc #'start-hand players)

      ; deal the hole cards
      (dotimes (i *num-hole-cards*)
        (dolist (player players)
          (push (pop deck) (cards player))))

      ; post blinds
      (incf pot (spend (player-at (+ dealer 1) players) (/ stake 2)))
      (incf pot (spend (player-at (+ dealer 2) players) stake))

      ; pre-flop betting
      (betting-round hand :pre-flop t)
      (when (= (length players) 1)
        (return (distribute pot players)))

      ; flop, turn and river
      (dolist (num-cards '(3 1 1))
        ; deal
        (dotimes (i num-cards)
          (push (pop deck) community-cards))
        ; bet
        (betting-round hand)
        ; if all but one players fold, we have a winner
        (when (= (length players) 1)
          (return-from play-hand (distribute pot players))))

      ; showdown
      (mapc #'calculate-best-hand players)
      (distribute pot players))))

