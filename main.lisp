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
   (dealer :initform 0)))


(defclass player ()
  ((all-in :initform nil :accessor all-in)
   (best-hand :reader best-hand)
   (cards :initform '() :accessor cards)
   (hand-rank :reader hand-rank)
   (name :initarg :name :accessor name)
   (stack :initarg :stack :initform 0 :accessor stack)
   (stake :initform 0 :accessor stake)))


(defclass hand ()
  ((community-cards :accessor community-cards)
   (dealer :initarg :dealer :accessor dealer)
   (players :initarg :players :accessor players)
   (pot :initform 0 :accessor pot)))


(defun player-at (pos players)
  (nth (rem pos (length players)) players))


(defmethod best-hand ((player player) community-cards)
  (first (ranked-hands (possible-hands (cons (cards player) community-cards)))))


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
  (setf (all-in player) nil)
  (setf (best-hand player) nil)
  (setf (hand-rank player) nil)
  (setf (cards player) '())
  (setf (stake player) 0))


(defun betting-round (hand &key (stake 0) (start 0))
  ; a. foreach player from big blind + 1
  ; b. fold, check, call or raise
  ; c. if all but one folded - last player wins
  ; d. until all players either folded, checked, called or all-in
  (dotimes (i (length (players hand)))
    (let* ((player (player-at (+ start i) (players hand)))
           (action (prompt player stake))
           (amount-to-call (- stake (stake player))))
      (cond
        ((eq (first action) 'fold) (setf (players hand)
                                         (remove player (players hand))))
        ((eq (first action) 'call) (incf (pot hand) (spend player amount-to-call)))
        ((eq (first action) 'bet) (let* ((amount (second action))
                                         (raise (- amount amount-to-call)))))))))


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


(defmethod play ((hand hand) &key (deck (shuffle (deck))) &aux (pot 0))
  ; play through a hand of poker
  (with-slots (community-cards dealer players stake) hand
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
      (return (win hand (first players))))

    ; flop, turn and river
    (dolist (num-cards '(3 1 1)) flop-turn-river
      ; deal
      (dotimes (i num-cards)
        (push (pop deck) community-cards))
      ; bet
      (betting-round hand)
      ; if all but one players fold, we have a winner
      (when (= (length players) 1)
        (return (win hand (first players)))))

    ; showdown
    (distribute pot players)))

