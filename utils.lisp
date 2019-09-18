(in-package #:poker)

(defun range (min max &optional (step 1))
  (when (<= min max)
    (cons min (range (+ min step) max step))))

(defun shuffle (items)
  (let ((index -1)
        (num-items (length items)))
    (mapcar
         (lambda (element)
           (declare (ignore element))
           (incf index)
           (let* ((rand (+ index (random (- num-items index))))
                  (item (elt items rand)))
             (rotatef (elt items index) (elt items rand))
             item))
         items)))

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

(defun list> (a b)
  (cond
    ((null b) (not (null a)))
    ((null a) nil)
    ((= (first a) (first b)) (list> (rest a) (rest b)))
    (t (> (first a) (first b)))))

(defun count-duplicates (items &aux counts)
  (dolist (item items counts)
    (let ((pair (assoc item counts)))
      (if pair
        (incf (cdr pair))
        (push (cons item 1) counts)))))

(defun sort-counts (counts)
  (sort counts (lambda (a b)
                 (if (> (cdr a) (cdr b))
                   t
                   (if (= (cdr a) (cdr b))
                     (> (car a) (car b))
                     nil)))))

