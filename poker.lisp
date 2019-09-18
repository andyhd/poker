(in-package #:poker)

(defun straight (hand)
  (let ((sorted-values (sorted-card-values hand)))
    (cond
      ((equal sorted-values '(12 3 2 1 0)) '(3))
      ((member sorted-values *straights* :test #'equal) (list (first sorted-values)))
      (t nil))))

(defun flush (hand)
  (when (apply #'= (mapcar #'rest hand))
    (sorted-card-values hand)))

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
         (value-count (sort-counts (count-duplicates (sorted-card-values hand))))
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

(defun possible-hands (cards &key (hand-size 5))
  ; return list of all possible hands from the list of cards
  (let ((hands '()))
    (when (>= (length cards) hand-size)
      (dolist (bitset (combinations (length cards)) hands)
        (push (select-by-bitset cards bitset) hands)))))

(defun ranked-hands (possible-hands)
  (let* ((ranks (mapcar #'rank possible-hands)))
    (sort (mapcar #'list possible-hands ranks)
          #'list>
          :key #'second)))

