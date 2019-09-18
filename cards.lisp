(in-package #:poker)

(defun deck ()
  (mapcar (lambda (card)
            (multiple-value-bind (suit value) (truncate card 13)
              (cons value suit)))
          (range 0 51)))

(defun sorted-card-values (hand)
  (sort (mapcar #'first hand) #'>))

