;;;; lalla.lisp

(in-package #:lalla)
(declaim (optimize speed))

(defparameter* (fail-amount (signed-byte 16)) 8000)

(defun* (negamax -> (signed-byte 16)) 
    ((side (unsigned-byte 1)) (alpha (signed-byte 16)) 
     (beta (signed-byte 16)) (depth-left (unsigned-byte 8)))
  (if (= depth-left 1) (static-eval side)
      (*let ((score (signed-byte 16) (- fail-amount))
	     (moves (vector (unsigned-byte 18)) 
		    (generate-moves side))
	     (saved-piece (unsigned-byte 4) 0)
	     (temp-score (signed-byte 16) 0))
	    (print side)
            (block inner
              (loop for current-move across moves do
                   (when (king-capture current-move)
                     (setf score fail-amount)
                     (return-from inner))
                   (setf saved-piece (make-move current-move))
                   (setf temp-score
                         (- (negamax
                             (if (= side 1) 0 1)
                             (- beta)
                             (if (> alpha score)
                                 (- alpha)
                                 (- score))
                             (- depth-left 1))))
                   (unmake-move current-move saved-piece)
                   (when (> temp-score score)
                     (setf score temp-score)
                     (when (>= score beta) (return-from inner)))))
	    score)))

(defparameter* (max-seconds positive-integer) 8)

(defun* (depth-limited-search -> (unsigned-byte 18))
    ((side (unsigned-byte 1)) (depth (unsigned-byte 8)))
  (*let ((moves (vector (unsigned-byte 18)) (generate-moves side))
         (best-move (unsigned-byte 18) 8)
         (best-score (signed-byte 16) (- fail-amount))
         (temp-score (signed-byte 16) 0)
         (saved-piece (unsigned-byte 4) 0))
        (loop for current-move across moves do
             (progn
               (setf saved-piece (make-move current-move))
               (setf temp-score
                     (- (negamax
                         (if (= side 1) 0 1)
                         -8000
                         8000
                         depth)))
               (unmake-move current-move saved-piece)
               (when (> temp-score best-score)
                   (setf best-score temp-score)
                   (setf best-move current-move))))
        best-move))

(defun* (iterative-deepening-search -> (unsigned-byte 18))
    ((side (unsigned-byte 1)))
  (*let ((start-time positive-integer (get-internal-real-time))
         (depth (unsigned-byte 8) 3)
         (best-move (unsigned-byte 18) 0))
        (loop while (< (/ (- start-time (get-internal-real-time))
                         internal-time-units-per-second)
                       max-seconds) do
             (progn
               (setf best-move (negamax side (- fail-amount) fail-amount depth))
               (incf depth)))
        best-move))
