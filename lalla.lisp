;;;; lalla.lisp
;;;;
;;;; This file contains the main search procedures.

(in-package #:lalla)
(declaim (optimize speed))

;; The fail-amount is an arbitrary big number. It gets used when
;; we need an arbitrary high (or low) number.
(defparameter* (fail-amount (signed-byte 16)) 8000)



;; This is the main search procedure for the program. It's where the A.I. magic is.
;; I'm not going to explain the basics of the minimax algorithm here, so google for it
;; if you're curious how/why it works.
(defun* (negamax -> (signed-byte 16)) 
    ((side (unsigned-byte 1)) (alpha (signed-byte 16)) 
     (beta (signed-byte 16)) (depth-left (unsigned-byte 8)))

  ;; If we're at a leaf (depth-left=0), then return the static evaluation for this node.
  (if (= depth-left 0) (static-eval side)

      ;; We're at an internal node of the search tree, so we need to generate children
      ;; and recurse on each of them
      (*let ((score (signed-byte 16) (- fail-amount))
	     (moves (vector (unsigned-byte 18)) 
		    (generate-moves side))

             ;; temporary variable for saving data from make-move
	     (saved-state (unsigned-byte 13) 0)

             ;; temporary variable for saving the current score
	     (temp-score (signed-byte 16) 0))

            ;; named block for inner loop
            (block inner

              ;; loop over all generated moves
              (loop for current-move across moves do

                   ;; if it's a king capture, the parent node generated an illegal move
                   (when (king-capture current-move)
                     (setf score fail-amount)
                     (return-from inner))

                   ;; make the move and save the state
                   (setf saved-state (make-move current-move))

                   ;; recurse and save the score to temp-score
                   (setf temp-score
                         ;; negate returned score, because it's negamax
                         (- (negamax
                             ;; switch side
                             (if (= side 1) 0 1)
                             ;; update alpha and beta
                             (- beta)
                             (if (> alpha score)
                                 (- alpha)
                                 (- score))
                             ;; decrement depth-left
                             (- depth-left 1))))

                   ;; pass saved-state to unmake-move
                   (unmake-move current-move saved-state)

                   ;; have we improved our best score?
                   (when (> temp-score score)
                     (setf score temp-score)
                     
                     ;; give up if this node is not worth searching anymore
                     (when (>= score beta) (return-from inner)))))

            ;; return the score
	    score)))


;; A depth-limited search uses the above negamax function and searches to a finite
;; depth, using that information to choose a best move.
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

;; How long do we want to search?
;; NOTE: this isn't actually a time limit for the search, but a time
;; at which we would not do another search. The actual search might take
;; several seconds longer than this value.
(defparameter* (max-seconds positive-integer) 8)


;; Iterative deepening searches do incremental depth-limited searches,
;; increasing the depth each time, up to a certain point. It then uses
;; the result of the last search as its value.
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
