;;;; move.lisp
;;;;
;;;; This file contains the procedures that generate and handle moves. Moves are stored in numbers with bitmapping.


(in-package #:lalla)
(declaim (optimize speed))

;; Moves are stored in 18-bit words.
;; They consist of a from square, a to square, and a series of tags.
;; The following functions are short convenience functions and are marked for inlining.
(defun* (move-from -> (unsigned-byte 7)) ((m (unsigned-byte 18)))
  (ldb (byte 7 0) m))
(defun* (move-to -> (unsigned-byte 7)) ((m (unsigned-byte 18)))
  (ldb (byte 7 7) m))
(defun* (move-tag -> (unsigned-byte 4)) ((m (unsigned-byte 18)))
  (ldb (byte 4 14) m))
(defun* (move-capture-bit -> (unsigned-byte 1)) ((m (unsigned-byte 18)))
  (ldb (byte 1 0) (move-tag m)))
(defun* (move-capture -> boolean) ((m (unsigned-byte 18)))
  (= (move-capture-bit m) 1))
(defun* (move-promotion -> boolean) ((m (unsigned-byte 18)))
  (= (ldb (byte 1 1) (move-tag m)) 1))
(defun* (move-ep-bit -> (unsigned-byte 1)) ((m (unsigned-byte 18)))
  (ldb (byte 1 2) (move-tag m)))
(defun* (move-ep -> boolean) ((m (unsigned-byte 18)))
  (= (move-ep-bit m) 1))
(defun* (move-castle -> boolean) ((m (unsigned-byte 18)))
  (= (ldb (byte 1 3) (move-tag m)) 1))

;; Create a number that represents a move
(defun* (create-move -> (unsigned-byte 18))
    ((from (unsigned-byte 7)) (to (unsigned-byte 7)) 
     (capture (unsigned-byte 1)) (promotion (unsigned-byte 1))
     (ep (unsigned-byte 1)) (castle (unsigned-byte 1)))
  (let ((m 0))
    (declare ((unsigned-byte 18) m))
    (setf (ldb (byte 7 0) m) from)
    (setf (ldb (byte 7 7) m) to)
    (setf (ldb (byte 1 14) m) capture)
    (setf (ldb (byte 1 15) m) promotion)
    (setf (ldb (byte 1 16) m) ep)
    (setf (ldb (byte 1 17) m) castle)
    m))
(declaim (inline move-from move-to move-tag move-capture move-capture-bit
		 move-promotion move-ep move-ep-bit move-castle make-move))

;; This is the maximum number of moves that could be generated.
(defconstant max-move-count 218)

;; These are some functions for converting stuff to strings
(defparameter* (file-string string) "abcdefgh") ;; use char index of string

(defun* (position-file -> standard-char) ((index (mod 128)))
  (char file-string (logand index 7)))

(defparameter* (rank-string string) "12345678")

(defun* (position-rank -> standard-char) ((index (mod 128)))
  (char rank-string (- 7 (ash index -4))))

;; Create a string that represents a move (useful for printing to the screen)
(defun* (move->string -> string) ((m (unsigned-byte 18)))

  ;; with-output-to-string is pretty cool
  (with-output-to-string (stream)
    (princ (position-file (move-from m)) stream)
    (princ (position-rank (move-from m)) stream)
    (princ (position-file (move-to m)) stream)
    (princ (position-rank (move-to m)) stream)

    ;; all generated promotions are queen promotions
    (when (move-promotion m) (princ #\q stream))))
     
;; Generate moves for a certain side!
;; Just a warning: you're going to need a wide screen/window to read this function. 
;; It gets nested pretty deep (it might help if you listen to Inception music...
;; do people still tell that joke? "Must go deeper..." Sigh...)
(defun* (generate-moves -> (vector (unsigned-byte 18)))
    ((turn-color (unsigned-byte 1)))

  ;; Create a vector for moves, with a fill pointer. When moves are generated
  ;; they will be pushed onto the vector.
  ;; This is not ideal for performance. It would be better to store moves in 
  ;; a simple array and manage the fill pointer manually as a separate value.
  ;; That would allow SBCL to optimize access to the structure. For now, it has
  ;; to do some extra work at runtime to manage access to the vector.
  (let ((moves-vector 
	 (make-array max-move-count
		     :element-type '(unsigned-byte 18)
		     :fill-pointer 0
		     :initial-element 0)))


    ;; Move generation involves one loop nested inside another. The outer loop
    ;; goes through every square on the board (which is inefficient---eventually
    ;; it should use a list of piece locations), and the inner loop generates
    ;; moves for each piece. When moves are generated they're pushed onto
    ;; moves-vector.
    (loop 
       for square from 0 to 127 
       when (and (not (off-board square)) 
                 (not (blank-square square))
		 (= turn-color (piece-color (aref board square)))) do
         
       ;; Now that we've found a square, we extract information about it
       ;; and bind it to values in a big let (you know the drill by now).
	 (*let ((piece (unsigned-byte 4) (aref board square))
		(color (unsigned-byte 1) (piece-color piece))
		(type (unsigned-byte 3) (piece-type piece))
		(sliding boolean (aref sliding-piece type))
		(start (unsigned-byte 8) (aref step-offset type))
		(step (signed-byte 8) (aref piece-steps start))
		(iter-square (mod 128) square))
               
               ;; pawns have extra rules, so we check that first
               (generate-pawn-special square moves-vector type)

               ;; iterate through each step amount.
               ;; there's an array of step offsets in piece.lisp, so see
               ;; the values there for each piece if you're curious. they're
               ;; pretty self-explanatory.
	       (loop while (/= step 0) do
                    
                  ;; this is the general strategy for generating moves:
                  ;; each piece is either sliding or non-sliding, and
                  ;; each piece has a series of "step offsets" associated
                  ;; with it. for example, the bishop is a sliding piece
                  ;; and it can go +15, +17, -15, and -17. if you go look
                  ;; at the board, or remember the layout of the 0x88 board,
                  ;; you can see that adding these numbers to the current
                  ;; bishop location will yield one-step jumps for it.
                    
		    (setf iter-square square) ;; add offset


                  ;; start inner block. this cancel be returned from in the loop
		    (block inner
                      
                      (loop while (and (> (+ iter-square step) -1)
                                       (< (+ iter-square step) 128)) do 
                         ;; inner loop for different step amounts
                           (incf iter-square step) ; make one step
                           (if (or (off-board iter-square) ; jump off board
                                   (and (not (blank-square iter-square)) ; non-blank square
                                        (= color (square-color iter-square)))) ; hit own piece
                               (return-from inner) ; break	    
                               (progn ; make move
                                        ; generate base move
                                 (unless (and (or (= type 1)  ; pawns must capture on
                                                  (= type 2)) ; diagonal moves
                                              (blank-square iter-square))

                                   ;; time to generate a move!
                                   (vector-push
                                    (create-move square iter-square ; to/from
                                                 (if (blank-square iter-square) 0 1) ; capture
                                                 (if (and (is-pawn square)
                                                          (or (and (= (get-rank iter-square) 7)
                                                                   (= turn-color 1))
                                                              (and (= (get-rank iter-square) 0)
                                                                   (= turn-color 0))))
                                                     1 0) ; promotion if we reach end of board 
                                                 0 ; ep
                                                 0) ; castle
                                    moves-vector))
                                 (unless sliding (return-from inner)))))) ;; break if not sliding pieces
                    
                    ;; increment the start value to try the next offset amount
                    (incf start))
               (setf step (aref piece-steps start)))


       ;; now that the moves have been generated, they need to be sorted.
       ;; currently this just uses a very simple sort procedure. all captures
       ;; go first, then everything else

         (sort moves-vector #'> :key #'move-capture-bit))))
		    

;; Pawns have weird rules. This function handles moving forward.
(defun* (generate-pawn-special -> :void) 
    ((square (mod 128)) (moves-vector (vector (unsigned-byte 18)))
     (type (unsigned-byte 3)))
  ;; needs to be a pawn!
  (when (or (and (= type 1) (= (get-rank square) 6))
	    (and (= type 2) (= (get-rank square) 1)))

    ;; come up with an increment value. either positive and negative
    ;; depending on whether we're moving up or down
    (let ((increment (if (= type 1) -16 16)))

      ;; generate one step up
      (when (blank-square (+ square increment))
	(vector-push
	 (create-move square (+ square increment)
		      0
		      0
		      0
		      0)
	 moves-vector)

        ;; generate two steps up
        ;; we can't go two steps if the first step wasn't possible, so
        ;; we nest this inside the above when clause.
	(when (blank-square (+ square (* 2 increment)))
	  (vector-push
	   (create-move square (+ square (* 2 increment))
			0
			0
			1 ; ep move
			0)
	   moves-vector)))))
  (values)) ; return nothing


;; this determines if a move is a king capture. it's useful later in the search process
(defun* (king-capture -> boolean) ((m (unsigned-byte 18)))
  (*let ((to (unsigned-byte 7) (move-to m))
	 (replaced (unsigned-byte 4) (aref board to))
	 (type (unsigned-byte 3) (piece-type replaced)))
	(= type 7)))
(declaim (inline king-capture))
	
