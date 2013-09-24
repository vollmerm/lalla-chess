;;;; move.lisp

(in-package #:lalla)
(declaim (optimize speed))

;; Moves are stored in 18-bit words.
;; They consist of a from square, a to square, and a special tag.
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
(defconstant max-move-count 218)

(defparameter* (file-string string) "abcdefgh")
(defun* (position-file -> standard-char) ((index (mod 128)))
  (char file-string (logand index 7)))

(defparameter* (rank-string string) "12345678")
(defun* (position-rank -> standard-char) ((index (mod 128)))
  (char rank-string (- 7 (ash index -4))))

(defun* (move->string -> string) ((m (unsigned-byte 18)))
  (with-output-to-string (stream)
    (princ (position-file (move-from m)) stream)
    (princ (position-rank (move-from m)) stream)
    (princ (position-file (move-to m)) stream)
    (princ (position-rank (move-to m)) stream)
    (when (move-promotion m) (princ #\q stream))))
     
;; Generate moves using tables in piece.lisp
(defun* (generate-moves -> (vector (unsigned-byte 18))) ((turn-color (unsigned-byte 1)))
  (let ((moves-vector 
	 (make-array max-move-count
		     :element-type '(unsigned-byte 18)
		     :fill-pointer 0
		     :initial-element 0)))
    (loop 
       for square from 0 to 127 
       when (and (not (off-board square)) 
                 (not (blank-square square))
		 (= turn-color (piece-color (aref board square)))) do
	 (*let ((piece (unsigned-byte 4) (aref board square))
		(color (unsigned-byte 1) (piece-color piece))
		(type (unsigned-byte 3) (piece-type piece))
		(sliding boolean (aref sliding-piece type))
		(start (unsigned-byte 8) (aref step-offset type))
		(step (signed-byte 8) (aref piece-steps start))
		(iter-square (mod 128) square))
               (generate-pawn-special square moves-vector type)
	       (loop while (/= step 0) do
		    (setf iter-square square)
		    (block inner
                      (loop while (and (> (+ iter-square step) -1)
                                       (< (+ iter-square step) 128)) do 
					; inner loop for different step amounts
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
                                   (vector-push
                                    (create-move square iter-square ; to/from
                                                 (if (blank-square iter-square) 0 1) ; capture
                                                 0 ; promotion
                                                 0 ; ep
                                                 0) ; castle
                                    moves-vector)) ; TODO: set move tag
                                 (unless sliding (return-from inner)))))) 
                                        ; break if not sliding pieces
		    (incf start) ; try next step
		    (setf step (aref piece-steps start)))))
    (sort moves-vector #'> :key #'move-capture-bit)))
		    

(defun* (generate-pawn-special -> :void) 
    ((square (mod 128)) (moves-vector (vector (unsigned-byte 18)))
     (type (unsigned-byte 3)))
  (when (or (and (= type 1) (= (get-rank square) 6))
	    (and (= type 2) (= (get-rank square) 1)))
    (let ((increment (if (= type 1) -16 16)))
      (when (blank-square (+ square increment))
	(vector-push
	 (create-move square (+ square increment)
		      0
		      0
		      0
		      0)
	 moves-vector)
	(when (blank-square (+ square (* 2 increment)))
	  (vector-push
	   (create-move square (+ square (* 2 increment))
			0
			0
			1 ; ep move
			0)
	   moves-vector)))))
  (values)) ; return nothing
		    
(defun* (king-capture -> boolean) ((m (unsigned-byte 18)))
  (*let ((to (unsigned-byte 7) (move-to m))
	 (replaced (unsigned-byte 4) (aref board to))
	 (type (unsigned-byte 3) (piece-type replaced)))
	(= type 7)))
(declaim (inline king-capture))
	
