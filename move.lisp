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
(defun* (move-capture -> boolean) ((m (unsigned-byte 18)))
  (= (ldb (byte 1 0) (move-tag m)) 1))
(defun* (move-promotion -> boolean) ((m (unsigned-byte 18)))
  (= (ldb (byte 1 1) (move-tag m)) 1))
(defun* (move-ep -> boolean) ((m (unsigned-byte 18)))
  (= (ldb (byte 1 2) (move-tag m)) 1))
(defun* (move-castle -> boolean) ((m (unsigned-byte 18)))
  (= (ldb (byte 1 3) (move-tag m)) 1))
(defun* (make-move -> (unsigned-byte 16))
    ((from (unsigned-byte 7)) (to (unsigned-byte 7)) 
     (capture (unsigned-byte 1)) (promotion (unsigned-byte 1))
     (ep (unsigned-byte 1)) (castle (unsigned-byte 1)))
  (let ((m 0))
    (declare ((unsigned-byte 16) m))
    (setf (ldb (byte 7 0) m) from)
    (setf (ldb (byte 7 7) m) to)
    (setf (ldb (byte 1 14) m) capture)
    (setf (ldb (byte 1 15) m) promotion)
    (setf (ldb (byte 1 16) m) ep)
    (setf (ldb (byte 1 17) m) castle)
    m))
(declaim (inline move-from move-to move-tag move-capture move-promotion
		 move-ep move-castle make-move))
(defconstant max-move-count 218)
     
;; Generate moves using tables in piece.lisp
(defun* (generate-moves -> (vector (unsigned-byte 16))) ((turn-color (unsigned-byte 1)))
  (let ((moves-vector 
	 (make-array max-move-count
		     :element-type '(unsigned-byte 16)
		     :fill-pointer 0
		     :initial-element 0)))
    (loop 
       for square from 0 to 127 
       when (and (not (off-board square)) 
		 (= turn-color (piece-color (aref board square)))) do
	 (*let ((piece (unsigned-byte 4) (aref board square))
		(color (unsigned-byte 1) (piece-color piece))
		(type (unsigned-byte 3) (piece-type piece))
		(sliding boolean (aref sliding-piece type))
		(start (unsigned-byte 8) (aref step-offset type))
		(step (signed-byte 8) (aref piece-steps start))
		(iter-square (mod 128) square))
	       (loop while (/= step 0) do
		    (setf iter-square square)
		    (loop while (and (> (+ iter-square step) -1)
				     (< (+ iter-square step) 128)) do 
					; inner loop for different step amounts
			 (incf iter-square step) ; make one step
			 (if (or (off-board iter-square) ; jump off board
				 (and (not (blank-square iter-square)) ; non-blank square
				      (= color (square-color iter-square)))) ; hit own piece
			     (return) ; break	    
			     (progn ; make move
			       ; generate base more
			       (vector-push
				(make-move square iter-square ; to/from
					   (if (blank-square iter-square) 0 1) ; capture
					   0 ; promotion
					   0 ; ep
					   0) ; castle
				moves-vector) ; TODO: set move tag
			       (generate-pawn-special square moves-vector type)
			       (unless sliding (return))))) ; break if not sliding piece		   
		    (incf start) ; try next step
		    (setf step (aref piece-steps start)))))
    (sort moves-vector #'> #'move-capture)))
		    

(defun* (generate-pawn-special -> :void) 
    ((square (mod 128)) (moves-vector (simple-array (unsigned-byte 16 max-move-count)))
     (type (unsigned-byte 3)))
  (when (or (and (= type 1) (= (get-rank square) 6))
	    (and (= type 2) (= (get-rank square) 1)))
    (let ((increment (if (= type 1) 16 -16)))
      (when (blank-square (+ square increment))
	(vector-push
	 (make-move square (+ square increment)
		    0
		    0
		    0)
	 moves-vector)
	(when (blank-square (+ square (* 2 increment)))
	  (vector-push
	   (make-move square (+ square (* 2 increment))
		      0
		      1 ; ep move
		      0))))))
  (values)) ; return nothing
		    
