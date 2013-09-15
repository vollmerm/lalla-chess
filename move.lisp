;;;; move.lisp

(in-package #:lalla)

;; Moves are stored in 16-bit words.
;; They consist of a from square, a to square, and a special tag.
(declare (inline move-from move-to move-tag move-capture move-promotion make-move))
(defun* (move-from -> (unsigned-byte 7)) ((m (unsigned-byte 16)))
  (ldb (byte 7 0) m))
(defun* (move-to -> (unsigned-byte 7)) ((m (unsigned-byte 16)))
  (ldb (byte 7 7) m))
(defun* (move-tag -> (unsigned-byte 2)) ((m (unsigned-byte 16)))
  (ldb (byte 2 14) m))
(defun* (move-capture -> boolean) ((m (unsigned-byte 16)))
  (= (ldb (byte 1 0) (move-tag m)) 1))
(defun* (move-promotion -> boolean) ((m (unsigned-byte 16)))
  (= (ldb (byte 1 1) (move-tag m)) 1))
(defun* (make-move -> (unsigned-byte 16))
    ((from (unsigned-byte 6)) (to (unsigned-byte 6)) 
     (tag (unsigned-byte 4)))
  (let ((m 0))
    (declare ((unsigned-byte 16) m))
    (setf (ldb (byte 6 0) m) from)
    (setf (ldb (byte 6 6) m) to)
    (setf (ldb (byte 4 12) m) tag)
    m))

;; Generate moves using tables in piece.lisp
(defconstant max-move-count 218)
(defun* (generate-moves -> (vector (unsigned-byte 16))) ((square (mod 128)))
  (*let ((piece (unsigned-byte 4) (aref board square))
	 (color (unsigned-byte 1) (piece-color piece))
	 (type (unsigned-byte 3) (piece-type piece))
	 (sliding boolean (aref sliding-piece type))
	 (start (unsigned-byte 8) (aref step-offset type))
	 (step (unsigned-byte 8) (aref piece-steps start))
	 (iter-square (mod 129) square)
	 (moves-vector (vector (unsigned-byte 16))
		       (make-array max-move-count :fill-pointer 0)))
	(loop while (/= step 0) do
	     (setf iter-square square)
	     (loop while (< (+ iter-square step) 129) do ; inner loop for different step amounts
		  (incf iter-square step) ; make one step
		  (if (or (off-board iter-square) ; jump off board
			  (and (not (blank-square iter-square)) ; non-blank square
			       (= color (square-color iter-square)))) ; hit own piece
		      (return) ; break	    
		      (progn ; make move
			(vector-push (make-move square iter-square 0) moves-vector) ; TODO: set move tag
			(unless sliding (return))))) ; break if not sliding piece		   
	     (incf start) ; try next step
	     (setf step (aref piece-steps start)))
	moves-vector)) ; return moves vector
		    
		    
