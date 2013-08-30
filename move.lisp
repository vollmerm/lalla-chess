;;;; move.lisp

(in-package #:lalla)

;; Moves are stored in 16-bit words.
;; They consist of a from square, a to square, and a special tag.
(declare (inline move-from move-to move-tag move-capture move-promotion make-move))
(defun* (move-from -> (unsigned-byte 6)) ((m (unsigned-byte 16)))
  (ldb (byte 6 0) m))
(defun* (move-to -> (unsigned-byte 6)) ((m (unsigned-byte 16)))
  (ldb (byte 6 6) m))
(defun* (move-tag -> (unsigned-byte 4)) ((m (unsigned-byte 16)))
  (ldb (byte 4 12) m))
(defun* (move-capture -> boolean) ((m (unsigned-byte 16)))
  (= (ldb (byte 1 2) (move-tag m)) 1))
(defun* (move-promotion -> boolean) ((m (unsigned-byte 16)))
  (= (ldb (byte 1 3) (move-tag m)) 1))
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
(defun* (generate-moves -> (vector (unsigned-byte 16))) ((square (mod 129)))
  (*let ((piece (unsigned-byte 4) (aref board square))
	 (color (unsigned-byte 1) (piece-color piece))
	 (type (unsigned-byte 3) (piece-type piece))
	 (sliding boolean (aref sliding-piece type))
	 (start (unsigned-byte 8) (aref step-offset type))
	 (step (unsigned-byte 8) (aref piece-steps start))
	 (moves-vector (vector (unsigned-byte 16))
		       (make-array max-move-count :fill-pointer 0)))
	))
