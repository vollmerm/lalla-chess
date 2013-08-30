;;;; piece.lisp

(in-package #:lalla)

;; Pieces are 4-bit words
;; They consist of a color bit and a 3-bit piece type
(declare (inline piece-color piece-type))
(defun* (piece-color -> (unsigned-byte 1)) ((p (unsigned-byte 4)))
  (ldb (byte 1 3) p))
(defun* (piece-type -> (unsigned-byte 3)) ((p (unsigned-byte 4)))
  (ldb (byte 3 0) p))

;; Piece steps define the number of steps each piece takes
(defconstant* (piece-steps (simple-array 24 (signed-byte 8)))
    (make-array 36
		:element-type (signed-byte 8)
		:initial-contents
		#(16 15 17 0                     ;; white pawn
		  -16 -15 -17 0                  ;; black pawn
		  14 18 31 33 -14 -18 -31 -33 0  ;; knight
		  15 17 -15 -17 0                ;; bishop
		  1 16 -1 -16 0                  ;; rook
		  1 16 15 17 -1 -16 -15 -17 0))) ;; king/queen
(defconstant* (step-offset (simple-array 8 (signed-byte 8)))
    (make-array 8
		:element-type (signed-byte 8)
		:initial-contents
		#(0 0 4 8 17 22 27 27)))
(defconstant* (sliding-piece (simple-array 8 boolean))
    (make-array 8
		:element-type boolean
		:initial-contents
		#(nil nil nil nil t t t nil)))
