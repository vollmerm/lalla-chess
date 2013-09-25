;;;; piece.lisp
;;;;
;;;; This file takes care of basic piece handling, and some stuff for move generation.
;;;; I'm on the fence about whether this file should exist or if it should be merged
;;;; into move.lisp.

(in-package #:lalla)
(declaim (optimize speed))

;; Pieces are 4-bit words
;; They consist of a color bit and a 3-bit piece type
;;
;; Piece types:
;;  1. White pawn
;;  2. Black pawn
;;  3. Knight
;;  4. Bishop
;;  5. Rook
;;  6. Queen
;;  7. King
(defun* (piece-color -> (unsigned-byte 1)) ((p (unsigned-byte 4)))
  (ldb (byte 1 3) p))
(defun* (piece-type -> (unsigned-byte 3)) ((p (unsigned-byte 4)))
  (ldb (byte 3 0) p))

(declaim (inline piece-color piece-type))

;; Piece steps define the number of steps each piece takes. The move generator
;; will start on one number and try the next and the next until it hits zero.
;; So, for example, if the move generator found a rook and wanted to generate
;; moves for it, it would look up the rook in the step-offset table below
;; and find that it would start with a step of 1. It would keep adding one to
;; the initial location of the rook until it hit a square it couldn't pass,
;; then it would start adding 16, and so on, until it reached a 0 in the
;; array below.
(defparameter* (piece-steps (simple-array (signed-byte 16) (34))) 
    (make-array 34
                :element-type '(signed-byte 16)
                :initial-contents '(-15 -17 0                      ;; white pawn
				    15 17 0                        ;; black pawn
				    14 18 31 33 -14 -18 -31 -33 0  ;; knight
				    15 17 -15 -17 0                ;; bishop
				    1 16 -1 -16 0                  ;; rook
				    1 16 15 17 -1 -16 -15 -17 0))) ;; king/queen

;; This array tells us the index for the above steps array that we should start at. So, a 
;; black pawn is represented by type=2, and it would start at index 3 above (which is the value 15).
(defparameter* (step-offset (simple-array (mod 26) (8)))
    (make-array 8
                :element-type '(mod 26)
                :initial-contents '(0 0 3 6 15 20 25 25)))

;; This array tells us whether each piece is a sliding piece.
(defparameter* (sliding-piece (simple-array boolean (8)))
    (make-array 8
                :element-type 'boolean
                :initial-contents '(nil nil nil nil t t t nil)))
