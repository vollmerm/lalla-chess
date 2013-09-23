;;;; piece.lisp

(in-package #:lalla)
(declaim (optimize speed))

;; Pieces are 4-bit words
;; They consist of a color bit and a 3-bit piece type
(defun* (piece-color -> (unsigned-byte 1)) ((p (unsigned-byte 4)))
  (ldb (byte 1 3) p))
(defun* (piece-type -> (unsigned-byte 3)) ((p (unsigned-byte 4)))
  (ldb (byte 3 0) p))
(declaim (inline piece-color piece-type))

;; Piece steps define the number of steps each piece takes
(defparameter* (piece-steps (simple-array (signed-byte 16) 34)) 
    (make-array 34
                :element-type (signed-byte 16)
                :initial-contents
                #(15 17 0                        ;; white pawn
                  -15 -17 0                      ;; black pawn
                  14 18 31 33 -14 -18 -31 -33 0  ;; knight
                  15 17 -15 -17 0                ;; bishop
                  1 16 -1 -16 0                  ;; rook
                  1 16 15 17 -1 -16 -15 -17 0))) ;; king/queen
(defparameter* (step-offset (simple-array (mod 28) 8))
    (make-array 8
                :element-type '(mod 8)
                :initial-contents #(0 0 4 8 17 22 27 27)))
(defparameter* (sliding-piece (simple-array boolean 8))
    (make-array 8
                :element-type 'boolean
                :initial-contents #(nil nil nil nil t t t nil)))
