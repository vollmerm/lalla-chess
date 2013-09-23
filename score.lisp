;;;; score.lisp

(in-package #:lalla)
(declaim (optimize speed))

(defparameter* (piece-scores (simple-array (signed-byte 16) (8)))
  (make-array 8
	      :element-type '(signed-byte 16)
	      :initial-contents '(0 100 100 320 325 500 975 1500)))

(defparameter* (pawn-table (simple-array (signed-byte 16) (136)))
  (make-array 136
	      :element-type '(signed-byte 16)
	      :initial-contents '(0 0 0 0 0 0 0 0
				    0 0 0 0 0 0 0 0
				    0 0 0 0 0 0 0 0
				    50 50 50 50 50 50 50 50
				    0 0 0 0 0 0 0 0
				    10 10 20 30 30 20 10 10
				    0 0 0 0 0 0 0 0
				    5 5 10 27 27 10 5 5
				    0 0 0 0 0 0 0 0
				    0 0 0 25 25 0 0 0
				    0 0 0 0 0 0 0 0
				    5 -5 -10 0 0 -10 -5 5
				    0 0 0 0 0 0 0 0
				    5 10 10 -25 -25 10 10 5
				    0 0 0 0 0 0 0 0
				    0 0 0 0 0 0 0 0
				    0 0 0 0 0 0 0 0)))

(defparameter* (knight-table (simple-array (signed-byte 16) (136)))
  (make-array 136
	      :element-type '(signed-byte 16)
	      :initial-contents '(0 0 0 0 0 0 0 0
				    -50 -40 -30 -30 -30 -30 -40 -50
				    0 0 0 0 0 0 0 0
				    -40 -50 0 0 0 0 -20 -40
				    0 0 0 0 0 0 0 0
				    -30 0 10 15 15 10 0 -30
				    0 0 0 0 0 0 0 0
				    -30 5 15 20 20 15 5 -30
				    0 0 0 0 0 0 0 0
				    -30 0 15 20 20 15 0 -30
				    0 0 0 0 0 0 0 0
				    -30 5 10 15 15 10 5 -30
				    0 0 0 0 0 0 0 0
				    -40 -20 0 5 5 0 -20 -40
				    0 0 0 0 0 0 0 0
				    -50 -40 -20 -30 -30 -20 -40 -50
				    0 0 0 0 0 0 0 0)))

(defparameter* (bishop-table (simple-array (signed-byte 16) (136)))
  (make-array 136
	      :element-type '(signed-byte 16)
	      :initial-contents '(0 0 0 0 0 0 0 0
				    -20 -10 -10 -10 -10 -10 -10 -20
				    0 0 0 0 0 0 0 0
				    -10 0 0 0 0 0 0 -10
				    0 0 0 0 0 0 0 0
				    -10 0 5 10 10 5 0 -10
				    0 0 0 0 0 0 0 0
				    -10 5 5 10 10 5 5 -10
				    0 0 0 0 0 0 0 0
				    -10 0 10 10 10 10 0 -10
				    0 0 0 0 0 0 0 0
				    -10 10 10 10 10 10 10 -10
				    0 0 0 0 0 0 0 0
				    -10 5 0 0 0 0 5 -10
				    0 0 0 0 0 0 0 0
				    -20 -10 -40 -10 -10 -40 -10 -20
				    0 0 0 0 0 0 0 0)))

(defparameter* (king-table (simple-array (signed-byte 16) (136)))
  (make-array 136
	      :element-type '(signed-byte 16)
	      :initial-contents '(0 0 0 0 0 0 0 0
				    -30 -40 -40 -50 -50 -40 -40 -30
				    0 0 0 0 0 0 0 0
				    -30 -40 -40 -50 -50 -40 -40 -30
				    0 0 0 0 0 0 0 0
				    -30 -40 -40 -50 -50 -40 -40 -30
				    0 0 0 0 0 0 0 0
				    -30 -40 -40 -50 -50 -40 -40 -30
				    0 0 0 0 0 0 0 0
				    -20 -30 -30 -40 -40 -30 -30 -20
				    0 0 0 0 0 0 0 0
				    -10 -20 -20 -20 -20 -20 -20 -10
				    0 0 0 0 0 0 0 0
				    20 20 0 0 0 0 20 20
				    0 0 0 0 0 0 0 0
				    20 30 10 0 0 10 30 20
				    0 0 0 0 0 0 0 0)))

(defparameter* (king-end-table (simple-array (signed-byte 16) (136)))
  (make-array 136
	      :element-type '(signed-byte 16)
	      :initial-contents '(0 0 0 0 0 0 0 0
				    -50 -40 -30 -20 -20 -30 -40 -50
				    0 0 0 0 0 0 0 0
				    -30 -20 -10 0 0 -10 -20 -30
				    0 0 0 0 0 0 0 0
				    -30 -10 20 30 30 20 -10 -30
				    0 0 0 0 0 0 0 0
				    -30 -10 30 40 40 30 -10 -30
				    0 0 0 0 0 0 0 0
				    -30 -10 30 40 40 30 -10 -30
				    0 0 0 0 0 0 0 0
				    -30 -10 20 30 30 20 -10 -30
				    0 0 0 0 0 0 0 0
				    -30 -30 0 0 0 0 -30 -30
				    0 0 0 0 0 0 0 0
				    -50 -30 -30 -30 -30 -30 -30 -50
				    0 0 0 0 0 0 0 0)))

(defun* (score-table-index -> (mod 136))
    ((index (mod 128)) (side (unsigned-byte 1)))
  (if (= side 0) (+ index 8) (- 127 index)))

(defun* (piece-score -> (signed-byte 16))
    ((piece (unsigned-byte 4)))
  (aref piece-scores (piece-type piece)))

(defun* (score-table -> (signed-byte 16))
    ((index (mod 128)) (piece (unsigned-byte 4)))
  (*let ((new-index (mod 136) (score-table-index index (piece-color piece)))
	 (type (unsigned-byte 3) (piece-type piece)))
	(cond ((= type 0) 0)
	      ((or (= type 1) (= type 2)) (aref pawn-table new-index))
	      ((= type 3) (aref knight-table new-index))
	      ((= type 4) (aref bishop-table new-index))
	      ((= type 5) 0)
	      ((= type 6) 0)
	      (t (aref king-table new-index)))))

(declaim (inline score-table piece-score score-table-index))

(defun* (score-square -> (signed-byte 16)) ((index (mod 128)) (side (unsigned-byte 1)))
  (*let ((piece (unsigned-byte 4) (get-piece index))
	 (color (unsigned-byte 1) (piece-color piece))
	 (mul (signed-byte 16) (if (= color side) 1 -1)))
	(* mul (+ (piece-score piece)
		  (score-table index piece)))))

(declaim (inline score-square))

(defun* (static-eval -> (signed-byte 16)) ((side (unsigned-byte 1)))
  (*let ((score (signed-byte 16) 0))
        (loop for square from 0 to 127
	      when (and (not (blank-square square))
			(not (off-board square))) 
	      do (incf score (score-square square side)))
	score))


