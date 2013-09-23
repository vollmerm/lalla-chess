;;;; board.lisp

(in-package #:lalla)
(declaim (optimize speed))

;; Upper-right corner of the board (A8) is index 0, so black pieces
;; are inserted at the beginning of the array and white at the end.
;; Downward moving pieces increase in index, and upward moving pieced
;; decrease in index.
(defparameter initial-positions
  #(13 11 12 14 15 12 11 13
     0  0  0  0  0  0  0  0
    10 10 10 10 10 10 10 10 ;; black pieces    
     0  0  0  0  0  0  0  0
     0  0  0  0  0  0  0  0
     0  0  0  0  0  0  0  0
     0  0  0  0  0  0  0  0
     0  0  0  0  0  0  0  0
     0  0  0  0  0  0  0  0
     0  0  0  0  0  0  0  0
     0  0  0  0  0  0  0  0
     0  0  0  0  0  0  0  0
     1  1  1  1  1  1  1  1
     0  0  0  0  0  0  0  0
     5  3  4  6  7  4  3  5 ;; white pieces
     0  0  0  0  0  0  0  0))
(defparameter* (board (simple-array (unsigned-byte 4) 128))
  (make-array 128 :element-type '(unsigned-byte 4) 
	      :initial-contents initial-positions))
(defun reset-board ()
  (loop for i from 0 to 127 do 
       (setf (aref board i) (aref initial-positions i))))

(defparameter* (w-castle boolean) nil)
(defparameter* (b-castle boolean) nil)
(defparameter* (board-ep boolean) nil)
(defparameter* (w-ep (mod 8)) 0)
(defparameter* (b-ep (mod 8)) 0)

(defun* (convert-128-to-64 -> (mod 64)) ((s (mod 128)))
  (+ (logand s 7) (* 8 (ash s -4))))

(defun* (off-board -> boolean) ((i (mod 128)))
  (/= (logand i #x88) 0))
(defun* (next-square -> (mod 128)) ((i (mod 128)))
  (logand (+ i 9) (lognot #x88)))
(defun* (get-piece -> (unsigned-byte 4)) ((i (mod 128)))
  (aref board i))
(defun* (get-rank -> (mod 8)) ((i (mod 128)))
  (/ (logand i #x70) 16))
(defun* (square-color -> (unsigned-byte 1)) ((i (mod 128)))
  (piece-color (get-piece i)))
(defun* (square-type -> (unsigned-byte 3)) ((i (mod 128)))
  (piece-type (get-piece i)))
(defun* (blank-square -> boolean) ((i (mod 128)))
  (= 0 (square-type i)))
(declaim (inline off-board next-square get-piece square-color square-type blank-square))

(defun* (make-move -> (unsigned-byte 4)) ((m (unsigned-byte 16)))
  (*let ((from (unsigned-byte 7) (move-from m))
	 (to (unsigned-byte 7) (move-to m))
	 (ep (unsigned-byte 1) (move-ep m))
	 (moving (unsigned-byte 4) (get-piece from))
	 (replaced (unsigned-byte 4) (get-piece to)))
	(setf (aref board from) 0)
	(setf (aref board to) moving)
	(when (= ep 1)
	  (setf board-ep t)
	  (let ((color (piece-color moving)))
	    (if (= color 0)
		(setf b-ep (mod to 8))
		(setf w-ep (mod to 8)))))
	replaced))
(defun* (unmake-move -> :void) ((m (unsigned-byte 16)) (r (unsigned-byte 4)))
  (*let ((from (unsigned-byte 7) (move-from m))
	 (to (unsigned-byte 7) (move-to m))
	 (ep (unsigned-byte 1) (move-ep m))
	 (moving (unsigned-byte 4) (get-piece to)))
	(setf (aref board from) moving)
	(setf (aref board to) r)
	(when (= ep 1) (setf board-ep nil))
	(values)))
