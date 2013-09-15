;;;; board.lisp

(defconstant initial-positions
  #( 5  3  4  6  7  4  3  5
     0  0  0  0  0  0  0  0
     1  1  1  1  1  1  1  1
     0  0  0  0  0  0  0  0
     0  0  0  0  0  0  0  0
     0  0  0  0  0  0  0  0
     0  0  0  0  0  0  0  0
     0  0  0  0  0  0  0  0
     0  0  0  0  0  0  0  0
     0  0  0  0  0  0  0  0
     0  0  0  0  0  0  0  0
     0  0  0  0  0  0  0  0
    10 10 10 10 10 10 10 10
     0  0  0  0  0  0  0  0
    13 11 12 15 14 12 11 13
     0  0  0  0  0  0  0  0))
(defparameter* (board (simple-array 128 (unsigned-byte 4)))
    (make-array 128 :element-type (unsigned-byte 4) 
		:initial-contents initial-positions))
(defun reset-board ()
  (loop for i from 0 to 127 do 
       (setf (aref board i) (aref initial-positions i))))

(defparameter* (w-castle boolean) nil)
(defparameter* (b-castle boolean) nil)

(declare (inline off-board next-square get-piece square-color square-type blank-square))
(defun* (off-board -> boolean) ((i (mod 128)))
  (/= (logand i #x88) 0))
(defun* (next-square -> (mod 128)) ((i (mod 128)))
  (logand (+ i 9) (lognot #x88)))
(defun* (get-piece -> (unsigned-byte 4)) ((i (mod 128)))
  (aref board i))
(defun* (square-color -> (unsigned-byte 1)) ((i (mod 128)))
  (piece-color (get-piece i)))
(defun* (square-type -> (unsigned-byte 3)) ((i (mod 128)))
  (piece-type (get-piece i)))
(defun* (blank-square -> boolean) ((i (mod 128)))
  (= 0 (square-type i)))
