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
     0  0  0  0  0  0  0  0  0))
(defparameter* (board (simple-array 129 (unsigned-byte 4)))
    (make-array 129 :element-type (unsigned-byte 4) 
		:initial-contents initial-positions))
(defun reset-board ()
  (loop for i from 0 to 128 do 
       (setf (aref board i) (aref initial-positions i))))

(defparameter* (w-castle boolean) nil)
(defparameter* (b-castle boolean) nil)

(declare (inline off-board next-square get-piece))
(defun* (off-board -> boolean) ((i (mod 129)))
  (/= (logand i #x88) 0))
(defun* (next-square -> (mod 129)) ((i (mod 129)))
  (logand (+ i 9) (lognot #x88)))
(defun* (get-piece -> (unsigned-byte 4)) ((i (mod 129)))
  (aref board i))
