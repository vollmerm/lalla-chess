;;;; lalla.lisp

(in-package #:lalla)
(declaim (optimize speed))

(defparameter* (fail-amount (signed-byte 16)) 8000)

(defun* (static-eval -> (signed-byte 16)) ()
  )

(defun* (negamax -> (signed-byte 16)) 
    ((side (unsigned-byte 1)) (window-high (signed-byte 16)) 
     (window-low (signed-byte 16)) (depth-left (unsigned-byte 8)))
  (*let ((score (signed-byte 16) 
		(if (> dept-left 1)
		    (- fail-amount)
		    (static-eval)))
	 (moves (vector (unsigned-byte 16)) (generate-moves side)))
	(loop for current-move in moves do
	     ()	   
		
  ))
