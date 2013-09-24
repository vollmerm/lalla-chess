;; ui.lisp

(in-package #:lalla)

(defun convert-coordinates (r64 f64)
  (let ((sq (+ f64 (* 8 r64))))
    (+ sq (logand sq (lognot 7)))))

(defun parse-string (s)
  (let ((from-rank-char (char s 0))
	(from-file-char (char s 1))
	(to-rank-char (char s 3))
	(to-file-char (char s 4))
	(promotion-char (if (= (length s) 5)
		       (char s 5)
		       nil)))
    (let ((from-rank (position from-rank-char "hgfedcba"))
	  (from-file (position from-file-char "12345678"))
	  (to-rank (position to-rank-char "hgfedcba"))
	  (to-file (position to-file-char "12345678"))
	  (promotion (if promotion-char
			 (position promotion-char "___kbrq")
			 nil)))
      #((convert-coordinates from-rank from-file)
	(convert-coordinates to-rank to-file) promotion))))

(defun validate-player-move (mv side)
  (let ((valid nil)
	(moves (generate-moves side)))
    (block find-move
      (loop for current-move across moves do
	   (let ((from (move-from current-move))
		 (to (move-to current-move)))
	     (when (and (= from (elt mv 1))
			(= to (elt mf 2)))
	       (setf valid t)
	       (return-from find-move)))))
    valid))

(defun make-player-move (mv)
  )
