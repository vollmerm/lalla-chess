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
                         (position promotion-char "___nbrq")
                         nil)))
      (let ((from (convert-coordinates from-rank from-file))
            (to (convert-coordinates to-rank to-file)))
          #(from to promotion (is-ep-move from to) nil)))))

(defun validate-player-move (mv side)
  (let ((valid nil)
	(moves (generate-moves side)))
    (block find-move
      (loop for current-move across moves do
	   (let ((from (move-from current-move))
		 (to (move-to current-move)))
	     (when (and (= from (elt mv 0))
			(= to (elt mv 1)))
	       (setf valid t)
	       (return-from find-move)))))
    valid))

(defun make-player-move (mv side)
  (let ((from (elt mv 0))
        (to (elt mv 1))
        (promotion (elt mv 2))
        (ep (elt mv 3))
        (castle (elt mv 4))
        (move 0))
    (setf move (create-move from to
                            (if (blank-square to) 0 1)
                            (if promotion 1 0)
                            (if ep 1 0)
                            (if castle 1 0)))
    (make-move move)
    (when promotion
      (setf (aref board to) promotion))))
