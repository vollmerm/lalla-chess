(defun make-bb-from-string (b)
  (declare (optimize speed (safety 0)) 
           (type string b))
  (the (unsigned-byte 64) (parse-integer b :radix 2)))

(defun make-bb-string-from-vector (v)
  (declare (type (simple-array (mod 2) (8 8)) v))
  (let ((ret ""))
    (dotimes (x 8)
      (dotimes (y 8)
        (setq ret (concatenate 'string ret 
                               (write-to-string (aref v x y))))))
    ret))

(defun make-bb (contents)
  (make-bb-from-string 
   (make-bb-string-from-vector
    (make-array '(8 8) :element-type '(mod 2) 
		:initial-contents contents))))

(defun transpose (x) (apply #'mapcar (cons #'list x)))
(defun inverse (x) (mapcar (lambda (y) (mapcar (lambda (z) (- 1 z)) y)) x))

(defconstant full-rank (list 1 1 1 1 1 1 1 1))
(defconstant empty-rank (list 0 0 0 0 0 0 0 0))


(defmacro make-precomputed-table (size name list)
  `(declaim (type (simple-array (unsigned-byte 64) (,size)) ,name))
  `(defconstant ,name
                (make-array ,size :element-type '(unsigned-byte 64)
                            :initial-contents (mapcar #'make-bb ,list))))


(defun 1-at (n)
  (let ((line empty-rank))
    (loop
       for cell on line
       for i from 0
       when (< i n) collect (car cell)
       else collect 1
       and nconc (rest cell)
       and do (loop-finish))))



(defconstant clear-rank-list
  (let ((rank-list '()))
    (dotimes (rank 8)
      (let ((this-list (make-list 8 :initial-element full-rank)))
	(setf (nth rank this-list) empty-rank)
	(push this-list rank-list)))
    rank-list))

(make-precomputed-table 8 clear-rank clear-rank-list)


(defconstant clear-file-list
            (mapcar #'transpose clear-rank-list))

(make-precomputed-table 8 clear-file clear-file-list)

(print clear-file-list)
(print (mapcar #'make-bb clear-file-list))

(defconstant mask-rank-list
             (mapcar #'inverse clear-rank-list))

(make-precomputed-table 8 mask-rank mask-rank-list)

(defconstant mask-file-list
             (mapcar #'transpose mask-rank-list))

(make-precomputed-table 8 mask-file mask-file-list)

(defun on-board (move-list)
  (remove-if (lambda (m) 
		   (let ((i (car m)) (j (cadr m)))
		     (or (> i 7) (< i 0)
			 (> j 7) (< j 0))))
		 move-list))

(defun position-list-to-board-list (p)
  (let ((board))
    (loop for i from 0 to 7 do
	 (let ((row))
	   (loop for j from 0 to 7 do
		(if (member (list i j) p :test #'equal)
		    (push 1 row)
		    (push 0 row)))
	   (push row board)))
    (nreverse board)))

(defun gen-move-list (func)
  (let ((moves))
    (loop for i from 0 to 7 do
	 (loop for j from 0 to 7 do
	      (let ((move-set (funcall func i j)))
		(push move-set moves))))
    moves))
  

(defun gen-king-moves (i j)
  (on-board  `((,(+ i 1) ,(+ j 1))
	       (,(+ i 1) ,j)
	       (,(+ i 1) ,(- j 1))
	       (,(- i 1) ,(+ j 1))
	       (,(- i 1) ,j)
	       (,(- i 1) ,(- j 1))
	       (,i ,(+ j 1))
	       (,i ,(- j 1)))))


(defun gen-knight-moves (i j)
  (on-board `((,(+ i 2) ,(+ j 1))
	      (,(+ i 1) ,(+ j 2))
	      (,(+ i 2) ,(- j 1))
	      (,(+ i 1) ,(- j 2))
	      (,(- i 2) ,(+ j 1))
	      (,(- i 1) ,(+ j 2))
	      (,(- i 2) ,(- j 1))
	      (,(- i 1) ,(- j 2)))))

(defun gen-pawn-diagonal-down (i j)
  (on-board `((,(+ i 1) ,(- j 1))
	      (,(+ i 1) ,(+ j 1)))))

(defun gen-pawn-diagonal-up (i j)
  (on-board `((,(- i 1) ,(- j 1))
	      (,(- i 1) ,(+ j 1)))))

(defun get-pawn-down (i j)
  (if (= i 1)
      (on-board `((,(+ i 2) ,j)
		  (,(+ i 1) ,j)))
      (on-board `((,(+ i 1) ,j)))))

(defun get-pawn-up (i j)
  (if (= i 6)
      (on-board `((,(- i 2) ,j)
		  (,(- i 1) ,j)))
      (on-board `((,(- i 1) ,j)))))

(defun gen-line-down (i j)
  (let ((moves))
    (loop for mi from (+ i 1) to 7 do
	 (push `(,mi ,j) moves))
    moves))

(defun gen-line-up (i j)
  (let ((moves))
    (loop for mi from (- i 1) downto 0 do
	 (push `(,mi ,j) moves))
    moves))

(defun gen-line-right (i j)
  (let ((moves))
    (loop for mj from (+ j 1) to 7 do
	 (push `(,i ,mj) moves))
    moves))

(defun gen-line-left (i j)
  (let ((moves))
    (loop for mj from (- j 1) downto 0 do
	 (push `(,i ,mj) moves))
    moves))

(defun gen-diag-down-right (i j)
  (let ((moves))
    (let ((mi (+ i 1))
	  (mj (+ j 1)))
      (loop while (and (<= mi 7) (<= mj 7)) do
	   (push `(,mi ,mj) moves)
	   (incf mi)
	   (incf mj)))
    moves))

(defun gen-diag-up-right (i j)
  (let ((moves))
    (let ((mi (- i 1))
	  (mj (+ j 1)))
      (loop while (and (>= mi 0) (<= mj 7)) do
	   (push `(,mi ,mj) moves)
	   (decf mi)
	   (incf mj)))
    moves))

(defun gen-diag-down-left (i j)
  (let ((moves))
    (let ((mi (+ i 1))
	  (mj (- j 1)))
      (loop while (and (<= mi 7) (>= mj 0)) do
	   (push `(,mi ,mj) moves)
	   (incf mi)
	   (decf mj)))
    moves))

(defun gen-diag-up-left (i j)
  (let ((moves))
    (let ((mi (- i 1))
	  (mj (- j 1)))
      (loop while (and (>= mi 0) (>= mj 0)) do
	   (push `(,mi ,mj) moves)
	   (decf mi)
	   (decf mj)))
    moves))

(defconstant king-moves-list
  (gen-move-list #'gen-king-moves))

(defconstant knight-moves-list
  (gen-move-list #'gen-knight-moves))   

(defconstant pawn-up-list
  (gen-move-list #'gen-pawn-up))

(defconstant pawn-down-list
  (gen-move-list #'gen-pawn-down))

(defconstant pawn-up-diag-list
  (gen-move-list #'gen-pawn-diagonal-up))

(defconstant pawn-down-diag-list
  (gen-move-list #'gen-pawn-diagonal-down))

(defconstant line-up-list
  (gen-move-list #'gen-line-up))

(defconstant line-down-list
  (gen-move-list #'gen-line-down))

(defconstant line-left-list
  (gen-move-list #'gen-line-left))

(defconstant line-right-list
  (gen-move-list #'gen-line-right))

(defconstant diag-up-right-list
  (gen-move-list #'gen-diag-up-right))

(defconstant diag-up-left-list
  (gen-move-list #'gen-diag-up-left))

(defconstant diag-down-right-list
  (gen-move-list #'gen-diag-down-right))

(defconstant diag-down-left-list
  (gen-move-list #'gen-diag-down-left))

(make-precomputed-table 
 64 king-positions (mapcar #'position-list-to-board-list king-moves-list))

(make-precomputed-table
 64 knight-positions (mapcar #'position-list-to-board-list knight-moves-list))

(make-precomputed-table
 64 pawn-up (mapcar #'position-list-to-board-list pawn-up-list))

(make-precomputed-table
 64 pawn-down (mapcar #'position-list-to-board-list pawn-down-list))

(make-precomputed-table
 64 pawn-up (mapcar #'position-list-to-board-list pawn-up-list))

(make-precomputed-table
 64 line-up-diag (mapcar #'position-list-to-board-list line-up-diag-list))

(make-precomputed-table
 64 pawn-down-diag (mapcar #'position-list-to-board-list pawn-down-diag-list))

(make-precomputed-table
 64 line-down (mapcar #'position-list-to-board-list line-down-list))

(make-precomputed-table
 64 line-left (mapcar #'position-list-to-board-list line-left-list))

(make-precomputed-table
 64 line-right (mapcar #'position-list-to-board-list line-right-list))

(make-precomputed-table
 64 diag-up-right (mapcar #'position-list-to-board-list diag-up-right-list))

(make-precomputed-table
 64 diag-up-left (mapcar #'position-list-to-board-list diag-up-left-list))

(make-precomputed-table
 64 diag-down-right (mapcar #'position-list-to-board-list diag-down-right-list))

(make-precomputed-table
 64 diag-down-left (mapcar #'position-list-to-board-list diag-down-left-list))

(defun position-list (bb)
  (declare (optimize speed)
	   (type (unsigned-byte 64) bb))
  (let ((position-list))
    (loop while (> bb 0) do
	 (let ((index (- (integer-length bb) 1)))
	   (push index position-list)
	   (setf bb (logxor bb (ash 1 index)))))
    position-list))

(defun position-board (bb)
  (format t "~%" #\linefeed)
  (print (position-list bb))
  (let ((list (position-list bb)))
    (loop for i from 64 downto 0 do
	 (if (find i list)
	     (format t "1 ")
	     (format t "0 "))
	 (if (= (mod i 8) 0)
	     (format t "~%" #\linefeed)))))

(print (mapcar #'position-list-to-board-list king-moves-list))
(print (mapcar #'make-bb (mapcar #'position-list-to-board-list king-moves-list)))
(print (mapcar #'position-list
	       (mapcar #'make-bb (mapcar #'position-list-to-board-list king-moves-list))))
(print (integer-length 4))
(print (logxor 4 (ash 1 (- (integer-length 4) 1))))
(print (position-list (aref knight-positions 33)))
(print (position-list (make-bb (list 
				empty-rank
				empty-rank
				empty-rank
				empty-rank
				empty-rank
				empty-rank
				empty-rank
				(list 1 0 0 0 0 0 0 1)))))
(print (position-board (aref knight-positions 33)))
(print (position-board (make-bb (list 
				empty-rank
				empty-rank
				empty-rank
				empty-rank
				empty-rank
				empty-rank
				empty-rank
				(list 1 0 0 0 0 0 0 1)))))
(print (position-board (aref line-up 33)))
