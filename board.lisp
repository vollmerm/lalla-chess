(defconstant table64 '(63 0 58 1 59 47 53 2
                       60 39 48 27 54 33 42 3
      	               61 51 37 40 49 18 28 20
                       55 30 34 11 43 14 22 4
                       62 57 46 52 38 26 32 41
                       50 36 17 19 29 10 13 21
                       56 45 25 31 35 16 9 12
                       44 24 15  8 23 7 6 5))
(defconstant index64 (make-array 64 :element-type '(unsigned-byte 16)
                                :initial-contents table64))
(defconstant debruijn64 #x07EDD5E59A4E28C2)

(defun bitscan (bb)
  (declare (optimize speed)
	   (type (unsigned-byte 64) bb))
  (let ((ashbb (ash (ldb (byte 64 0)
			 (* (logand bb (- (lognot bb) 1))
			    debruijn64))
		    -58)))
    (aref index64 ashbb)))

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
	      (push (funcall func i j) moves)))
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

(defconstant king-moves-list
  (gen-move-list #'gen-king-moves))

(defun gen-knight-moves (i j)
  (on-board `((,(+ i 2) ,(+ j 1))
	      (,(+ i 1) ,(+ j 2))
	      (,(+ i 2) ,(- j 1))
	      (,(+ i 1) ,(- j 2))
	      (,(- i 2) ,(+ j 1))
	      (,(- i 1) ,(+ j 2))
	      (,(- i 2) ,(- j 1))
	      (,(- i 1) ,(- j 2)))))

(make-precomputed-table 
 64 king-positions (mapcar #'position-list-to-board-list king-moves-list))

(print (mapcar #'position-list-to-board-list king-moves-list))
(print (mapcar #'make-bb (mapcar #'position-list-to-board-list king-moves-list)))
