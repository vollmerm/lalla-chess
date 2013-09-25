;;;; board.lisp
;;;;
;;;; This file contains functions for manipulating the board.

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
(defparameter* (board (simple-array (unsigned-byte 4) (128)))
  (make-array 128 :element-type '(unsigned-byte 4) 
	      :initial-contents initial-positions))
(defun reset-board ()
  (loop for i from 0 to 127 do 
       (setf (aref board i) (aref initial-positions i))))


;; These global values represent part of the state of the board.
;; If board-ep is true, then an e.p. capture is possible. Similarly,
;; the w-castle and b-castle values represent whether either side
;; has castled.
(defparameter* (w-castle boolean) nil)
(defparameter* (b-castle boolean) nil)
(defparameter* (board-ep boolean) nil)
(defparameter* (square-ep (mod 128)) 0)



;; These are a bunch of utility functions, and they're marked to be
;; inlined because they are so short and used so often.
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
(defun* (is-pawn -> boolean) ((i (mod 128)))
  (*let ((type (square-type i)))
        (or (= type 1)
            (= type 2))))
(defun* (is-ep-move -> boolean) ((from (mod 128)) (to (mod 128)))
  (and (is-pawn from)
       (or (and (= (get-rank from) 1)
                (= (get-rank to) 3))
           (and (= (get-rank from) 6)
                (= (get-rank to) 4)))))
(declaim (inline off-board next-square get-piece square-color square-type
                 blank-square is-pawn))


;; Make-move and unmake-move are responsible for changing the state of
;; the board during searching. Everything that's done to the board needs
;; to be undone, or at least be able to be undone. For that reason, make-move
;; returns a number with the data necessary to restore the board state.
(defun* (make-move -> (unsigned-byte 12)) ((m (unsigned-byte 13)))
  ;; Bind some data to names so it is easier to work with
  (*let ((from (unsigned-byte 7) (move-from m)) ;; extract out move positions
	 (to (unsigned-byte 7) (move-to m))
	 (ep (unsigned-byte 1) (move-ep-bit m))
	 (moving (unsigned-byte 4) (get-piece from))
	 (replaced (unsigned-byte 12) (get-piece to))

         ;; Some information will have to be restored when this move is unmade,
         ;; and all that information is bundled up in the unsigned integer ret.
         ;; This data will be an extra parameter to unmake-move.
         (ret (unsigned-byte 13) replaced)) 

        ;; Handle e.p. capture
        (when (and board-ep
                   (= to square-ep))
          ;; This is an e.p. capture, on square square-ep
          (setf (ldb (byte 1 12) ret) 1) ; set flag in ret for e.p. capture 
          (if (= (get-rank to) 6)        ; set captured square to 0
              (setf (aref board (- to 16)) 0)
              (setf (aref board (+ to 16)) 0)))


        ;; Update board with new moved piece
	(setf (aref board from) 0)      ; zero out moved-from square
	(setf (aref board to) moving)   ; move piece to new position

        ;; Record old values for board-ep and square-ep to be restored
        (setf (ldb (byte 1 4) ret)
              (if board-ep 1 0))
        (setf (ldb (byte 7 5) ret) square-ep)        

        ;; This was an e.p. move, so set the global states accordingly
	(if (= ep 1)
            (progn
              (setf board-ep t)
              ; the square-ep value should be the square that was jumped over
              (setf square-ep
                    (if (= (get-rank to) 6) (- to 16) (+ to 16))))
            
            ;; This was not an e.p. move, so the board-ep value needs to
            ;; be nil. If it was true previously, it no longer applies,
            ;; because e.p. captures must be made immediately after the pawn moves.
            (setf board-ep nil))

        ;; ret has the data needed to restore board state, so it should be returned
	ret))

;; Unmake-move is responsible for restoring things to the way they were.
(defun* (unmake-move -> :void) ((m (unsigned-byte 18)) (r (unsigned-byte 13)))
  (*let ((from (unsigned-byte 7) (move-from m)) ;; bind these for convenience
	 (to (unsigned-byte 7) (move-to m))
	 (ep (unsigned-byte 1) (ldb (byte 1 4 r))) ;; restore to board-ep
         (sqep (unsigned-byte 7) (ldb (byte 7 5) r)) ;; restore to square-ep
         (epcap (unsigned-byte 1) (ldb (byte 1 12) r)) ;; was an ep capture done?
         (old-piece (unsigned-byte 4) (ldb (byte 4 0) r)) ;; piece replaced in move
	 (moving (unsigned-byte 4) (get-piece to)))

        ;; trivially "move the piece back" and restore the attacked square
	(setf (aref board from) moving)
	(setf (aref board to) old-piece)

        ;; restore ep state
	(setf board-ep ep)
        (setf square-ep sqep)

        ;; check if an ep capture occured
        (when (= epcap 1)
          ;; an ep capture occurred!
          ;; we don't need to have a saved copy of the captured square, because
          ;; it has to be a pawn. we can also conclude the color of the pawn 
          ;; based on where on the board the ep capture took place
          (if (= (get-rank to) 6)
              (setf (aref board (- to 16)) 1)
              (setf (aref board (+ to 16)) 10)))

        ;; return nothing
	(values)))

(defparameter* (board-piece-string string) "-p nbrqk  PNBRQK")

(defun* (board->string -> string) ()
  (with-output-to-string (stream)
      (loop for i from 0 to 127 
         when (not (off-board i)) do
           (progn
             (when (and (> i 0) (= (mod i 8) 0))
               (write-char #\return stream)
               (write-char #\linefeed stream))
             (write-char (char board-piece-string (get-piece i)) stream) ))))
