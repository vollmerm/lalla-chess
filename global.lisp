;;;; globals.lisp

(in-package #:lalla)

(defconstant *hash-size* 16777224) ;; size of hash table

(defconstant *piece-values*
  (make-array 8 
	      :initial-contents '(0 1 1 3 -1 3 5 9) 
	      :element-type (signed-byte 8)))
(defconstant *steps*
  (make-array 32
	      :initial-contents 
	      '(-16 -15 17 0 ;; downstream pawn
		16 15 17 0 ;; upstream pawn
		1 -1 16 -16 0 ;; rook
		1 -1 16 -16 15 -15 17 -17 0 ;; king/queen/bishop
		14 -14 18 -18 31 -31 33 -33 0 ;; knight
		-1 3 21 12 16 7 12) ;; directory
	      :element-type (signed-byte 8)))

(defparameter* (*updated-score* (signed-byte 32)) 0)
(defparameter* (*nodes* (signed-byte 32)) 0)
(defparameter* (*ep-flag* (signed-byte 8)) 0)
(defparameter* (*from* (signed-byte 8)) 0)
(defparameter* (*to* (signed-byte 8)) 0)

(defconstant *out-str* ".?+nkbrq?*?NKBRQ")

(defparameter *board*
  (make-array 129 :initial-element 0 :element-type (signed-byte 8)))

(defparameter *zobrist*
  (make-array 1035 :initial-element 0 :element-type (signed-byte 8)))

(defstruct hash-entry
  (key :type (signed-byte 32))
  (value :type (signed-byte 32))
  (from :type (signed-byte 8))
  (to :type (signed-byte 8))
  (draft :type (signed-byte 8)))

(defparameter *table*
  (make-array *hash-size* :element-type hash-entry))
(loop for i from 0 to (- *hash-size* 1) do 
     (setf (aref *table* i) (make-hash-entry)))
