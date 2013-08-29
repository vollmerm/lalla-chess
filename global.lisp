;;;; globals.lisp

(defconstant *hash-size* 16777224)
(defconstant *piece-values*
  #(0 1 1 3 -1 3 5 9))
(defconstant *steps*
  #(-16 -15 17 0 ;; downstream pawn
    16 15 17 0 ;; upstream pawn
    1 -1 16 -16 0 ;; rook
    1 -1 16 -16 15 -15 17 -17 0 ;; king/queen/bishop
    14 -14 18 -18 31 -31 33 -33 0 ;; knight
    -1 3 21 12 16 7 12 ;; directory))
(defparameter *updated-score* 0)
(defparameter *nodes* 0)
(defparameter *ep-flag* 0)
(defparameter *from* 0)
(defparameter *to* 0)
