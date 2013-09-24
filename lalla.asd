;;;; lalla.asd

(asdf:defsystem #:lalla
  :serial t
  :description "A portable, modern Lisp chess AI"
  :author "Mike Vollmer <mike@recurial.com>"
  :license "Simplified BSD License"
  :depends-on (#:defstar)
  :components ((:file "package")
               (:file "piece")
	       (:file "board")
	       (:file "move")
	       (:file "score")
	       (:file "ui")
	       (:file "lalla")))

