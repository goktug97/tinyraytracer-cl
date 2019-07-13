;;;; tinyraytracer-cl.asd

(asdf:defsystem #:tinyraytracer-cl
  :description "My common lisp version of the tinyraytracer"
  :author "Goktug <karakasligk@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
	       (:file "linalg")
	       ;(:file "objects")
               (:file "tinyraytracer-cl")))
