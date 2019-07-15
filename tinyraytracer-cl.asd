;;;; tinyraytracer-cl.asd

(asdf:defsystem #:tinyraytracer-cl
  :description "My common lisp version of the tinyraytracer"
  :author "Goktug <karakasligk@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on ("zpng"
	       "png-read")
  :components ((:file "package")
	       (:file "linalg")
	       (:file "objects")
	       (:file "sphere")
	       (:file "scene")
               (:file "tinyraytracer-cl")))
