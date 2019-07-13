;;;; package.lisp

(defpackage #:linalg-cl
  (:use #:cl)
  (:export :dot-product
	   :m+ :m* :m- :m/
	   :./ :.*
	   :negative
	   :norm
	   :normalize
	   :Vec2f
	   :Vec3f
	   :Vec4f))

(defpackage #:tinyraytracer-cl
  (:use #:cl
	#:linalg-cl))
