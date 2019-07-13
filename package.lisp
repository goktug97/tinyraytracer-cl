;;;; package.lisp

(defpackage #:linalg-cl
  (:use #:cl)
  (:export :dot-product
	   :m+ :m* :m- :m/
	   :./ :.*
	   :negative
	   :norm
	   :normalize
	   :vec-x
	   :vec-y
	   :vec-z
	   :Vec2f
	   :Vec3f
	   :Vec4f))

(defpackage #:tinyraytracer-cl
  (:use #:cl
	#:linalg-cl)
  (:export :scene
	   :scene-objects
	   :ray-intersect
	   :ray
	   :ray-direction
	   :ray-origin))
