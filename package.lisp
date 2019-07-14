;;;; package.lisp

(defpackage #:linalg-cl
  (:use #:cl)
  (:export :dot-product
	   :m+ :m* :m- :m/
	   :./ :.*
	   :cross
	   :negative
	   :norm
	   :normalize
	   :vec-x
	   :vec-y
	   :vec-z
	   :vec2f
	   :vec3f
	   :vec4f))

(defpackage #:tinyraytracer-cl
  (:use #:cl
	#:linalg-cl)
  (:export :scene
	   :object
	   :sphere
	   :light
	   :render
	   :simple-scene
	   ))
