(in-package :tinyraytracer-cl)

(defclass material ()
  ((refractive-index
    :initarg :refractive-index
    :initform 1f0
    :reader refractive-index)
   (albedo
    :initarg :albedo
    :initform (vec4f #(1f0 0f0 0f0 0f0))
    :reader albedo)
   (diffuse-color
    :initarg :diffuse-color
    :reader diffuse-color)
   (specular-exponent
    :initarg :specular-exponent
    :initform 0f0
    :reader specular-exponent)))

(defclass object ()
  ((material
    :initarg :material
    :reader object-material)))

(defclass ray ()
  ((origin
    :initarg :origin
    :type simple-array
    :reader ray-origin)
   (direction
    :initarg :direction
    :type simple-array
    :reader ray-direction)))

(defclass light ()
  ((position
    :initarg :position
    :type simple-array
    :reader light-position)
   (intensity
    :initarg :intensity
    :type single-float
    :reader light-intensity)))

(defgeneric ray-intersect (ray object))

(defgeneric object-normal (ray object t0))

(defun define-ray (origin direction)
  (make-instance 'ray :direction direction :origin origin))


