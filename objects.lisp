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
  ((center
    :initarg :center
    :type simple-array
    :reader object-center)
   (material
    :initarg :material
    :reader object-material)))

(defclass sphere (object)
  ((radius
    :initarg :radius
    :type single-float
    :reader sphere-radius)))

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

(defmethod ray-intersect ((ray ray) (sphere object))
  (let* ((sphere-center (slot-value sphere 'center))
	 (sphere-radius (slot-value sphere 'radius))
	 (ray-origin (slot-value ray 'origin))
	 (ray-direction (slot-value ray 'direction))
	 (vpc (m- sphere-center ray-origin))
	 (tca (dot-product vpc ray-direction))
	 (d2 (- (dot-product vpc vpc) (expt tca 2)))
	 (radius2 (expt sphere-radius 2)))
    (unless (> d2 radius2)
      (let* ((thc (sqrt (- radius2 d2)))
	     (t0 (- tca thc)))
	(when (< t0 0)
	  (setf t0 (+ tca thc)))
	(unless (< t0 0)
	  t0)))))

(defun define-ray (origin direction)
  (make-instance 'ray :direction direction :origin origin))


