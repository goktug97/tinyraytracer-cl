(in-package :tinyraytracer-cl)

(defclass object ()
  ((center
    :initarg :center
    :type simple-array
    :reader :object-center)))

(defclass sphere (object)
  ((radius
    :initarg :radius
    :type single-float
    :reader :sphere-radius)))

(defclass ray ()
  ((origin
    :initarg :origin
    :type simple-array
    :reader :ray-origin)
   (direction
    :initarg :direction
    :type simple-array
    :reader :ray-direction)))

(defgeneric ray-intersect (ray sphere))

(defmethod ray-intersect ((ray ray) (sphere sphere))
  (let* ((sphere-center (slot-value sphere 'center))
	 (sphere-radius (slot-value sphere 'radius))
	 (ray-origin (slot-value ray 'origin))
	 (ray-direction (slot-value ray 'direction))
	 (vpc (m- sphere-center ray-origin))
	 (tca (dot-product vpc ray-direction))
	 (d2 (- (dot-product vpc vpc) (expt tca 2)))
	 (radius2 (expt sphere-radius 2)))
    (if (> d2 radius2)
	nil
	(let* ((thc (sqrt (- radius2 d2)))
	       (t0 (- tca thc)))
	  (when (< t0 0)
	    (setf t0 (+ tca thc)))
	  (if (< t0 0)
	      nil
	      t0)))))

(defparameter *sphere* (make-instance 'sphere
				      :center (vec3f #(0f0 0f0 30f0))
				      :radius 10f0))
(defparameter *ray* (make-instance 'ray
				   :direction (vec3f #(0f0 0f0 1f0))
				   :origin (vec3f #(0f0 0f0 0f0))))


