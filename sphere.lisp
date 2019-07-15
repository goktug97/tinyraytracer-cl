(in-package :tinyraytracer-cl)

(declaim (optimize (speed 3) (safety 0) (debug 0)))

(defclass sphere (object)
  ((radius
    :initarg :radius
    :type single-float
    :reader sphere-radius)
   (center
    :initarg :center
    :reader sphere-center)))

(defmethod ray-intersect ((ray ray) (sphere sphere))
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
	     (t0 (- tca thc))
	     (hit (m+ ray-origin (.* ray-direction t0)))
	     (normal (normalize (m- hit sphere-center))))
	(when (< t0 0)
	  (setf t0 (+ tca thc)))
	(unless (< t0 0)
	  (list t0 hit normal))))))



