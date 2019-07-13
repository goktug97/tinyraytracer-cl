(in-package :tinyraytracer-cl)

(defclass scene ()
  ((objects
    :initarg objects
    :initform '()
    :reader scene-objects)
   (lights
    :initarg lights
    :initform '()
    :reader scene-lights)))

(defun push-object (scene object)
  (with-slots (objects) scene
    (push object objects)))

(defun pop-object (scene)
  (pop (slot-value scene 'objects)))

(defun push-light (scene light)
  (push light (slot-value scene 'lights)))

(defun pop-light (scene)
  (pop (slot-value scene 'lights)))

(defun scene-intersect (ray scene)
  (let ((spheres-dist most-positive-single-float)
	(checkerboard-dist most-positive-single-float)
	(ray-direction (ray-direction ray))
	(ray-origin (ray-origin ray))
	(closest-object))
    (loop :for object :in (scene-objects scene)
       :for intersection-distance = (ray-intersect ray object)
       :when (and intersection-distance (< intersection-distance spheres-dist))
       :do
	 (progn
	    (setf spheres-dist intersection-distance)
	    (setf closest-object object)))
    (when closest-object
      (let* ((hit (m+ ray-origin (.* ray-direction spheres-dist)))
	     (normal (normalize (m- hit (object-center closest-object)))))
	(when (> (abs (vec-y ray-direction)) 1e-3)
	  (let* ((d (- (/ (+ (vec-y ray-origin) 4f0) (vec-y ray-direction))))
		 (pt (m+ ray-origin (.* ray-direction d))))
	    (when (and (< (abs (vec-x pt)) 10)
		       (< (vec-z pt) -10)
		       (> (vec-z pt) -30)
		       (< d spheres-dist))
	      (setf checkerboard-dist d
		    hit pt
		    normal (vec3f #(0f0 1f0 0f0)))
	      (setf (slot-value (object-material closest-object) 'diffuse-color)
		    (if (oddp (+ (truncate (* 0.5f0 (+ (vec-x hit) 1000)))
				 (truncate (* 0.5f0 (vec-z hit)))))
			(vec3f #(76.5f0 76.5f0 76.5f0))
			(vec3f #(76.5f0 51.0f0 25.5f0)))))))
	(when (< (min spheres-dist checkerboard-dist) 1000)
	  (values hit normal (object-material closest-object)))))))
