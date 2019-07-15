(in-package :tinyraytracer-cl)

(declaim (optimize (speed 3) (safety 0) (debug 0)))

(defclass scene ()
  ((objects
    :initarg :objects
    :initform '()
    :reader scene-objects)
   (lights
    :initarg :lights
    :initform '()
    :reader scene-lights)
   (background
    :initarg :background
    :reader background)))

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
  (let ((object-dist most-positive-single-float)
	(checkerboard-dist most-positive-single-float)
	(ray-direction (ray-direction ray))
	(ray-origin (ray-origin ray))
	(closest-object)
	(hit)
	(normal)
	(material (make-instance 'material)))
    (loop :for object :in (scene-objects scene)
       :for intersection = (ray-intersect ray object)
       :when (and intersection (< intersection object-dist))
       :do
	 (progn
	   (setf object-dist intersection
		 closest-object object)))
    (when closest-object
      (setf material (object-material closest-object))
      (multiple-value-bind (point object-normal)
	  (object-normal ray closest-object object-dist)
	(setf hit point
	      normal object-normal)))
    (when (> (abs (vec-y ray-direction)) 1e-3)
      (let* ((d (- (/ (+ (vec-y ray-origin) 4f0) (vec-y ray-direction))))
	     (pt (m+ ray-origin (.* ray-direction d))))
	(when (and (> d 0)
		   (< (abs (vec-x pt)) 10)
		   (< (vec-z pt) -10)
		   (> (vec-z pt) -30)
		   (< d object-dist))
	  (setf checkerboard-dist d
		hit pt
		normal (vec3f #(0f0 1f0 0f0)))
	  (setf (slot-value material 'diffuse-color)
		(if (oddp (+ (truncate (+ 1000 (* 0.5f0 (vec-x hit))))
			     (truncate (* 0.5f0 (vec-z hit)))))
		    (vec3f #(76.5f0 76.5f0 76.5f0))
		    (vec3f #(76.5f0 51.0f0 25.5f0)))))))
    (when (< (min object-dist checkerboard-dist) 1000)
      (list hit normal material))))

(defun reflect (ray-direction normal)
  (m- ray-direction (.* (.* normal 2f0) (dot-product ray-direction normal))))

(defun refract (ray-direction normal eta-t &optional (eta-i 1f0))
  (let ((cosi (- (max -1f0 (min 1f0 (dot-product ray-direction normal))))))
    (if (< cosi 0)
	(refract ray-direction (negative normal) eta-i eta-t)
	(let* ((eta (/ eta-i eta-t))
	       (k (- 1 (* (expt eta 2) (- 1 (expt cosi 2))))))
	  (if (< k 0)
	      (vec3f #(1f0 0f0 0f0))
	      (m+ (.* ray-direction eta) (.* normal (- (* eta cosi) (sqrt k)))))))))

(defun cast-ray (ray scene &optional (depth 0))
  (let ((intersects (scene-intersect ray scene)))
    (if (and (< depth 5) intersects)
      (destructuring-bind (hit normal material) intersects
	(let* ((ray-direction (ray-direction ray))
	       (refractive-index (refractive-index material))
	       (diffuse-color (diffuse-color material))
	       (specular-exponent (specular-exponent material))
	       (albedo (albedo material))
	       (reflect-direction (normalize (reflect ray-direction normal)))
	       (refract-direction (normalize
				   (refract
				    ray-direction normal refractive-index)))
	       (offset (.* normal 1e-3))
	       (reflect-origin (if (< (dot-product reflect-direction normal) 0)
				   (m- hit offset)
				   (m+ hit offset)))
	       (refract-origin (if (< (dot-product refract-direction normal) 0)
				   (m- hit offset)
				   (m+ hit offset)))
	       (reflect-ray (define-ray reflect-origin reflect-direction))
	       (refract-ray (define-ray refract-origin refract-direction))
	       (reflect-color (cast-ray reflect-ray scene (+ depth 1)))
	       (refract-color (cast-ray refract-ray scene (+ depth 1)))
	       (diffuse-light-intensity 0f0)
	       (specular-light-intensity 0f0)
	       (lights (scene-lights scene)))
	  (loop for light in lights
	     for light-intensity = (light-intensity light)
	     and light-position = (light-position light)
	     for light-direction = (normalize (m- light-position hit))
	     and light-distance = (norm (m- light-position hit))
	     for shadow-origin = (if (< (dot-product light-direction normal) 0)
				     (m- hit offset)
				     (m+ hit offset))
	     for shadow-ray = (define-ray shadow-origin light-direction)
	     for intersects = (scene-intersect shadow-ray scene)
	     unless (and intersects
			 (destructuring-bind (shadow-hit _a _b) intersects
			   (declare (ignore _a _b))
			   (< (norm (m- shadow-hit shadow-origin))
			      light-distance)))
	     do (progn
		  (incf diffuse-light-intensity
			(* light-intensity
			   (max 0f0 (dot-product light-direction normal))))
		  (unless (= specular-exponent 0f0)
		    (incf specular-light-intensity
			  (* (expt (max 0
					(dot-product
					 (negative
					  (reflect (negative light-direction) normal))
					 ray-direction))
				   specular-exponent)
			     light-intensity))))
	     finally (return
		       (m+ (.* diffuse-color
			       diffuse-light-intensity
			       (aref albedo 0))
			   (.* (vec3f #(255f0 255f0 255f0))
			       specular-light-intensity
			       (aref albedo 1))
			   (m+ (.* reflect-color
				   (aref albedo 2))
			       (.* refract-color
				   (aref albedo 3))))))))
      (let* ((background-data (background scene))
	     (ray-direction (normalize (ray-direction ray)))
	     (env-shape (array-dimensions background-data))
	     (env-width (first env-shape))
	     (env-height (second env-shape))
	     (i (floor (* env-width (+ 0.5 (/ (atan (vec-z ray-direction)
						    (vec-x ray-direction))
					      (* 2 (coerce pi 'single-float)))))))
	     (j (floor (* (/ (acos (vec-y ray-direction))
			     (coerce pi 'single-float))
			  env-height)))
	     (red (coerce (aref background-data i j 0) 'single-float))
	     (green (coerce (aref background-data i j 1) 'single-float))
	     (blue (coerce (aref background-data i j 2) 'single-float)))
	(vec3f `#(,red ,green ,blue))))))

(defun simple-scene ()
  (let* ((ivory (make-instance 'material
			       :refractive-index 1f0
			       :albedo (vec4f #(0.6 0.3 0.1 0f0))
			       :diffuse-color (vec3f #(102f0 102f0 77f0))
			       :specular-exponent 50f0))
	 (glass (make-instance 'material
			       :refractive-index 1.5f0
			       :albedo (vec4f #(0f0 0.5 0.1 0.8))
			       :diffuse-color (vec3f #(153f0 179f0 204f0))
			       :specular-exponent 125f0))
	 (red-rubber (make-instance 'material
				    :refractive-index 1f0
				    :albedo (vec4f #(0.9 0.1 0f0 0f0))
				    :diffuse-color (vec3f #(77f0 22.5 22.5))
				    :specular-exponent 10f0))
	 (mirror (make-instance 'material
				:refractive-index 1f0
				:albedo (vec4f #(0f0 10.0 0.8 0f0))
				:diffuse-color (vec3f #(255f0 255f0 255f0))
				:specular-exponent 1425f0))
	 (sphere-1 (make-instance 'sphere
				  :radius 2
				  :center (vec3f #(-3f0 0f0 -16f0))
				  :material ivory))
	 (sphere-2 (make-instance 'sphere
				  :radius 2
				  :center (vec3f #(-1f0 -1.5 -12f0))
				  :material glass))
	 (sphere-3 (make-instance 'sphere
				  :radius 3
				  :center (vec3f #(1.5 -0.5 -18f0))
				  :material red-rubber))
	 (sphere-4 (make-instance 'sphere
				  :radius 4
				  :center (vec3f #(7f0 5f0 -18f0))
				  :material mirror))
	 (duck (make-instance 'model :model-path "./duck.obj" :material glass))
	 (light-1 (make-instance 'light :intensity 1.5
				 :position (vec3f #(-20f0 20f0 20f0))))
	 (light-2 (make-instance 'light :intensity 1.8
				 :position (vec3f #(30f0 50f0 -25f0))))
	 (light-3 (make-instance 'light :intensity 1.7
				 :position (vec3f #(30f0 20f0 30f0))))
	 (scene (make-instance 'scene
			       :objects (list sphere-1 sphere-2 sphere-3 sphere-4 )
			       :lights (list light-1 light-2 light-3)
			       :background (png-read:image-data
					    (png-read:read-png-file "envmap.png")))))
    scene))
