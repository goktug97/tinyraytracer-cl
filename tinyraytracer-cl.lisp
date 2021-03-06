;;;; tinyraytracer-cl.lisp

(in-package #:tinyraytracer-cl)

(declaim (optimize (speed 3) (safety 0) (debug 0)))

(defun render (scene)
  (let* ((width 1024)
	 (height 768)
	 (fov (/ (coerce pi 'single-float) 3f0))
	 (screen-width (* 2 (tan (/ fov 2))))
	 (dir-z (- (/ height screen-width)))
	 (center-x (/ width 2))
	 (center-y (/ height 2))
	 (rendered-image (make-instance 'zpng:png :height height :width width)))
    (loop for j upto (1- height) do
	 (loop for i upto (1- width)
	    for dir-x = (coerce (- (+ i 0.5) center-x) 'single-float)
	    and dir-y = (coerce (- center-y (+ j 0.5)) 'single-float)
	    for ray = (define-ray (vec3f #(0f0 0f0 0f0))
			  (normalize (vec3f `#(,dir-x ,dir-y ,dir-z))))
	    for color = (cast-ray ray scene)
	    do (progn
		 (let* ((max-color (max (vec-x color) (vec-y color) (vec-z color))))
		   (when (> max-color 255)
		     (setf color (.* color (/ 255f0 max-color)))))
		 (setf (aref (zpng:data-array rendered-image) j i 0)
		       (max 0 (min 255 (floor (vec-x color))))
		       (aref (zpng:data-array rendered-image) j i 1)
		       (max 0 (min 255 (floor (vec-y color))))
		       (aref (zpng:data-array rendered-image) j i 2)
		       (max 0 (min 255 (floor (vec-z color)))))
		 (print j)))
       finally (zpng:write-png rendered-image "./rendered_image.png"))))
