(in-package :linalg-cl)

(declaim (optimize (speed 3) (debug 0) (safety 0)))

(declaim (inline dot-product))
(defun dot-product (vec1 vec2)
  (declare (type (simple-array single-float) vec1 vec2))
  (the single-float (reduce #'+ (map 'simple-vector #'* vec1 vec2))))


(defmacro define-elementwise-vector-op (name op)
  `(defun ,name (&rest operands)
     (the (simple-array single-float)
	  (reduce
	   #'(lambda (vec1 vec2)
	       (declare (type (simple-array single-float) vec1 vec2))
	       (the (simple-array single-float)
		    (make-array (car (array-dimensions vec1))
				:element-type 'single-float
				:initial-contents
				(map 'simple-vector
				     #'(lambda (x1 x2)
					 (declare (type single-float x1 x2))
					 (funcall ,op x1 x2))
				     vec1 vec2))))
	   operands))))


(declaim (inline m+ m- m* m/))
(define-elementwise-vector-op m+ #'+)
(define-elementwise-vector-op m- #'-)
(define-elementwise-vector-op m* #'*)
(define-elementwise-vector-op m/ #'/)

(declaim (inline ./ .*))

(defun ./ (vec scalar)
  (declare (type (simple-array single-float) vec))
  (declare (type single-float scalar))
  (let* ((m (car (array-dimensions vec)))
	 (scalar-vec (make-array m :element-type 'single-float
				 :initial-element (coerce scalar 'single-float))))
    (declare (type (simple-array single-float) scalar-vec))
    (the (simple-array single-float) (m/ vec scalar-vec))))

(defun .* (vec &rest scalars)
  (declare (type (simple-array single-float) vec))
  (let* ((m (car (array-dimensions vec)))
	 (result vec))
    (declare (type (simple-array single-float) result))
    (loop for scalar in scalars
       for scalar-vec = 
	 (make-array m :element-type 'single-float
		     :initial-element (coerce scalar 'single-float))
       do (setf result (the (simple-array single-float) (m* result scalar-vec)))
	 finally (return result))))

(declaim (inline negative norm normalize cross))

(defun negative (vec)
  (declare (type (simple-array single-float) vec))
  (the (simple-array single-float) (make-array (car (array-dimensions vec))
					       :element-type 'single-float
					       :initial-contents
					       (map 'simple-vector
						    #'(lambda (x)
							(declare (type single-float x))
							(- x))
						    vec))))

(defun norm (vec)
  (declare (type (simple-array single-float) vec))
  (loop for elt across vec
     for y = (expt elt 2) then (+ y (expt elt 2))
     finally (return (the single-float (sqrt y)))))

(defun normalize (vec)
  (declare (type (simple-array single-float) vec))
  (the (simple-array single-float) (./ vec (the single-float (norm vec)))))

(defun cross (vec1 vec2)
  (declare (type (simple-array single-float) vec1 vec2))
  (let ((x1 (vec-x vec1))
	(y1 (vec-y vec1))
	(z1 (vec-z vec1))
	(x2 (vec-x vec2))
	(y2 (vec-y vec2))
	(z2 (vec-z vec2)))
    (declare (type single-float x1 y1 z1 x2 y2 z2))
    (the (simple-array single-float) (vec3f `#(,(- (* y1 z2) (* z1 y2))
					      ,(- (* z1 x2) (* x1 z2))
					      ,(- (* x1 y2) (* y1 x2)))))))

(declaim (inline vec2f vec3f vec4f vec-x vec-y vec-z))

(defmacro define-vector-of-size (size)
  (let ((name
	 (cond ((= size 2) (read-from-string "vec2f"))
	       ((= size 3) (read-from-string "vec3f"))
	       ((= size 4) (read-from-string "vec4f")))))
    `(defun ,name (data)
       (make-array ,size :element-type 'single-float :initial-contents data))))

(define-vector-of-size 2)
(define-vector-of-size 3)
(define-vector-of-size 4)

(defun vec-x (vec)
  (declare (type (simple-array single-float) vec))
  (the single-float (aref vec 0)))

(defun vec-y (vec)
  (declare (type (simple-array single-float) vec))
  (the single-float (aref vec 1)))

(defun vec-z (vec)
  (declare (type (simple-array single-float) vec))
  (the single-float (aref vec 2)))
