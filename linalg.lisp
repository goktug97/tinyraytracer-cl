(in-package :linalg-cl)

(defun dot-product (vec1 vec2 &aux (sum 0f0))
  (map 'vector #'(lambda (x1 x2)
		   (declare (type single-float x1 x2))
		   (incf sum (the single-float (* x1 x2))))
       vec1 vec2)
  (the single-float sum))

(defmacro define-elementwise-vector-op (name op)
  `(defun ,name (&rest operands)
     (reduce #'(lambda (vec1 vec2)
		 (map 'vector #'(lambda (x1 x2) (funcall ,op x1 x2)) vec1 vec2))
	     operands)))

(define-elementwise-vector-op m+ #'+)
(define-elementwise-vector-op m- #'-)
(define-elementwise-vector-op m* #'*)
(define-elementwise-vector-op m/ #'/)

(defun ./ (vec scalar)
  (let* ((m (car (array-dimensions vec)))
	 (scalar-vec (make-array m :element-type 'simple-array
				 :initial-element (coerce scalar 'single-float))))
    (m/ vec scalar-vec)))

(defun .* (vec &rest scalars)
  (let* ((m (car (array-dimensions vec)))
	 (result vec))
    (loop for scalar in scalars
       for scalar-vec = 
	 (make-array m :element-type 'simple-array
		     :initial-element (coerce scalar 'single-float))
       do (setf result (m* result scalar-vec))
	 finally (return result))))

(defun negative (vec)
  (map 'vector #'(lambda (x1) (- x1)) vec))

(defun norm (vec)
  (loop for elt across vec
     for y = (expt elt 2) then (+ y (expt elt 2))
     finally (return (sqrt y))))

(defun normalize (vec)
  (./ vec (norm vec)))

(defun cross (vec1 vec2)
  (let ((x1 (vec-x vec1))
	(y1 (vec-y vec1))
	(z1 (vec-z vec1))
	(x2 (vec-x vec2))
	(y2 (vec-y vec2))
	(z2 (vec-z vec2)))
    (vec3f `#(,(- (* y1 z2) (* z1 y2))
	     ,(- (* z1 x2) (* x1 z2))
	     ,(- (* x1 y2) (* y1 x2))))))

(defmacro define-vector-of-size (size)
  (let ((name
	 (cond ((= size 2) (read-from-string "vec2f"))
	       ((= size 3) (read-from-string "vec3f"))
	       ((= size 4) (read-from-string "vec4f")))))
    `(defun ,name (data)
       (make-array ,size :element-type 'simple-float :initial-contents data))))

(define-vector-of-size 2)
(define-vector-of-size 3)
(define-vector-of-size 4)

(defun vec-x (vec)
  (aref vec 0))

(defun vec-y (vec)
  (aref vec 1))

(defun vec-z (vec)
  (aref vec 2))
