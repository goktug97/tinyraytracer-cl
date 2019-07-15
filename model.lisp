(in-package :tinyraytracer-cl)

(declaim (optimize (speed 3) (safety 0) (debug 0)))

(defun delimiterp (c) (or (char= c #\Space) (char= c #\,)))

(defun parse-float (line)
  (with-input-from-string (s line)
    (loop
      :for num := (read s nil nil)
      :while num
      :return num)))

(defun split (string &key (delimiterp #'delimiterp))
  (loop :for beg = (position-if-not delimiterp string)
     :then (position-if-not delimiterp string :start (1+ end))
     :for end = (and beg (position-if delimiterp string :start beg))
     :when beg :collect (subseq string beg end)
     :while end))

(defun read-obj (path)
  (with-open-file (stream path)
    (do* ((line (read-line stream nil)
	       (read-line stream nil))
	  (split-line (split line)
		      (split line))
	 (faces)
	 (verts))
	((null line) (list faces verts))
      (when split-line
	(cond 
	      ((equal (coerce (elt split-line 0) 'character) #\v)
	       (push (vec3f `#(,(parse-float (elt split-line 1))
			      ,(parse-float (elt split-line 2))
			      ,(parse-float (elt split-line 3))))
		     verts))
	      ((equal (coerce (elt split-line 0) 'character) #\f)
	       (push (list (elt verts (1- (parse-float (elt split-line 1))))
			   (elt verts (1- (parse-float (elt split-line 2))))
			   (elt verts (1- (parse-float (elt split-line 3)))))
		     faces)))))))

(defclass model (object)
  ((model-path
    :initarg :model-path
    :initform (error "Model path must be provided"))
   (verts
    :reader model-verts)
   (faces
    :reader model-faces)))

(defmethod initialize-instance :after ((model model) &key model-path)
  (destructuring-bind (faces verts) (read-obj model-path)
    (setf (slot-value model 'verts) verts)
    (setf (slot-value model 'faces) faces)))

(defmethod ray-intersect ((ray ray) (model model))
  (let ((ray-direction (ray-direction ray))
	(ray-origin (ray-origin ray))
	(faces (model-faces model)))
    (loop :for face :in faces
       :for edge1 := (m- (elt face 1) (elt face 0))
       :and edge2 := (m- (elt face 2) (elt face 0))
       :for pvec := (cross ray-direction edge2)
       :for det := (dot-product edge1 pvec)
       :unless (< det 1e-5) :do
	 (let* ((tvec (m- ray-origin (elt face 0)))
		(u (dot-product tvec pvec)))
	   (unless (or (< u 0) (> u det))
	     (let* ((qvec (cross tvec edge1))
		    (v (dot-product ray-direction qvec)))
	       (unless (or (< v 0) (> (+ u v) det))
		 (let* ((tnear (* (dot-product edge2 qvec) (/ 1f0 det)))
			(hit (m+ ray-origin (.* ray-direction tnear)))
			(normal (normalize (cross edge1 edge2))))
		   (when (> tnear 1e-5)
		     (return (list tnear hit normal)))))))))))

(defun vert (face li)
  (elt face li))

(defun point (verts i)
  (elt verts i))
