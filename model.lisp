(in-package :tinyraytracer-cl)

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
	(cond ((equal (coerce (elt split-line 0) 'character) #\f)
	       (push (vec3f `#(,(parse-float (elt split-line 1))
			      ,(parse-float (elt split-line 2))
			      ,(parse-float (elt split-line 3))))
		     faces))
	      ((equal (coerce (elt split-line 0) 'character) #\v)
	       (push (vec3f `#(,(parse-float (elt split-line 1))
			      ,(parse-float (elt split-line 2))
			      ,(parse-float (elt split-line 3))))
		     verts)))))))
