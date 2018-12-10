(load "/Users/roelant/quicklisp/setup.lisp")
(ql:quickload :cl-ppcre)
(ql:quickload "zpng")
(use-package '#:zpng)

(defun get-file (filename)
  "read file line-by-line, return contents in a list"
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
       while line
       collect line)))

(defun parse-input (string)
  "parses the input string. If nil, check that there are parentheses and double backslashes in the regexp. "
  (setf (values match position)
	(cl-ppcre:scan-to-strings "position=< *(-?\\d*), *(-?\\d*)> velocity=< *(-?\\d*), *(-?\\d*)>$" string))
  (list
   (list 
    (parse-integer (aref position 0))
    (parse-integer (aref position 1))
    )
   (list
    (parse-integer (aref position 2))
    (parse-integer (aref position 3))
    )))

(defun updateposition (position)
  ""
  (list 
   (list
    (+ (first (first position)) (first (second position)))
    (+ (second (first position)) (second (second position))))
   (second position)))

(defun updatepositions (positions)
  ""
  (loop for x from 0 to (1- (length positions))
     collect
       (updateposition (nth x positions))))

(defun hasposition (positions position)
  ""
  (member T (mapcar #'(lambda (x) (equal x position)) (mapcar #'first positions))))

(defun determine-bounds (list)
  ""
  (list 
   (apply #'min list)
   (apply #'max list)))

(defun determine-x-bounds (positions)
  (determine-bounds (mapcar #'first (mapcar #'first positions))))
(defun determine-y-bounds (positions)
  (determine-bounds (mapcar #'second (mapcar #'first positions))))

(defun correct (position xcorrect ycorrect &optional scale)
  ""
  (list
   (list
    (floor (+ xcorrect (first (first position))) scale)
    (floor (+ ycorrect (second (first position))) scale)
    )
   (second position)))


(defun project (positions &optional (scale 100))
  "computes projection of the data (start at (0 0)), scales."
  (let* ((minx (- 0 (first (determine-x-bounds positions))))
	 (miny (- 0 (first (determine-y-bounds positions)))))
    (mapcar #'(lambda (x) (correct x minx miny scale)) positions)))

(defun visualize (positions)
  ""
  (terpri)
  (loop for y from (first (determine-y-bounds positions)) to (second (determine-y-bounds positions))
     do
       (progn 
	 (loop for x from (first (determine-x-bounds positions)) to (second (determine-x-bounds positions))
	    do
	      (if (hasposition positions (list x y))
		  (format t "#")
		  (format t "."))
	      )
	 (terpri))))

(defun dimensions (positions)
  ""
  (list
   (1+ (- (second (determine-x-bounds positions)) (first (determine-x-bounds positions))))
   (1+ (- (second (determine-y-bounds positions)) (first (determine-y-bounds positions))))))


(defun visualize-png (positions step)
  ""
  (let* ((height (+ 2 (- (second (determine-y-bounds positions)) (first (determine-y-bounds positions)))))
	 (width (+ 2 (- (second (determine-x-bounds positions)) (first (determine-x-bounds positions)))))
	 (file (concatenate 'string "pngfile" (write-to-string step) ".png"))
	 (png (make-instance 'png
			     :color-type :grayscale-alpha
			     :width width
			     :height height))
	 (image (data-array png)))
    (mapcar #'(lambda (position) (progn
				   (setf (aref image
					       (second (first position))
					       (first (first position))
					       1 ) 255))) positions) (write-png png file))))

(defun determine-scale-factor (positions)
  ""
  (let*
      ((dimensions (dimensions positions)))
    (if (< 2048 (apply #'max dimensions))
	(/ 1 (/ 2048 (apply #'max (dimensions positions)))) 1 )))


;; or use sample.txt for samples. 
(setf positions (mapcar #'(lambda (line) (parse-input line)) (remove nil (get-file "../day10/input.txt"))))

(format t "")
(loop for step from 0 to 80000
   do
     (progn
       (if (equal 62 (apply #'max (dimensions positions)))
	   (format t "~a: ~a ~%" step (apply #'max (dimensions positions)))
	   )
       (if (> 1024 (apply #'max (dimensions positions)))
	   (visualize-png (project positions (determine-scale-factor positions)) step)
	   )
       (setf positions (updatepositions positions))))


(defun visualize-gif (positions step)
  (let* ((height (1+ (- (second (determine-y-bounds positions)) (first (determine-y-bounds positions)))))
	 (width (1+ (- (second (determine-x-bounds positions)) (first (determine-x-bounds positions)))))
         (data-stream (make-data-stream :height height
                                        :width width
                                        :color-table t))
         (image (make-image :height height :width width))
	 (white (ensure-color (rgb-color #xFF #xFF #xFF)
                              (color-table data-stream)))
         (green (ensure-color (rgb-color #x00 #xFF #x00)
			      (color-table data-stream)))
         (red (ensure-color (rgb-color #xFF #x00 #x00)
                            (color-table data-stream)))
         )
    ;;    (fill (image-data image) white)
    (setf myimagedata (coerce (make-my-image-data positions) '(array (unsigned-byte 8))))
    (setf image (make-image :height height
			    :width width
			    :top-position 0
			    :left-position 0
			    :image-data myimagedata))
    (add-image image data-stream)
    (output-data-stream data-stream (concatenate 'string "../day10/example-" (write-to-string step) ".gif"))))

