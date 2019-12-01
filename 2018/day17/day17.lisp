(load "/Users/roelant/quicklisp/setup.lisp")
(ql:quickload :cl-ppcre)
(ql:quickload "zpng")
(use-package '#:zpng)

(defun parse-input (string)
  "parses the input string. If nil, check that there are parentheses and double backslashes in the regexp. "
  (setf (values match position)
	(cl-ppcre:scan-to-strings "\([xy]\)=(\\d*), \([xy]\)=(\\d*)\.\.(\\d*)$" string))
  (if (equal (aref position 0) "x")
      (convert-coordinates-xy (parse-integer (aref position 1))    (parse-integer (aref position 3))    (parse-integer (aref position 4)))
      (convert-coordinates-yx (parse-integer (aref position 1))    (parse-integer (aref position 3))    (parse-integer (aref position 4)))
      ))

(defun get-file (filename)
  "read file line-by-line, return contents in a list"
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
       while line
       collect line)))

(defun convert-coordinates-xy (first-coord second-coord-from second-coord-to)
  "converts a list with 3 elements to an expanded list"
  (loop for x from second-coord-from to second-coord-to
     collect
       (list first-coord x)))

(defun convert-coordinates-yx (first-coord second-coord-from second-coord-to)
  "converts a list with 3 elements to an expanded list"
  (loop for x from second-coord-from to second-coord-to
     collect
       (list x first-coord)))

(defun read-input (&optional (filename  "../day17/sample.txt"))
  (remove-duplicates (apply #'append (mapcar #'parse-input (get-file filename)))))


(defun expand-to-rect-with-upper-left (coord)
  ""
  (apply 'append 
	 (loop for y from (second coord) to (+ 1 (second coord))
	    collect
	      (loop for x from (first coord) to (+ 2 (first coord))
		 collect		   
		   (list x y)))))

(defun convert-to-string (field coord water)
;;  (format t "convert-to-string ~a ~a ~a ~%" (length field) coord (length water))
  (concatenate 'string (mapcar #'(lambda (coord)
				   (if (> (second coord) *minimum-y-coord*)
				       #\B
				       (if (member coord field :test 'equal)
					   #\C
					   (if (member coord water :test 'equal)
					       #\W
					       #\S
					       )))) (expand-to-rect-with-upper-left coord))))

(defparameter *rect* '((0 0) (1 0) (2 0) (0 1) (1 1) (2 1)))

(defun check-for-water-string (tiles-string coord rect)
  (check-for-water (coerce tiles-string 'list) coord rect ()))
(defun check-for-water (tiles coord rect result)
  ""
  (if tiles
      (if (or
	   (equal (first tiles) #\I)
	   (equal (first tiles) #\W))
	  (progn
	    ;;	    (break)
	    (check-for-water (cdr tiles) coord (cdr rect) (append result
								  (list (list (+ (first coord) (first (first rect)))
									      (+ (second coord) (second (first rect)))))))
	    )
	  (check-for-water (cdr tiles) coord (cdr rect) result))
      result))

;; test code: (check-for-water-string "SWSSWS" '(499 0) *rect*)

     
    
(defun match-rule-string (rule-string pattern-string)
  ""
  (match-rule (coerce rule-string 'list) (coerce pattern-string 'list)))

(defun match-rule (rule pattern)
  ""
  (if (null rule)
      T
      (if (or
	   (equal (first rule) #\X)
	   (and
	    (equal (first rule) #\Y)
	    (or
	     (equal (first pattern) #\C)
	     (equal (first pattern) #\W)))
	   (equal (first rule) (first pattern)))
	  (match-rule (cdr rule) (cdr pattern))
	  nil)))


(defun rewrite-string (rule-string pattern-string)
  ""
  (coerce (rewrite (coerce rule-string 'list) (coerce pattern-string 'list) ()) 'string))

(defun rewrite (rule pattern result)
  ""
  (if rule
      (progn
	(if (or
	     (equal (first rule) #\X)
	     (equal (first rule) #\Y))
	    (rewrite (cdr rule) (cdr pattern) (append result (list (first pattern))))
	    (rewrite (cdr rule) (cdr pattern) (append result (list (first rule))))))
      result))



(defun apply-rules (rules pattern)
  ""
  (setf result pattern)
  (loop for rule in rules
     do
       (if (match-rule-string (first rule) pattern)
	   (progn
;;	     (format t "rule ~a applies to pattern ~a ~%" rule pattern)
	     (return (setf result (rewrite-string (third rule) pattern))))))
  result)

(apply-rules rules pattern)


(defun simulate-round (field water n)
  (let ((water
	 (sort-water
	  (remove-duplicates
	   (remove 'nil
		   (append water
			   
			   (loop for coord in water
			      collect
				(if (setf found (find-water (list (1- (first coord)) (second coord)) water))
				    (progn
				      (format t "found expansion rule, returning ~a~%" found)
				      (return found)))))):test 'equal))))
    (format t "~%ROUND ~a~%" n)
    (visualize field water)
    (if (< 0 n)
	(simulate-round field water (1- n)))
    water))

(setf water '((500 0)))
(setf water (sort-water
	     (remove-duplicates (append water
		    
		    (loop for coord in water
		       collect
			 (if (setf found (find-water (list (1- (first coord)) (second coord)) water))
			     (return found)))):test 'equal)))

(sort (copy-list water) #'(lambda (x y) (> (second x) (second y))))

(defun sort-water (water)
  "returns a sort of water, starting at bottom right"
  (sort (copy-list water) #'(lambda (x y) (if (= (second x) (second y))
					      (> (first x) (first y))
					      (> (second x) (second y)))))
  )


(defun simulate-game (field n)
  (let ((water (list '(500 0))))
    (simulate-round field water n)))

(trace simulate-water)
(defun simulate-water (field coord water old-pos)
  "simulates what happens if one drop drops"
;;  (format t "~%SIMULATE water is ~a~% coord is ~a~%old-pos is ~a~%" water coord old-pos)
  ;;(visualize field water)
  (setf now-water-seen (append now-water-seen (list coord)))
  (let ((rect-upper-left (list (1- (first coord)) (second coord))))
    
    (if (> *minimum-y-coord* (second coord))
	
	(let ((new-water (set-difference (remove coord (find-water rect-upper-left water) :test 'equal) water :test 'equal)))
	  ;;  (format t "Found new water: ~a~%" new-water)
	  (if (and
	       (null (member coord old-pos :test 'equal))
	       (< 0 (length new-water)))
	      (first (loop for new-coord in new-water
			collect
			  (simulate-water field new-coord (append water new-water) (append old-pos (list coord)))))
	      coord))
	coord
	)))
  

(defun bound (l &optional (min-y 1800))
  (remove 'nil (mapcar #'(lambda (el) (if (> min-y (second el)) el nil)) l)))

(bound '((1 1) (10 10) (100 100)))
(defun simulate-game-recursive (field  max-new)
  (setf ever-has-seen ())
  (let ((water '((500 0)))
	(new-water ()))
    (loop for x from 0 to max-new 
       do 
	 (progn
	   (format t "DROP BEGIN ~a~%" x)
	   (setf previous-now-water-seen (copy-list now-water-seen))
	   (setf now-water-seen ())
	   (setf prev-water (copy-list water))
	   (setf new-water (append water (list (simulate-water field (first water) water ()))))
;;	   (format t "DROP END ~a has seen ~a, which is ~a different from previous times~%" x (length (remove-duplicates now-water-seen :test 'equal)) (length (set-difference (remove-duplicates now-water-seen :test 'equal) (remove-duplicates previous-now-water-seen :test 'equal))))
;;	   (format t "The new we have seen: ~a~%"
;;		   (set-difference now-water-seen ever-has-seen :test 'equal))
	   (setf ever-has-seen (remove-duplicates (append ever-has-seen now-water-seen) :test 'equal))
	   (setf water new-water)
	   (if (= 0 (length (set-difference (bound water) (bound prev-water) :test 'equal)))
	       (progn
;;		 (break)
		 (format t "STOP CONDITION ~a~%" (length (remove-duplicates (bound ever-water-seen) :test 'equal)))))
	   (if (= 0 (mod x 1000))
	       (visualize-png field water x))
	   )))
  (format t "I have seen ~a~%" (length (remove-duplicates (bound ever-water-seen) :test 'equal))))
	
(untrace simulate-water)


(simulate-game-recursive field 5000)
(visualize field (remove-duplicates ever-water-seen :test 'equal))
(length (remove-duplicates ever-water-seen :test 'equal))

(visualize field (remove-duplicates ever-water-seen :test 'equal))

(setf water '((500 0)))
(setf water (append water (simulate-game-recursive field water)))

(simulate-game field 51)

(defparameter *minimum-y-coord* 1876)

(defun find-water (coord water)
;;  (format t "Finding water at coord ~a (~a)~%" coord (convert-to-string field coord water))
  (if (< *minimum-y-coord* (second coord))
      nil
      (progn 
	(setf new-block (apply-rules rules (convert-to-string field coord water)))
;;	(format t "Result of applying the rule: are pattern and rule the same? ~a~%" (equal new-block (convert-to-string field coord water)))
;;	(format t "result of checking for water string: ~a~%"       (check-for-water-string new-block coord *rect*))
	(if (equal new-block (convert-to-string field coord water))
	    nil
	    (check-for-water-string new-block coord *rect*)
	    ))))

(find-water '(499 0) water)

(append (mapcar 'first field) (mapcar 'first water))
(defun visualize-png (field water step)
  ""
  (let*
      ((minx (apply 'min (append (mapcar 'first field) (mapcar 'first water))))
       (maxx (apply 'max (append (mapcar 'first field) (mapcar 'first water))))
       (miny (apply 'min (append (mapcar 'second field) (mapcar 'second water))))
       (maxy (apply 'max (append (mapcar 'second field) (mapcar 'second water))))
       (height (+ 5 (- maxy miny)))
       (width (+ 5 (- maxx minx)))
       (file (concatenate 'string "output/pngfile-" (write-to-string step) ".png"))
       (png (make-instance 'png
			   :color-type :truecolor-alpha
			   :width width
			   :height height))
       (image (data-array png)))
    (format t "~a x ~a" width height)
    (mapcar #'(lambda (position) (progn
				   (let ((x (- (first position) minx))
					 (y (- (second position) miny)))
				     
				     (setf (aref image y x 0) 0)
				     (setf (aref image y x 1) 0)
				     (setf (aref image y x 2) 0)
				     (setf (aref image y x 3) 255)))) field)
    
    (mapcar #'(lambda (position) (progn
				   (let ((x (- (first position) minx))
					 (y (- (second position) miny)))
				     
				     (setf (aref image y x 3) 255)
				     (setf (aref image y x 2) 255)))) water)

    (write-png png file)))
(visualize-png field water 1)

(defun visualize (field water)
  "creates visualization of the field"
  (format t "~%")
  (let
      ((minx (apply 'min (mapcar 'first field)))
       (maxx (apply 'max (mapcar 'first field)))
       (miny (apply 'min (mapcar 'second field)))
       (maxy (apply 'max (mapcar 'second field))))
    (loop for y from miny to maxy
       do
	 (progn
	   (format t "~%~a " y)
	   (loop for x from minx to (+ 10 maxx)
	      do
		(let ((coord (list x y)))
		  (if (member coord water :test 'equal)
		      (format t "W")
		      (if (member coord field :test 'equal)
			  (format t "#")
			  (format t ".")
			  ))))))))

(visualize water field)

  )

(visualize water field)

(convert-to-string field '(499 0))

(setf field (read-input "input.txt"))
(setf field (read-input))

(setf rules (list
	     '("XWXXSX" => "XWXXWX")
	     '("SWSYYY" => "WWWYYY")
	     '("CWSCCC" => "CWWCCC")
	     '("SWYYYY" => "WWYYYY")
	     '("SWWYYY" => "WWWCCC")
	     '("XXWSWW" => "XXWWWW")
	     '("XXXSWW" => "XXXWWW")
	     '("XXXWSW" => "XXXWWW")
	     '("XXXWWS" => "XXXWWW")
	     '("SSWYYY" => "SWWYYY")
	     '("WSSYYY" => "WWSYYY")
	     '("YWSXXX" => "YWWXXX")
	     '("SWWXCW" => "WWWXCW")
	     ))

(simulate-game field 11)

(* 3 3 3 3 3 3)
