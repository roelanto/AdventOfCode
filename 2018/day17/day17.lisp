(load "/Users/roelant/quicklisp/setup.lisp")
(ql:quickload :cl-ppcre)

(defun parse-input (string)
  "parses the input string. If nil, check that there are parentheses and double backslashes in the regexp. "
  (setf (values match position)
	(cl-ppcre:scan-to-strings "\([xy]\)=(\\d*), \([xy]\)=(\\d*)\.\.(\\d*)$" string))
  (if (equal (aref position 0) "x")
      (convert-coordinates-xy (parse-integer (aref position 1))    (parse-integer (aref position 3))    (parse-integer (aref position 4)))
      (convert-coordinates-yx (parse-integer (aref position 1))    (parse-integer (aref position 3))    (parse-integer (aref position 4)))
      ))

 

  (list
   (aref position 0)
   (parse-integer (aref position 1))
   (aref position 2)
   (parse-integer (aref position 3))
   (parse-integer (aref position 4))))


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

(coerce "abc" 'list)
(defun convert-to-string (field coord water)
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
(check-for-water-string "SWSSWS" '(499 0) *rect*)

     
    
(defun convert-string-to-water-coords (string coord water)
  ""
  )

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

(cons "b" (cons "a" ()))
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


(rewrite (coerce "Xrl" 'list) (coerce "aaa" 'list) ())
	
	    (first pattern))
	    

(defun apply-rules (rules pattern)
  ""
  (setf result pattern)
  (loop for rule in rules
     do
       (if (match-rule-string (first rule) pattern)
	   (progn
	     (format t "rule ~a applies to pattern ~a ~%" rule pattern)
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

(simulate-game field 51)

(defparameter *minimum-y-coord* 14)

(defun find-water (coord water)
  (format t "Finding water at coord ~a (~a)~%" coord (convert-to-string field coord water))
  (setf new-block (apply-rules rules (convert-to-string field coord water)))
  (format t "Result of applyinf the rule: are pattern and rule the same? ~a~%" (equal new-block (convert-to-string field coord water)))
  (format t "result of checking for water string: ~a~%"       (check-for-water-string new-block coord *rect*))
  (if (equal new-block (convert-to-string field coord water))
      nil
      (check-for-water-string new-block coord *rect*)
      )
  )

(find-water '(499 0) water)

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
	   (loop for x from minx to maxx
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

(setf field (read-input))

(setf rules (list
	     '("XWXXSX" => "XWXXWX")
	     '("SWSYYY" => "WWSYYY")
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
	     '("XWXXBX" => "XIXXIX")
	     '("XWXXIX" => "XIXXIX")
	     ))

(simulate-game field 11)

(* 3 3 3 3 3 3)
