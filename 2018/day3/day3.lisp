;; this code runs excruciating slow. Bruteforcing is punished here. 

(load "/Users/roelant/quicklisp/setup.lisp")
(ql:quickload :cl-ppcre)

(defun get-file (filename)
  "read file line-by-line, return contents in a list"
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
       collect line)))


(defun expand-list (alist)
  "expands a list like '(0 (1 2)) into '((0 1) (0 2))"
  (mapcar (lambda (x y) (list y x)) (first (last alist)) (make-list (length (first (last alist))) :initial-element (first alist))))

(defun expand (startx starty width height)
  "expands a given combination into its component coordinates"
  (apply #'append (mapcar #'expand-list
			  (loop :for x :below width :collect
			     (list (+ startx x)
				   (loop :for y :below height :collect (+ starty y)))))))

(defun expand-with-input (string)
  ""
  (setf parsedinput (parse-input string))
  (list (getf parsedinput :id) (expand (getf parsedinput :startx) (getf parsedinput :starty) (getf parsedinput :width) (getf parsedinput :height)))
  )


(defun duplicates (list element num)
  (if (null list)
      num
      (if (equal (first list) element)
	  (duplicates (rest list) element (1+ num))
	  (duplicates (rest list) element num))))


(defun is-one (list num)
  ""
  (if (null list)
      num
      (if (equal (first list) 1)
	  (is-one (rest list) (1+ num))
	  (is-one (rest list) num))))

(defun is-more-than-one (list num)
  ""
  (if (null list)
      num
      (if (> (first list) 1)
	  (is-more-than-one (rest list) (1+ num))
	  (is-more-than-one (rest list) num))))


(defun parse-input (string)
  "parses the input string"
  (setf (values match position)
	(cl-ppcre:scan-to-strings "#(.*) @ (\\d+),(\\d+): (\\d+)x(\\d+)" string))
  (list
   :id (parse-integer (aref position 0))
   :startx (parse-integer (aref position 1))
   :starty (parse-integer (aref position 2))
   :width (parse-integer (aref position 3))
   :height (parse-integer (aref position 4))
   ))

(defun seq (start length)
  ""
  (loop :for n :below length :collect (+ n start))
  )


(defun write-numeric-list(filename l)
  (with-open-file (out filename :direction :output :if-exists :append :if-does-not-exist :create)
    (dolist (segment l)
      (format out "~D~%" segment))
    (format out "~%")))

(write-numeric-list "/tmp/allcoords" allcoords)

(defun list-occupant-squares (string)
  ""
  (setf inputs (get-file string))
  (setf allsquares (mapcar #'expand-with-input inputs))
  (setf allindividualsquares (apply 'append (mapcar 'second allsquares)))
  (setf uniquesquares (remove-duplicates (apply 'append (mapcar 'second allsquares)) :test 'equal))
  (mapcar #'(lambda (x) (list x (length (intersection allindividualsquares (list x) :test 'equal)))) uniquesquares))
(setf allsquareswithoccupants (list-occupant-squares "../day3/input.txt"))

  
(defun findanswer1 (string)
  ""
  (setf inputs (get-file string))
  (setf allsquares (mapcar #'expand-with-input inputs))
  (setf allindividualsquares (apply 'append (mapcar 'second allsquares)))
  
  (setf uniquesquares (remove-duplicates (apply 'append (mapcar 'second allsquares)) :test 'equal))
  
  (mapcar #'(lambda (x) (length (intersection allindividualsquares (list x) :test 'equal))) uniquesquares)
  )
(setf occurrences (findanswer1 "../day3/input.txt"))
;; (write-numeric-list "../day3/occurrences"  occurrences)

(is-more-than-one  occurrences 0)

;; part 2


(defun eltmulti (list1 list2 retval)
  ""
  (if (null list2)
      retval
      (eltmulti list1 (rest list2) (append retval (list (elt list1 (first list2)))))))
  
(eltmulti occurrences '(0 1 2) ())

(defun which (list elements num result)
  ""
  (if (null list)
      result
      (if (intersection (list (first list)) elements :test 'equal)
	  (which (rest list) elements (1+ num) (append result (list num)))
	  (which (rest list) elements (1+ num) result))))

(defun numberofclaims (coords uniquesquares occurrences)
  "uniquesqares and occurrences should be a single "
  (eltmulti occurrences (which uniquesquares coords 0 ()) ()))

(setf string "../day3/input.txt")
(setf inputs (get-file string))
(setf allsquares (mapcar #'expand-with-input inputs))
(setf allindividualsquares (apply 'append (mapcar 'second allsquares)))
(setf uniquesquares (remove-duplicates (apply 'append (mapcar 'second allsquares)) :test 'equal))

(setf claims
      (mapcar #'(lambda(x) (/ (length x) (apply #'+ x)))
	(mapcar #'(lambda (x) (numberofclaims (first (rest x)) uniquesquares occurrences)) allsquares)
	))


;; watch out: lisp starts at 0 but ids at 1, so add a 1 to the final answer. 
(which claims '(1) 0 ())
      
