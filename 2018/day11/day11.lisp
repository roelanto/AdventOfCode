(defun rackid (coord)
  "returns the rack ID: x-coord + 10"
  (+ 10 (first coord)))

(defun powerlevel (coord)
  "returns the powerlevel: rack-id + y-coord"
  (* (rackid coord) (second coord)))

(defun serial (coord serial)
  "returns the pwerlevel + serial"
  (+ (powerlevel coord) serial))

(defun hundreds-digit (number)
  "returns hundreds digit"
  (- (floor number 100) (* (floor number 1000) 10)))

(defun jumble (coord serial)
  "multiplies coord by rack id; takes hundreds digit; subtracts 5"
  (- (hundreds-digit (* (serial coord serial) (rackid coord)))5))

;; testing code
(defun tests ()
  ""
  ;; should return 4, -5, 0, 4
  (jumble '(3 5) 8)
  (jumble '(122 79) 58)
  (jumble '(217 196) 39)
  (jumble '(101 153) 71)))

(defun generate-field-as-list (xlim ylim serial)
  ""
  (loop for y from 1 to ylim
     collect
       (loop for x from 1 to xlim
	  collect
	    (jumble (list x y) serial)))
  )

(defun generate-field (xlim ylim serial)
  ""
  (let ((ar (make-array (list xlim ylim))))
    (loop for y from 1 to (1- ylim)
       do
	 (loop for x from 1 to (1- xlim)
	    do
	      (setf (aref ar x y) (jumble (list x y) serial))))
    ar))


(defun get-fuel-cell (l-o-l coord)
  "gets the values of square with top-left coord coord from l-o-l"
  (aref l-o-l (first coord) (second coord)))

(defun get-square-sum (l-o-l coord width)
  "gets the 3x3 square with t-l coord coord from l-o-l"
  (let ((miny (second coord))
	 (minx (first coord))
	 (maxy (+ (1- width) (second coord)))
	 (maxx (+ (1- width) (first coord))))
    (loop for y from miny to maxy
       sum 
	 (loop for x from minx to maxx
	    sum
	      (get-fuel-cell l-o-l (list x y))))))

(defun get-square (l-o-l coord width)
  "gets the 3x3 square with t-l coord coord from l-o-l"
  (let ((miny (second coord))
	 (minx (first coord))
	 (maxy (+ (1- width) (second coord)))
	 (maxx (+ (1- width) (first coord))))
    (loop for y from miny to maxy
       collect
	 (loop for x from minx to maxx
	    collect
	      (get-fuel-cell l-o-l (list x y))))))


(defun which (list elements num result)
  ""
  (if (null list)
      result
      (if (intersection (list (first list)) elements :test 'equal)
	  (which (rest list) elements (1+ num) (append result (list num)))
	  (which (rest list) elements (1+ num) result))))

(defun find-maxval (l-o-l)
  "finds the maximum value in a grid"
  (apply #'max  (mapcar #'(lambda (row) (apply #'max row)) l-o-l)))

(defun find-maxval-loop (l-o-l)
  "finds the maximum value in a grid"
  (loop for x from 1 to (1- (length l-o-l))
     maximizing
       (loop for y from 1 to (1- (length l-o-l))
	  maximizing
	    (nth x (nth y l-o-l)))))

;; find coord for max
(defun find-coord-with-value (l-o-l val)
  (block nested-loops
    (loop for y from 1 to (length (first l-o-l))
       collect
	 (loop for x from 1 to (length (first l-o-l))
	    do
	      (if (equal val (nth x (nth y l-o-l)))
		  (return-from nested-loops (list (1+ x) (1+ y))))))))

;; given in the exercise
(defparameter *serial* 7165) 
(defparameter *serial* 18)

(defun findpower (l-o-l size)
  
  (loop for x from 1 to 100
     do
        (get-square-sum l-o-l '(20 20) size)
       )
  )


(defun generate-power-cells (subgridsize)
    ""
  (let ((l-o-l (generate-field 300 300 *serial*)))
    (loop for y from 1 to (- 300 subgridsize)
       collect
	 (loop for x from 1 to (- 300 subgridsize)
	    collect
	      (get-square-sum l-o-l (list x y) subgridsize)))))



(defun get-max-power-with-size (size)
  (let* ((generated-power-cells (generate-power-cells size))
	 (maxval (find-maxval generated-power-cells))
	 (maxcoord (find-coord-with-value generated-power-cells maxval)))
    (format t "At size ~a the max is ~a at coord ~a~%" size maxval maxcoord)
    (list maxval maxcoord size)))

(defun print-result (result)
  "pretty-prints a result"
  (format t "~a,~a,~a~%"
	  (first (second result))
	  (second (second result))
	  (third result)))


;; testing statements:
;; (find-coord-with-value (generate-power-cells 3) (find-maxval (generate-power-cells 3)))
;; (generate-power-cells 3)
;; (get-max-power-with-size 250)

;; solution for day 11, 1.
(print-result (get-max-power-with-size 3))

;; solution for day 11, 2. Once it starts degrading beyond 0, maximum has been reached. 
(setf results (loop for x from 1 to 300 
		 collect
		   (let ((result (get-max-power-with-size x)))
		     (if (< (first result) 0)
			 (loop-finish)
			 result))))
(print-result (nth (first (which (mapcar #'first results) (list (apply #'max (mapcar #'first results))) 0 ())) results))
	  
