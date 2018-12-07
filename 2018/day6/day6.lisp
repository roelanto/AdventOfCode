(load "/Users/roelant/quicklisp/setup.lisp")
(ql:quickload :cl-ppcre)

(defun parse-input (string)
  "parses the input string. If nil, check that there are parentheses and double backslashes in the regexp. "
  (setf (values match position)
	(cl-ppcre:scan-to-strings "(\\d*), (\\d*)$" string))
  (list
   (parse-integer (aref position 0))
   (parse-integer (aref position 1))))


(defun get-file (filename)
  "read file line-by-line, return contents in a list"
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
       collect line)))

(defun expand-list (alist)
  "expands a list like '(0 (1 2)) into '((0 1) (0 2))"
  (mapcar (lambda (x y) (list y x)) (first (last alist)) (make-list (length (first (last alist))) :initial-element (first alist))))

(defun expand (upperleft bottomright)
  "expands a given combination into its component coordinates"
  (let ((width (+ 1 (- (first bottomright) (first upperleft))))
	(height (+ 1 (- (second bottomright) (second upperleft))))
	(startx (first upperleft))
	(starty (second upperleft)))
    (apply #'append (mapcar #'expand-list
			    (loop :for x :below width :collect
			       (list (+ startx x)
				     (loop :for y :below height :collect (+ starty y))))))))
(defun which (list elements num result)
  ""
  (if (null list)
      result
      (if (intersection (list (first list)) elements :test 'equal)
	  (which (rest list) elements (1+ num) (append result (list num)))
	  (which (rest list) elements (1+ num) result))))


(defun get-owner (l)
  "find the owner or nil if there are multiple"
  
  (setf result (which l (list (apply 'min l)) 0 ()))
  (if (equal (length result) 1)
      result
      nil))

(defun delete-nth (n list)
  "Delete the nth element of a list (destructive)."
  (if (>= n (length list))
      list
      (setf (nthcdr n list) (nthcdr (1+ n) list))))

(defun remove-nth (n list)
  "Remove the nth elements (n is a list) of a list."
  (if (>= n (length list))
      list
      (let ((list (copy-tree list)))
	(setf (nthcdr n list) (nthcdr (1+ n) list))
	list)))

(defun remove-nth-list (n-list list)
  (remove-duplicates (apply #'append (mapcar #'(lambda (x) (remove-nth x list)) n-list))))

;; read 1 file
(setf coords (mapcar #'(lambda (line) (parse-input line)) (remove nil (get-file "../day6/input.txt"))))

;; determine boundaries
(setf gridupper (apply #'min (mapcar #'second coords)))
(setf gridleft (apply #'min (mapcar #'first coords)))
(setf gridbottom (apply #'max (mapcar #'second coords)))
(setf gridright (apply #'max (mapcar #'first coords)))
(setf grid (expand (list gridleft gridupper) (list gridright gridbottom)))

;; determine which center is closest to each point (= owner of that point)
(setf owners (reduce #'append (mapcar #'get-owner dists)))

;; determine how many centers belong to each owner (= determine size of each area)
(setf areas (loop for x from 0 to (reduce #'max (copy-list owners))
	       collect 
		 (length (intersection (append (copy-list owners)) (list x)))))

;; remove the boundary centers, because they can't be the solution says the exercise
(setf boundarycases (union
		     (union
		      (union
		       (which (mapcar #'first coords) (list gridleft) 0 ())
		       (which (mapcar #'first coords) (list gridright) 0 ()))
		      (which (mapcar #'second coords) (list gridupper) 0 ()))
		     (which (mapcar #'second coords) (list gridbottom) 0 ())))

;; solution for exercise 1
(apply #'max (remove-nth-list boundarycases areas))

;; solution for exercise 2
(setf dists (mapcar #'(lambda(y) (mapcar #'(lambda(x) (manhattandistance x y)) coords)) grid))
(reduce #'+ (mapcar #'(lambda (x) (if (> 10000 x) 1 0)) (mapcar #'(lambda (x) (reduce #'+ x)) dists) ))

