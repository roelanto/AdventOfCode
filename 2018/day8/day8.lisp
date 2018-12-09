(load "/Users/roelant/quicklisp/setup.lisp")
(ql:quickload :cl-ppcre)

(defun get-file (filename)
  "read file line-by-line, return contents in a list"
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
       collect line)))

(defun parse-integers (s &optional (start 0))
  (loop with num do 
    (setf (values num start) (parse-integer s :start start :junk-allowed t))
     while num collect num))

(defun length-of-fragment (l)
  ""
  (+ 2 (second l)))

(defun store-sub-node (l num sub)
  ""
  (if (null (get-sub-node-cache l num))
      (setf subnodeslist (acons sub (list l num)  subnodeslist)))
  sub)

(defun get-sub-node-cache (l num)
  ""
  (first (rassoc (list l num) subnodeslist :test 'equal)))

(defun get-sub-node (l num)
  "returns the numth subnode of l"
  (defvar ptr)
  (if (null (setf result (get-sub-node-cache l num)))
      (progn 
	(setf ptr 2)
	(loop for x from 1 to (1- num)
	   do
	     (setf ptr (+ ptr (get-length-node (subseq l ptr) 0 0))))
	(store-sub-node l num (subseq l ptr)))
      result))

(defun get-length-node (l ptr len)
  ""
  (if (equal 0 (first l))
      (length-of-fragment l)
      (+ 2 (apply #'+ (loop for x from 1 to (first l)
			 collect (get-length-node (get-sub-node l x) 0 0))) (second l))))

(defun get-metadata-node (l ptr len)
  ""
  (if (equal 0 (first l))
      (subseq l 2 (+ 2 (second l)))
      (apply #'append
	     (append 
	      (loop for x from 1 to (first l)
		 collect
		   (get-metadata-node (get-sub-node l x) 0 0))
	      (list (subseq l (- (get-length-node l 0 0) (second l)) (get-length-node l 0 0)))))))


;; clear cache
(setf subnodeslist (list ))

;; set input
(setf input (parse-integers (first (get-file "../day8/input.txt"))))
;; for the example
;; (setf input '(2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2))

;; compute answer for exercise 1
(apply #'+ (get-metadata-node '(2 2 0 1 99 0 1 88 2 3 4 5 6 10 11 ) 0 0))
(apply #'+ (get-metadata-node input 0 0))

