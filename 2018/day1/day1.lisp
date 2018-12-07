(defun get-file (filename)
  "read file line-by-line, return contents in a list"
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
       collect (parse-integer line))))

(defun has-seen (l e)
  "determines whether e occurs in l"
  (if (null l)
      nil
      (if (equal (first l) e)
	  e
	  (has-seen (rest l) e))))

(defun has-double (l)
  "determines whether there is any double occurrence in list l"
  (if (= 0 (length l))
      nil
      (if (has-seen (rest l) (first l))
	  (first l)
	  (has-double (rest l))))
  )

(defun next-val (l)
  "returns next val from l, eventually resetting ptr if ptr is length l"
  (setf ptr (+ ptr 1))
  (if (= ptr (length l))
      (setf ptr 0))
  (elt l ptr))

;; puzzle 1: run on sample data
;; (setf inputdata (get-file "sample1.txt"))
;; (apply #'+ inputdata)

;; puzzle 1: run on real data
;; (setf inputdata (get-file "input.txt"))
;; (apply #'+ inputdata)


;; puzzle 2

(defvar ptr)
(defvar beginvar)
;;(next-val inputdata)

(defun find-doubles-in-file (filename)
  "solution of the second puzzle"
  (defvar inputdata)
  (setf beginvar 0)
   (setf ptr -1)
   (setf valuelist (list 0))
   (setf sumlist ())
   (setf inputdata (get-file filename))
   (loop
      while
	(not (member (setf newsum (reduce #'+ valuelist)) sumlist))
      do
;;	(write (length sumlist))
;;	(write " ")
	(setf sumlist (append sumlist (list newsum)))		 
	(setf valuelist (append valuelist (list (+ beginvar (next-val inputdata))))))
   newsum)

(defun find-doubles-in-file (filename)
  "solution of the second puzzle"
  (defvar inputdata)
  (setf beginvar 0)
   (setf ptr -1)
   (setf valuelist (list 0))
   (setf sumlist ())
   (setf inputdata (get-file filename))
   (setf newsum 0)
   (loop
      while
	(not (member (setf newsum (+ newsum (next-val inputdata))) sumlist))
      do
;;	(write (length sumlist))
;;	(write " ")
;;	(write newsum)
;;	(write " ")
	(setf sumlist (append sumlist (list newsum))))		 
   newsum)

(find-doubles-in-file "input.txt")
	
