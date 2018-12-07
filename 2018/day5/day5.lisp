(load "/Users/roelant/quicklisp/setup.lisp")
(ql:quickload :cl-ppcre)

(defun get-file (filename)
  "read file line-by-line, return contents in a list"
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
       collect line)))


(defun isopposite (ch1 ch2)
  "determines if ch1 is an opposite of ch2"
  (if (equal (string-upcase ch1) (string-upcase ch2))
      (if (upper-case-p ch1)
	  (if (lower-case-p ch2)
	      T
	      nil)
	  (if (upper-case-p ch2)
	      (if (lower-case-p ch1)
		  T
		  nil)
	      nil))
      nil))


(defun eatopposites (list result stratfast)
  "eats a string like abBc and returns ac"
  (if (not (null list))
      (if (isopposite (first list) (first (rest list)))
	  (progn
	    (if (not (null stratfast))
		(eatopposites nil (append result (rest (rest list))) stratfast)
		(eatopposites (rest (rest list)) result stratfast))

	    )
	  (progn
	    (eatopposites (rest list) (append result (list (first list))) stratfast)
	    ))
      result))

(defun eatall (list)
  "repeats string reduction ad infinitum"
  (setf prevresult list)
  (setf pass 0)
  (setf stratfast nil)
  (loop while (or (not (equal (length (setf result (time (eatopposites prevresult () stratfast)))) (length prevresult))) (equal 0 pass))
     do
     ;;(if (and (< (length result) 15000) (> (length result) 12000))
       (if (equal 0 (mod pass 10))
	   (setf stratfast nil)
	   (setf stratfast T))
       (terpri)
       (write "pass ")
       (write pass)
       (write "  ")
       (write (length prevresult))
       (write "  ")
       (write stratfast)
       (setf pass (1+ pass))
       (setf prevresult result))
  prevresult)
(time (+ 1 2))

(length (eatall (coerce "dabAcCaCBAcCcaDA" 'list)))

(eatall (coerce "abBcCdef" 'list) T)
       
  (if (not (null list))
      (if (isopposite (first list) (first (rest list)))
	  (progn
	    (write "duplicates")
	    (eatopposites (rest (rest list)) result)
	    )
	  (progn
	    (write "not duplicates")
	    (eatopposites (rest list) (append result (list (first list))))
	    ))
      result))


(setf input (get-file "../day5/input.txt"))
(setf output (eatall (coerce (first input) 'list))
(length output)

(setf output2 (mapcar #'(lambda (ch1)
	    (list ch1
		  (length (eatall (remove ch1 (remove (coerce (string-upcase ch1) 'character) (coerce (first input) 'list))))))) (coerce "abcdefghijklmnopqrstuvwxyz" 'list)))

;; lowest of this sequence is the right answer for puzzle 2
(mapcar #'(lambda (x) (second x)) output2)
