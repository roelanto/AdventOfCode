(load "/Users/roelant/quicklisp/setup.lisp")
(ql:quickload :cl-ppcre)

(defun maxint (list)
  "Returns maximum of given list of integers, discards nils."
  (first  (sort (remove nil (copy-list list)) '>)))


(defun get-file (filename)
  "read file line-by-line, return contents in a list"
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
       collect line)))

(defun parse-input (string)
  "parses the input string"
  (setf (values match position)
	(cl-ppcre:scan-to-strings "[\d-(\d+)-\(d+) (\d\d):(\d\d)] .+$" string))
  (list
   (parse-integer (aref position 0))
   (parse-integer (aref position 1)))
   :starty (parse-integer (aref position 2))
   :width (parse-integer (aref position 3))
   :height (parse-integer (aref position 4))
   ))

(defun parse-event (string)
  "parses event"
  (setf (values match position) (cl-ppcre:scan-to-strings ".(\\d+)-(\\d+)-(\\d+) (\\d\\d):(\\d\\d). (.+)$" string))
  (setf logline (list :year (parse-integer (aref position 0))
		      :month (parse-integer (aref position 1))
		      :day (parse-integer (aref position 2))
		      :hour (parse-integer (aref position 3))
		      :minutes (parse-integer (aref position 4))
		      :event (aref position 5)
		      :monthday (cons (parse-integer (aref position 1)) (parse-integer (aref position 2)))
		      ))
  (setf currentevent "change")
  (write (getf logline :event))
  (if (not (null (setf (values match position) (cl-ppcre:scan-to-strings "Guard .(\\d+).+" (getf logline :event)))))
      (setf currentguard (parse-integer (aref position 0)))
      (if (not (null (setf (values match position) (cl-ppcre:scan-to-strings "falls asleep" (getf logline :event)))))
	  (setf currentevent "asleep")
	  (setf currentevent "wakes up")))
  (let ((timesincelastevent (- (getf logline :minutes) timeoflastevent)))
      (setf timeoflastevent (getf logline :minutes))
      (list :id currentguard :event currentevent :monthday (getf logline :monthday) :minutes (getf logline :minutes) :timesincelastevent timesincelastevent)))


(defun wasasleep (log)
  ""
  (if (equal (getf log :event) "wakes up")
      (progn
	(setf allminutes (make-list 60 :initial-element 0))
	(setf startofsleep (- (getf log :minutes) (getf log :timesincelastevent)))
	(loop :for x :below (getf log :timesincelastevent) :collect (setf (nth (+ startofsleep x) allminutes) 1))
	(list (getf log :id) (getf log :monthday) (getf log :timesincelastevent) allminutes))
      nil))

(setf lines (get-file "../day4/inputsorted.txt"))
(setf lines (get-file "../day4/sample1.txt"))

(setf currentevent "")
(setf currentguard 0)
(setf timeoflastevent 0)

;; read the lines
(setf parsedlines (mapcar #'parse-event lines))
;; parse the loglines to determine who was asleep when
(setf whenasleep (mapcar #'wasasleep parsedlines))

;; find all the days and all the guards that we have log-lines on
(setf alldays (remove-duplicates (mapcar #'second whenasleep) :test 'equal))
(setf allguards (remove-duplicates (mapcar #'second parsedlines)))

(defun cumasleep (guardid whenasleep)
  "Filter out the when-asleep records for guardid from the list
whenasleep, returns list of when-asleep records, one list per day,
each list representing for each minute whether the guard was asleep or
not: (0 0 0 0) (0 0 1 0 0 ) etc. "
  (mapcar #'(lambda (x) (if (equal (first x) guardid)
			    (third x)
			    0)) whenasleep)
  )

(defun aggregatebyday (guardid dayid whenasleep)
  "Returns the WHENASLEEP records for a given guard (GUARDID) and a given day
(DAYID).  The DAYID is a list that looks like (11 . 15) for november
15th. Returns all by-minute logs for the given day."
  (mapcar #'(lambda (x) (if (equal (first x) guardid)
			    (if (equal (second x) dayid)
				(progn
				  (fourth x))
				nil)
			    nil)
		    ) whenasleep))


(defun sleepiestguard (whenasleep allguards)
  "Returns a list of lists, with each embedded list representing the
guard (first element) and his cumulative sleeptime (second element)."
  (mapcar #'(lambda (y) (list (first y) (apply #'+ (second y)))) (mapcar #'(lambda (x) (list x (cumasleep x whenasleep))) allguards))
  )



(defun whensleepiest (guardid alldays whenasleep)
  "Returns a list of lists, with each embedded list representing the sleepiest minute (first element) and total sleeptime (second element) over all days."
  (setf byday (remove nil (apply #'append (mapcar #'(lambda (day)  (aggregatebyday guardid day whenasleep)) alldays))))
  (if (not (null byday))
      (progn
	(setf hourscum (apply #'mapcar #'+ byday))
	(list (which hourscum (list (first (sort (copy-list hourscum) '>))) 0 ()) (first (sort (copy-list hourscum) '>)))
	)
      nil
      ))

(defun whensleepiestbyminute (guardid alldays whenasleep)
  "Returns a list of lists, with each embedded list representing the sleepiest minute (first element) and total sleeptime (second element) over all days."
  (setf byday (remove nil (apply #'append (mapcar #'(lambda (day)  (aggregatebyday guardid day whenasleep)) alldays))))
  (if (not (null byday))
      (progn
	(apply #'mapcar #'+ byday)
	)
      nil
      ))

; which guard is the sleepiest?
(setf guardid (first (first (sort (sleepiestguard whenasleep allguards) (lambda(x y) (< (second y) (second x)))))))

; how sleepy is the guard?
(* guardid (first (first (whensleepiest guardid alldays whenasleep))))

; which guard has the the sleepiest minute?
(setf guardindex (which (mapcar #'maxint (mapcar #'(lambda (x) (whensleepiestbyminute x alldays whenasleep)) allguards))
			(list (maxint (mapcar #'maxint (mapcar #'(lambda (x) (whensleepiestbyminute x alldays whenasleep)) allguards)))) 0 ()))

(setf guardid (nth (first guardindex) allguards))

; which minute is that?
(setf minute (which (nth (first guardindex) (remove nil (mapcar #'(lambda (x) (whensleepiestbyminute x alldays whenasleep)) allguards)))
		    (list (maxint (nth (first guardindex) (remove nil (mapcar #'(lambda (x) (whensleepiestbyminute x alldays whenasleep)) allguards))))) 0 ()))

(* guardid (first minute))

