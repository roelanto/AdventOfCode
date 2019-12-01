(load "/Users/roelant/quicklisp/setup.lisp")
(ql:quickload :cl-ppcre)

(defun parse-input (string)
  "parses the input string. If nil, check that there are parentheses and double backslashes in the regexp. "
  (setf (values match position)
	(cl-ppcre:scan-to-strings "Step (.+) must be finished before step (.+) can begin.$" string))
  (list
   (intern (aref position 0))
    (intern (aref position 1))))
(find-symbol "K")

(defun get-file (filename)
  "read file line-by-line, return contents in a list"
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
       collect line)))


(defun which (list elements num result)
  ""
  (if (null list)
      result
      (if (intersection (list (first list)) elements :test 'equal)
	  (which (rest list) elements (1+ num) (append result (list num)))
	  (which (rest list) elements (1+ num) result))))

(defun compare-rule (x y)
  ""
  (let ((whiches
	 (append
	  (which (coerce "*ABCDEFGHIJKLMNOPQRSTUVWXYZ" 'list) (list (coerce (first x) 'character)) 0 ())
	  (which (coerce "*ABCDEFGHIJKLMNOPQRSTUVWXYZ" 'list) (list (coerce (first y) 'character)) 0 ()))))
    (< (first whiches) (second whiches))))

(defun compare-2-rule (x y)
  ""
  (let ((whiches
	 (append
	  (which (coerce "*ABCDEFGHIJKLMNOPQRSTUVWXYZ" 'list) (list (coerce (second x) 'character)) 0 ())
	  (which (coerce "*ABCDEFGHIJKLMNOPQRSTUVWXYZ" 'list) (list (coerce (second y) 'character)) 0 ()))))
    (< (first whiches) (second whiches))))

(defun wasseen (x hasseen)
  "returns 1 if x was seen"
  (length (intersection (list (second x)) hasseen)))

(defun compare-hasseen (x y hasseen)
  ""
  (< (wasseen x hasseen) (wasseen y hasseen)))
  
(defun sort-rules (rule-list hasseen)
  ""
  (sort
   (sort 
    (copy-list (sort (copy-list rule-list) #'compare-2-rule))
    #'(lambda (x y) (< (next-rule-score x rule-list hasseen) (next-rule-score y rule-list hasseen))))
  #'(lambda (x y)   (< (wasseen x hasseen) (wasseen y hasseen)))))

(defun next-available-rules (rule-list activated hasseen result)
  ""
  (if (null rule-list)
      result
      (if (equal (first (first rule-list)) activated)
	  (next-available-rules (rest rule-list) activated hasseen (append result (list (first rule-list))))
	  (next-available-rules (rest rule-list) activated hasseen result))))

(defun next-rule (rule-list hasseen)
  "gets list of rules "
  (if (null rule-list)
      nil
      (progn
	(write (remove-duplicates (mapcar #'first rule-list)))
	(write (intersection (remove-duplicates (mapcar #'first rule-list))
			     hasseen))
	(if (null
	   (intersection (list (first (first rule-list)))  hasseen))
	  (next-rule (rest rule-list) hasseen)
	  (first rule-list)))))

(defun filter-rule (rule rule-list result)
  ""
  (if (null rule-list)
      result
      (if (equal (second rule) (second (first rule-list)))
	  (filter-rule rule (rest rule-list) (append result (list (first rule-list))))
	  (filter-rule rule (rest rule-list) result))))
      
(defun next-rule-score (rule rule-list hasseen)
  "gets list of rules "
  (if (null rule)
      nil
      (progn
	(if (equal (length (mapcar #'first (filter-rule rule allrules ())))
		   (length (intersection
			    (mapcar #'first (filter-rule rule allrules ()))
			    hasseen)))
	    0
	    1))))

(defun lookup-time (rule)
  ""
  (first (which (coerce "*ABCDEFGHIJKLMNOPQRSTUVWXYZ" 'list) (list (coerce (first rule) 'character)) 0 ())))
  
(defun adorn-with-time (rule time)
  ""
  (list (first rule) (second rule) time))

(mapcar #'(lambda (rule) rule) rulestack)

(mapcar #'(lambda (rule) (adorn-with-time rule (lookup-time rule))) rulestack)

(adorn-with-time '(A C) (lookup-time '(A C)))

(defun getallrules ()
  "loads all rules, adds *-expansion to generate root rules"
  (setf allrules (mapcar #'(lambda (line) (parse-input line)) (remove nil (get-file "../day7/sample.txt"))))
  (setf allrules (mapcar #'(lambda (rule) (adorn-with-time rule (lookup-time rule))) allrules))

  (setf beginsymbols (set-difference (remove-duplicates (mapcar #'first allrules) )
				     (remove-duplicates (mapcar #'second allrules) )))
  (append allrules (mapcar #'(lambda (x) (list '* x 0)) beginsymbols)))

  

(setf allrules (getallrules))
(setf terminalnode (first (remove-duplicates (set-difference (mapcar #'second allrules) (mapcar #'first allrules)))))

(setf beginsymbols (list '*))
(setf hasseen '(*))
;; initial call to determine first nodes
(setf rulestack
      (sort-rules (apply #'append (mapcar #'(lambda (symbol) (next-available-rules allrules symbol () ())) beginsymbols)) hasseen)) 
(setf state (list (first (first rulestack))))

;;(loop while (not (equal (first (last state)) terminalnode))
;;   do
;;     (progn
(setf thetime 0)
rulestack
       (setf rulestack
	     (sort-rules
	      (progn
		
		(if (equal (third (first rulestack)) 0)
		    (progn 
		      (setf hasseen (remove-duplicates (append hasseen (list (first (first (next-available-rules allrules (second (first rulestack)) hasseen ())))))))
		      (if (member (first (first rulestack)) (coerce beginsymbols 'list))
			  (setf state (append state (list (first (first rulestack))))))
		      (setf state (append state (list (second (first rulestack)))))
		      (append (next-available-rules allrules (second (first rulestack)) hasseen ()) (rest rulestack)))
		    rulestack))	     
	      hasseen))
state

;; still need to implement workers
(setf rulestack (append (list (list (first (first rulestack)) (second (first rulestack)) (1- (third (first rulestack))))) (rest rulestack)))
(setf thetime (1+ thetime))
(setf rulestack (mapcar #'(lambda (x) (if (and (< (next-rule-score  x rulestack hasseen ) 1)
			       (or (null (fourth x)) (< (fourth x) 1)))
			  (append x 0)
			  (append x 1))) rulestack))
(second (append (first rulestack) 0))
(length (first rulestack))
(cons '(A B) 2)
(length (append '(A B))


(> (fourth '(0 1 2 3 4)) 1)
(third (first rulestack))
(first rulestack)
(length  rulestack)
(append '((A B)) (rest rulestack))


	     (append (list (first (first rulestack)) (second (first rulestack)) (1- (third (first rulestack)))) (rest rulestack))))


;; final answer still has *s as initial states
(remove '* state)

