(defun get-file (filename)
  "read file line-by-line, return contents in a list"
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
       collect line)))

(defun sortstring (string)
  "sorts characters of the string"
  (sort (copy-seq string) #'char<)
  )

(defun eatequalhead (string ch )
  ""
  (if (equal (first string) ch)
      (eatequalhead (rest string) ch)
      string
      ))

(defun determinereps (string nums)
  ""
  (if (null string)
      nums
      (progn
	(let ((string2 (eatequalhead string (first string))))
	  (determinereps string2 (append nums (list (- (length string) (length string2)))))))))


(defun has (list num)
  ""
  (if (not (null (member num list)))
      1
      0))


(defun determine-score (string num)
  "determines the score"
  (let ((occurrences (determinereps (coerce (sortstring string) 'list) ())))
     (has occurrences num)))

(defun compute-checksum (stringlist)
  "computes the checksum as in the puzzle"
  (* (apply #'+ (mapcar #'determine-score stringlist (make-list (length stringlist) :initial-element 2)))
     (apply #'+ (mapcar #'determine-score stringlist (make-list (length stringlist) :initial-element 3)))))

;; example from the puzzle
;; (setf stringlist '("abcdef" "bababc" "abbcde" "abcccd" "aabcdd" "abcdee" "ababab"))

(let ((stringlist (get-file "input.txt")))
  (compute-checksum stringlist))

;; part two

(defun is-different (char1 char2)
  ""
  (if (equal char1 char2)
      0
      1))

(defun compute-difference (stringarg1 stringarg2 num)
  "computes the number of characters that are different"
  (let ((string1 (coerce stringarg1 'list))
	(string2 (coerce stringarg2 'list)))
	  
  (if (null string1)
      num
      (let ((newnum (is-different (first string1) (first string2))))
	(compute-difference (rest string1) (rest string2) (+ num newnum))))))

(defun compute-difference-list (string  list)
  "computes the difference between string1 and each member of list"
  (mapcar #'compute-difference list
	  (make-list (length list) :initial-element string)
	  (make-list (length list) :initial-element 0)))

(defun compute-difference-list-list (stringlist)
  ""
  (mapcar #'compute-difference-list
	  stringlist
	  (make-list (length stringlist) :initial-element stringlist))
  )

(defun has-one (list) ""
       (not (null (member 1 list))))

(defun which-is-true (truelist valuelist resultlist)
  ""
  (if (not (null truelist))
      (if (first truelist)
	  (let ((resultlist (append resultlist (list (first valuelist)))))
	    (progn
	      (which-is-true (rest truelist) (rest valuelist) resultlist)))
	  (which-is-true (rest truelist) (rest valuelist) resultlist))
      resultlist))


;; computation of the actual answer

(write
 (let ((stringlist (get-file "input.txt")))
   (defvar similar-strings)
   (setf similar-strings (which-is-true (mapcar #'has-one (compute-difference-list-list stringlist)) stringlist ()))
   
   (coerce (which-is-true
	    (mapcar #'equal (coerce (first similar-strings) 'list) (coerce (first (last similar-strings)) 'list))
	    (coerce (first similar-strings) 'list)
	    ()) 'string)
   ))
