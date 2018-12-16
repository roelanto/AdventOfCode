;; (load "/Users/roelant/quicklisp/setup.lisp")
(ql:quickload :cl-ppcre)

(defun parse-input (string)
  "parses the input string. If nil, check that there are parentheses and double backslashes in the regexp. "
  (setf (values match position)
	(cl-ppcre:scan-to-strings "(\\d*), (\\d*)$" string))
  (list
   (parse-integer (aref position 0))
   (parse-integer (aref position 1))))

(defun parse-input-1 (string)
  (setf (values match position)
	(cl-ppcre:scan-to-strings "Before: .(\\d*), (\\d*), (\\d*), (\\d*).$" string))
  (list
   (parse-integer (aref position 0))
   (parse-integer (aref position 1))
   (parse-integer (aref position 2))
   (parse-integer (aref position 3))))

(defun parse-input-2 (string)
  (setf (values match position)
	(cl-ppcre:scan-to-strings "(\\d*) (\\d*) (\\d*) (\\d*)$" string))
  (list
   (parse-integer (aref position 0))
   (parse-integer (aref position 1))
   (parse-integer (aref position 2))
   (parse-integer (aref position 3))))

(defun parse-input-3 (string)
  (setf (values match position)
	(cl-ppcre:scan-to-strings "After: *.(\\d*), (\\d*), (\\d*), (\\d*).$" string))
  (list
   (parse-integer (aref position 0))
   (parse-integer (aref position 1))
   (parse-integer (aref position 2))
   (parse-integer (aref position 3))))


(defun get-file (filename)
  "read file line-by-line, return contents in a list"
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
       collect line)))

(setf input-lines (get-file "../day16/input_1.txt"))
(setf input-lines2 (mapcar #'parse-input-2 (get-file "../day16/input_2.txt")))

(setf allinputs
      (loop for x from 0 to (length input-lines) by 4 
   collect
     (list 
      (parse-input-1 (nth x input-lines))
      (parse-input-2 (nth (1+ x) input-lines))
      (parse-input-3 (nth (1+ (1+ x)) input-lines)))
     ))



(defun apply-to-r (arg-list registers funcall-name)
  ""
  (setf (nth (third arg-list) registers) (funcall funcall-name (nth (first arg-list) registers) (nth (second arg-list) registers)))
  registers)

(defun apply-to-i (arg-list registers funcall-name)
  ""
  (setf (nth (third arg-list) registers) (funcall funcall-name (nth (first arg-list) registers) (second arg-list)))
  registers)


(defun mulr (arg-list registers)
  "mulr (multiply register) stores into register C the result of multiplying register A and register B."
  (apply-to-r arg-list registers #'*))

(defun muli (arg-list registers)
  "muli (multiply immediate) stores into register C the result of multiplying register A and value B."
  (apply-to-i arg-list registers #'*))

(defun addr (arg-list registers)
  "addr (add register) stores into register C the result of adding register A and register B."
  (apply-to-r arg-list registers #'+))

(defun addi (arg-list registers)
  "addi (add immediate) stores into register C the result of adding register A and value B."
  (apply-to-i arg-list registers #'+))

(defun banr (arg-list registers)
  "banr (bitwise AND register) stores into register C the result of the bitwise AND of register A and register B."
  (apply-to-r arg-list registers #'logand))

(defun bani (arg-list registers)
  "bani (bitwise AND immediate) stores into register C the result of the bitwise AND of register A and value B."
  (apply-to-i arg-list registers #'logand))

(defun borr (arg-list registers)
  "borr (bitwise OR register) stores into register C the result of the bitwise OR of register A and register B."
  (apply-to-r arg-list registers #'logior))

(defun bori (arg-list registers)
  "bori (bitwise OR immediate) stores into register C the result of the bitwise OR of register A and value B."
  (apply-to-i arg-list registers #'logior))


(defun setr (arg-list registers)
  "setr (set register) copies the contents of register A into register C. (Input B is ignored.)"
  (setf (nth (third arg-list) registers) (nth (first arg-list) registers))
  registers)

(defun seti (arg-list registers)
  "seti (set immediate) stores value A into register C. (Input B is ignored.)"
  (setf (nth (third arg-list) registers) (first arg-list))
  registers)


(defun gtir (arg-list registers)
  "gtir (greater-than immediate/register) sets register C to 1 if value A is greater than register B. Otherwise, register C is set to 0."
  (if (> (first arg-list) (nth (second arg-list) registers))
      (setf (nth (third arg-list) registers) 1)
      (setf (nth (third arg-list) registers) 0))
  registers)

(defun gtri (arg-list registers)
  "gtir (greater-than register/immediate) ..."
  (if (> (nth (first arg-list) registers) (second arg-list))
      (setf (nth (third arg-list) registers) 1)
      (setf (nth (third arg-list) registers) 0))
  registers)

(defun gtrr (arg-list registers)
  "gtir (greater-than register/register) "
  (if (> (nth (first arg-list) registers) (nth (second arg-list) registers))
      (setf (nth (third arg-list) registers) 1)
      (setf (nth (third arg-list) registers) 0))
  registers)

(defun eqir (arg-list registers)
  "eqir (equal immediate/register) sets register C to 1 if value A is equal to register B. Otherwise, register C is set to 0."
  (if (equal (first arg-list) (nth (second arg-list) registers))
      (setf (nth (third arg-list) registers) 1)
      (setf (nth (third arg-list) registers) 0))
  registers)

(defun eqri (arg-list registers)
  "eqri (equal register/immediate) sets register C to 1 if register A is equal to value B. Otherwise, register C is set to 0."
  (if (equal (nth (first arg-list) registers) (second arg-list))
      (setf (nth (third arg-list) registers) 1)
      (setf (nth (third arg-list) registers) 0))
  registers)

(defun eqrr (arg-list registers)
  "eqrr (equal register/register) sets register C to 1 if register A is equal to register B. Otherwise, register C is set to 0."
  (if (equal (nth (first arg-list) registers) (nth (second arg-list) registers))
      (setf (nth (third arg-list) registers) 1)
      (setf (nth (third arg-list) registers) 0))
  registers)

(defun test-functions (possible-functions registers input output )
  (let ((copy-of-input (copy-list input))
	(copy-of-registers (copy-list registers)))
    (list (remove 'nil (mapcar
     #'(lambda (funcname)
	 (setf result (funcall funcname (cdr copy-of-input) (copy-list registers)))
;;	 (format t "application of input ~a to function ~a (registers: ~a) yields ~a which is ~a compared to output ~a ~%" copy-of-input funcname registers result (equal output result) output)
	 (if (equal output result)
	     funcname
	     nil)) possible-functions)))))



(setf possible-functions (list #'mulr #'muli #'addr #'addi #'banr #'bani #'borr #'bori #'setr #'seti #'gtir #'gtri #'gtrr #'eqir #'eqri #'eqrr))

(setf results (mapcar #'(lambda (x) (apply #'length (test-functions possible-functions (first x) (second x) (third x)))) allinputs))

;; next line of code yields the right number
(length (remove 'nil (mapcar #'(lambda (x) (< 2 x)) results)))

;; part 2
(defun determine-mapping (unknown known allinputs)
  ""
  (if (= 16 (length known))
      known
      (loop for input in allinputs
	 do
	   (if (= 1 (length (setf matching-functions (first (test-functions unknown (first input) (second input) (third input))))))
	       (progn 
		 (return (determine-mapping (remove (first matching-functions) unknown) (append known (list (list (first (second input)) (first matching-functions)))) allinputs)))))))

(defun lookup-rule (rule-mappings n)
  (loop for rule-mapping in rule-mappings
     do
       (if (equal (first rule-mapping) n)
	   (return (second rule-mapping)))))

;; determine rule mappings
(setf unknown (copy-list possible-functions))
(setf rule-mappings (determine-mapping unknown () allinputs))

;; apply mapped rules to input
(setf registers '(0 0 0 0))
(loop for input in input-lines2
   do
     (setf therule (lookup-rule rule-mappings (first input)))
     (setf registers (funcall therule (cdr input) registers))
     (format t "after applying ~a ~a: registers is: ~a~%" (first input) therule registers))

