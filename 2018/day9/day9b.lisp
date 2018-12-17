(defun position-cw (boardsize from-pos &optional (n 1))
  "returns the position n from pos, clockwise"
  (1+ (mod (+ from-pos n) boardsize)))

(defun position-ccw (boardsize from-pos &optional (n 1))
  "returns the position n from pos, clockwise"
;;  (format t "new position ~a~%" (mod (- from-pos n) boardsize))
;;  (format t "frompos and boardsize ~a ~a~%" from-pos boardsize)
  (mod (- from-pos n) boardsize))

(defun addmarble-ll (marbles pos value)
  
(defun addmarble (marbles pos value)
  ""
  ;;  (push value (cdr (nthcdr (1- pos) marbles)))
;;  (format t "adding marble ~a at pos ~a~%" value pos)
  (push value (nthcdr pos marbles))
  (setf boardsize (1+ boardsize))
  marbles)

(defun removemarble (marbles pos)
  ""
  (pop (nthcdr pos marbles)))

(defun add-player-score (marble)
  ""
  ;;(format t "Player has scored ~a~%" marble))
  )
  
(defun get-player-score (playerscores no)
  "returns the score of player no"
  (nth no playerscores))

(defun add-to-player-score (playerscores no value)
  ""
;;  (format t "Adding score ~a to player~%" value)
;;  (format t "~a~%" playerscores)
  (setf playerscores (set-nth-list playerscores no (+ value (get-player-score playerscores no))))
  playerscores)


(defun playturn (numturns)
  ""
  (format t "looping to ~a~%" numturns)
  (loop for i from 0 to (1- numturns)
     do
       (if (= 0 (mod i 1000))
	   (format t "~a~%" i))
       (setf currentmarble (1+ currentmarble))
       (if (and
	    (< 0 i)
	    (= 0 (mod currentmarble 23)))
	   (progn
;;	     (format t "SPECIAL at ~a, currentpos is ~a ~%" currentmarble currentpos)
;;	     (format t "~a: ~a ~a ~%" (mod i *maxplayers*) i marbles)
;;	     (format t "sum of marbles ~a~%" (apply #'+ marbles))
	     (setf playerscores (add-to-player-score playerscores (mod (1+ i) *maxplayers*) (removemarble marbles (position-ccw boardsize currentpos 7))))
	     (setf playerscores (add-to-player-score playerscores (mod (1+ i) *maxplayers*) currentmarble))
	     (setf currentpos (position-ccw boardsize currentpos 7))
;;	     (format t "current pos is now: ~a~%" currentpos)
	     (setf boardsize (1- boardsize)))
	     
	   (progn
	     (setf currentpos (position-cw boardsize currentpos))
	     (addmarble marbles currentpos currentmarble)))
       ))



;; Part 2

(defparameter *maxplayers* 468)
(setf playerscores (make-list *maxplayers* :initial-element 0))

(setf *print-circle* t)
(setf initial-node (make-node :val 0))
(setf (node-next initial-node) initial-node)
(setf (node-prev initial-node) initial-node)
(setf cur-node initial-node)
(time
(loop for x from 1 to (1+ (* 100 71010))
   do
     (progn
;;       (format t "~a: " x)
;;       (visualize-node (node-next initial-node))
;;       (format t "~%")
       (if (= 0 (mod x 23))
	   (progn
	     (setf node-to-delete (node-prev (node-prev (node-prev (node-prev (node-prev (node-prev (node-prev cur-node))))))))
	     (setf (node-next (node-prev node-to-delete)) (node-next node-to-delete))
	     (setf (node-prev (node-next node-to-delete)) (node-prev node-to-delete))
	     (setf cur-node (node-next node-to-delete))
	     (setf playerscores (add-to-player-score playerscores (mod (1- x) *maxplayers*) x))
	     (setf playerscores (add-to-player-score playerscores (mod (1- x) *maxplayers*) (node-val node-to-delete)))
	     
	     ;;	     (format t "node-val ~a~%" (node-val node-to-delete))
	     )
	   (progn 
	     (setf cur-node (setf (node-next (node-next cur-node)) (make-node :val x :prev (node-next cur-node) :next (node-next (node-next cur-node)))))
	     (setf (node-prev (node-next cur-node)) cur-node))))))


(defun visualize-node (node)
  (if (< 0 (node-val  node))
      (progn
	(format t "~a " (node-val node) )
	(visualize-node (node-next node)))))

(visualize-node initial-node)

;; prints the final result
(setf finalresult (apply #'max playerscores))
;; result is 3083412635
