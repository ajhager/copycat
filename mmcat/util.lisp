(defun select-list-position (list &aux (sum 0) (value 0))
; LIST is a list of integers. Probabilistically chooses one of the 
; integers on the list according to value. Returns the position of that 
; integer.
  (if* list 
   then (setq sum (loop for item in list sum item))
        (if* (<= sum 0)
         then (random (length list))
         else (setq value (random sum))
              (loop for item in list
	            with newsum = 0
	            with counter = 0
	            do (incf newsum item)
	               (setq counter (1+ counter))
	            when (> newsum value)
	            return (1- counter)))
   else nil))	

;---------------------------------------------

(defun select-assoc (list &aux (sum 0) (value 0))
; LIST is an assoc-list of the form 
;       ((item . probability) (item . probability) . . .)
; Returns one of the items, chosen probabilistically.
  (if* list 
   then (setq sum (loop for pair in list sum (cdr pair)))
        (if* (<= sum 0)
         then nil
         else (setq value (random sum))
              (loop for pair in list
 	            with newsum = 0
	            do (incf newsum (cdr pair))
	            when (> newsum value)
	            return (car pair)))
   else nil))

;---------------------------------------------

(defun select-list-item-by-method (list method &optional argument)
; Applies the method to the list to get a list of numbers.
; Probabilisticallly selects a position in the list of numbers
; and returns the item at that position in the original list.
  (if* (null list) 
   then nil
   else (if* argument
         then (nth (select-list-position 
		       (send-method-to-list list method argument))
		   list)
         else (nth (select-list-position (send-method-to-list list method)) 
		   list))))

;---------------------------------------------
; make-probability-distribution | Distribution.__init__
; defflavor probability-distribution
; probability-distribution.update
;---------------------------------------------

;---------------------------------------------
; probability-distribution.vset | Distribution.set_value
;---------------------------------------------

;---------------------------------------------
; probability-distribution.choose | Distribution.choose
;---------------------------------------------

;---------------------------------------------
; structure-vs-structure
;---------------------------------------------

;---------------------------------------------
; fight-it-out
;---------------------------------------------

(defun flip-coin (&optional (prob-of-heads .5))
; Returns heads or tails.  If no argument, probability of each is .5, 
; otherwise, probability of heads is equal to the argument (which is 
; assumed to be between 0 and 1).
  (if* (>= prob-of-heads 1) 
   then 'heads
   else (select-assoc `((heads . ,(round (* prob-of-heads 1000)))
	   	        (tails . ,(round (* (- 1 prob-of-heads) 1000)))))))

;---------------------------------------------

(defun average (&rest list-of-values)
; Returns the arithmetic mean of its arguments.
  (/ (list-sum list-of-values) (length list-of-values)))

;---------------------------------------------

(defun weighted-average (value-weight-list)
; Returns the weighted arithmetic mean of its arguments.  The 
; value-weight-list is of the form 
;          ((value1 . weight1) (value2 . weight2) ...)
  (loop for value-weight in value-weight-list
	sum (* (car value-weight) (cdr value-weight)) into value-sum
	sum (cdr value-weight) into weight-sum 
	finally (return (round (/ value-sum weight-sum)))))

;---------------------------------------------

(defun same-letter-category? (obj1 obj2)
  (string-equal (send obj1 :pname) (send obj2 :pname)))

;---------------------------------------------

(defun flatten (l)
; Flattens a list so that there are no nested lists.
  (cond ((null l) nil)
	((atom l) l)
        ((not (listp (car l)))
	 (cons (car l) (flatten (cdr l))))
        (t (append (flatten (car l)) (flatten (cdr l))))))

;---------------------------------------------

(defun list-average (l)
  (/ (list-sum l) (length l)))

;---------------------------------------------

(defun std-dev (list-of-values &aux (cumulative 0) (cumulative-squares 0))
  (cond ((= (length list-of-values) 0) nil)
	((= (length list-of-values) 1) 0)
        (t (loop for item in list-of-values do
              (incf cumulative item)
              (incf cumulative-squares (sqr item)))
           (sqrt (/ (abs (- cumulative-squares
	   	            (/ (sqr cumulative) (length list-of-values))))
   	            (1- (length list-of-values)))))))

;---------------------------------------------

(defun std-err (list-of-values)
  (float (/ (std-dev list-of-values) (sqrt (length list-of-values)))))

;---------------------------------------------

(defun blur (n &aux blur-amount)
; Returns a number close to n (i.e., within (sqrt n) of n).
; This function will return n twice as often as it returns
; each of the other possible numbers.  Ideally this should be
; a normal distribution around n, but that wasn't necessary for this
; purpose.
  (setq blur-amount (round (sqrt n)))
  (funcall (random-list-item '(+ -)) n (random (1+ blur-amount))))
