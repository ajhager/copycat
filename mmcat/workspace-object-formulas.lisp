;---------------------------------------------
; workspace-object.calculate-raw-importance | Object.calculate_raw_importance
;---------------------------------------------

;---------------------------------------------
; workspace-object.calculate-intra-string-happiness
; workspace-object.calculate-inter-string-happiness
; workspace-object.calculate-total-happiness
; workspace-object.calculate-intra-string-unhappiness
; workspace-object.calculate-inter-string-unhappiness
; workspace-object.calculate-total-unhappiness
;---------------------------------------------

;---------------------------------------------
; workspace-object.update-object-values | Object.update_object_values
;---------------------------------------------

(defmethod (workspace-object :calculate-intra-string-salience) ()
; An object's intra-string salience represents how much the object is
; crying out for attention from codelets that build structures
; inside a single string (i.e., bonds and groups).  It is a function
; of the object's relative importance in its string and its intra-string
; unhappiness.  Here, intra-string unhappiness counts much more than the
; importance, since we want there to be pressure for the program to 
; formulate a coherent structuring of the entire string, and not
; leave out objects, even if they're not too importance.  This is somewhat
; domain-dependent.  Salience can be clamped to 100, as it is for the
; object causing a snag, if one is hit.
  (if* clamp-salience?
   then 100
   else (round (weighted-average 
		   `((,(send self :relative-importance) . 2)
   		     (,(send self :intra-string-unhappiness) . 8))))))

;--------------------------------------------

(defmethod (workspace-object :calculate-inter-string-salience) ()
; An object's inter-string salience represents how much the object is
; crying out for attention from codelets that build structures
; between strings (i.e., correspondences).  It is a function
; of the object's relative importance in its string and its inter-string
; unhappiness.  Here, the importance counts much more than the
; inter-string unhappiness, since we want there to be pressure for 
; the program to map important objects, and not pay too much attention
; to mapping unimportant ones.  Salience can be clamped to 100, as it is for
; the object causing a snag, if one is hit.
  (if* clamp-salience?
   then 100
   else (round (weighted-average 
		   `((,(send self :relative-importance) . 8)
   		     (,(send self :inter-string-unhappiness) . 2))))))

;---------------------------------------------

(defmethod (workspace-object :calculate-total-salience) ()
  (round (average (send self :intra-string-salience) 
	          (send self :inter-string-salience))))

(defmethod (workspace-string :update-relative-importances) 
           (&aux total-raw-importance)
; This updates the relative (normalized) importances of all the objects in 
; the string
  (setq total-raw-importance 
	(list-sum (send-method-to-list 
		      (send self :object-list) :raw-importance)))
  (loop for obj in (send self :object-list) do
	(send obj :set-relative-importance
	      (if* (= total-raw-importance 0)
               then 0 
	       else (round (* 100 (/ (send obj :raw-importance) 
				     total-raw-importance)))))))
  
(defmethod (workspace-string :calculate-intra-string-unhappiness) ()
; This returns the average of the intra-string unhappinesses of all
; the objects in the string.
  (round (list-average (send-method-to-list (send self :object-list) 
		                            :intra-string-unhappiness))))

(defmethod (workspace-string :update-intra-string-unhappiness) ()
; This updates the string's intra-string unhappiness (the average of the 
; intra-string unhappinesses of all the objects in the string.
  (send self :set-intra-string-unhappiness 
	(send self :calculate-intra-string-unhappiness)))
