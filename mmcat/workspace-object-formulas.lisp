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
; workspace-object.calculate-intra-string-salience
; workspace-object.calculate-inter-string-salience
; workspace-object.calculate-total-salience
;---------------------------------------------

;---------------------------------------------
; workspace-object.update-object-values | Object.update_object_values
;---------------------------------------------

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
