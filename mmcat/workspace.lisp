(defun in-group? (obj1 obj2)
; Returns t if the two objects are in a group.
 (and (send obj1 :group) (eq (send obj1 :group) (send obj2 :group))))

;---------------------------------------------
; defflavor workspace | Workspace.__init__
;---------------------------------------------

;---------------------------------------------
; workspace.proposed-bond-list
;---------------------------------------------

;---------------------------------------------
; workspace.bond-list
;---------------------------------------------

;---------------------------------------------
; workspace.proposed-group-list
;---------------------------------------------

;---------------------------------------------
; workspace.group-list
;---------------------------------------------

;---------------------------------------------
; proposed-correspondence-list
;---------------------------------------------

;---------------------------------------------
; workspace :correspondence-list
;---------------------------------------------

;---------------------------------------------
; workspace.add-replacement
;---------------------------------------------

;---------------------------------------------
; add-proposed-correspondence
;---------------------------------------------

;---------------------------------------------
; delete-proposed-corresponence | remove_proposed_correspondence
;---------------------------------------------

;---------------------------------------------
; workspace.add-correspondence
;---------------------------------------------

;---------------------------------------------
; workspace.delete-correspondence
;---------------------------------------------

;---------------------------------------------
; workspace.correspondence-present?
;---------------------------------------------

;---------------------------------------------
; workspace.slippage-present? | REMOVED
;---------------------------------------------

;---------------------------------------------
; workspace.object-list
;---------------------------------------------

;---------------------------------------------
; workspace.letter-list
;---------------------------------------------

;---------------------------------------------
; workspace.structure-list
;---------------------------------------------

(defmethod (workspace :structure-in-snag-structure-list?) (s)
; This method is used after a snag has been hit and the temperature has
; been clamped to determine whether or not to release the temperature
; clamp. This method is called from the function "update-everything"
; in the file "run.l".  Returns t if the given structure is in the list of 
; structures that were present when the last snag was hit.  (If this method 
; returns nil, that is, this structure was built since the snag was hit,
; then there is some chance that the temperature clamp will be released.)
  (cond ((typep s 'bond) 
	 (loop for structure in *snag-structure-list*
	       when (typep structure 'bond) do
	       (if* (and (eq (send structure :from-obj) (send s :from-obj))
			 (eq (send structure :to-obj) (send s :to-obj))
			 (eq (send structure :bond-category)
			     (send s :bond-category))
			 (eq (send structure :direction-category)
			     (send s :direction-category)))
                then (return t))))


	((typep s 'group) 
	 (loop for structure in *snag-structure-list*
	       when (typep structure 'group) do
	       (if* (and (eq (send structure :left-obj) (send s :left-obj))
			 (eq (send structure :right-obj) (send s :right-obj))
			 (eq (send structure :group-category)
			     (send s :group-category))
			 (eq (send structure :direction-category)
			     (send s :direction-category)))
                then (return t))))

        ((typep s 'correspondence) 
	 (loop for structure in *snag-structure-list*
	       when (typep structure 'correspondence) do
	       (if* (and (eq (send structure :obj1) (send s :obj1))
			 (eq (send structure :obj2) (send s :obj2))
			 (>= (length (send structure 
					   :relevant-distinguishing-cms))
			     (length (send s :relevant-distinguishing-cms))))
                then (return t))))
	       

        ((typep s 'rule) 
	 (loop for structure in *snag-structure-list*
	       when (typep structure 'rule)
	       return (rule-equal? structure s)))))
	

;---------------------------------------------
; workspace.random-string
;---------------------------------------------

;---------------------------------------------
; workspace.random-object
;---------------------------------------------

;---------------------------------------------
; workspace.random-group
;---------------------------------------------

;---------------------------------------------
; workspace.random-correspondence
;---------------------------------------------

;---------------------------------------------
; workspace.choose-object | Workspace.choose_object
;---------------------------------------------

;---------------------------------------------
; workspace.null-replacement?
;---------------------------------------------

;---------------------------------------------
; workspace.unrelated-objects
;---------------------------------------------

;---------------------------------------------
; workspace.ungrouped-objects
;---------------------------------------------

;---------------------------------------------
; workspace.ungrouped-bonds
;---------------------------------------------

;---------------------------------------------
; workspace.unreplaced-objects
;---------------------------------------------

;---------------------------------------------
; workspace.uncorresponding-objects
;---------------------------------------------

;---------------------------------------------
; workspace.rough-num-of-unrelated-objects
;---------------------------------------------

;---------------------------------------------
; workspace.rough-num-of-ungrouped-objects
;---------------------------------------------

;---------------------------------------------
; workspace.rough-num-of-unreplaced-objects
;---------------------------------------------

;---------------------------------------------
; workspace.rough-num-of-uncorresponding-objects
;---------------------------------------------

;---------------------------------------------
; workspace.rough-importance-of-uncorresponding-objects
;---------------------------------------------

;---------------------------------------------
; workspace.delete-proposed-structure | Workspace.remove_proposed_structure
;---------------------------------------------

;---------------------------------------------
; workspace.slippage-list | Workspace.slippages()
;---------------------------------------------

;---------------------------------------------
; workspace.intra-string-unhappiness
;---------------------------------------------

;---------------------------------------------
; workspace.inter-string-unhappiness
;---------------------------------------------

;---------------------------------------------
; workspace.total-unhappiness
;---------------------------------------------
