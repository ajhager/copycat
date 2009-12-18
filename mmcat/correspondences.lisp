(defun get-leftmost-and-rightmost-incompatible-correspondences
         (group1 group2 direction-category-cm
          &aux leftmost-obj1 rightmost-obj1 
  	       leftmost-obj2 rightmost-obj2 
	       leftmost-correspondence rightmost-correspondence 
	       incompatible-correspondences)
	 
; This function returns any correspondences 
; between leftmost and rightmost objects in group1 and group2 that are 
; incompatible with a correspondence between the groups.
  (setq leftmost-obj1 (send group1 :left-obj) rightmost-obj1 
	              (send group1 :right-obj))
  (setq leftmost-obj2 (send group2 :left-obj) rightmost-obj2 
	              (send group2 :right-obj))

  (if* (eq (send direction-category-cm :label) plato-identity)
   then (if* (and (setq leftmost-correspondence 
			(send leftmost-obj1 :correspondence))
		 (not (eq (send leftmost-correspondence :obj2) 
			  leftmost-obj2)))
	 then (push leftmost-correspondence incompatible-correspondences))
  
        (if* (and (setq rightmost-correspondence 
			(send rightmost-obj1 :correspondence))
		 (not (eq (send rightmost-correspondence :obj2) 
			  rightmost-obj2)))
	 then (push rightmost-correspondence incompatible-correspondences)))
  
  (if* (eq (send direction-category-cm :label) plato-opposite)
   then (if* (and (setq leftmost-correspondence 
			(send leftmost-obj1 :correspondence))
		 (not (eq (send leftmost-correspondence :obj2) 
			  rightmost-obj2)))
	 then (push leftmost-correspondence incompatible-correspondences))
  
        (if* (and (setq rightmost-correspondence 
			(send rightmost-obj1 :correspondence))
		 (not (eq (send rightmost-correspondence :obj2) 
			  leftmost-obj2)))
	 then (push rightmost-correspondence incompatible-correspondences)))
  incompatible-correspondences)

;---------------------------------------------

(defun supporting-correspondences? (c1 c2 &aux (result nil))
; returns t if c1 supports c2, nil otherwise.  for now, c1 is 
; defined to support c2 if c1 is not incompatible with c2, and 
; has a concept-mapping that supports the concept-mappings of c2.

  (cond ((or (eq (send c1 :obj1) (send c2 :obj1))	
	     (eq (send c1 :obj2) (send c2 :obj2)))
         (setq result nil))
	((incompatible-correspondences? c1 c2) (setq result nil))
        (t (loop for cm1 in (send c1 :distinguishing-concept-mappings) 
		 until result do
                  (loop for cm2 in (send c2 :distinguishing-concept-mappings)
		        when (supporting-concept-mappings? cm1 cm2) 
			do (setq result t)
			   (return)))))
  result)

;---------------------------------------------

(defun incompatible-correspondences? (c1 c2 &aux (result nil))
; returns t if c1 is incompatible with c2, nil otherwise.  for now, c1 is 
; defined to be incompatible with c2 if c1 and c2 share objects, or c1 has a 
; concept-mapping incompatible with the concept-mappings of c2.


  (if* (or (eq (send c1 :obj1) (send c2 :obj1))	
	  (eq (send c1 :obj2) (send c2 :obj2)))
   then (setq result t)
   else (loop for cm1 in (send c1 :concept-mapping-list) until result do
              (loop for cm2 in (send c2 :concept-mapping-list) 
		    when (incompatible-concept-mappings? cm1 cm2) do
                         (setq result t)
			 (return))))
  result)

(defun get-concept-mapping-list (obj1 obj2 descriptions1 descriptions2 
				 &aux result)
; Get the list of concept-mappings between the given descriptions of
; these two objects.  For now, the two descriptors in a concept-mapping have 
; to be equal or linked by a slip-link in the Slipnet, and have 
; the same description-type.
  (loop for d1 in descriptions1 do
        (loop for d2 in descriptions2 
  	      when (and (eq (send d1 :description-type)  (send d2 :description-type))
			(or (eq (send d1 :descriptor) (send d2 :descriptor))
			    (slip-linked? (send d1 :descriptor) 
			              (send d2 :descriptor))))
              do (push (make-concept-mapping 
			   (send d1 :description-type) (send d2 :description-type) 
	                   (send d1 :descriptor) (send d2 :descriptor)
                           obj1 obj2) result)))
  result)

;---------------------------------------------
; make-correspondence | Correspondence.__init__
; defflavor correspondence
;---------------------------------------------

;---------------------------------------------
; correspondence.print | REMOVED
;---------------------------------------------

;---------------------------------------------
; correspondence.other-obj
;---------------------------------------------

;---------------------------------------------
; correspondence.other-group
;---------------------------------------------

;---------------------------------------------
; correspondence.letter-span
;---------------------------------------------

;---------------------------------------------
; correspondence.add-accessory-concept-mapping
;---------------------------------------------

;---------------------------------------------
; build-correspondence | Workspace.build_correspondence
;---------------------------------------------

;---------------------------------------------
; break-correspondence | Workspace.break_correspondence
;---------------------------------------------

;---------------------------------------------
; bottom-up-correspondence-scout | CorrespondenceBottomUpScout
;---------------------------------------------

;---------------------------------------------
; important-object-correspondence-scout | CorrespondenceImportantObjectScout
;---------------------------------------------

;---------------------------------------------
; correspondence-strength-tester | CorrespondenceStrengthTester
;---------------------------------------------

;---------------------------------------------
; correspondence-builder | CorrespondenceBuilder
;---------------------------------------------

;---------------------------------------------
; correspondence.concept-mapping-present?
;---------------------------------------------

;---------------------------------------------
; correspondence.add-concept-mappings
;---------------------------------------------

;---------------------------------------------
; correspondence.slippage-list
;---------------------------------------------

;---------------------------------------------
; correspondence.relevant-concept-mappings
;---------------------------------------------

;---------------------------------------------
; correspondence.distinguishing-concept-mappings
;---------------------------------------------

;---------------------------------------------
; correspondence.relevant-distinguishing-cms
;---------------------------------------------

;---------------------------------------------
; correspondence.get-incompatible-correspondences | ditto
;---------------------------------------------

;---------------------------------------------
; correspondence.get-incompatible-bond
;---------------------------------------------

;---------------------------------------------
; correspondence.incompatible-rule?
;---------------------------------------------

;-------------------------------------------------------
; correspondence.proposed? | Correspondence.is_proposed
;-------------------------------------------------------

;---------------------------------------------
; propose-correspondence | Workspace.propose_correspondence 
;---------------------------------------------
