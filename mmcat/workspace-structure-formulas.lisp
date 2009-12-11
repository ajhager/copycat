;---------------------------------------------
; workspace-structure.update_strengths
;---------------------------------------------

;---------------------------------------------
; description.calculate-internal-strength
;---------------------------------------------

;---------------------------------------------
; description.calculate-external-strength
;---------------------------------------------

;---------------------------------------------
; bond.calculate-internal-strength
; bond.calculate-external-strength
;---------------------------------------------

;---------------------------------------------
; group.calculate-internal-strength
;---------------------------------------------
    
;---------------------------------------------
; group.calculate-external-strength
;---------------------------------------------

;---------------------------------------------
; correspondence.calculate-internal-strength
; correspondence.calculate-external-strength
;---------------------------------------------

(defmethod (correspondence :internally-coherent?) 
           (&aux cm-list result)
; for now this returns t if there is any pair of relevant-distinguishing 
; concept-mappings that support each other.  this isn't quite right.
  (setq cm-list
	(send self :relevant-distinguishing-cms))
  (if* (> (length cm-list) 1)
   then (loop for cm in cm-list until result do
	      (loop for other-cm in (remove cm cm-list) 
	            when (supporting-concept-mappings? cm other-cm)
	            return (setq result t))))
  result)
	      
;---------------------------------------------
; rule.calculate-internal-strength
;---------------------------------------------

;---------------------------------------------
; rule.calculate-external-strength
;---------------------------------------------

;---------------------------------------------
; workspace-structure.calculate-total-strength
;---------------------------------------------

;---------------------------------------------
; workspace-structure.total-weakness
;---------------------------------------------

;---------------------------------------------
; concept-mapping.slippability
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

;---------------------------------------------
; supporting-concept-mappings? | Mapping.is_supporting_concept_mapping
;---------------------------------------------

;---------------------------------------------
; incompatible-concept-mappings? | Mapping.is_incompatible_concept_mapping
; incompatible-concept-mapping-lists?
;---------------------------------------------

;---------------------------------------------
; related? | slipnode.are_related
;---------------------------------------------

;---------------------------------------------
; linked? | slipnode.are_linked
;---------------------------------------------

;---------------------------------------------
; slip-linked? | slipnode.are_slip_linked
;---------------------------------------------

;---------------------------------------------
; slipnode.bond-degree-of-association | slipnode.bond_degree_of_association
;---------------------------------------------

;---------------------------------------------
; bond.importance
;---------------------------------------------

;---------------------------------------------
; bond.happiness
; bond.unhappiness
;---------------------------------------------

;---------------------------------------------
; bond.salience
;---------------------------------------------

;---------------------------------------------
; slipnode.local-descriptor-support | slipnode.local_descriptor_support
;---------------------------------------------
  
;---------------------------------------------
; slipnode.local-description-type-support | slipnode.local_description_type_support
;---------------------------------------------

;---------------------------------------------
; slipnode.total-description-type-support | slipnode.total_desription_type_support
;---------------------------------------------

;---------------------------------------------
; description.local-support
;---------------------------------------------

;---------------------------------------------
; bond.number-of-local-supporting-bonds
;---------------------------------------------

;---------------------------------------------
; bond.local-density
;---------------------------------------------

;---------------------------------------------
; bond.local-support
;---------------------------------------------

;---------------------------------------------
; group.number-of-local-supporting-groups
;---------------------------------------------

;---------------------------------------------
; group.local-density
;---------------------------------------------

;---------------------------------------------
; group.local-support
;---------------------------------------------
	   
;---------------------------------------------
; correspondence.support | Correspondence.support
;---------------------------------------------

;---------------------------------------------
; local-bond-category-relevance
;---------------------------------------------

;---------------------------------------------
; local-direction-category-relevance
;---------------------------------------------

;---------------------------------------------
; recursive-group-member? | Gropu.is_recursive_member
;---------------------------------------------
