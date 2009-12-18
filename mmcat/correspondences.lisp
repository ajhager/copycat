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
; supporting-correspondences?
;---------------------------------------------

;---------------------------------------------
; incompatible-correspondences?
;---------------------------------------------

;---------------------------------------------
; get-concept-mapping-list | Workspace.get_concept_mappings
;---------------------------------------------

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
