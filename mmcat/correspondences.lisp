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
; correspondence.get-incompatible-correspondences | ditto
;---------------------------------------------

(defmethod (correspondence :get-incompatible-bond)
           (&aux bond1 bond2 bond-concept-mapping-list 
		 incompatible-bond)
            
; Returns the bond that is incompatible with this correspondence, if any.
; E.g., consider the problem "abc -> abd, srqp -> ?"  If there is a 
; left-to-right bond from the A to the B in "abc" and a right-to-left 
; bond from the P to the Q in "srqp", then the P-Q bond is 
; incompatible with a correspondence between the C and the P.

  (if* (send obj1 :leftmost-in-string?)
   then (setq bond1 (send obj1 :right-bond))
   else (setq bond1 (send obj1 :left-bond)))

  (if* (send obj2 :leftmost-in-string?)
   then (setq bond2 (send obj2 :right-bond))
   else (setq bond2 (send obj2 :left-bond)))

  (if* (and bond1 bond2 (send bond1 :direction-category) 
	                (send bond2 :direction-category))
   then (setq bond-concept-mapping-list
	      (list (make-concept-mapping 
			plato-direction-category plato-direction-category
		        (send bond1 :direction-category)
			(send bond2 :direction-category)
			nil nil)))
        (if* (incompatible-concept-mapping-lists? 
		 bond-concept-mapping-list
                 concept-mapping-list)
         then (setq incompatible-bond bond2)))
  incompatible-bond)

;---------------------------------------------

(defmethod (correspondence :incompatible-rule?) ()
; A rule is incompatible with this correspondence 
; if obj1 is the changed object, and obj2 doesn't have the rule's descriptor1 
; (possibly modulo slippages in this correspondence or other already-existing 
; slippages) in its relevant-descriptions.
; For example, if the problem is abc -> abd, bcd -> ? and the rule is 
; "Replace rightmost letter by successor", if a correspondence is built 
; between the two C's, then the rule is incompatible with that correspondence,
; because the C in the target string doesn't have the description "rightmost" 
; (or a slipped version of it).  
  (and (send obj1 :changed?) *rule*
       (and 
           ; Not a member of this correspondence's concept-mapping-list.
	   (not (memq (send *rule* :descriptor1) 
		      (send-method-to-list concept-mapping-list :descriptor1)))
           ; Not already a member of a slippage in the workspace.
           (not (memq (send *rule* :descriptor1) 
		      (send-method-to-list
		          (send-method-to-list 
			      (send obj2 :relevant-descriptions) :descriptor) 
			  :apply-slippages 
			  (send *workspace* :slippage-list)))))))

;-------------------------------------------------------
; correspondence.proposed? | Correspondence.is_proposed
;-------------------------------------------------------

;---------------------------------------------
; propose-correspondence | Workspace.propose_correspondence 
;---------------------------------------------
