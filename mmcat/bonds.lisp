;---------------------------------------------
; make-bond | Bond.__init__
; defflavor bond
;---------------------------------------------

;---------------------------------------------
; bond.print | REMOVED
;---------------------------------------------

;---------------------------------------------
; bond :letter-span
;---------------------------------------------

;---------------------------------------------
; bond.leftmost-in-string?
; bond.rightmost-in-string?
;---------------------------------------------

;---------------------------------------------
; bond.choose-left-neighbor
;---------------------------------------------

;---------------------------------------------
; bond.choose-right-neighbor
;---------------------------------------------

;---------------------------------------------
; bond :bond-members? REMOVED
;---------------------------------------------

;---------------------------------------------
; bond.in-group?
;---------------------------------------------

;---------------------------------------------
; top-down-bond-scout--category | BondTopDownCategoryScout
;---------------------------------------------

;---------------------------------------------
; top-down-bond-scout--direction | BondTopDownDirectionScout
;---------------------------------------------

;---------------------------------------------
; bond-strength-tester | BondStrengthTester
;---------------------------------------------

;---------------------------------------------
; bond-builder | BondBuilder
;---------------------------------------------

;---------------------------------------------
; build-bond | Workspace.build_bond
;---------------------------------------------
	     
;---------------------------------------------
; break-bond | Workspace.break_bond
;---------------------------------------------

;---------------------------------------------
; bond.flipped-version
;---------------------------------------------

;---------------------------------------------
; same-bond? | Bond.__eq__
;---------------------------------------------

;---------------------------------------------
; get-bond-category | get_bond_category
;---------------------------------------------

;---------------------------------------------
; bond.get-incompatible-bonds | Bond.incompatible_bonds
;---------------------------------------------

(defmethod (bond :get-incompatible-correspondences) 
           (&aux correspondence string-position-category-concept-mapping
		 other-obj other-bond bond-concept-mapping
		 incompatible-correspondence-list)
; Returns the correspondences that are incompatible with this bond.
; This only applies to directed bonds and to correspondences between
; objects at the edges of strings.  E.g., in "abc -> abd, pqrs -> ?",
; if there is a correspondence between the 'a' and the 'p' (with
; concept-mapping "leftmost -> leftmost"), and a right-going successor bond
; from the 'a' to the 'b' in 'abc', then the correspondence will be 
; incompatible with a left-going predecessor bond from the 'q' to the 'p'
; in 'pqrs', because the correspondence would then imply both 
; "leftmost -> leftmost" (the letters) and "right -> left" (the bonds).
(block nil
  (if* (send self :leftmost-in-string?)
   then (setq correspondence (send (send self :left-obj) :correspondence))
        (if* (null correspondence) then (return))
        ; See if the correspondence has a string-position-category 
	; concept mapping.
	(setq string-position-category-concept-mapping
	      (loop for cm in (send correspondence :concept-mapping-list)
                    when (eq (send cm :description-type1) 
			     plato-string-position-category)
		    return cm))
	(if* (null string-position-category-concept-mapping) then (return))

        ; Now see if there is a conflicting bond.
        (setq other-obj 
	      (send correspondence :other-obj (send self :left-obj)))
        (if* (send other-obj :leftmost-in-string?)
         then (setq other-bond (send other-obj :right-bond))
	 else (if* (send other-obj :rightmost-in-string?)
               then (setq other-bond (send other-obj :left-bond))
	       else (return)))
	(if* (or (null other-bond) 
		 (null (send other-bond :direction-category))) 
	 then (return))
	(setq bond-concept-mapping
	      (make-concept-mapping 
		  plato-direction-category plato-direction-category
 	          direction-category (send other-bond :direction-category)
		  nil nil))
        (if* (incompatible-concept-mappings?
		 bond-concept-mapping
                 string-position-category-concept-mapping)
         then (push correspondence incompatible-correspondence-list))))

  (block nil
  (if* (send self :rightmost-in-string?)
   then (setq correspondence (send (send self :right-obj) :correspondence))
        (if* (null correspondence) then (return))
        ; See if the correspondence has a string-position-category 
	; concept mapping.
	(setq string-position-category-concept-mapping
	      (loop for cm in (send correspondence :concept-mapping-list)
                    when (eq (send cm :description-type1) 
			     plato-string-position-category)
		    return cm))
	(if* (null string-position-category-concept-mapping) then (return))
        ; Now see if there is a conflicting bond.
        (setq other-obj 
	      (send correspondence :other-obj (send self :right-obj)))
        (if* (send other-obj :leftmost-in-string?)
         then (setq other-bond (send other-obj :right-bond))
	 else (if* (send other-obj :rightmost-in-string?)
               then (setq other-bond (send other-obj :left-bond))
	       else (return)))
	(if* (or (null other-bond) 
		 (null (send other-bond :direction-category))) 
	 then (return))
	(setq bond-concept-mapping
	      (make-concept-mapping 
		  plato-direction-category plato-direction-category
	          direction-category (send other-bond :direction-category)
		  nil nil))
        (if* (incompatible-concept-mappings? 
		 bond-concept-mapping
                 string-position-category-concept-mapping)
         then (push correspondence incompatible-correspondence-list))))
  
  incompatible-correspondence-list)
  
;---------------------------------------------
; proposed? | Bond.is_proposed
;---------------------------------------------

;---------------------------------------------
; propose-bond | Workspace.propose_bond
;---------------------------------------------

;---------------------------------------------
; choose-bond-facet | Workspace.choose_bond_facet
;---------------------------------------------
