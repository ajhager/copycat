(defflavor bond
    (left-string-position  
     right-string-position
     bond-category ; e.g., plato-successor
     (direction-category nil) ; e.g., plato-right.
     from-obj to-obj ; The objects that the bond comes from and goes to.
     left-obj right-obj ; The leftmost and rightmost objects in the bond.
     bond-facet ; Which facet of the objects is being related, 
                    ; e.g., plato-letter-category or plato-length.
     from-obj-descriptor ; Which descriptor of the from-obj is being 
                         ; related, e.g., plato-a.
     to-obj-descriptor)  ; Which descriptor of the to-obj is being related,
                         ; e.g., plato-b.
    (workspace-structure)

;---------------------------------------------

(defun make-bond (from-obj to-obj bond-category bond-facet 
		  from-obj-descriptor to-obj-descriptor &aux new-bond)
; This returns a new bond.    
  (setq new-bond
       (make-instance 'bond 
         :from-obj from-obj 
         :to-obj to-obj
	 :structure-category 'bond
	 :bond-category bond-category
	 :direction-category ; Sameness bonds have no direction; 
	                     ; other bonds do.
	 (if* (eq bond-category plato-sameness) 
          then nil
          else (if* (< (send from-obj :left-string-position) 
		       (send to-obj :left-string-position))
                then plato-right else plato-left))
         :string (send from-obj :string)
         :left-string-position 
	 (min (send from-obj :left-string-position) 
              (send to-obj :left-string-position))
         :right-string-position 
	 (max (send from-obj :right-string-position) 
              (send to-obj :right-string-position))
         :bond-facet bond-facet ; For now this is always either 
                                        ; letter-category or length.
	 :from-obj-descriptor from-obj-descriptor
	 :to-obj-descriptor to-obj-descriptor))
  
  (if* (< (send from-obj :left-string-position) 
	  (send to-obj :left-string-position))
   then (send new-bond :set-left-obj from-obj)
        (send new-bond :set-right-obj to-obj)
   else (send new-bond :set-left-obj to-obj)
        (send new-bond :set-right-obj from-obj))
  
  (if* %workspace-graphics% then (send new-bond :init-graphics))
  new-bond)

;---------------------------------------------
; bond.print | REMOVED
;---------------------------------------------

(defmethod (bond :letter-span) ()
; Returns the number of letters spanned by the bond.  This is 2 if the 
; objects are not groups; otherwise it is the sum of the lengths of the
; groups.
  (+ (send from-obj :letter-span) (send to-obj :letter-span)))

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

(defmethod (bond :bond-members?) (obj1 obj2)
; Returns t if the two objects are the objects in this bond.    
  (and (or (eq from-obj obj1) (eq to-obj obj1)) 
       (or (eq from-obj obj2) (eq to-obj obj2))))

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

(defun same-bond? (b1 b2)
; Returns t if b1 and b2 represent the same bond.    
  (and b1 b2
       (eq (send b1 :from-obj) (send b2 :from-obj))
       (eq (send b1 :to-obj) (send b2 :to-obj))
       (eq (send b1 :bond-category) (send b2 :bond-category))
       (eq (send b1 :direction-category) (send b2 :direction-category))))

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
