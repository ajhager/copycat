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
    :gettable-instance-variables
    :settable-instance-variables
    :initable-instance-variables)

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

(defmethod (bond :leftmost-in-string?) ()
; Returns t if the bond is on the left edge of the string.
  (= left-string-position 0))

;---------------------------------------------

(defmethod (bond :rightmost-in-string?) ()
; Returns t if the bond is on the right edge of the string.
  (if* (= right-string-position (1- (send string :length)))
   then t else nil))

;---------------------------------------------

(defmethod (bond :choose-left-neighbor) (&aux possible-left-neighbor
				              left-neighbor-list)
; Returns one of the left-neighbors of the bond, probabilistically,
; by salience.
  (if* (send self :leftmost-in-string?)
   then nil
   else (setq left-neighbor-list
	      (loop for left-neighbor-obj 
	            in (send (send self :left-obj) :all-left-neighbors) do
                       (setq possible-left-neighbor
		             (aref (send string :left-right-bond-array)
                                   (send left-neighbor-obj :string-number)
			           (send (send self :left-obj) 
					 :string-number)))
	      when possible-left-neighbor
	      collect possible-left-neighbor))
        (select-list-item-by-method left-neighbor-list ':salience)))

;---------------------------------------------

(defmethod (bond :choose-right-neighbor) (&aux possible-right-neighbor
				   	       right-neighbor-list)
; Returns one of the right-neighbors of the bond, probabilistically,
; by salience.
  (if* (send self :rightmost-in-string?)
   then nil
   else (setq right-neighbor-list
	      (loop for right-neighbor-obj 
	            in (send (send self :right-obj) :all-right-neighbors) do
                    (setq possible-right-neighbor
		          (aref (send string :left-right-bond-array)
			             (send (send self :right-obj) 
				           :string-number)
                                     (send right-neighbor-obj 
				           :string-number)))
	      when possible-right-neighbor
	      collect possible-right-neighbor))
         (select-list-item-by-method right-neighbor-list ':salience)))

;---------------------------------------------

(defmethod (bond :bond-members?) (obj1 obj2)
; Returns t if the two objects are the objects in this bond.    
  (and (or (eq from-obj obj1) (eq to-obj obj1)) 
       (or (eq from-obj obj2) (eq to-obj obj2))))

;---------------------------------------------

(defmethod (bond :in-group?) (g)
; Returns t if the bond is in the group g.
  (and (member from-obj (send g :object-list)) 
       (member to-obj (send g :object-list))))

;---------------------------------------------

(defun top-down-bond-scout--category 
    (bond-category &aux string obj1 obj2 from-obj to-obj 
			    bond-facet obj1-descriptor obj2-descriptor  
			    from-obj-descriptor to-obj-descriptor
			    i-relevance t-relevance
		            i-unhappiness t-unhappiness)
; Chooses a string probabilistically by the relevance of the given 
; bond-category in the string and the string's unhappiness.  Chooses an 
; object in that string probabilistically by intra-string-salience and a 
; neighbor of that object probabilistically, also by intra-string-salience.  
; Chooses a bond-facet (letter-category or length) 
; probabilistically, by relevance in the string.  Sees if there is a bond 
; of the given category between the two descriptors of this facet, and if so, 
; posts a bond-strength-tester codelet with urgency a function of the 
; degree of association of bonds of the bond-category.
(block nil

  (if* %verbose% 
   then (format t "~%In top-down-bond-scout--category, category: ~a~&" 
		(send bond-category :pname)))
	          
  ; Choose string.
  (setq i-relevance (send *initial-string* :local-bond-category-relevance 
			  bond-category))
  (setq t-relevance (send *target-string* :local-bond-category-relevance 
			  bond-category))
  (setq i-unhappiness (send *initial-string* :intra-string-unhappiness))
  (setq t-unhappiness (send *target-string* :intra-string-unhappiness))

  (if* %verbose% 
   then (format t "About to choose string.  Relevance of ~a is: " 
		(send bond-category :pname))
        (format t "initial string: ~a, target string: ~a~&"
	        i-relevance t-relevance)
	(format t "i-unhappiness: ~a, t-unhappiness: ~a~&"
		i-unhappiness t-unhappiness))

  (setq string 
	(nth (select-list-position 
		 (list (round (average i-relevance i-unhappiness))
		       (round (average t-relevance t-unhappiness))))
	     (list *initial-string* *target-string*)))
		
  (if* %verbose% 
   then (format t "Chose ~a~&" (send string :pname)))

  ; Choose object.
  (setq obj1 (send string :choose-object ':intra-string-salience))
  (if* %verbose% 
   then (format t "Chose obj1: ") (send obj1 :print))

  ; Choose neighbor.
  (setq obj2 (send obj1 :choose-neighbor))  
  (if* (null obj2) 
   then (if* %verbose% 
	 then (format t "This object has no neighbor.  Fizzling.~&"))
        (return))

  (if* %verbose% then (format t "Chose obj2: ") (send obj2 :print))
  
  (if* %workspace-graphics% then (draw-bond-grope obj1 obj2))

  ; Choose bond-facet.
      
  (setq bond-facet (choose-bond-facet obj1 obj2))

  (if* (null bond-facet)
   then (if* %verbose% 
         then (format t "No possible bond-facet.  Fizzling.~&"))
        (return))
     
  (if* %verbose% 
   then (format t "Using bond-facet ~a~&" (send bond-facet :pname)))

  ; Get the two descriptors of this facet, if they exist.
  (setq obj1-descriptor (send obj1 :get-descriptor bond-facet))
  (setq obj2-descriptor (send obj2 :get-descriptor bond-facet))

  (if* (or (null obj1-descriptor) (null obj2-descriptor))
   then (if* %verbose% 
	 then (format t "One object has no description with this facet.~&"))
        (return))

  ; See if there is a possible bond.
  (if* (eq (get-bond-category obj1-descriptor obj2-descriptor) 
	   bond-category)
   then (setq from-obj obj1 to-obj obj2
   	      from-obj-descriptor obj1-descriptor
	      to-obj-descriptor obj2-descriptor)
   else (if* (eq (get-bond-category obj2-descriptor obj1-descriptor) 
		 bond-category)
	 then (setq from-obj obj2 to-obj obj1
	            from-obj-descriptor obj2-descriptor
		    to-obj-descriptor obj1-descriptor)
	 else (if* %verbose% 
	       then (format t "No bond.  Fizzling.~&"))
              (return)))

  (propose-bond from-obj to-obj bond-category bond-facet 
		from-obj-descriptor to-obj-descriptor)))

;---------------------------------------------

(defun top-down-bond-scout--direction
    (direction-category &aux string from-obj to-obj bond-facet 
			     from-obj-descriptor to-obj-descriptor  
  			     bond-category i-relevance t-relevance
			     i-unhappiness t-unhappiness)
; Chooses a string probabilistically by the relevance of the given 
; direction-category in the string and the string's unhappiness.  Chooses an 
; object in that string probabilistically by intra-string-salience and a 
; neighbor of that object in the given direction.
; Chooses a bond-facet (letter-category or length) 
; probabilistically, by relevance in the string.  Sees if there is a bond 
; of the given direction between the two descriptors of this facet, and if so, 
; posts a bond-strength-tester codelet with urgency a function of the 
; degree of association of bonds of the bond-category.
(block nil

  (if* %verbose% 
   then (format t "~%In top-down-bond-scout--direction, direction: ~a~&" 
		(send direction-category :pname)))
	          
  ; Choose string probabilistically as a function of 
  ; local-direction-category-relevance.
  (setq i-relevance (send *initial-string* :local-direction-category-relevance 
			direction-category))
  (setq t-relevance (send *target-string* :local-direction-category-relevance 
			direction-category))

  (setq i-unhappiness (send *initial-string* :intra-string-unhappiness))
  (setq t-unhappiness (send *target-string* :intra-string-unhappiness))

  (if* %verbose% 
   then (format t "About to choose string.  Relevance of ~a is: " 
		  (send direction-category :pname))
        (format t "initial string: ~a, target string: ~a~&"
		  i-relevance t-relevance)
	(format t "i-unhappiness: ~a, t-unhappiness: ~a~&"
		  i-unhappiness t-unhappiness))

  (setq string 
	(nth (select-list-position 
		 (list (round (average i-relevance i-unhappiness))
		       (round (average t-relevance t-unhappiness))))
	     (list *initial-string* *target-string*)))
		
  (if* %verbose% 
   then (format t "Chose ~a~&" (send string :pname)))

  ; Choose object.
  (setq from-obj (send string :choose-object ':intra-string-salience))
  (if* %verbose% 
   then (format t "Chose from-obj: ") (send from-obj :print))

  ; Choose neighbor.
  (setq to-obj (if* (eq direction-category plato-left)
                then (send from-obj :choose-left-neighbor)
                else (send from-obj :choose-right-neighbor)))

  (if* (null to-obj) 
   then (if* %verbose% 
	 then (format t "This object has no ~a neighbor.  Fizzling.~&"
		      (send direction-category :pname)))
        (return))

  (if* %verbose% 
   then (format t "Chose to-obj: ") (send to-obj :print))
  
  (if* %workspace-graphics% then (draw-bond-grope from-obj to-obj))

  ; Choose bond-facet.
  (setq bond-facet (choose-bond-facet from-obj to-obj))

  (if* (null bond-facet)
   then (if* %verbose% 
         then (format t "No possible bond-facet.  Fizzling.~&"))
        (return))
     
  (if* %verbose% 
   then (format t "Using bond-facet ~a~&" (send bond-facet :pname)))

  ; Get the two descriptors of this facet, if they exist.
  (setq from-obj-descriptor (send from-obj :get-descriptor bond-facet))
  (setq to-obj-descriptor (send to-obj :get-descriptor bond-facet))

  (if* (or (null from-obj-descriptor) (null to-obj-descriptor))
   then (if* %verbose% 
	 then (format t 
		      "One object has no description with this facet.~&"))
        (return))

  ; See if there is a possible bond.
  (setq bond-category 
	(get-bond-category from-obj-descriptor to-obj-descriptor))

  (if* (or (null bond-category) (not (send bond-category :directed?)))
   then (if* %verbose% 
	 then (format t "No bond in this direction.  Fizzling.~&"))
        (return))

  (propose-bond from-obj to-obj bond-category bond-facet 
		from-obj-descriptor to-obj-descriptor)))

;---------------------------------------------
; bond-strength-tester | BondStrengthTester
;---------------------------------------------

;---------------------------------------------
; bond-builder | BondBuilder
;---------------------------------------------
	     
(defun build-bond (new-bond)
; This function actually builds the new bond.
  (send new-bond :set-proposal-level %built%)
  (send (send new-bond :string) :add-bond new-bond)
  (send (send new-bond :from-obj) :add-outgoing-bond new-bond)
  (send (send new-bond :to-obj) :add-incoming-bond new-bond)

  ; All sameness bonds go in both directions.  Build the bond in the
  ; other direction.
  (if* (eq (send new-bond :bond-category) plato-sameness)  
   then (send (send new-bond :to-obj) :add-outgoing-bond new-bond)
        (send (send new-bond :from-obj) 
	      :add-incoming-bond new-bond))

  (send (send new-bond :left-obj) :set-right-bond new-bond)
  (send (send new-bond :right-obj) :set-left-bond new-bond)

  ; Activate-from-workspace descriptions of the bond.
  (send (send new-bond :bond-category) :activate-from-workspace)
  (if* (send new-bond :direction-category)
   then (send (send new-bond :direction-category) 
	      :activate-from-workspace))
  (if* %workspace-graphics% then (send new-bond :draw)))
  
;---------------------------------------------

(defun break-bond (bond)
  (send (send bond :string) :delete-bond bond)
  (send (send bond :from-obj) 
	:set-outgoing-bonds 
	(remove bond (send (send bond :from-obj) :outgoing-bonds)))
  (send (send bond :to-obj) 
	:set-incoming-bonds 
	(remove bond (send (send bond :to-obj) :incoming-bonds)))

  ; All sameness bonds go in both directions.  Break the bond in the
  ; other direction.
  (if* (eq (send bond :bond-category) plato-sameness)  
   then (send (send bond :to-obj) :set-outgoing-bonds 
	      (remove bond (send (send bond :to-obj) 
				     :outgoing-bonds)))
        (send (send bond :from-obj) :set-incoming-bonds 
	      (remove bond (send (send bond :from-obj) 
				     :incoming-bonds))))

  (send (send bond :left-obj) :set-right-bond nil)
  (send (send bond :right-obj) :set-left-bond nil)

  (if* %workspace-graphics% then (send bond :erase)))

;---------------------------------------------

(defmethod (bond :flipped-version) (&aux flipped-bond)
; Returns the flipped version of this bond (e.g., if the bond is
; a successor bond going to the right, returns a predecessor bond 
; going to the left, using the same two objects).
  (setq flipped-bond
	(make-bond to-obj from-obj 
                   (send bond-category :get-related-node plato-opposite)
                    bond-facet to-obj-descriptor from-obj-descriptor))
  (send flipped-bond :set-proposal-level (send self :proposal-level))
  flipped-bond)

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

(defmethod (bond :get-incompatible-bonds) ()
; Returns the bonds that are incompatible with the given bond, that is
; any bonds involving one or both of the same two objects bonded by 
; this bond.
  (remove-duplicates 
      (flatten (list (send left-obj :right-bond) 
		     (send right-obj :left-bond)))))

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
