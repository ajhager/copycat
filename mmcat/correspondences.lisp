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
; bottom-up-correspondence-scout | CorrespondenceButtomUpScout
;---------------------------------------------

(defun important-object-correspondence-scout 
       (&aux obj1 obj2-candidates obj2 obj1-description obj1-descriptor 
	     obj2-descriptor concept-mapping-list slippage-probability
	     concept-mappings-possible? 
	     distinguishing-concept-mapping-list flip-obj2?  
	     possible-opposite-concept-mappings old-obj2-string-number)

; Chooses an object from the initial string probabilistically based on 
; importance.  Probabilistically picks a description of the object, and 
; looks for an object in the target string with the same description, modulo 
; the appropriate slippage, if any of the slippages currently in the 
; workspace apply.  If such an object is found, then finds all 
; concept-mappings between nodes at most one link away. If any are 
; found, makes a proposed correspondence between the two objects, including 
; all the concept-mappings, and posts a correspondence-strength-tester 
; codelet with urgency a function of the average strength of the 
; distinguishing concept-mappings.

(block nil
  (if* %verbose% 
   then (format t "In important-obj-correspondence-scout~&"))

  (setq obj1 (send *initial-string* :choose-object ':relative-importance))
  (if* %verbose% 
   then (format t "Chose object ") (send obj1 :print))

   ; Choose a description probabilistically, by conceptual-depth.
  (setq obj1-description 
       (send obj1 :choose-relevant-distinguishing-description-by-conceptual-depth))

  (if* (null obj1-description) 
   then (if* %verbose% 
	 then (format t "Can't choose description for obj1.  Fizzling.~&"))
        (return))

  (setq obj1-descriptor (send obj1-description :descriptor))

  ; Find the corresponding obj2-descriptor, given the current slippages in 
  ; the workspace.
  (setq obj2-descriptor
	(loop for slippage in (send *workspace* :slippage-list)
	      when (eq (send slippage :descriptor1) obj1-descriptor)
	      return (send slippage :descriptor2)
	      finally (return obj1-descriptor)))
  
  (if* %verbose% 
   then (format t "obj1 descriptor: ~a; obj2 descriptor: ~a~&"
	        (send obj1-descriptor :pname) (send obj2-descriptor :pname)))

  ; Now look for a object with that description in the target string
  ; If there is more than one, choose one probabilistically by 
  ; inter-string-salience.  
  (setq obj2-candidates 
	(loop for obj in (send *target-string* :object-list) 
  	      when (loop for d in (send obj :relevant-descriptions)
	                 when (eq (send d :descriptor) 
				  obj2-descriptor)
                             return t
			     finally (return nil))
	      collect obj))

  ; If there are no such objects in the target string, fizzle.
  (if* (null obj2-candidates) 
   then (if* %verbose% 
	 then (format t "No object with that descriptor.  Fizzling.~&"))
        (return))

  (if* %verbose% 
   then (format t "The candidates for obj2 are ")
	(send-method-to-list obj2-candidates :print))

  ; Select a target-string object from the possible candidates, 
  ; probabilistically by inter-string-salience.
  (setq obj2 (select-list-item-by-method obj2-candidates 
		                         ':inter-string-salience))

  (if* %verbose% then (format t "Chose obj2: " (send obj2 :print)))

  ; If one object spans the whole string and the other doesn't, then fizzle.
  ; (This probably isn't right.)
  (if* (or (and (send obj1 :spans-whole-string?)
		(not (send obj2 :spans-whole-string?)))
           (and (send obj2 :spans-whole-string?)
		(not (send obj1 :spans-whole-string?))))
   then (if* %verbose% 
	 then (format t "One obj spans string; other doesn't. Fizzling.~&"))
        (return))

  (if* %workspace-graphics% then (draw-correspondence-grope obj1 obj2))

  ; Get the list of possible concept-mappings.
  (setq concept-mapping-list 
	(get-concept-mapping-list 
	    obj1 obj2 (send obj1 :relevant-descriptions) 
	    (send obj2 :relevant-descriptions)))

  ; Decide probabilistically whether to go ahead based on the slippability of
  ; the concept-mappings.
  (setq concept-mappings-possible?
	(loop for cm in concept-mapping-list do
              (setq slippage-probability 
	            (get-temperature-adjusted-probability 
			(/ (send cm :slippability) 100)))
              (if* %verbose% 
               then (format t 
			    "About to decide on concept mapping: ")
	            (send cm :print) (format t "~%")
	            (format t "Slippage probability is ~a~&" 
			    slippage-probability))
              when (eq (flip-coin slippage-probability) 'heads)
	      return t
	      finally (return nil)))
  
  (if* (null concept-mappings-possible?) 
   then (if* %verbose% 
	 then (format t "Can't make necessary slippages.  Fizzling.~&"))
        (return))

  (if* %verbose% 
   then (format t "The concept-mapping list is ~&") 
        (loop for cm in concept-mapping-list do
	      (send cm :print)
	      (format t "; "))
	(format t "~%"))

  (setq distinguishing-concept-mapping-list
	(loop for cm in concept-mapping-list
	      when (send cm :distinguishing?)
	      collect cm))

  ; If no distinguishing concept-mappings, then fizzle.
  (if* (null distinguishing-concept-mapping-list)
   then (if* %verbose% 
	 then (format t "No distinguishing concept-mappings.  Fizzling.~&"))
        (return))

  (if* %verbose% 
   then (format t "Distinguishing concept-mappings: ") 
        (loop for cm in distinguishing-concept-mapping-list do
	      (send cm :print))
	(format t "~%"))

  ; If both objects span the string, and if all the distinguishing 
  ; concept-mappings (except string-position-category concept-mappings), 
  ; are opposites, and plato-opposite isn't active, then consider a 
  ; correspondence with the target-string group flipped.
  ; E.g., suppose in the problem "abc -> abd, pqrs -> ?"
  ; that "abc" has been described as an left-to-right succgrp and
  ; "pqrs" has been described as a right-to-left predgrp.  This puts
  ; top-down pressure on the program to flip "pqrs" so that it has
  ; the same description as "abc".  Notice that this can only happen
  ; at the time that the two strings are explicitly compared by a
  ; correspondence-scout codelet.
  (setq possible-opposite-concept-mappings
	(loop for cm in distinguishing-concept-mapping-list
	      when (and (not (eq (send cm :description-type1) 
			          plato-string-position-category))
  			 (not (eq (send cm :description-type1) 
			          plato-bond-facet)))
	      collect cm))

  (if* (and (send obj1 :string-spanning-group?) 
	    (send obj2 :string-spanning-group?)
	    (memq plato-direction-category
		  (send-method-to-list possible-opposite-concept-mappings
		                       :description-type1))
	    (all-opposite-concept-mappings? 
		possible-opposite-concept-mappings)
	    (not (send plato-opposite :active?)))
   then (setq old-obj2-string-number (send obj2 :string-number))
        (setq obj2 (send obj2 :flipped-version))
        (send obj2 :set-string-number old-obj2-string-number)
        (setq concept-mapping-list 
	      (get-concept-mapping-list 
		  obj1 obj2 (send obj1 :relevant-descriptions) 
		  (send obj2 :relevant-descriptions)))
	(setq flip-obj2? t)
	(if* %verbose% 
	 then (format t "Musing about a flipped target-string object.~&")))
	
  (propose-correspondence obj1 obj2 concept-mapping-list flip-obj2?)))

;---------------------------------------------

(defun correspondence-strength-tester 
       (proposed-correspondence flip-obj2?
        &aux obj1 obj2 proposed-correspondence-strength build-probability 
	     urgency)
; Calculates the proposed-correspondence's strength, and probabilistically 
; decides whether or not to post a correspondence-builder codelet.  If so, 
; the urgency of the correspondence-builder codelet is a function of the 
; strength.
(block nil      
  (if* %verbose% 
   then (format t "In correspondence-strength-tester with correspondence ")
        (send proposed-correspondence :print))

  (setq obj1 (send proposed-correspondence :obj1))
  (setq obj2 (send proposed-correspondence :obj2))

  ; If either of the two objects (or possibly a flipped version) no longer 
  ; exist, then fizzle.
  (if* (or (not (memq obj1 (send *workspace* :object-list)))
  	   (and (not (memq obj2 (send *workspace* :object-list)))
		(not (and flip-obj2? 
			  (send *target-string* :group-present? 
				(send obj2 :flipped-version))))))
   then (if* %verbose% 
	 then (format t "One or both of the objs no longer exist. ")
              (format t "Fizzling.~&"))
        (return))

  (if* %workspace-graphics% 
   then (send proposed-correspondence :flash-proposed))

  ; Calculate the proposed-correspondence's strength.
  (send proposed-correspondence :update-strength-values)
  (setq proposed-correspondence-strength 
	(send proposed-correspondence :total-strength))

  (if* %verbose% 
   then (format t "Proposed correspondence's strength: ~a~&" 
	          proposed-correspondence-strength))

  ; Decide whether or not to post a correspondence-builder codelet, based 
  ; on the strength of the proposed-correspondence.  This also depends on 
  ; temperature.  

  (setq build-probability 
	(get-temperature-adjusted-probability 
	    (/ proposed-correspondence-strength 100)))

  (if* %verbose% 
   then (format t "Build-probability: ~a~&" build-probability))

  (if* (eq (flip-coin build-probability) 'tails)
   then (if* %verbose% 
	 then (format t "Correspondence not strong enough.  Fizzling.~&"))
        (send *workspace*
	      :delete-proposed-correspondence proposed-correspondence)
        (if* %workspace-graphics% 
	 then (send proposed-correspondence :erase-proposed))
        (return))
        
  ; Activate-from-workspace some descriptions.
  (loop for cm in (send proposed-correspondence :concept-mapping-list) do
	(send (send cm :description-type1) :activate-from-workspace)
	(send (send cm :descriptor1) :activate-from-workspace)
	(send (send cm :description-type2) :activate-from-workspace)
	(send (send cm :descriptor2) :activate-from-workspace))

  (if* %workspace-graphics% 
   then (send proposed-correspondence :erase-proposed))
  (send proposed-correspondence :set-proposal-level 2)
  (setq urgency proposed-correspondence-strength)
  (if* %verbose% 
   then (format t "Strong enough. ")
        (format t "Posting correspondence-builder with urg: ~a~&"
		(get-urgency-bin urgency)))

  ; Post the correspondence-builder codelet.  If "flip-obj2?" is t, then
  ; the proposed correspondence proposes a flipped version of obj2 rather
  ; than obj2 itself.
  (send *coderack* :post 
	(make-codelet 'correspondence-builder 
	    (list proposed-correspondence ':flip-obj2? flip-obj2?)
            (get-urgency-bin urgency)))
  (if* %workspace-graphics% 
   then (send proposed-correspondence :draw-proposed))))

;---------------------------------------------

(defun correspondence-builder 
       (proposed-correspondence &key flip-obj2? 
	&aux obj1 obj2 existing-correspondence existing-obj2-group
             concept-mappings-to-be-added incompatible-correspondences 
	     incompatible-group incompatible-bond incompatible-rule? 
	     fight-result)
; Tries to build the proposed correspondence, fighting against competitors if
; necessary.  If "flip-obj2?" is t, then the proposed correspondence proposes 
; a flipped version of obj2 rather than obj2 itself.
							    
(block nil  
  (if* %verbose% 
   then (format t "In correspondence-builder with correspondence: ")
        (send proposed-correspondence :print)
        (format t "~%"))

  (setq obj1 (send proposed-correspondence :obj1))
  (setq obj2 (send proposed-correspondence :obj2))

  ; If either of the two objects (or possibly a flipped version no longer 
  ; exist, then fizzle.
  (if* (or (not (memq obj1 (send *workspace* :object-list)))
  	   (and (not (memq obj2 (send *workspace* :object-list)))
		(not (and flip-obj2? 
			  (setq existing-obj2-group 
				(send *target-string* :group-present? 
				      (send obj2 :flipped-version)))))))
   then (if* %verbose% 
	 then (format t "One or both objs no longer exist. Fizzling.~&"))
        (return))

  ; If this correspondence is already present, then add any new 
  ; concept-mappings, activate the concept-mapping labels, and fizzle.
  (if* (setq existing-correspondence 
	     (send *workspace* :correspondence-present? 
		               proposed-correspondence))
   then (if* %verbose% 
	 then (format t "This correspondence already exists.~&")
	      (format t "Add new concept-mappings and fizzle...~&"))
        (send *workspace* :delete-proposed-correspondence 
	                  proposed-correspondence)
	(send-method-to-list 
	    (send-method-to-list 
		(send proposed-correspondence :concept-mapping-list) :label) 
	    :activate-from-workspace)
        (setq concept-mappings-to-be-added
	      (loop for cm in (send proposed-correspondence 
				    :concept-mapping-list) do
                    when (and (not (send existing-correspondence 
			                 :concept-mapping-present? cm)))
	                  collect cm))
        (if* concept-mappings-to-be-added
         then (if* %workspace-graphics% 
               then (send existing-correspondence :flash)
	            (send existing-correspondence :erase-concept-mappings))
	      (send existing-correspondence :add-concept-mappings 
		                            concept-mappings-to-be-added)
	      (if* %workspace-graphics% 
	       then (send existing-correspondence :draw-concept-mappings)))
	(return))

  ; If any of the concept-mappings on the concept-mapping list are no longer 
  ; relevant, then fizzle.
  (let ((all-concept-mappings-still-relevant?
	 (loop for cm in (send proposed-correspondence :concept-mapping-list)
	       when (not (send cm :relevant?))
	       return nil
	       finally (return t))))

    (if* (not all-concept-mappings-still-relevant?)
     then (if* %verbose% 
	   then (format t "Not all cm's are still relevant.  Fizzling.~&"))
          (send *workspace* :delete-proposed-correspondence 
		            proposed-correspondence)
          (if* %workspace-graphics% 
           then (send proposed-correspondence :erase-proposed))
          (return)))

  (if* %workspace-graphics% 
   then (if* (and (null (send (send proposed-correspondence :obj1) 
			      :correspondence)) (null flip-obj2?))
         then (send proposed-correspondence :flash-proposed)))
	   
  ; Take the proposed correspondence off the list of proposed correspondences.
  (send *workspace* :delete-proposed-correspondence proposed-correspondence)

  ; If there are incompatible correspondences, then fight.  The fight is 
  ; decided probabilistically.

  ; Find incompatible correspondences.
  (setq incompatible-correspondences 
	(send proposed-correspondence :get-incompatible-correspondences))

  (if* (and %verbose% incompatible-correspondences)
   then (format t "About to fight against incompatible correspondences: ~&")
        (loop for c in incompatible-correspondences do
	      (send c :print) (format t "~%")))

  ; The weights for the fight depend on the letter-span of the objects.
  ; This is one of the reasons the program prefers correspondences to groups 
  ; rather than to letters.  Another reason is that groups are more salient
  ; than letters, so they are more likely to be chosen by correspondence 
  ; scouts.
  (setq fight-result 
	(loop for incompatible-correspondence in incompatible-correspondences 
	      when (null (fight-it-out 
			     proposed-correspondence 
			     (send proposed-correspondence :letter-span)
	                     (list incompatible-correspondence)
			     (send incompatible-correspondence :letter-span)))
	      return nil
	      finally (return t)))

  (if* (null fight-result)
   then (if* %verbose% then (format t "Lost. Fizzling.~&"))
        (if* %workspace-graphics% 
	 then (send proposed-correspondence :erase-proposed))
        (return nil))

  (if* (and %verbose% incompatible-correspondences) 
   then (format t "Won against incompatible-correspondences!!~&"))

  ; If there is an incompatible bond, then fight against it, and its
  ; group, if any.  For now, only correspondences between objects
  ; on the ends of strings have the possibility of having incompatible
  ; bonds.  For example, in the problem "abc -> abd, pqrs -> ?",
  ; if there is a left-to-right successor bond from the 'a' to the 'b'
  ; in "abc", and a right-to-left predecessor bond from the 'q' to the 'p'
  ; in "pqrs", then these bonds will be incompatible with a correspondence 
  ; from the 'a' to the 'p' with concept-mapping "leftmost -> leftmost",
  ; since such a correspondence would also in effect be saying "right -> left",
  ; from the mapping between the two bonds.
  (if* (and (or (send obj1 :leftmost-in-string?) 
		(send obj1 :rightmost-in-string?))
	    (or (send obj2 :leftmost-in-string?)
                (send obj2 :rightmost-in-string?)))
   then (setq incompatible-bond 
	      (send proposed-correspondence :get-incompatible-bond))
        (if* incompatible-bond
         then (if* %verbose%
               then (format t "About to fight incompatible bond.~&"))
              ; A bond is weighted only 2/3 as much as a correspondence.
              (setq fight-result 
	            (fight-it-out proposed-correspondence 3
		                  (list incompatible-bond) 2))
              (if* (null fight-result)        
               then (if* %verbose% 
                     then (format t "Lost. Fizzling.~&"))
                    (if* %workspace-graphics% 
   	             then (send proposed-correspondence :erase-proposed))
                    (return))
              (if* %verbose% 
               then (format t "Won against incompatible bond!!~&"))
              ; If the incompatible bond is in a group, then fight 
	      ; against the group as well.
              (if* (setq incompatible-group 
			 (send incompatible-bond :group))
               then (if* %verbose%
                     then (format t "About to fight incompatible group.~&"))
                    ; The correspondence and the group get equal weights.
                    (setq fight-result 
	                  (fight-it-out proposed-correspondence 1 
		                        (list incompatible-group) 1))
                    (if* (null fight-result)        
                     then (if* %verbose% 
                           then (format t "Lost. Fizzling.~&"))
                          (if* %workspace-graphics% 
   	                   then (send proposed-correspondence :erase-proposed))
                          (return))
                    (if* %verbose% 
                     then (format t "Won against incompatible group!!~&")))))

  ; If the desired obj2 for this correspondence is a flipped version of the 
  ; existing object (indicated by a value of t for "flip-obj2?"), then try to 
  ; flip the existing group.  
  (if* flip-obj2?
   then (if* %verbose% 
	 then (format t "About to fight against existing obj2 group.~&"))
        (if* (fight-it-out proposed-correspondence 1 
		           (list existing-obj2-group) 1)
         then (if* %verbose% 
	       then (format t "Won against existing obj2!!~&"))
         else (if* %verbose% 
               then (format t "Lost. Fizzling.~&"))
              (if* %workspace-graphics% 
	       then (send proposed-correspondence :erase-proposed))
	      (return)))
        
  ; If there is an incompatible rule, then fight.
  (setq incompatible-rule? (send proposed-correspondence :incompatible-rule?))
  (if* incompatible-rule?
   then (if* %verbose% 
         then (format t "About to fight with incompatible rule.~&"))
        ; The correspondence and rule get equal weights.
        (setq fight-result (fight-it-out proposed-correspondence 1
		                        (list *rule*) 1))
        (if* (null fight-result)
         then (if* %verbose% 
               then (format t "Lost. Fizzling.~&"))
              (if* %workspace-graphics% 
               then (send proposed-correspondence :erase-proposed))
                    (return)))

  ; If this codelet hasn't fizzled by now, it has won against all incompatible
  ; structures, and can now break them.
  (if* incompatible-correspondences
   then (if* %verbose% 
	 then (format t "About to break incompatible correspondences~&"))
        (loop for c in incompatible-correspondences do 
	      (break-correspondence c)))

  (if* incompatible-bond
   then (if* %verbose% 
	 then (format t "About to break incompatible bond~&"))
	(break-bond incompatible-bond))

  (if* incompatible-group
   then (if* %verbose% then (format t "About to break incompatible group~&"))
	(break-group incompatible-group))

  (if* existing-obj2-group
   then (if* %verbose% 
	 then (format t "About to break old obj2 and build flipped version~&"))
	(break-group existing-obj2-group)
        (loop for bond 
	      in (send existing-obj2-group :bond-list) do
	      (break-bond bond))
        (loop for bond 
	      in (send obj2 :bond-list) do
	      (build-bond bond))
	(build-group obj2))

  (if* incompatible-rule?
   then (break-rule *rule*))

  (if* %verbose% 
   then (format t "About to build correspondence.~&"))
  (if* %workspace-graphics% 
   then (if* (send proposed-correspondence :drawn?) 
         then (send proposed-correspondence :erase-line)))

  (build-correspondence proposed-correspondence)))

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
