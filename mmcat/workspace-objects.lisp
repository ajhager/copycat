;---------------------------------------------
; defflavor letter | Letter.__init__
; make-letter
;---------------------------------------------

;---------------------------------------------
; workspace-object.print | REMOVED
;---------------------------------------------

(defflavor workspace-object 
     (string ; The string the object is in (e.g., initial-string).

      string-number ; The object's unique identifying number in its string.
                    ; This number is used for storing and referencing 
		    ; objects and structures stored in vectors and arrays.

      left-string-position ; If the object is a group, this is the string 
                           ; position of the leftmost letter in the group.  
			   ; If the object is a letter, this is its string 
			   ; position.

      right-string-position ; Similar to left-string-position.  For letters,
                            ; this is equal to left-string-position.

     ; Some values for the object.  See the file "object-formulas.l"
     ; for descriptions of these values.
     (raw-importance 0)  
     (relative-importance 0) 
     (intra-string-happiness 0) (intra-string-unhappiness 0) 
     (inter-string-happiness 0) (inter-string-unhappiness 0)
     (total-happiness 0) (total-unhappiness 0)
     (intra-string-salience 0) (inter-string-salience 0) (total-salience 0)

     (descriptions nil) ; A list of descriptions of the object.
     (extrinsic-descriptions nil) ; A list of descriptions with respect to
                                  ; other objects.  For now, only the
				  ; letter in the modified string that 
				  ; corresponds to the changed letter in
				  ; the initial string can be given an 
				  ; extrinsic description (e.g., in 
                                  ; "abc -> abd, pqrs -> ?", the 'd' would
				  ; probably get the extrinsic description
				  ; "Successor of the 'c' in 'abc'").

     ; Variables for structures that can be attached to the object.
     (outgoing-bonds nil) (incoming-bonds nil) 
     (left-bond nil) (right-bond nil)
     (group nil) (replacement nil) (correspondence nil) 

     (changed? nil) ; T if the letter is the initial-string letter that
                    ; changed.

     (new-answer-letter? nil) ; T if this is a new letter for the answer,
                              ; not corresponding to any of the target-string
			      ; letters (this is used when the 
			      ; translated-rule calls for length changes).

     (clamp-salience? nil) ; T if the salience of this object is to be 
                           ; clamped (used when a snag has been hit, to
                           ; clamp the salience of the object causing the 
                           ; snag).
     pname ; The print name of the object.
     ())

;---------------------------------------------
; workspace-object.letter-span | Object.letter_span
;---------------------------------------------

;---------------------------------------------
; workspace-object.letter-list | Object.letters
;---------------------------------------------

;---------------------------------------------
; workspace-object.leftmost-in-string?
; workspace-object.rightmost-in-string?
; workspace-object.middle-in-string?
;---------------------------------------------

;---------------------------------------------
; workspace-object.spans-whole-string? | Object.spans_whole_string
; workspace-object.string-spanning-group? | Object.is_string_spanning_group
;---------------------------------------------

;---------------------------------------------
; workspace-object.ungrouped-left-neighbor
; workspace-object.ungrouped-right-neighbor
;---------------------------------------------

;---------------------------------------------
; workspace-object.all-left-neighbors()
; workspace-object.all-right-neighbors()
; workspace-object.all-neighbors()
;---------------------------------------------

;---------------------------------------------
; workspace-object.random-left-neighbor
; workspace-object.random-right-neighbor
; workspace-object.random-neighbor
;---------------------------------------------

;---------------------------------------------
; workspace-object.choose-left-neighbor
; workspace-object.choose-right-neighbor
; workspace-object.choose-neighbor | Object.choose_neighber
;---------------------------------------------

;---------------------------------------------
; workspace-object.all-bonds | Object.all_bonds
;---------------------------------------------

;---------------------------------------------
; workspace-object.add-description
; workspace-object.add-extrinsic-description
;---------------------------------------------

;---------------------------------------------
; workspace-object.get-descriptor | Object.get_descriptor
;---------------------------------------------

;----------------------------------------------
; workspace-object.relevant-descriptions
; workspace-object.distinguishing-descriptions
; workspace-object.non-distinguishing-descriptions
;----------------------------------------------

(defmethod (workspace-object :distinguishing-descriptor?) 
           (descriptor &aux other-objects other-descriptors)
; Returns t if no other object of the same type has the same descriptor.
; For now, object-category and length descriptions are not 
; distinguishing.  Maybe this should be changed.  
  (if* (or (eq descriptor plato-letter) (eq descriptor plato-group)
           (member descriptor *slipnet-numbers*))
   then nil
   else (if* (typep self 'letter)
         then (setq other-objects (remove self (send string :letter-list)))
         else (setq other-objects (remove self (send string :group-list)))
              (if* (send self :group) ; Don't count the group this object is 
		                      ; inside of, if there is one.
               then (setq other-objects 
			  (remove (send self :group) other-objects)))
              ; Don't count other groups inside of this group.
              (loop for obj in (send self :object-list) 
                    when (typep obj 'group) do 
                    (setq other-objects (remove obj other-objects))))
        (setq other-descriptors 
              (send-method-to-list 
	          (flatten (send-method-to-list other-objects 
			       :descriptions)) :descriptor))
        (null (memq descriptor other-descriptors))))

;----------------------------------------------
  
(defmethod (workspace-object :relevant-distinguishing-descriptions) ()
  (loop for description in (send self :distinguishing-descriptions)
	when (send (send description :description-type) :active?) 
	collect description))

;----------------------------------------------

(defmethod (workspace-object :relevant-non-distinguishing-descriptions) ()
  (loop for description in (send self :non-distinguishing-descriptions)
	when (send (send description :description-type) :active?) 
	collect description))

;----------------------------------------------

(defmethod (workspace-object :choose-relevant-description-by-activation) 
           (&aux relevant-descriptions)
; Chooses a relevant description probabilistically, based on the descriptor's
; activation.
  (setq relevant-descriptions (send self :relevant-descriptions))
  (if* (null relevant-descriptions)
   then nil
   else (nth (select-list-position 
		 (send-method-to-list 
		     (send-method-to-list relevant-descriptions :descriptor)
	             :activation)) 
	     relevant-descriptions)))

;----------------------------------------------

(defmethod (workspace-object 
	       :choose-relevant-distinguishing-description-by-conceptual-depth) 
           (&aux relevant-distinguishing-descriptions)
; Chooses a relevant, distinguishing description probabilistically, based on 
; the descriptor's conceptual-depth.
  (setq relevant-distinguishing-descriptions 
	(send self :relevant-distinguishing-descriptions))
  (if* (null relevant-distinguishing-descriptions)
   then nil
   else (select-list-item-by-method relevant-distinguishing-descriptions
	                            :conceptual-depth)))

;----------------------------------------------

(defmethod (workspace-object :description-present?) (new-description)
; Returns t if this object already has this description.
  (loop for d in descriptions
	when (and (eq (send d :description-type) 
		      (send new-description :description-type))
		  (eq (send d :descriptor) 
		      (send new-description :descriptor)))
	return t
	finally (return nil)))

;----------------------------------------------

(defmethod (workspace-object :rule-initial-string-descriptions) ()
; Returns all the descriptions that can be used in making the initial-string
; part of the rule, with this object as the changed object in the 
; initial-string.
  (loop for d in descriptions 
	when (and (send (send d :description-type) :active?) 
	          (send self :distinguishing-descriptor? 
			(send d :descriptor))
		  (not (eq (send d :description-type) plato-object-category)))
	collect d))

;----------------------------------------------

(defmethod (workspace-object :rule-modified-string-descriptions) ()
; Returns all the non-extrinsic descriptions that can be used in making the
; modified-string part of the rule, with this object as the object in the
; modified string corresponding to the initial-string changed object.
  (loop for d in descriptions 
	when (and (send (send d :description-type) :active?) 
	          (send self :distinguishing-descriptor? 
			(send d :descriptor))
                  (not (eq (send d :description-type) 
			    plato-string-position-category))
		  (not (eq (send d :description-type) plato-object-category)))
	collect d))

;----------------------------------------------

(defmethod (workspace-object :add-incoming-bond) (b)
; Adds a new incoming bond to the object.
  (push b incoming-bonds))

;----------------------------------------------

(defmethod (workspace-object :add-outgoing-bond) (b)
; Adds a new outgoing bond to the object.
  (push b outgoing-bonds))

;----------------------------------------------

(defmethod (workspace-object :structure-list) ()
; Returns a list of the structures attached to the object.
  (append descriptions 
	  (list left-bond right-bond group correspondence)))

;----------------------------------------------

(defmethod (workspace-object :descriptor-present?) (d)
; Returns t if this object has a description with this descriptor.
  (loop for description in descriptions
	when (eq (send description :descriptor) d) return t
	finally (return nil)))   	       

;----------------------------------------------

(defmethod (workspace-object :description-type-present?) (given-description-type)
; Returns t if this object has a description with this description-type.
  (loop for description in descriptions
	when (eq (send description :description-type) given-description-type) return t
	finally (return nil)))   	       

;----------------------------------------------

(defun letter-distance (obj1 obj2 &aux left-obj right-obj)
; Returns the distance in letters between the two objects.
  (if* (< (send obj1 :left-string-position) 
	  (send obj2 :left-string-position))    
   then (setq left-obj obj1 right-obj obj2)
   else (setq left-obj obj2 right-obj obj1))
  (- (send right-obj :left-string-position) 
     (send left-obj :right-string-position)))

;----------------------------------------------

(defun get-common-groups (obj1 obj2)
; Returns any groups that contain both objects (at some level).
  (loop for group in (send (send obj1 :string) :group-list)
	when (and (recursive-group-member? obj1 group)
		  (recursive-group-member? obj2 group))
	collect group))
