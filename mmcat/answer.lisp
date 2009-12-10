;---------------------------------------------
; answer-builder
;---------------------------------------------

;---------------------------------------------
; get-objects-to-change-for-answer | Workspace.get_objects_to_change_for_answer
;---------------------------------------------

(defun get-modified-letters-for-answer 
       (obj description-type &aux new-descriptor new-letter-category
			      new-letter new-string-position
			      first-letter modified-letter-list 
			      group-direction)
; Returns a list of letters modified as directed by the translated rule.
(block nil
  (if* (typep obj 'letter)  ; Only one letter needs to be changed.
   then 
        (setq new-descriptor (get-new-descriptor-for-answer obj description-type))
        (if* (null new-descriptor) ; A snag!  The translated rule cannot
	                           ; be followed for some reason.
        then (setq *snag-object* obj)
             (return))

        ; No snag, so make a modified letter.
        (if* (eq description-type plato-letter-category)
         then (setq new-letter 
		    (make-letter *answer-string* new-descriptor 
			(send obj :left-string-position)))
	      (push new-letter modified-letter-list)
         else (setq *snag-object* obj)) ; Snag:  the description-type is 
	                                ; length, which can't be 
					; applied to a letter.

   else ; Obj is a group.  If a new letter-category is directed (e.g., the
        ; translated-rule is 
	; " Replace letter-category of rightmost group by 'D' "), 
        ; then modify all the letters in the group to be this new category, 
	; with new pname.
	; If a new length is directed (e.g., the translated-rule
        ; is "Replace length of rightmost group by successor"), then
        ; add or subtract letters from the letter-list.  Note that this may
	; not work (I think) in some cases if the group itself contains any 
	; groups.  This should be fixed.
        (if* (eq description-type plato-letter-category)
	 then (loop for letter in (send obj :letter-list) do
	            (setq new-descriptor 
			  (get-new-descriptor-for-answer letter description-type))
		    (if* (null new-descriptor) ; A snag!  
                     then (setq *snag-object* letter)
		          (return))
		    (setq new-letter 
			  (make-letter *answer-string* 
			      new-descriptor
			      (send letter :left-string-position)))
		    (push new-letter modified-letter-list))

         else ; Description-type is length.  
              ; If the group is directed  (e.g., "srqp", going
              ; to the left), increase or decrease the group in the given 
	      ; direction (e.g., srqp).
              (setq *changed-length-group* obj)
              (setq new-descriptor 
		    (get-new-descriptor-for-answer obj description-type))
              (if* (not (memq new-descriptor *slipnet-numbers*)) ; A snag!  
               then (setq *snag-object* obj)
	            (return))

              ; If the group has any groups as members, then signal a snag.
	      ; (The program can't deal with this kind of situation right now.)
              (if* (loop for group-member in (send obj :object-list)
			 when (typep group-member 'group) return t
			 finally (return nil))
               then (setq *snag-object* obj)
	            (return))

	      (setq group-direction (send obj :direction-category))
	      
	      (setq *amount-length-changed* 
		    (- (node-to-number new-descriptor)
		       (node-to-number 
			   (send obj :get-descriptor 
				 plato-length))))

  	      (if* (or (null group-direction)
		       (eq group-direction plato-right))
               then (setq first-letter 
			  (send (send obj :string) :get-letter 
				(send obj :left-obj-position)))
	            (setq new-string-position 
			  (send first-letter :left-string-position))
               else (setq first-letter 
			  (send (send obj :string) :get-letter 
				(send obj :right-obj-position)))
	            (setq new-string-position 
			  (+ (send first-letter :left-string-position)
			     *amount-length-changed*)))

              (setq new-letter 
		    (make-letter *answer-string* 
			(send first-letter :get-descriptor 
			      plato-letter-category)
			new-string-position))

	      (push new-letter modified-letter-list)
              (setq new-string-position 
		    (send new-letter :left-string-position))

              (loop for i from 1 to (1- (node-to-number new-descriptor))
		    until (null new-letter) do
                    (setq new-string-position 
			  (if* (or (null group-direction)
				   (eq group-direction plato-right))
			   then (1+ new-string-position)
			   else (1- new-string-position)))

                    (setq new-letter-category 
			  (funcall (send (send obj :group-category)  
					     :iterate-group) 
			           (get-plato-letter 
				       (string-upcase 
					   (send new-letter :pname)))))
                    (if* (null new-letter-category) ; A snag!
	             then (setq *snag-object* new-letter)
		          (return))
		    (setq new-letter 
			  (make-letter *answer-string*
			      new-letter-category
			      new-string-position))
		    (push new-letter modified-letter-list))
	      (setq *modified-letter-list* modified-letter-list)))

  modified-letter-list))

;---------------------------------------------
; get-new-descriptor-for-answer | Workspace.get_new_descriptor_for_answer
;---------------------------------------------

(defun get-unmodified-letters-for-answer (objects-to-change)
; Return the letters from the target-string that don't need to be changed
; for the answer.
  (loop for letter in (send *target-string* :letter-list)
	when (not (loop for obj in objects-to-change
			when (member letter (send obj :letter-list))
			return t
			finally (return nil)))
	collect (make-letter 
		    *answer-string* 
		    (send letter :get-descriptor plato-letter-category)
		    (send letter :left-string-position))))

