;---------------------------------------------
; answer-builder
;---------------------------------------------

;---------------------------------------------
; get-objects-to-change-for-answer | Workspace.get_objects_to_change_for_answer
;---------------------------------------------

;---------------------------------------------
; get-modified-letters-for-answer | Workspace.get_modified_letters_for_answer
;---------------------------------------------

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

