;---------------------------------------------
; defflavor description | Description
; make-description
;---------------------------------------------

;---------------------------------------------
; description.print | REMOVED
; description.description-string
;---------------------------------------------

;---------------------------------------------
; description.relevant?
;---------------------------------------------

;---------------------------------------------
; description.conceptual-depth
;---------------------------------------------

;---------------------------------------------
; description.bond-description?
;---------------------------------------------

;---------------------------------------------
; description.apply-slppages
;---------------------------------------------

;----------------------------------------------
; descriptions-equal? | Description.__eq__
;----------------------------------------------

;----------------------------------------------
; description-member? | REMOVED
;----------------------------------------------

;----------------------------------------------
; build-description | Workspace.build_description
;----------------------------------------------

;----------------------------------------------
; propose-description | Workspace.propose_description
;----------------------------------------------

;----------------------------------------------
; bottom-up-description-scout  | DescriptionBottomUpScout
;----------------------------------------------

(defun top-down-description-scout (description-type 
				   &aux chosen-object possible-descriptors
				        chosen-descriptor)

; Chooses an object probabilistically by total salience, and sees if this 
; object fits any of the descriptions in this description-type's "has-instance" 
; list.  (E.g., if the description-type is "alphabetic-position-category", sees
; if the chosen object can be described as "first" or "last" in the alphabet.)
; If so, proposes a description based on the property, and posts a 
; description-strength-tester codelet with urgency a function of the 
; activation of the proposed descriptor.. 

(block nil
  (if* %verbose% 
   then (format t "~%In top-down-description-scout with description-type ~a~&"
	        (send description-type :pname)))

  ; Choose an object.
  (setq chosen-object (send *workspace* :choose-object ':total-salience))

  (if* %verbose% 
   then (format t "Chose object ") 
        (send chosen-object :print))

  ; See if a description of this type can be made.
  (setq possible-descriptors 
	(send description-type :get-possible-descriptors chosen-object))
  (if* (null possible-descriptors)
   then (if* %verbose% 
	 then (format t 
		      "Couldn't make description.  Fizzling.~&"))
        (return))
  
  (setq chosen-descriptor 
	(select-list-item-by-method possible-descriptors ':activation))

  (propose-description chosen-object description-type chosen-descriptor)))

;---------------------------------------------
; description-strength-tester | DescriptionStrengthTester
;---------------------------------------------

;---------------------------------------------
; description-builder | DescriptionBuilder
;---------------------------------------------

;---------------------------------------------
; defflavor extrinsic-description | ExtrinsicDescription
; make-extrinsic-description
;---------------------------------------------

;----------------------------------------------
; extrinsic-description.print | REMOVED
;----------------------------------------------

;----------------------------------------------
; extrinsice-description.conceptual-depth
;----------------------------------------------
