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

(defun description-strength-tester (proposed-description 
				    &aux proposed-description-strength
				         build-probability urgency)
; Calculates the proposed-description's strength, and probabilistically decides
; whether or not to post a description-builder codelet.  If so, the urgency of
; the description-builder codelet is a function of the strength.
(block nil

  ; Activate the descriptor.
  (send (send proposed-description :descriptor) :activate-from-workspace)

  ; Update the strength values for this description.
  (send proposed-description :update-strength-values)

  (setq proposed-description-strength 
	(send proposed-description :total-strength))

  (if* %verbose% 
   then (format t "Proposed description: ")
        (send proposed-description :print)
        (format t "~% Strength: ~a~&" proposed-description-strength))

  ; Decide whether or not to post a description-builder codelet, based on the 
  ; total strength of the proposed-description.
  (setq build-probability 
	(get-temperature-adjusted-probability 
	    (/ proposed-description-strength 100)))
  (if* %verbose% 
   then (format t "Build-probability: ~a~&" build-probability))
  (if* (eq (flip-coin build-probability) 'tails)
   then (if* %verbose% 
	 then (format t "Description not strong enough.  Fizzling.~&"))
        (return))
        
  (setq urgency proposed-description-strength)

  (if* %verbose% 
   then (format t "Posting a description-builder with urgency ~a.~&"
		(get-urgency-bin urgency)))

  ; Post the description-builder codelet.
  (send *coderack* :post 
	(make-codelet 'description-builder (list proposed-description)
	              (get-urgency-bin urgency)))))

;---------------------------------------------

(defun description-builder (proposed-description)
; Tries to build the proposed description, fizzling if the object no longer
; exists or if the description already exists.

(block nil
  (if* %verbose% 
   then (format t "In description-builder with proposed-description ")
        (send proposed-description :print)
	(format t "~%"))
  
  ; If the object no longer exists, then fizzle.
  (if* (null (memq (send proposed-description :object)
		   (send *workspace* :object-list)))
   then (if* %verbose% 
	 then (format t 
		      "This object no longer exists.  Fizzling.~&"))
        (return))

  ; If this description already exists, then fizzle.
  (if* (send (send proposed-description :object) 
	     :description-present? proposed-description)
   then (if* %verbose% 
 	 then (format t "This description already exists.  Fizzling.~&"))
        (send (send proposed-description :description-type) :activate-from-workspace)
        (send (send proposed-description :descriptor) :activate-from-workspace)
        (return))

  (build-description proposed-description)))

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
