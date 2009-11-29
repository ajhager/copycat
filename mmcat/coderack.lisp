(defflavor codelet
  (codelet-type ; E.g., bottom-up-bond-scout.
   arguments 
   urgency-bin ; E.g., %very-high-bin%
   (index-in-bin nil) ; This codelet's position in its urgency bin.
   (time-stamp nil) ; The time (in codelet-steps) when this codelet was 
                    ; posted
   structure-category) ; E.g., 'bond.
  ()
  :gettable-instance-variables
  :settable-instance-variables
  :initable-instance-variables)

;---------------------------------------------
; codelet.print-to-output-file | REMOVED
;---------------------------------------------

;---------------------------------------------
; codelet.print | REMOVED
;---------------------------------------------

(defmethod (codelet :run) ()
; This is the method that runs the codelet.    
  (apply codelet-type arguments))

;---------------------------------------------
; codelet.remove-probability | Coderack.remove_probability
;---------------------------------------------

(defun make-codelet (codelet-type arguments urgency-bin-name 
	             &optional structure-category)
; Returns a new codelet.
  (make-instance 'codelet 
      :codelet-type codelet-type
      :arguments arguments
      :urgency-bin (eval urgency-bin-name)
      :structure-category structure-category))
      
;---------------------------------------------
; coderack.total-num-of-codelets | REMOVED
;---------------------------------------------

;---------------------------------------------
; defflavor coderack-bin | Bin.__init__
;---------------------------------------------

;---------------------------------------------
; coderack.spy | REMOVED
;---------------------------------------------

;---------------------------------------------
; coderack-bin.fill-pointer | REMOVED
;---------------------------------------------

;---------------------------------------------
; coderack-bin.set-fill-pointer | REMOVED
;---------------------------------------------

;---------------------------------------------
; coderack-bin.num-of-codelets-in-bin | REMOVED
;---------------------------------------------

;---------------------------------------------
; coderack-bin.urgency-sum | Bin.urgency_sum
;---------------------------------------------

;---------------------------------------------
; coderack-bin.urgency-value | Bin.urgency
;---------------------------------------------

;---------------------------------------------
; coderack.codelet-list | Coderack.codelets
;---------------------------------------------

;---------------------------------------------
; coderack.empty | Coderack.clear
;---------------------------------------------

;---------------------------------------------
; make-coderack | Coderack.__init__
; init-coderack
; defflavor coderack
;---------------------------------------------

;---------------------------------------------
; coderack.total-urgency-sum | Coderack.urgency_sum
;---------------------------------------------

;---------------------------------------------
; coderack.empty? | Coderack.is_empty
;---------------------------------------------

;---------------------------------------------
; coderack.post | Coderack.post
; coderack.post-without-removing
; coderakc.post-codelet-list
;---------------------------------------------

;---------------------------------------------
; coderack.display | REMOVED
;---------------------------------------------

;---------------------------------------------
; get-urgency-bin | MERGED Coderack.post
;---------------------------------------------

;---------------------------------------------
; get-coderack-bin | MERGED Coderack.post
;---------------------------------------------

;---------------------------------------------
; coderack.choose | Coderack.choose & Bin.choose
;---------------------------------------------

(defmethod (coderack :remove-codelets) 
           (num-to-remove &aux remove-probability-list codelet argument bin 
	                       index (num-removed 0))
; Removes the given number of codelets from the coderack 
; probabilistically, biased towards deleting low-urgency, older codelets.
(block nil
  (if* (send self :empty?) 
   then (format t "Can't remove any more codelets: coderack is empty.~&") 
        (return))     

  (setq remove-probability-list
	(send-method-to-list *codelet-list* :remove-probability))
  (loop until (or (= num-removed num-to-remove) (send self :empty?)) do
        (setq codelet (nth (select-list-position remove-probability-list)
			   *codelet-list*))
	(if* codelet
         then (setq bin (send codelet :urgency-bin))
              (setq index (send codelet :index-in-bin))
	      (vset (send bin :vector) index nil)
              (setq *codelet-list* (remove codelet *codelet-list*))
              (send *coderack* :delete-codelet-from-graphics codelet)
              (setq argument (car (send codelet :arguments)))
              (if* (and (not (eq (send codelet :codelet-type) 'breaker))
			(typep argument 'workspace-structure)
			(not (or (typep argument 'rule) 
				 (typep argument 'description))))
               then (send *workspace* :delete-proposed-structure argument)
                    (if* (and %workspace-graphics% 
			      (send argument :graphics-obj))
	             then (send argument :erase-proposed)))
		    
              (if* %verbose% 
               then (format t "Removed ") 
	            (send codelet :print))
	      (incf num-removed)

              ; Fill in hole in bin left by removed codelet, if necessary.
              (if* (< index (1- (send bin :fill-pointer)))
               then (vset (send bin :vector) index 
	    	          (vref (send bin :vector) 
				(1- (send bin :fill-pointer))))
                    ; Give the codelet that moved its new bin index.
                    (send (vref (send bin :vector) index) 
			  :set-index-in-bin index))
              (send bin :set-fill-pointer 
		    (1- (send bin :fill-pointer)))))
  (if* (send self :empty?)
   then (format t "Can't remove any codelets: coderack is empty.~&"))))
    
(defun get-bottom-up-codelets ()
; Adds various bottom-up codelets to *codelets-to-post*, deciding on how
; many to add and urgency, as a function of how much each type of codelet 
; is needed.

  ; Add bottom-up description codelets.
  (if* (eq (flip-coin (get-post-codelet-probability 'description))
	   'heads)
   then (loop for i from 1 
	      to (get-num-of-codelets-to-post 'description) do
	      (push (make-codelet 'bottom-up-description-scout nil 
		                  '*low-bin*) *codelets-to-post*)))

  ; Add bottom-up bond codelets.
  (if* (eq (flip-coin (get-post-codelet-probability 'bond)) 'heads)
   then (loop for i from 1 to (get-num-of-codelets-to-post 'bond) do
              (push (make-codelet 'bottom-up-bond-scout nil 
		                  '*low-bin*) *codelets-to-post*)))

  ; Add bottom-up group codelets.
  (if* (eq (flip-coin (get-post-codelet-probability 'group)) 'heads)
   then (loop for i from 1 to (get-num-of-codelets-to-post 'group) do
              (push (make-codelet 'group-scout--whole-string nil 
		                 '*low-bin*) *codelets-to-post*)))

  ; Add bottom-up replacement codelets.
  (if* (eq (flip-coin (get-post-codelet-probability 'replacement)) 
	   'heads)
   then (loop for i from 1 
	      to (get-num-of-codelets-to-post 'replacement) do
              (push (make-codelet 'replacement-finder nil '*low-bin*) 
	            *codelets-to-post*)))

  ; Add bottom-up correspondence codelets.
  (if* (eq (flip-coin (get-post-codelet-probability 'correspondence))
	              'heads)
   then (loop for i from 1 
	      to (get-num-of-codelets-to-post 'correspondence) do
              (push (make-codelet 'bottom-up-correspondence-scout nil 
		                  '*low-bin*) *codelets-to-post*)
              (push (make-codelet 'important-object-correspondence-scout nil 
		                  '*low-bin*) *codelets-to-post*)))

  ; Add bottom-up rule codelets.
  (if* (eq (flip-coin (get-post-codelet-probability 'rule)) 'heads)
   then (loop for i from 1 to (get-num-of-codelets-to-post 'rule) do
              (push (make-codelet 'rule-scout nil '*low-bin*) 
		    *codelets-to-post*)))

  ; Add bottom-up rule-translator codelets.
  (if* (eq (flip-coin (get-post-codelet-probability 'translated-rule)) 
	   'heads)
   then (loop for i from 1 
	      to (get-num-of-codelets-to-post 'translated-rule) do
              (push (make-codelet 'rule-translator nil 
		                  (if* (> *temperature* 25) 
		                   then '*low-bin* else '*high-bin*))
	            *codelets-to-post*)))

  ; Add bottom-up breaker codelets.
  (push (make-codelet 'breaker nil '*extremely-low-bin*) 
	*codelets-to-post*))

(defun post-initial-codelets ()
; Post the initial codelets the program starts out with.
  (loop for i from 1 to (* 2 (length (send *workspace* :object-list))) do
        (send *coderack* :post 
	      (make-codelet 'bottom-up-bond-scout nil '*very-low-bin*))
	(send *coderack* :post 
	      (make-codelet 'replacement-finder nil '*very-low-bin*))
	(send *coderack* :post
	      (make-codelet 'bottom-up-correspondence-scout nil 
		            '*very-low-bin*)))

  (send *coderack* :post-codelet-list *codelets-to-post*)
  (setq *codelets-to-post* nil))
