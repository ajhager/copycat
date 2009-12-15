;---------------------------------------------
; make-codelet | Codelet.__init__
; defflavor codelet
;---------------------------------------------

;---------------------------------------------
; codelet.print-to-output-file | REMOVED
;---------------------------------------------

;---------------------------------------------
; codelet.print | REMOVED
;---------------------------------------------

;---------------------------------------------
; codelet.run | Codelet.run
;---------------------------------------------

;---------------------------------------------
; codelet.remove-probability | Coderack.remove_probability
;---------------------------------------------

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

;---------------------------------------------
; coderack.remove-codelets
;---------------------------------------------
    
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


;---------------------------------------------
; post-initial-codelets | Workspace.initial_codelets
;---------------------------------------------
