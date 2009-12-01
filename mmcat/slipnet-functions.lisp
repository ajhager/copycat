;---------------------------------------------
; slipnode.activate-from-workspace | REMOVED
; slipnode.add-activation-to-buffer
; slipnode.subtract-activation-from-buffer
;---------------------------------------------

;---------------------------------------------
; slipnode.decay | Slipnode.decay
;---------------------------------------------

;---------------------------------------------
; get-top-down-codelets | Slipnet.top_down_codelets
;---------------------------------------------

;---------------------------------------------
; update-slipnet | Slipnet.update
;---------------------------------------------

(defmethod (slipnode :get-codelets) ()
  (loop for codelet in codelets do 
        (if* (eq (flip-coin (get-post-codelet-probability 
				(send codelet :structure-category))) 'heads)
         then (loop for i from 1 to (get-num-of-codelets-to-post
					(send codelet :structure-category)) do
                    (push (make-codelet (send codelet :codelet-type)
			                (send codelet :arguments)
                                        (get-urgency-bin 
                                                (* (send self :activation)
						   (/ (send self :conceptual-depth) 
						      100))))
                           *codelets-to-post*)))))
