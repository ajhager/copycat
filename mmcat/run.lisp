(defun run-ccat ()
  (loop until *quit-program* do

        (if* (= (mod *codelet-count* %time-step-length%) 0)
             then (update-everything))

        (if* (send *coderack* :empty?)
             then (loop for node in *initially-clamped-slipnodes* do
                        (send node :set-clamp t))
             (post-initial-codelets))

	(step-ccat)

    (if* *translated-rule*   
         then (answer-builder) 
         (if* *found-answer* 
              then (update-everything)

  		    (setq *quit-program* t)))))


(defun update-everything (&aux new-structure-list unclamp-probability)
  (if* (= *codelet-count* 
	  (* %initial-slipnode-clamp-time% %time-step-length%))
   then (loop for node in *initially-clamped-slipnodes*
	      do (send node :set-clamp nil)))

  (if* (> *codelet-count* 0) 
   then (update-temperature)
        (get-bottom-up-codelets) 
        (get-top-down-codelets)
        (update-slipnet))

  (if* *codelets-to-post*
   then (send *coderack* :post-codelet-list *codelets-to-post*))
  (setq *codelets-to-post* nil)

;---------------------------------------------

(defun step-ccat (&aux codelet)  
  (setq codelet (send *coderack* :choose))
  (send codelet :run)
  (setq *codelet-count* (1+ *codelet-count*))

;---------------------------------------------

(defun deal-with-snag ()
; If there is a snag in building the answer, then delete all 
; proposed structures, empty the coderack, raise and clamp the 
; temperature, and activate and clamp the activation of all the descriptions 
; of the object causing the snag.  

  (incf *snag-count*)
  (setq *last-snag-time* *codelet-count*)
  ; Save the current set of structures.  
  (setq *snag-structure-list* (send *workspace* :structure-list))

  ; Erase proposed structures.  (Their builder codelets will 
  ; disappear when the coderack is initialized.)
  (if* %workspace-graphics%
   then (loop for b in (send *workspace* :proposed-bond-list)
	      do (send (send b :string) 
		       :delete-proposed-bond b)
	         (if* (not (send (send b :string) 
				 :bond-present? b))
		  then (send b :erase-spline)))
        (loop for g in (send *workspace* :proposed-group-list)
	      do (send (send g :string) 
		       :delete-proposed-group g)
	         (if* (not (send (send g :string) 
				 :group-present? g))
                  then (send g :erase-rectangle)))
        (loop for c in (send *workspace* 
			     :proposed-correspondence-list)
	      do (send *workspace* 
		       :delete-proposed-correspondence c)
                 (if* (not (send *workspace* 
				 :correspondence-present? c))
                  then (send c :erase-line))))
  (send *coderack* :empty)
  (if* %coderack-graphics% then (update-coderack-display))

  (if* (and %workspace-graphics% *translated-rule*)
   then (send *translated-rule* :erase %translated-rule-mode%))
  (setq *translated-rule* nil)
  (setq *answer-string* nil)
  (setq *snag-condition* t)
  (setq *temperature* 100)
  (setq *clamp-temperature* t)
  (loop for d in (send *snag-object* :descriptions) do
	(send (send d :descriptor) :set-clamp t))
  (send *snag-object* :set-clamp-salience? t)
  (send *coderack* :empty)
  (post-initial-codelets)
  (update-everything))
