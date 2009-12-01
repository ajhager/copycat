(defmethod (slipnode :directed?) ()
; Returns t if the slipnode represents a directed bond or group.
  (or (eq self plato-predecessor) (eq self plato-successor) 
      (eq self plato-predgrp) (eq self plato-succgrp)))

(defflavor slipnode 
    (activation 
     activation-buffer ; A buffer for storing activation between updates.
     (clamp nil) ; If this is t, then the activation of the node is clamped
                 ; to 100.

     (intrinsic-link-length nil) ; The intrinsic link-length of this links
                                 ; labeled by this node.
     (shrunk-link-length nil)  ; For now this is .4 of the intrinsic 
                               ; link-length
     conceptual-depth

     pname  ; A string giving the name of this node.
     symbol-name ; A symbol giving the name of this node.
     short-name ; A string to use for slipnet graphics.
     cm-name ; A string to use for concept-mapping graphics.

     (category-links nil)  
     (instance-links nil)
     (has-property-links nil)
     (lateral-slip-links nil)
     (lateral-nonslip-links nil)
     (incoming-links nil)

     (codelets nil)  ; A list of codelets attached to this node.

     (description-tester nil) ; A function for testing if this node
                              ; can be used as a descriptor for some object.

     (iterate-group nil) ; For nodes representing groups, a function used to 
                         ; iterate the group (e.g., if succgrp is given "a", it
                         ; will return "b").

     graphics-obj ; The graphics object representing this node.
    )
					   
  ())

(defflavor slipnet-link 
    (from-node to-node (label nil) (fixed-length nil))
    ; If a link has no label, then it is assigned a fixed length.
    ())

;---------------------------------------------
; slipnode.print | REMOVED
;---------------------------------------------

;---------------------------------------------
; slipnode.outgoing-links | Slipnode.outgoing_links

;---------------------------------------------
; slipnode.active? | Slipnode.is_active
;---------------------------------------------

;---------------------------------------------
; slipnode.category | Slipnode.category
;---------------------------------------------

;---------------------------------------------
; slipnet-link.print | REMOVED
;---------------------------------------------

(defmethod (slipnet-link :intrinsic-degreex-of-association) ()
  (if* fixed-length
   then (fake-reciprocal fixed-length)
   else (send label :intrinsic-degree-of-association)))

;---------------------------------------------
; slipnode.intrinsic-degree-of-association | Slipnode.intrinsic_degree_of_association
;---------------------------------------------

(defmethod (slipnet-link :degree-of-association) ()
  (if* fixed-length
   then (fake-reciprocal fixed-length)
   else (send label :degree-of-association)))

;---------------------------------------------
; slipnode.degree-of-association | Slipnode.degree_of_association
;---------------------------------------------

(defmethod (slipnode :similar-has-property-links) ()
  (loop for link in has-property-links 
	when (eq (flip-coin (get-temperature-adjusted-probability
				(/ (send link :degree-of-association) 100))) 
		 'heads)
	collect link))

;---------------------------------------------


(defun init-slipnet ()
; This function initializes all the nodes in the slipnet.  The links are 
; defined in the file "sliplinks.l".

  ; LETTERS
   
  (setq plato-a 
          :incoming-links '(b-a-link letter-category-a-link)
	  :category-links '(a-letter-category-link)
          :has-property-links '(a-first-link)
	  :lateral-nonslip-links '(a-b-link)))

  (setq plato-b 
          :incoming-links '(a-b-link c-b-link letter-category-b-link)
	  :category-links '(b-letter-category-link)
          :lateral-nonslip-links '(b-a-link b-c-link)))

  (setq plato-c 
          :incoming-links '(b-c-link d-c-link letter-category-c-link)
	  :category-links '(c-letter-category-link)
          :lateral-nonslip-links '(c-b-link c-d-link)))

  (setq plato-d 
          :incoming-links '(c-d-link e-d-link letter-category-d-link)
	  :category-links '(d-letter-category-link)
	  :lateral-nonslip-links '(d-c-link d-e-link)))

  (setq plato-e 
          :incoming-links '(d-e-link f-e-link letter-category-e-link)
	  :category-links '(e-letter-category-link)
          :lateral-nonslip-links '(e-d-link e-f-link)))
 
  (setq plato-f 
          :incoming-links '(e-f-link g-f-link letter-category-f-link)
	  :category-links '(f-letter-category-link)
	  :lateral-nonslip-links '(f-e-link f-g-link)))

  (setq plato-g 
          :incoming-links '(f-g-link h-g-link letter-category-g-link)
	  :category-links '(g-letter-category-link)
          :lateral-nonslip-links '(g-f-link g-h-link)))

  (setq plato-h 
          :incoming-links '(g-h-link i-h-link letter-category-h-link)
	  :category-links '(h-letter-category-link)
	  :lateral-nonslip-links '(h-g-link h-i-link)))

  (setq plato-i 
          :incoming-links '(h-i-link j-i-link letter-category-i-link)
	  :category-links '(i-letter-category-link)
	  :lateral-nonslip-links '(i-h-link i-j-link)))

  (setq plato-j 
          :incoming-links '(i-j-link k-j-link letter-category-j-link)
	  :category-links '(j-letter-category-link)
	  :lateral-nonslip-links '(j-i-link j-k-link)))

  (setq plato-k 
          :incoming-links '(j-k-link l-k-link letter-category-k-link)
	  :category-links '(k-letter-category-link)
          :lateral-nonslip-links '(k-j-link k-l-link)))

  (setq plato-l 
          :incoming-links '(k-l-link m-l-link letter-category-l-link)
	  :category-links '(l-letter-category-link)
          :lateral-nonslip-links '(l-k-link l-m-link)))
			           
  (setq plato-m 
          :incoming-links '(l-m-link n-m-link letter-category-m-link)
	  :category-links '(m-letter-category-link)
          :lateral-nonslip-links '(m-l-link m-n-link)))

  (setq plato-n 
          :incoming-links '(m-n-link o-n-link letter-category-n-link)
	  :category-links '(n-letter-category-link)
	  :lateral-nonslip-links '(n-m-link n-o-link)))

  (setq plato-o 
          :incoming-links '(n-o-link p-o-link letter-category-o-link)
	  :category-links '(o-letter-category-link)
	  :lateral-nonslip-links '(o-n-link o-p-link)))

  (setq plato-p 
          :incoming-links '(o-p-link q-p-link letter-category-p-link)
	  :category-links '(p-letter-category-link)
	  :lateral-nonslip-links '(p-o-link p-q-link)))

  (setq plato-q 
          :incoming-links '(p-q-link r-q-link letter-category-q-link)
	  :category-links '(q-letter-category-link)
	  :lateral-nonslip-links '(q-p-link q-r-link)))

  (setq plato-r 
          :incoming-links '(q-r-link s-r-link letter-category-r-link)
	  :category-links '(r-letter-category-link)
	  :lateral-nonslip-links '(r-q-link r-s-link)))

  (setq plato-s 
          :incoming-links '(r-s-link t-s-link letter-category-s-link)
	  :category-links '(s-letter-category-link)
	  :lateral-nonslip-links '(s-r-link s-t-link)))

  (setq plato-t 
          :incoming-links '(s-t-link u-t-link letter-category-t-link)
	  :category-links '(t-letter-category-link)
	  :lateral-nonslip-links '(t-s-link t-u-link)))

  (setq plato-u 
          :incoming-links '(t-u-link v-u-link letter-category-u-link)
	  :category-links '(u-letter-category-link)
	  :lateral-nonslip-links '(u-t-link u-v-link)))

  (setq plato-v 
          :incoming-links '(u-v-link w-v-link letter-category-v-link)
	  :category-links '(v-letter-category-link)
	  :lateral-nonslip-links '(v-u-link v-w-link)))
			        
  (setq plato-w 
          :incoming-links '(v-w-link x-w-link letter-category-w-link)
	  :category-links '(w-letter-category-link)
	  :lateral-nonslip-links '(w-v-link w-x-link)))
			        
  (setq plato-x 
          :incoming-links '(w-x-link y-x-link letter-category-x-link)
	  :category-links '(x-letter-category-link)
	  :lateral-nonslip-links '(x-w-link x-y-link)))

  (setq plato-y 
          :incoming-links '(x-y-link z-y-link letter-category-y-link)
	  :category-links '(y-letter-category-link)
	  :lateral-nonslip-links '(y-x-link y-z-link)))
		    
  (setq plato-z 
          :incoming-links '(y-z-link letter-category-z-link)
	  :category-links '(z-letter-category-link)
	  :has-property-links '(z-last-link)
	  :lateral-nonslip-links '(z-y-link)))

  ; NUMBERS

  (setq plato-one 
          :incoming-links '(length-1-link 2-1-link)
	  :category-links '(1-length-link)
	  :lateral-nonslip-links '(1-2-link)
	  :description-tester 
	  '(lambda (object) 
             (and (typep object 'group) 
  	          (= (send  object :length) 1)))))
   
  (setq plato-two 
          :incoming-links '(length-2-link 1-2-link)
	  :category-links '(2-length-link)
          :lateral-nonslip-links '(2-3-link 2-1-link)
	  :description-tester 
	  '(lambda (object) 
             (and (typep object 'group) 
  	          (= (send object :length) 2)))))
   
  (setq plato-three
          :incoming-links '(length-3-link 2-3-link 4-3-link)
	  :category-links '(3-length-link)
          :lateral-nonslip-links '(3-4-link 3-2-link)
	  :description-tester 
	  '(lambda (object) 
             (and (typep object 'group) 
  	          (= (send object :length) 3)))))
   
  (setq plato-four
          :incoming-links '(length-4-link 3-4-link 5-4-link)
	  :category-links '(4-length-link)
          :lateral-nonslip-links '(4-5-link 4-3-link)
	  :description-tester 
	  '(lambda (object) 
             (and (typep object 'group) 
  	          (= (send object :length) 4)))))
   
  (setq plato-five
          :incoming-links '(length-5-link 4-5-link)
	  :category-links '(5-length-link)
          :lateral-nonslip-links '(5-4-link)
	  :description-tester 
	  '(lambda (object) 
             (and (typep object 'group) 
  	          (= (send object :length) 5)))))
   
  ; STRING-POSITIONS

  (setq plato-leftmost 
          :incoming-links '(string-position-category-leftmost-link 
			    rightmost-leftmost-link 
			    left-leftmost-link 
			    right-leftmost-link
			    first-leftmost-link
			    last-leftmost-link)
          :category-links '(leftmost-string-position-category-link)
	  :lateral-slip-links '(leftmost-rightmost-link)
	  :lateral-nonslip-links '(leftmost-left-link leftmost-right-link
			 leftmost-first-link leftmost-last-link)
	  :description-tester 
	  '(lambda (object) 
	     (if* (and (not (send object :spans-whole-string?))
		       (send object :leftmost-in-string?))
	      then t else nil))))
			 
  (setq plato-rightmost 
          :incoming-links '(string-position-category-rightmost-link 
			    leftmost-rightmost-link 
			    right-rightmost-link 
			    left-rightmost-link
			    first-rightmost-link last-rightmost-link)
          :category-links '(rightmost-string-position-category-link)
	  :lateral-slip-links '(rightmost-leftmost-link)
	  :lateral-nonslip-links '(rightmost-right-link rightmost-left-link
			 rightmost-first-link rightmost-last-link)
	  :description-tester 
	  '(lambda (object) 
             (if* (and (not (send object :spans-whole-string?))
		       (send object :rightmost-in-string?))
              then t else nil))))
								    
  (setq plato-middle 
          :incoming-links '(string-position-category-middle-link)
          :category-links '(middle-string-position-category-link)
	  :description-tester 
	  '(lambda (object) 
 	     (let ((left-neighbor 
		       (send object :ungrouped-left-neighbor))
 	           (right-neighbor 
		       (send object :ungrouped-right-neighbor)))
               (and left-neighbor right-neighbor 
		    (send left-neighbor :leftmost-in-string?)
		    (send right-neighbor :rightmost-in-string?))))))

  (setq plato-single 
          :incoming-links '(string-position-category-single-link
			    whole-single-link)
          :category-links '(single-string-position-category-link)
	  :lateral-slip-links '(single-whole-link)
	  :description-tester 
	  '(lambda (object) 
		   (and (typep object 'letter)
			(send object :spans-whole-string?)))))

  (setq plato-whole 
          :incoming-links '(string-position-category-whole-link
			    single-whole-link)
          :category-links '(whole-string-position-category-link)
	  :lateral-slip-links '(whole-single-link)
	  :description-tester 
	  '(lambda (object) 
		   (and (typep object 'group) 
			(send object :spans-whole-string?)))))

  ; ALPHABETIC-POSITION NODES

  (setq plato-first 
          :incoming-links '(a-first-link last-first-link 
			    alphabetic-position-category-first-link
			    leftmost-first-link rightmost-first-link)
          :category-links '(first-alphabetic-position-category-link)
	  :lateral-slip-links '(first-last-link)
	  :lateral-nonslip-links '(first-leftmost-link first-rightmost-link)
	  :description-tester
	  '(lambda (object) 
                   (eq (send object :get-descriptor 
			                  plato-letter-category) plato-a))))

  (setq plato-last
          :incoming-links '(z-last-link first-last-link 
			    alphabetic-position-category-last-link
			    leftmost-last-link rightmost-last-link)
          :category-links '(last-alphabetic-position-category-link)
	  :lateral-slip-links '(last-first-link)
	  :lateral-nonslip-links '(last-leftmost-link last-rightmost-link)
	  :description-tester
	  '(lambda (object) 
                   (eq (send object :get-descriptor 
			                  plato-letter-category) plato-z))))


  ; DIRECTIONS

  (setq plato-left 
          :incoming-links '(direction-category-left-link right-left-link 
			    leftmost-left-link rightmost-left-link)
          :category-links '(left-direction-category-link)
          :lateral-slip-links '(left-right-link)
	  :lateral-nonslip-links '(left-leftmost-link left-rightmost-link)))

  (setq plato-right 
          :incoming-links '(direction-category-right-link left-right-link 
			    rightmost-right-link leftmost-right-link)
          :category-links '(right-direction-category-link)
	  :lateral-slip-links '(right-left-link)
	  :lateral-nonslip-links '(right-rightmost-link right-leftmost-link)))
			        
  ; BONDS

  (setq plato-predecessor 
          :incoming-links '(bond-category-predecessor-link 
			    successor-predecessor-link 
			    predgrp-predecessor-link)
	  :category-links '(predecessor-bond-category-link)
          :lateral-slip-links '(predecessor-successor-link)
          :lateral-nonslip-links '(predecessor-predgrp-link)))
                            		            
  (setq plato-successor 
          :incoming-links '(bond-category-successor-link 
			    predecessor-successor-link 
			    succgrp-successor-link)
	  :category-links '(successor-bond-category-link)
          :lateral-slip-links '(successor-predecessor-link)
          :lateral-nonslip-links '(successor-succgrp-link)))
			   
  (setq plato-sameness      
	  :incoming-links '(bond-category-sameness-link 
			    samegrp-sameness-link)
	  :category-links '(sameness-bond-category-link)
          :lateral-nonslip-links '(sameness-samegrp-link)))	  

  ; GROUPS

  (setq plato-predgrp
          :incoming-links '(group-category-predgrp-link succgrp-predgrp-link 
			    predecessor-predgrp-link)
	  :category-links '(predgrp-group-category-link)
	  :lateral-slip-links '(predgrp-succgrp-link)
	  :lateral-nonslip-links '(predgrp-predecessor-link 
			 predgrp-length-link)
	  :iterate-group
	  '(lambda (letter-category) 
		   (send letter-category :get-related-node 
			 plato-predecessor))))


  (setq plato-succgrp
          :incoming-links '(group-category-succgrp-link predgrp-succgrp-link 
			    successor-succgrp-link)
	  :category-links '(succgrp-group-category-link)
	  :lateral-slip-links '(succgrp-predgrp-link)
	  :lateral-nonslip-links '(succgrp-successor-link
			 succgrp-length-link)
 	  :iterate-group
	  '(lambda (letter-category) 
		   (send letter-category :get-related-node 
			 plato-successor))))

  (setq plato-samegrp 
	  :incoming-links '(group-category-samegrp-link sameness-samegrp-link)
	  :category-links '(samegrp-group-category-link)
	  :lateral-nonslip-links '(samegrp-sameness-link samegrp-length-link)
  	  :iterate-group
	  '(lambda (letter-category) letter-category)))

  ; OBJECTS

  (setq plato-letter 
	  :incoming-links '(object-category-letter-link 
			    group-letter-link)
	  :category-links '(letter-object-category-link)
          :lateral-slip-links '(letter-group-link)
	  :description-tester '(lambda (object) (typep object 'letter))))

  (setq plato-group 
          :incoming-links '(object-category-group-link 
			    letter-group-link)
          :category-links '(group-object-category-link)
          :lateral-slip-links '(group-letter-link)
	  :description-tester '(lambda (object) (typep object 'group))))
	  

  ; CATEGORIES

  (setq plato-letter-category
          :incoming-links 
	  (append (list 'length-letter-category-link 
			'bond-facet-letter-category-link
			'samegrp-letter-category-link)
 	          (loop for l in *slipnet-letters*
		        collect 
		        (append-symbols (send l :symbol-name) 
			                '-letter-category-link)))
          :category-links '(letter-category-bond-facet-link)	      
	  :instance-links 
	      (loop for l in *slipnet-letters*
		    collect 
		    (append-symbols 'letter-category- 
			            (send l :symbol-name) '-link))
          :lateral-slip-links '(letter-category-length-link)))


  (setq plato-string-position-category
	  :incoming-links '(leftmost-string-position-category-link 
			    rightmost-string-position-category-link
			    middle-string-position-category-link
                            single-string-position-category-link
			    whole-string-position-category-link)
	  :instance-links '(string-position-category-leftmost-link 
				string-position-category-rightmost-link
				string-position-category-middle-link
                                string-position-category-single-link
				string-position-category-whole-link)))


  (setq plato-alphabetic-position-category
	  :incoming-links '(first-alphabetic-position-category-link 
			    last-alphabetic-position-category-link)
	  :instance-links '(alphabetic-position-category-first-link 
				alphabetic-position-category-last-link)))
	
  (setq plato-direction-category
	  :incoming-links '(left-direction-category-link 
			    right-direction-category-link)
	  :instance-links '(direction-category-left-link 
				direction-category-right-link)))

  (setq plato-bond-category
	  :incoming-links
	  '(successor-bond-category-link 
	    predecessor-bond-category-link 
	    sameness-bond-category-link)
	  :instance-links 
	  '(bond-category-successor-link 
            bond-category-predecessor-link
            bond-category-sameness-link)))

  (setq plato-group-category 
	  :incoming-links '(predgrp-group-category-link
			    succgrp-group-category-link
			    samegrp-group-category-link)
	  :instance-links '(group-category-succgrp-link
				group-category-predgrp-link
				group-category-samegrp-link)))
  
  (setq plato-length
	  :incoming-links
              (append (list 'letter-category-length-link 
			    'bond-facet-length-link
			    'predgrp-length-link
			    'succgrp-length-link
			    'samegrp-length-link)
 	            (loop for n in *slipnet-numbers*
		          collect (append-symbols (send n :pname) 
				                  '-length-link)))
          :category-links '(length-bond-facet-link)	      
	  :instance-links 
	      (loop for n in *slipnet-numbers*
		    collect (append-symbols 'length- 
				            (send n :pname) '-link))
          :lateral-slip-links '(length-letter-category-link)))


  (setq plato-object-category   
	  :incoming-links '(letter-object-category-link 
			    group-object-category-link)
	  :instance-links 
	  '(object-category-letter-link object-category-group-link)))
	
  (setq plato-bond-facet
          :incoming-links '(letter-category-bond-facet-link 
			    length-bond-facet-link)
	  :instance-links '(bond-facet-letter-category-link 
			       	bond-facet-length-link)))

  ; Initialize all the links in the slipnet.
  (init-slipnet-links)
            
  ; Now set up some other instance variables for each node.
  (loop for slipnode in *slipnet* do
        (send slipnode :set-activation 0)
        (send slipnode :set-activation-buffer 0)

        ; Set up shrunk link-lengths.
        (if* (send slipnode :intrinsic-link-length) 
         then (send slipnode :set-shrunk-link-length 
		    (round (*  (send slipnode :intrinsic-link-length) .4))))

        ; Now evaluate all the links in the various links variables
        (send slipnode :set-incoming-links 
	               (mapcar 'eval (send slipnode :incoming-links)))
        (send slipnode :set-category-links 
	               (mapcar 'eval (send slipnode :category-links)))
        (send slipnode :set-instance-links 
	               (mapcar 'eval (send slipnode :instance-links)))
        (send slipnode :set-has-property-links 
	               (mapcar 'eval (send slipnode :has-property-links)))
        (send slipnode :set-lateral-slip-links 
	               (mapcar 'eval (send slipnode :lateral-slip-links)))
        (send slipnode :set-lateral-nonslip-links 
		               (mapcar 'eval (send slipnode 
						   :lateral-nonslip-links))))

  ; Now set up the codelets attached to nodes in the slipnet.

  (send plato-left :set-codelets
	(list (make-codelet 'top-down-bond-scout--direction 
		  (list plato-left) nil 'bond)
	      (make-codelet 'top-down-group-scout--direction 
		  (list plato-left) nil 'group)))

  (send plato-right :set-codelets
	(list (make-codelet 'top-down-bond-scout--direction 
		  (list plato-right) nil 'bond)
	      (make-codelet 'top-down-group-scout--direction 
		  (list plato-right) nil 'group)))

  (send plato-predecessor :set-codelets 
	(list (make-codelet 'top-down-bond-scout--category 
		  (list plato-predecessor) nil 'bond)))

  (send plato-successor :set-codelets 
	(list (make-codelet 'top-down-bond-scout--category 
		  (list plato-successor) nil 'bond)))

  (send plato-sameness :set-codelets 
	(list (make-codelet 'top-down-bond-scout--category 
		  (list plato-sameness) nil 'bond)))

  (send plato-predgrp :set-codelets 
	(list (make-codelet 'top-down-group-scout--category 
		  (list plato-predgrp) nil 'group)))

  (send plato-succgrp :set-codelets 
	(list (make-codelet 'top-down-group-scout--category 
		  (list plato-succgrp) nil 'group)))

  (send plato-samegrp :set-codelets 
	(list (make-codelet 'top-down-group-scout--category 
		  (list plato-samegrp) nil 'group)))
   
  (send plato-string-position-category :set-codelets 
	(list (make-codelet 'top-down-description-scout 
		  (list plato-string-position-category) nil 'description)))
				
  (send plato-alphabetic-position-category :set-codelets 
	(list (make-codelet 'top-down-description-scout 
		  (list plato-alphabetic-position-category) nil 'description)))
				
  (send plato-length :set-codelets 
	(list (make-codelet 'top-down-description-scout 
		  (list plato-length) nil 'description)))

  (setq *initially-clamped-slipnodes* 
	(list plato-letter-category plato-string-position-category))
                                
  (setq *slipnet-initialized* t)

)
					  
;---------------------------------------------
; get-label-node | get_label_node
;---------------------------------------------

;---------------------------------------------
; slipnode.get-related-node | get_related_node
;---------------------------------------------

(defmethod (slipnode :apply-slippages) (slippage-list)
; Returns the node that is the translation of the given node
; according to the given slippage list.
  (loop for s in slippage-list 
	when (eq (send s :descriptor1) self)
	return (send s :descriptor2)
	finally (return self)))

;---------------------------------------------
; slipnode.get-possible-descriptors | Slipnode.get_possible_descriptors
;---------------------------------------------

;---------------------------------------------
; clear-slipnet | Slipnet.clear
;---------------------------------------------