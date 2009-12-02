;---------------------------------------------
; get-label-node | get_label_node
;---------------------------------------------

;---------------------------------------------
; slipnode.get-related-node | get_related_node
;---------------------------------------------

;---------------------------------------------
; slipnode.applay-slippages | Slipnode.apply_slippages
;---------------------------------------------

;---------------------------------------------
; slipnode.get-possible-descriptors | Slipnode.get_possible_descriptors
;---------------------------------------------

;---------------------------------------------
; clear-slipnet | Slipnet.clear
;---------------------------------------------

;---------------------------------------------
; slipnode.directed? | REMOVED
;---------------------------------------------

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

;---------------------------------------------
; slipnode.intrinsic-degree-of-association | Slipnode.intrinsic_degree_of_association
;---------------------------------------------

;---------------------------------------------
; slipnode.degree-of-association | Slipnode.degree_of_association
;---------------------------------------------

;---------------------------------------------
; slipnode.similar-has-property-links | Slipnode.similar_has_property_links
;---------------------------------------------

;---------------------------------------------
; slipnet-link.intrinsic-degree-of-association | Sliplink.intrinsic-degree-of-association
;---------------------------------------------

;---------------------------------------------
; slipnet-link.degree-of-association | Sliplink.degree-of-association
;---------------------------------------------

;---------------------------------------------
; defflavor slipnet-link | Sliplink.__init__
;---------------------------------------------

;---------------------------------------------
; defflavor slipnode | Slipnode __init__
;---------------------------------------------

(defun init-slipnet ()
  (setq plato-one 
	  :description-tester 
	  '(lambda (object) 
             (and (typep object 'group) 
  	          (= (send  object :length) 1)))))
   
  (setq plato-two 
	  :description-tester 
	  '(lambda (object) 
             (and (typep object 'group) 
  	          (= (send object :length) 2)))))
   
  (setq plato-three
	  :description-tester 
	  '(lambda (object) 
             (and (typep object 'group) 
  	          (= (send object :length) 3)))))
   
  (setq plato-four
	  :description-tester 
	  '(lambda (object) 
             (and (typep object 'group) 
  	          (= (send object :length) 4)))))
   
  (setq plato-five
	  :description-tester 
	  '(lambda (object) 
             (and (typep object 'group) 
  	          (= (send object :length) 5)))))
   
  (setq plato-leftmost 
	  :description-tester 
	  '(lambda (object) 
	     (if* (and (not (send object :spans-whole-string?))
		       (send object :leftmost-in-string?))
	      then t else nil))))
			 
  (setq plato-rightmost 
	  :description-tester 
	  '(lambda (object) 
             (if* (and (not (send object :spans-whole-string?))
		       (send object :rightmost-in-string?))
              then t else nil))))
								    
  (setq plato-middle 
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
	  :description-tester 
	  '(lambda (object) 
		   (and (typep object 'letter)
			(send object :spans-whole-string?)))))

  (setq plato-whole 
	  '(lambda (object) 
		   (and (typep object 'group) 
			(send object :spans-whole-string?)))))

  (setq plato-first 
	  :description-tester
	  '(lambda (object) 
                   (eq (send object :get-descriptor 
			                  plato-letter-category) plato-a))))

  (setq plato-last
	  :description-tester
	  '(lambda (object) 
                   (eq (send object :get-descriptor 
			                  plato-letter-category) plato-z))))

  (setq plato-predgrp
	  :iterate-group
	  '(lambda (letter-category) 
		   (send letter-category :get-related-node 
			 plato-predecessor))))


  (setq plato-succgrp
 	  :iterate-group
	  '(lambda (letter-category) 
		   (send letter-category :get-related-node 
			 plato-successor))))

  (setq plato-samegrp 
  	  :iterate-group
	  '(lambda (letter-category) letter-category)))

  (setq plato-letter 
	  :description-tester '(lambda (object) (typep object 'letter))))

  (setq plato-group 
	  :description-tester '(lambda (object) (typep object 'group))))
