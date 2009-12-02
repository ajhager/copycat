; PROBABILITY DISTRIBUTIONS
   
  ; Answer-temperature threshold distributions:
  ; These are used by the rule-translator codelet to decide whether 
  ; or not to fizzle as a function of temperature.  The distribution 
  ; used is a function of how many bonds have been built.  The idea is 
  ; that if many bonds have been built, then it's likely that more 
  ; structure will be discovered, and the rule-translator should tend to wait
  ; until the temperature is low.  If few bonds have been built (and,
  ; since the rule translator is running, there should have already been 
  ; opportunity for bonds to be built), then there is not much structure to 
  ; be discovered, and the rule-translator should tend to go ahead even 
  ; though the temperature is relatively high.

  (setq %very-low-answer-temperature-threshold-distribution% 
	(make-probability-distribution "very-low" 101))
  (loop for i from 0 to 100 do 
	(send %very-low-answer-temperature-threshold-distribution% :vset i 0))
  (send %very-low-answer-temperature-threshold-distribution% :vset 10 5)
  (send %very-low-answer-temperature-threshold-distribution% :vset 20 150)
  (send %very-low-answer-temperature-threshold-distribution% :vset 30 5)
  (send %very-low-answer-temperature-threshold-distribution% :vset 40 2)
  (send %very-low-answer-temperature-threshold-distribution% :vset 50 1)
  (send %very-low-answer-temperature-threshold-distribution% :vset 60 1)
  (send %very-low-answer-temperature-threshold-distribution% :vset 70 1)
  (send %very-low-answer-temperature-threshold-distribution% :vset 80 1)
  (send %very-low-answer-temperature-threshold-distribution% :vset 90 1)
  (send %very-low-answer-temperature-threshold-distribution% :vset 100 1)
  (send %very-low-answer-temperature-threshold-distribution% :update)

  (setq %low-answer-temperature-threshold-distribution% 
	(make-probability-distribution "low" 101))
  (loop for i from 0 to 100 do 
	(send %low-answer-temperature-threshold-distribution% :vset i 0))
  (send %low-answer-temperature-threshold-distribution% :vset 10 2)
  (send %low-answer-temperature-threshold-distribution% :vset 20 5)
  (send %low-answer-temperature-threshold-distribution% :vset 30 150)
  (send %low-answer-temperature-threshold-distribution% :vset 40 5)
  (send %low-answer-temperature-threshold-distribution% :vset 50 2)
  (send %low-answer-temperature-threshold-distribution% :vset 60 1)
  (send %low-answer-temperature-threshold-distribution% :vset 70 1)
  (send %low-answer-temperature-threshold-distribution% :vset 80 1)
  (send %low-answer-temperature-threshold-distribution% :vset 90 1)
  (send %low-answer-temperature-threshold-distribution% :vset 100 1)
  (send %low-answer-temperature-threshold-distribution% :update)	

  (setq %medium-answer-temperature-threshold-distribution% 
	(make-probability-distribution "medium" 101))
  (loop for i from 0 to 100 do 
	(send %medium-answer-temperature-threshold-distribution% :vset i 0))
  (send %medium-answer-temperature-threshold-distribution% :vset 10 1)
  (send %medium-answer-temperature-threshold-distribution% :vset 20 2)
  (send %medium-answer-temperature-threshold-distribution% :vset 30 5)
  (send %medium-answer-temperature-threshold-distribution% :vset 40 150)
  (send %medium-answer-temperature-threshold-distribution% :vset 50 5)
  (send %medium-answer-temperature-threshold-distribution% :vset 60 2)
  (send %medium-answer-temperature-threshold-distribution% :vset 70 1)
  (send %medium-answer-temperature-threshold-distribution% :vset 80 1)
  (send %medium-answer-temperature-threshold-distribution% :vset 90 1)
  (send %medium-answer-temperature-threshold-distribution% :vset 100 1)
  (send %medium-answer-temperature-threshold-distribution% :update)	

  (setq %high-answer-temperature-threshold-distribution% 
	(make-probability-distribution "high" 101))
  (loop for i from 0 to 100 do
	(send %high-answer-temperature-threshold-distribution% :vset i 0))
  (send %high-answer-temperature-threshold-distribution% :vset 10 1)
  (send %high-answer-temperature-threshold-distribution% :vset 20 1)
  (send %high-answer-temperature-threshold-distribution% :vset 30 2)
  (send %high-answer-temperature-threshold-distribution% :vset 40 5)
  (send %high-answer-temperature-threshold-distribution% :vset 50 150)
  (send %high-answer-temperature-threshold-distribution% :vset 60 5)
  (send %high-answer-temperature-threshold-distribution% :vset 70 2)
  (send %high-answer-temperature-threshold-distribution% :vset 80 1)
  (send %high-answer-temperature-threshold-distribution% :vset 90 1)
  (send %high-answer-temperature-threshold-distribution% :vset 100 1)
  (send %high-answer-temperature-threshold-distribution% :update)	

  (setq %very-high-answer-temperature-threshold-distribution% 
        (make-probability-distribution "very-high" 101))
  (loop for i from 0 to 100 do 
	(send %very-high-answer-temperature-threshold-distribution% :vset i 0))
  (send %very-high-answer-temperature-threshold-distribution% :vset 10 1)
  (send %very-high-answer-temperature-threshold-distribution% :vset 20 1)
  (send %very-high-answer-temperature-threshold-distribution% :vset 30 1)
  (send %very-high-answer-temperature-threshold-distribution% :vset 40 2)
  (send %very-high-answer-temperature-threshold-distribution% :vset 50 5)
  (send %very-high-answer-temperature-threshold-distribution% :vset 60 150)
  (send %very-high-answer-temperature-threshold-distribution% :vset 70 5)
  (send %very-high-answer-temperature-threshold-distribution% :vset 80 2)
  (send %very-high-answer-temperature-threshold-distribution% :vset 90 1)
  (send %very-high-answer-temperature-threshold-distribution% :vset 100 1)
  (send %very-high-answer-temperature-threshold-distribution% :update)

;---------------------------------------------
; GRAPHICS CONSTANTS | REMOVED
;---------------------------------------------

;---------------------------------------------
; FONT CONSTANTS | REMOVED
;---------------------------------------------

; CONSTANTS FOR CODELETS

;---------------------------------------------
; CODELET TYPE CONSTANTS | REMOVED
;---------------------------------------------

;---------------------------------------------
; SHORT NAME CONSTANTS | REMOVED
;---------------------------------------------

;---------------------------------------------
; %max-coderack-size% | Coderack.max_codelets
;---------------------------------------------

;---------------------------------------------
; %num-of-urgency-bins% | len(Coderack.bins)
;---------------------------------------------

;---------------------------------------------
; %urgency-value-array% | Bin.urgency
;---------------------------------------------
