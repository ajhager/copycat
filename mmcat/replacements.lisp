(defflavor replacement
    (obj1 obj2)
    (workspace-structure)
    :gettable-instance-variables
    :settable-instance-variables
    :initable-instance-variables)

;---------------------------------------------

(defun make-replacement (obj1 obj2 &aux new-replacement)
; Returns a new replacement.
  (setq new-replacement (make-instance 'replacement :obj1 obj1 :obj2 obj2))
  (if* %workspace-graphics% then (send new-replacement :init-graphics))
  new-replacement)

;---------------------------------------------
; replacement-finder | ReplacementFinder
;---------------------------------------------
